//! Actor model implementation for FluentAi
//!
//! Provides Erlang/Akka-style actors with supervision, fault tolerance,
//! and distributed communication capabilities.

use std::any::Any;
use std::sync::Arc;
use std::time::Duration;
use async_trait::async_trait;
use dashmap::DashMap;
use parking_lot::RwLock;
use tokio::sync::{mpsc, oneshot};
use tokio::time::timeout;
use uuid::Uuid;
use anyhow::{anyhow, Result};
use tracing::error;

pub mod mailbox;
pub mod router;
pub mod supervision;
pub mod cluster;
pub mod persistence;

use mailbox::MailboxConfig;
use supervision::{SupervisionStrategy, RestartStrategy};

/// Actor reference for sending messages
#[derive(Debug, Clone)]
pub struct ActorRef<M: Message> {
    id: ActorId,
    sender: mpsc::Sender<Envelope<M>>,
    system: Arc<ActorSystem>,
}

/// Unique actor identifier
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ActorId(Uuid);

impl ActorId {
    fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

/// Message trait that all actor messages must implement
pub trait Message: Send + Sync + Clone + 'static {}

/// Envelope wrapping a message with metadata
struct Envelope<M: Message> {
    message: M,
    sender: Option<ActorId>,
    correlation_id: Option<Uuid>,
}

/// Actor trait that all actors must implement
#[async_trait]
pub trait Actor: Send + Sync + 'static {
    type Message: Message;
    
    /// Called when actor starts
    async fn pre_start(&mut self) -> Result<()> {
        Ok(())
    }
    
    /// Called when actor stops
    async fn post_stop(&mut self) -> Result<()> {
        Ok(())
    }
    
    /// Handle a message
    async fn receive(&mut self, msg: Self::Message, ctx: &mut ActorContext<Self::Message>) -> Result<()>;
    
    /// Called when actor restarts
    async fn pre_restart(&mut self, _reason: &str) -> Result<()> {
        Ok(())
    }
    
    /// Called after actor restarts
    async fn post_restart(&mut self) -> Result<()> {
        Ok(())
    }
}

/// Actor context providing access to self and system
pub struct ActorContext<M: Message> {
    self_ref: ActorRef<M>,
    system: Arc<ActorSystem>,
    parent: Option<ActorId>,
    children: DashMap<String, ActorId>,
    watchers: DashMap<ActorId, ()>,
    timers: DashMap<String, tokio::task::JoinHandle<()>>,
}

impl<M: Message> ActorContext<M> {
    /// Get reference to self
    pub fn self_ref(&self) -> &ActorRef<M> {
        &self.self_ref
    }
    
    /// Get actor system
    pub fn system(&self) -> &Arc<ActorSystem> {
        &self.system
    }
    
    /// Send message to another actor
    pub async fn send<M2: Message>(&self, to: &ActorRef<M2>, msg: M2) -> Result<()> {
        to.tell(msg).await
    }
    
    /// Ask another actor (request-reply)
    pub async fn ask<M2: Message, R: Message>(&self, to: &ActorRef<M2>, msg: M2, timeout: Duration) -> Result<R> {
        to.ask(msg, timeout).await
    }
    
    /// Spawn a child actor
    pub async fn spawn_child<A: Actor>(&mut self, name: &str, actor: A) -> Result<ActorRef<A::Message>> {
        let child_ref = self.system.spawn(actor).await?;
        self.children.insert(name.to_string(), child_ref.id.clone());
        Ok(child_ref)
    }
    
    /// Stop a child actor
    pub async fn stop_child(&mut self, name: &str) -> Result<()> {
        if let Some((_, child_id)) = self.children.remove(name) {
            self.system.stop_actor(&child_id).await?;
        }
        Ok(())
    }
    
    /// Watch another actor for termination
    pub fn watch(&mut self, actor: &ActorId) {
        self.watchers.insert(actor.clone(), ());
    }
    
    /// Stop watching an actor
    pub fn unwatch(&mut self, actor: &ActorId) {
        self.watchers.remove(actor);
    }
    
    /// Schedule a message to self
    pub fn schedule_once(&mut self, name: String, delay: Duration, msg: M) 
    where M: Clone 
    {
        let self_ref = self.self_ref.clone();
        let handle = tokio::spawn(async move {
            tokio::time::sleep(delay).await;
            let _ = self_ref.tell(msg).await;
        });
        self.timers.insert(name, handle);
    }
    
    /// Cancel a scheduled message
    pub fn cancel_timer(&mut self, name: &str) {
        if let Some((_, handle)) = self.timers.remove(name) {
            handle.abort();
        }
    }
}

/// Actor system managing all actors
#[derive(Debug)]
pub struct ActorSystem {
    name: String,
    actors: DashMap<ActorId, ActorHandle>,
    config: ActorSystemConfig,
    shutdown: RwLock<bool>,
}

/// Actor system configuration
#[derive(Debug, Clone)]
pub struct ActorSystemConfig {
    /// Default mailbox size
    pub default_mailbox_size: usize,
    
    /// Default message timeout
    pub default_timeout: Duration,
    
    /// Enable persistence
    pub enable_persistence: bool,
    
    /// Enable clustering
    pub enable_clustering: bool,
    
    /// Thread pool size
    pub thread_pool_size: usize,
}

impl Default for ActorSystemConfig {
    fn default() -> Self {
        Self {
            default_mailbox_size: 1000,
            default_timeout: Duration::from_secs(5),
            enable_persistence: false,
            enable_clustering: false,
            thread_pool_size: num_cpus::get(),
        }
    }
}

/// Internal actor handle
#[derive(Debug)]
struct ActorHandle {
    task: tokio::task::JoinHandle<()>,
    sender: mpsc::Sender<Box<dyn Any + Send>>,
    supervision: SupervisionStrategy,
}

impl ActorSystem {
    /// Create a new actor system
    pub fn new(name: impl Into<String>, config: ActorSystemConfig) -> Arc<Self> {
        Arc::new(Self {
            name: name.into(),
            actors: DashMap::new(),
            config,
            shutdown: RwLock::new(false),
        })
    }
    
    /// Spawn a new actor
    pub async fn spawn<A: Actor>(self: &Arc<Self>, actor: A) -> Result<ActorRef<A::Message>> {
        self.spawn_with_config(actor, None, SupervisionStrategy::default()).await
    }
    
    /// Spawn actor with custom configuration
    pub async fn spawn_with_config<A: Actor>(
        self: &Arc<Self>,
        mut actor: A,
        mailbox_config: Option<MailboxConfig>,
        supervision: SupervisionStrategy,
    ) -> Result<ActorRef<A::Message>> {
        let id = ActorId::new();
        let mailbox_size = mailbox_config
            .as_ref()
            .map(|c| c.capacity)
            .unwrap_or(self.config.default_mailbox_size);
        
        let (tx, mut rx) = mpsc::channel::<Envelope<A::Message>>(mailbox_size);
        
        let actor_ref = ActorRef {
            id: id.clone(),
            sender: tx.clone(),
            system: Arc::clone(self),
        };
        
        let mut context = ActorContext {
            self_ref: actor_ref.clone(),
            system: Arc::clone(self),
            parent: None,
            children: DashMap::new(),
            watchers: DashMap::new(),
            timers: DashMap::new(),
        };
        
        // Pre-start hook
        actor.pre_start().await?;
        
        let system = Arc::clone(self);
        let actor_id = id.clone();
        
        // Spawn actor task
        let task = tokio::spawn(async move {
            while let Some(envelope) = rx.recv().await {
                match actor.receive(envelope.message, &mut context).await {
                    Ok(()) => {}
                    Err(e) => {
                        error!("Actor {} error: {}", actor_id.0, e);
                        // Handle supervision
                        match supervision.restart_strategy {
                            RestartStrategy::Resume => continue,
                            RestartStrategy::Restart => {
                                if let Err(e) = actor.pre_restart(&e.to_string()).await {
                                    error!("Pre-restart failed: {}", e);
                                    break;
                                }
                                if let Err(e) = actor.post_restart().await {
                                    error!("Post-restart failed: {}", e);
                                    break;
                                }
                            }
                            RestartStrategy::Stop => break,
                            RestartStrategy::Escalate => {
                                // Escalate to parent
                                break;
                            }
                        }
                    }
                }
            }
            
            // Post-stop hook
            let _ = actor.post_stop().await;
            
            // Notify watchers
            for watcher_id in context.watchers.iter() {
                // Send termination message
            }
        });
        
        // Create a dummy sender for type erasure
        let (erased_sender, _) = mpsc::channel::<Box<dyn Any + Send>>(1);
        
        self.actors.insert(id.clone(), ActorHandle {
            task,
            sender: erased_sender,
            supervision,
        });
        
        Ok(actor_ref)
    }
    
    /// Stop an actor
    pub async fn stop_actor(&self, id: &ActorId) -> Result<()> {
        if let Some((_, handle)) = self.actors.remove(id) {
            handle.task.abort();
            Ok(())
        } else {
            Err(anyhow!("Actor not found"))
        }
    }
    
    /// Shutdown the actor system
    pub async fn shutdown(&self) -> Result<()> {
        *self.shutdown.write() = true;
        
        // Stop all actors
        let handles: Vec<_> = self.actors.iter()
            .map(|entry| entry.value().task.abort_handle())
            .collect();
        
        for handle in handles {
            handle.abort();
        }
        
        Ok(())
    }
}

impl<M: Message> ActorRef<M> {
    /// Send a message (fire and forget)
    pub async fn tell(&self, msg: M) -> Result<()> {
        let envelope = Envelope {
            message: msg,
            sender: None,
            correlation_id: None,
        };
        
        self.sender.send(envelope).await
            .map_err(|_| anyhow!("Failed to send message"))
    }
    
    /// Ask pattern (request-reply)
    pub async fn ask<R: Message>(&self, msg: M, timeout_duration: Duration) -> Result<R> {
        let (_tx, rx) = oneshot::channel();
        
        // This would need special handling for reply
        // For now, simplified implementation
        self.tell(msg).await?;
        
        timeout(timeout_duration, rx)
            .await
            .map_err(|_| anyhow!("Ask timeout"))?
            .map_err(|_| anyhow!("Ask cancelled"))
    }
}

/// System messages for actor lifecycle
#[derive(Debug, Clone)]
pub enum SystemMessage {
    /// Actor terminated
    Terminated(ActorId),
    
    /// Supervision failure
    SupervisionFailure {
        child: ActorId,
        reason: String,
    },
    
    /// Watch request
    Watch(ActorId),
    
    /// Unwatch request
    Unwatch(ActorId),
}

impl Message for SystemMessage {}

/// Behaviors for common actor patterns
pub mod behaviors {
    use super::*;
    
    /// Finite State Machine behavior
    #[async_trait]
    pub trait FSM: Actor {
        type State: Send + Sync + Clone;
        
        fn current_state(&self) -> &Self::State;
        fn set_state(&mut self, state: Self::State);
        
        async fn on_transition(&mut self, _from: &Self::State, _to: &Self::State) -> Result<()> {
            Ok(())
        }
    }
    
    /// Event sourcing behavior
    #[async_trait]
    pub trait EventSourced: Actor {
        type Event: Send + Sync + Clone;
        type State: Send + Sync + Clone;
        
        fn apply_event(&mut self, event: &Self::Event) -> Result<()>;
        fn get_state(&self) -> &Self::State;
    }
}

/// Router patterns for message distribution
pub mod routers {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    
    /// Round-robin router
    pub struct RoundRobinRouter<M: Message> {
        routees: Vec<ActorRef<M>>,
        next: AtomicUsize,
    }
    
    impl<M: Message> RoundRobinRouter<M> {
        pub fn new(routees: Vec<ActorRef<M>>) -> Self {
            Self {
                routees,
                next: AtomicUsize::new(0),
            }
        }
        
        pub async fn route(&self, msg: M) -> Result<()> {
            if self.routees.is_empty() {
                return Err(anyhow!("No routees available"));
            }
            
            let idx = self.next.fetch_add(1, Ordering::Relaxed) % self.routees.len();
            self.routees[idx].tell(msg).await
        }
    }
    
    /// Broadcast router
    pub struct BroadcastRouter<M: Message> {
        routees: Vec<ActorRef<M>>,
    }
    
    impl<M: Message + Clone> BroadcastRouter<M> {
        pub fn new(routees: Vec<ActorRef<M>>) -> Self {
            Self { routees }
        }
        
        pub async fn route(&self, msg: M) -> Result<()> {
            let futures: Vec<_> = self.routees.iter()
                .map(|r| r.tell(msg.clone()))
                .collect();
            
            futures::future::try_join_all(futures).await?;
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[derive(Debug, Clone)]
    struct TestMessage(String);
    impl Message for TestMessage {}
    
    struct TestActor {
        state: String,
    }
    
    #[async_trait]
    impl Actor for TestActor {
        type Message = TestMessage;
        
        async fn receive(&mut self, msg: Self::Message, _ctx: &mut ActorContext<Self::Message>) -> Result<()> {
            self.state = msg.0;
            Ok(())
        }
    }
    
    #[tokio::test]
    async fn test_actor_spawn_and_tell() {
        let system = ActorSystem::new("test", ActorSystemConfig::default());
        
        let actor = TestActor {
            state: String::new(),
        };
        
        let actor_ref = system.spawn(actor).await.unwrap();
        actor_ref.tell(TestMessage("Hello".to_string())).await.unwrap();
        
        // Give actor time to process
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    
    #[tokio::test]
    async fn test_round_robin_router() {
        let system = ActorSystem::new("test", ActorSystemConfig::default());
        
        let mut routees = Vec::new();
        for _ in 0..3 {
            let actor = TestActor { state: String::new() };
            routees.push(system.spawn(actor).await.unwrap());
        }
        
        let router = routers::RoundRobinRouter::new(routees);
        
        for i in 0..6 {
            router.route(TestMessage(format!("Message {}", i))).await.unwrap();
        }
    }
}