//! Configurable thread pool for FluentAi runtime
//!
//! Provides fine-grained control over thread pool behavior for different workloads

use std::sync::Arc;
use std::thread;
use std::time::Duration;
use crossbeam::channel::{bounded, unbounded, Sender, Receiver};
use parking_lot::Mutex;
use std::sync::atomic::{AtomicUsize, AtomicBool, Ordering};

/// Thread pool configuration
#[derive(Debug, Clone)]
pub struct ThreadPoolConfig {
    /// Number of worker threads (None = number of CPU cores)
    pub num_threads: Option<usize>,
    
    /// Thread name prefix
    pub thread_name: String,
    
    /// Stack size for worker threads (None = system default)
    pub stack_size: Option<usize>,
    
    /// Queue capacity (None = unbounded)
    pub queue_capacity: Option<usize>,
    
    /// Keep-alive time for idle threads
    pub keep_alive: Duration,
    
    /// Enable work stealing between threads
    pub work_stealing: bool,
    
    /// Thread priority (platform-specific)
    pub thread_priority: ThreadPriority,
    
    /// CPU affinity settings
    pub cpu_affinity: CpuAffinity,
    
    /// Panic handler
    pub panic_handler: PanicHandler,
}

impl Default for ThreadPoolConfig {
    fn default() -> Self {
        Self {
            num_threads: None,
            thread_name: "fluentai-worker".to_string(),
            stack_size: None,
            queue_capacity: None,
            keep_alive: Duration::from_secs(60),
            work_stealing: true,
            thread_priority: ThreadPriority::Normal,
            cpu_affinity: CpuAffinity::None,
            panic_handler: PanicHandler::Restart,
        }
    }
}

/// Thread priority levels
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThreadPriority {
    Low,
    Normal,
    High,
    Realtime,
}

/// CPU affinity configuration
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CpuAffinity {
    /// No CPU affinity
    None,
    /// Pin each thread to a specific CPU core
    PinToCore,
    /// Restrict to specific CPU set
    CpuSet(Vec<usize>),
    /// NUMA-aware placement
    NumaAware,
}

/// Panic handling strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PanicHandler {
    /// Restart the panicked thread
    Restart,
    /// Abort the entire process
    Abort,
    /// Log and continue
    LogAndContinue,
    /// Custom handler
    Custom,
}

/// A configurable thread pool
pub struct ThreadPool {
    config: ThreadPoolConfig,
    workers: Vec<Worker>,
    sender: Sender<Job>,
    receiver: Arc<Mutex<Receiver<Job>>>,
    shutdown: Arc<AtomicBool>,
    active_count: Arc<AtomicUsize>,
    queued_count: Arc<AtomicUsize>,
    completed_count: Arc<AtomicUsize>,
}

type Job = Box<dyn FnOnce() + Send + 'static>;

struct Worker {
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl ThreadPool {
    /// Create a new thread pool with the given configuration
    pub fn new(config: ThreadPoolConfig) -> Self {
        let num_threads = config.num_threads.unwrap_or_else(num_cpus::get);
        
        let (sender, receiver) = if let Some(capacity) = config.queue_capacity {
            bounded(capacity)
        } else {
            unbounded()
        };
        
        let receiver = Arc::new(Mutex::new(receiver));
        let shutdown = Arc::new(AtomicBool::new(false));
        let active_count = Arc::new(AtomicUsize::new(0));
        let queued_count = Arc::new(AtomicUsize::new(0));
        let completed_count = Arc::new(AtomicUsize::new(0));
        
        let mut workers = Vec::with_capacity(num_threads);
        
        for id in 0..num_threads {
            workers.push(Worker::new(
                id,
                config.clone(),
                Arc::clone(&receiver),
                Arc::clone(&shutdown),
                Arc::clone(&active_count),
                Arc::clone(&queued_count),
                Arc::clone(&completed_count),
            ));
        }
        
        ThreadPool {
            config,
            workers,
            sender,
            receiver,
            shutdown,
            active_count,
            queued_count,
            completed_count,
        }
    }
    
    /// Execute a job in the thread pool
    pub fn execute<F>(&self, job: F) -> Result<(), &'static str>
    where
        F: FnOnce() + Send + 'static,
    {
        if self.shutdown.load(Ordering::Relaxed) {
            return Err("Thread pool is shutting down");
        }
        
        self.queued_count.fetch_add(1, Ordering::Relaxed);
        self.sender.send(Box::new(job))
            .map_err(|_| "Failed to send job to thread pool")
    }
    
    /// Get the number of active threads
    pub fn active_count(&self) -> usize {
        self.active_count.load(Ordering::Relaxed)
    }
    
    /// Get the number of queued jobs
    pub fn queued_count(&self) -> usize {
        self.queued_count.load(Ordering::Relaxed)
    }
    
    /// Get the number of completed jobs
    pub fn completed_count(&self) -> usize {
        self.completed_count.load(Ordering::Relaxed)
    }
    
    /// Resize the thread pool
    pub fn resize(&mut self, new_size: usize) {
        let current_size = self.workers.len();
        
        if new_size > current_size {
            // Add new workers
            for id in current_size..new_size {
                self.workers.push(Worker::new(
                    id,
                    self.config.clone(),
                    Arc::clone(&self.receiver),
                    Arc::clone(&self.shutdown),
                    Arc::clone(&self.active_count),
                    Arc::clone(&self.queued_count),
                    Arc::clone(&self.completed_count),
                ));
            }
        } else if new_size < current_size {
            // Remove excess workers
            // This is tricky - we'll mark them for shutdown
            self.workers.truncate(new_size);
        }
    }
    
    /// Shutdown the thread pool
    pub fn shutdown(self) {
        self.shutdown.store(true, Ordering::Relaxed);
        
        // Send dummy jobs to wake up all threads
        for _ in &self.workers {
            let _ = self.sender.send(Box::new(|| {}));
        }
        
        // Wait for all threads to finish
        for worker in self.workers {
            if let Some(thread) = worker.thread {
                let _ = thread.join();
            }
        }
    }
}

impl Worker {
    fn new(
        id: usize,
        config: ThreadPoolConfig,
        receiver: Arc<Mutex<Receiver<Job>>>,
        shutdown: Arc<AtomicBool>,
        active_count: Arc<AtomicUsize>,
        queued_count: Arc<AtomicUsize>,
        completed_count: Arc<AtomicUsize>,
    ) -> Self {
        let thread_name = format!("{}-{}", config.thread_name, id);
        let mut builder = thread::Builder::new().name(thread_name);
        
        if let Some(stack_size) = config.stack_size {
            builder = builder.stack_size(stack_size);
        }
        
        let thread = builder.spawn(move || {
            // Set thread priority if supported
            #[cfg(target_os = "linux")]
            if config.thread_priority != ThreadPriority::Normal {
                set_thread_priority_linux(config.thread_priority);
            }
            
            // Set CPU affinity if requested
            #[cfg(target_os = "linux")]
            match &config.cpu_affinity {
                CpuAffinity::PinToCore => {
                    set_cpu_affinity_linux(vec![id % num_cpus::get()]);
                }
                CpuAffinity::CpuSet(cpus) => {
                    set_cpu_affinity_linux(cpus.clone());
                }
                _ => {}
            }
            
            loop {
                let job = {
                    let receiver = receiver.lock();
                    receiver.recv_timeout(config.keep_alive)
                };
                
                match job {
                    Ok(job) => {
                        queued_count.fetch_sub(1, Ordering::Relaxed);
                        active_count.fetch_add(1, Ordering::Relaxed);
                        
                        // Handle panics based on configuration
                        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(job));
                        
                        if result.is_err() && config.panic_handler == PanicHandler::Abort {
                            std::process::abort();
                        }
                        
                        active_count.fetch_sub(1, Ordering::Relaxed);
                        completed_count.fetch_add(1, Ordering::Relaxed);
                    }
                    Err(_) => {
                        // Timeout or shutdown
                        if shutdown.load(Ordering::Relaxed) {
                            break;
                        }
                    }
                }
            }
        }).ok();
        
        Worker { id, thread }
    }
}

#[cfg(target_os = "linux")]
fn set_thread_priority_linux(priority: ThreadPriority) {
    use libc::{pthread_self, sched_param, sched_setscheduler, SCHED_NORMAL, SCHED_FIFO};
    
    unsafe {
        let policy = match priority {
            ThreadPriority::Realtime => SCHED_FIFO,
            _ => SCHED_NORMAL,
        };
        
        let priority_value = match priority {
            ThreadPriority::Low => 19,
            ThreadPriority::Normal => 0,
            ThreadPriority::High => -10,
            ThreadPriority::Realtime => 50,
        };
        
        let param = sched_param {
            sched_priority: priority_value,
        };
        
        sched_setscheduler(0, policy, &param);
    }
}

#[cfg(target_os = "linux")]
fn set_cpu_affinity_linux(cpus: Vec<usize>) {
    use libc::{cpu_set_t, CPU_SET, CPU_ZERO, sched_setaffinity};
    use std::mem;
    
    unsafe {
        let mut set: cpu_set_t = mem::zeroed();
        CPU_ZERO(&mut set);
        
        for cpu in cpus {
            CPU_SET(cpu, &mut set);
        }
        
        sched_setaffinity(0, mem::size_of::<cpu_set_t>(), &set);
    }
}

/// Builder for thread pool configuration
pub struct ThreadPoolBuilder {
    config: ThreadPoolConfig,
}

impl ThreadPoolBuilder {
    pub fn new() -> Self {
        Self {
            config: ThreadPoolConfig::default(),
        }
    }
    
    pub fn num_threads(mut self, num: usize) -> Self {
        self.config.num_threads = Some(num);
        self
    }
    
    pub fn thread_name(mut self, name: impl Into<String>) -> Self {
        self.config.thread_name = name.into();
        self
    }
    
    pub fn stack_size(mut self, size: usize) -> Self {
        self.config.stack_size = Some(size);
        self
    }
    
    pub fn queue_capacity(mut self, capacity: usize) -> Self {
        self.config.queue_capacity = Some(capacity);
        self
    }
    
    pub fn keep_alive(mut self, duration: Duration) -> Self {
        self.config.keep_alive = duration;
        self
    }
    
    pub fn work_stealing(mut self, enabled: bool) -> Self {
        self.config.work_stealing = enabled;
        self
    }
    
    pub fn thread_priority(mut self, priority: ThreadPriority) -> Self {
        self.config.thread_priority = priority;
        self
    }
    
    pub fn cpu_affinity(mut self, affinity: CpuAffinity) -> Self {
        self.config.cpu_affinity = affinity;
        self
    }
    
    pub fn panic_handler(mut self, handler: PanicHandler) -> Self {
        self.config.panic_handler = handler;
        self
    }
    
    pub fn build(self) -> ThreadPool {
        ThreadPool::new(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};
    
    #[test]
    fn test_thread_pool_basic() {
        let pool = ThreadPoolBuilder::new()
            .num_threads(4)
            .thread_name("test-worker")
            .build();
        
        let counter = Arc::new(AtomicUsize::new(0));
        
        for _ in 0..100 {
            let counter = Arc::clone(&counter);
            pool.execute(move || {
                counter.fetch_add(1, Ordering::Relaxed);
            }).unwrap();
        }
        
        // Wait for completion
        while pool.completed_count() < 100 {
            thread::sleep(Duration::from_millis(10));
        }
        
        assert_eq!(counter.load(Ordering::Relaxed), 100);
    }
    
    #[test]
    fn test_thread_pool_resize() {
        let mut pool = ThreadPoolBuilder::new()
            .num_threads(2)
            .build();
        
        assert_eq!(pool.workers.len(), 2);
        
        pool.resize(4);
        assert_eq!(pool.workers.len(), 4);
        
        pool.resize(1);
        assert_eq!(pool.workers.len(), 1);
    }
}