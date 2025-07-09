//! Supervision strategies for fault tolerance

/// Supervision strategy
#[derive(Debug, Clone)]
pub struct SupervisionStrategy {
    /// Restart strategy
    pub restart_strategy: RestartStrategy,

    /// Maximum restarts allowed
    pub max_restarts: u32,

    /// Time window for restart counting
    pub within_seconds: u64,
}

impl Default for SupervisionStrategy {
    fn default() -> Self {
        Self {
            restart_strategy: RestartStrategy::Resume,
            max_restarts: 3,
            within_seconds: 60,
        }
    }
}

/// Restart strategy for failed actors
#[derive(Debug, Clone, Copy)]
pub enum RestartStrategy {
    /// Resume processing next message
    Resume,

    /// Restart the actor
    Restart,

    /// Stop the actor
    Stop,

    /// Escalate to parent supervisor
    Escalate,
}
