//! Mailbox implementation for actors

/// Mailbox configuration
#[derive(Debug, Clone)]
pub struct MailboxConfig {
    /// Mailbox capacity
    pub capacity: usize,
    
    /// Whether to use unbounded mailbox
    pub unbounded: bool,
}

impl Default for MailboxConfig {
    fn default() -> Self {
        Self {
            capacity: 1000,
            unbounded: false,
        }
    }
}

/// Mailbox trait
pub trait Mailbox<M>: Send + Sync {
    /// Send a message to the mailbox
    fn send(&self, msg: M) -> Result<(), M>;
    
    /// Try to receive a message
    fn try_recv(&self) -> Option<M>;
}