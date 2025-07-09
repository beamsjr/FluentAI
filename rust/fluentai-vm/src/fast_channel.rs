//! High-performance channel implementation using lock-free queues

use crate::concurrent::{BoundedQueue, LockFreeQueue};
use anyhow::{anyhow, Result};
use fluentai_core::value::Value;
use parking_lot::{Condvar, Mutex};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;

/// Channel mode
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChannelMode {
    /// Synchronous (rendezvous) channel
    Sync,
    /// Buffered channel with specific capacity
    Buffered(usize),
    /// Unbounded channel
    Unbounded,
}

/// Fast channel implementation
pub struct FastChannel {
    _mode: ChannelMode,
    inner: ChannelInner,
}

enum ChannelInner {
    Sync(SyncChannel),
    Buffered(BufferedChannel),
    Unbounded(UnboundedChannel),
}

/// Synchronous (rendezvous) channel
struct SyncChannel {
    value: Mutex<Option<Value>>,
    has_sender: Condvar,
    has_receiver: Condvar,
    closed: AtomicBool,
}

/// Buffered channel using lock-free bounded queue
struct BufferedChannel {
    queue: Arc<BoundedQueue<Value>>,
    sender_count: Arc<AtomicUsize>,
    _receiver_count: Arc<AtomicUsize>,
    closed: Arc<AtomicBool>,
    // Notification for blocking operations
    not_empty: Arc<Condvar>,
    not_full: Arc<Condvar>,
    mutex: Arc<Mutex<()>>,
}

/// Unbounded channel using lock-free queue
struct UnboundedChannel {
    queue: Arc<LockFreeQueue<Value>>,
    sender_count: Arc<AtomicUsize>,
    _receiver_count: Arc<AtomicUsize>,
    closed: Arc<AtomicBool>,
}

impl FastChannel {
    /// Create a new channel with specified mode
    pub fn new(mode: ChannelMode) -> Self {
        let inner = match mode {
            ChannelMode::Sync => ChannelInner::Sync(SyncChannel {
                value: Mutex::new(None),
                has_sender: Condvar::new(),
                has_receiver: Condvar::new(),
                closed: AtomicBool::new(false),
            }),
            ChannelMode::Buffered(capacity) => {
                // Round up to power of 2 for BoundedQueue
                let capacity = capacity.next_power_of_two();
                ChannelInner::Buffered(BufferedChannel {
                    queue: Arc::new(BoundedQueue::new(capacity)),
                    sender_count: Arc::new(AtomicUsize::new(1)),
                    _receiver_count: Arc::new(AtomicUsize::new(1)),
                    closed: Arc::new(AtomicBool::new(false)),
                    not_empty: Arc::new(Condvar::new()),
                    not_full: Arc::new(Condvar::new()),
                    mutex: Arc::new(Mutex::new(())),
                })
            }
            ChannelMode::Unbounded => ChannelInner::Unbounded(UnboundedChannel {
                queue: Arc::new(LockFreeQueue::new()),
                sender_count: Arc::new(AtomicUsize::new(1)),
                _receiver_count: Arc::new(AtomicUsize::new(1)),
                closed: Arc::new(AtomicBool::new(false)),
            }),
        };

        Self { _mode: mode, inner }
    }

    /// Send a value on the channel
    pub fn send(&self, value: Value) -> Result<()> {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.send(value),
            ChannelInner::Buffered(ch) => ch.send(value),
            ChannelInner::Unbounded(ch) => ch.send(value),
        }
    }

    /// Try to send without blocking
    pub fn try_send(&self, value: Value) -> Result<()> {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.try_send(value),
            ChannelInner::Buffered(ch) => ch.try_send(value),
            ChannelInner::Unbounded(ch) => ch.send(value), // Always succeeds
        }
    }

    /// Receive a value from the channel
    pub fn recv(&self) -> Result<Value> {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.recv(),
            ChannelInner::Buffered(ch) => ch.recv(),
            ChannelInner::Unbounded(ch) => ch.recv(),
        }
    }

    /// Try to receive without blocking
    pub fn try_recv(&self) -> Result<Option<Value>> {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.try_recv(),
            ChannelInner::Buffered(ch) => ch.try_recv(),
            ChannelInner::Unbounded(ch) => ch.try_recv(),
        }
    }

    /// Close the channel
    pub fn close(&self) {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.close(),
            ChannelInner::Buffered(ch) => ch.close(),
            ChannelInner::Unbounded(ch) => ch.close(),
        }
    }

    /// Check if channel is closed
    pub fn is_closed(&self) -> bool {
        match &self.inner {
            ChannelInner::Sync(ch) => ch.closed.load(Ordering::Acquire),
            ChannelInner::Buffered(ch) => ch.closed.load(Ordering::Acquire),
            ChannelInner::Unbounded(ch) => ch.closed.load(Ordering::Acquire),
        }
    }
}

impl SyncChannel {
    fn send(&self, value: Value) -> Result<()> {
        if self.closed.load(Ordering::Acquire) {
            return Err(anyhow!("Channel closed"));
        }

        let mut slot = self.value.lock();

        // Wait for receiver to be ready
        while slot.is_some() && !self.closed.load(Ordering::Acquire) {
            self.has_receiver.wait(&mut slot);
        }

        if self.closed.load(Ordering::Acquire) {
            return Err(anyhow!("Channel closed"));
        }

        *slot = Some(value);
        self.has_sender.notify_one();
        Ok(())
    }

    fn try_send(&self, value: Value) -> Result<()> {
        if self.closed.load(Ordering::Acquire) {
            return Err(anyhow!("Channel closed"));
        }

        let mut slot = self.value.lock();
        if slot.is_some() {
            return Err(anyhow!("Channel full"));
        }

        *slot = Some(value);
        self.has_sender.notify_one();
        Ok(())
    }

    fn recv(&self) -> Result<Value> {
        let mut slot = self.value.lock();

        // Wait for sender to provide value
        while slot.is_none() && !self.closed.load(Ordering::Acquire) {
            self.has_sender.wait(&mut slot);
        }

        match slot.take() {
            Some(value) => {
                self.has_receiver.notify_one();
                Ok(value)
            }
            None => Err(anyhow!("Channel closed")),
        }
    }

    fn try_recv(&self) -> Result<Option<Value>> {
        let mut slot = self.value.lock();
        match slot.take() {
            Some(value) => {
                self.has_receiver.notify_one();
                Ok(Some(value))
            }
            None => {
                if self.closed.load(Ordering::Acquire) {
                    Err(anyhow!("Channel closed"))
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn close(&self) {
        self.closed.store(true, Ordering::Release);
        self.has_sender.notify_all();
        self.has_receiver.notify_all();
    }
}

impl BufferedChannel {
    fn send(&self, value: Value) -> Result<()> {
        loop {
            if self.closed.load(Ordering::Acquire) {
                return Err(anyhow!("Channel closed"));
            }

            if self.queue.try_push(value.clone()) {
                self.not_empty.notify_one();
                return Ok(());
            }

            // Queue is full, wait
            let mut guard = self.mutex.lock();
            if self.queue.is_full() && !self.closed.load(Ordering::Acquire) {
                self.not_full.wait(&mut guard);
            }
        }
    }

    fn try_send(&self, value: Value) -> Result<()> {
        if self.closed.load(Ordering::Acquire) {
            return Err(anyhow!("Channel closed"));
        }

        if self.queue.try_push(value) {
            self.not_empty.notify_one();
            Ok(())
        } else {
            Err(anyhow!("Channel full"))
        }
    }

    fn recv(&self) -> Result<Value> {
        loop {
            if let Some(value) = self.queue.try_pop() {
                self.not_full.notify_one();
                return Ok(value);
            }

            if self.closed.load(Ordering::Acquire) && self.queue.is_empty() {
                return Err(anyhow!("Channel closed"));
            }

            // Queue is empty, wait
            let mut guard = self.mutex.lock();
            if self.queue.is_empty() && !self.closed.load(Ordering::Acquire) {
                self.not_empty.wait(&mut guard);
            }
        }
    }

    fn try_recv(&self) -> Result<Option<Value>> {
        if let Some(value) = self.queue.try_pop() {
            self.not_full.notify_one();
            Ok(Some(value))
        } else if self.closed.load(Ordering::Acquire) {
            Err(anyhow!("Channel closed"))
        } else {
            Ok(None)
        }
    }

    fn close(&self) {
        self.closed.store(true, Ordering::Release);
        self.not_empty.notify_all();
        self.not_full.notify_all();
    }
}

impl UnboundedChannel {
    fn send(&self, value: Value) -> Result<()> {
        if self.closed.load(Ordering::Acquire) {
            return Err(anyhow!("Channel closed"));
        }

        self.queue.enqueue(value);
        Ok(())
    }

    fn recv(&self) -> Result<Value> {
        // Spin-wait for unbounded channel (could be improved with parking)
        loop {
            if let Some(value) = self.queue.dequeue() {
                return Ok(value);
            }

            if self.closed.load(Ordering::Acquire) && self.queue.is_empty() {
                return Err(anyhow!("Channel closed"));
            }

            std::hint::spin_loop();
        }
    }

    fn try_recv(&self) -> Result<Option<Value>> {
        if let Some(value) = self.queue.dequeue() {
            Ok(Some(value))
        } else if self.closed.load(Ordering::Acquire) {
            Err(anyhow!("Channel closed"))
        } else {
            Ok(None)
        }
    }

    fn close(&self) {
        self.closed.store(true, Ordering::Release);
    }
}

/// Channel handle for sending
pub struct Sender {
    channel: Arc<FastChannel>,
}

impl Sender {
    pub fn send(&self, value: Value) -> Result<()> {
        self.channel.send(value)
    }

    pub fn try_send(&self, value: Value) -> Result<()> {
        self.channel.try_send(value)
    }
}

impl Clone for Sender {
    fn clone(&self) -> Self {
        // Increment sender count based on channel type
        match &self.channel.inner {
            ChannelInner::Buffered(ch) => {
                ch.sender_count.fetch_add(1, Ordering::Relaxed);
            }
            ChannelInner::Unbounded(ch) => {
                ch.sender_count.fetch_add(1, Ordering::Relaxed);
            }
            _ => {}
        }
        Self {
            channel: self.channel.clone(),
        }
    }
}

impl Drop for Sender {
    fn drop(&mut self) {
        // Decrement sender count and close if last
        match &self.channel.inner {
            ChannelInner::Buffered(ch) => {
                if ch.sender_count.fetch_sub(1, Ordering::Release) == 1 {
                    self.channel.close();
                }
            }
            ChannelInner::Unbounded(ch) => {
                if ch.sender_count.fetch_sub(1, Ordering::Release) == 1 {
                    self.channel.close();
                }
            }
            _ => {}
        }
    }
}

/// Channel handle for receiving
pub struct Receiver {
    channel: Arc<FastChannel>,
}

impl Receiver {
    pub fn recv(&self) -> Result<Value> {
        self.channel.recv()
    }

    pub fn try_recv(&self) -> Result<Option<Value>> {
        self.channel.try_recv()
    }
}

/// Create a new channel pair
pub fn channel(mode: ChannelMode) -> (Sender, Receiver) {
    let channel = Arc::new(FastChannel::new(mode));
    (
        Sender {
            channel: channel.clone(),
        },
        Receiver { channel },
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;

    #[test]
    fn test_sync_channel() {
        let (tx, rx) = channel(ChannelMode::Sync);

        thread::spawn(move || {
            tx.send(Value::Integer(42)).unwrap();
        });

        assert_eq!(rx.recv().unwrap(), Value::Integer(42));
    }

    #[test]
    fn test_buffered_channel() {
        let (tx, rx) = channel(ChannelMode::Buffered(4));

        // Fill buffer
        for i in 0..4 {
            tx.send(Value::Integer(i)).unwrap();
        }

        // Should block on 5th
        assert!(tx.try_send(Value::Integer(5)).is_err());

        // Receive all
        for i in 0..4 {
            assert_eq!(rx.recv().unwrap(), Value::Integer(i));
        }
    }

    #[test]
    fn test_unbounded_channel() {
        let (tx, rx) = channel(ChannelMode::Unbounded);

        // Send many values
        for i in 0..1000 {
            tx.send(Value::Integer(i)).unwrap();
        }

        // Receive all
        for i in 0..1000 {
            assert_eq!(rx.recv().unwrap(), Value::Integer(i));
        }
    }
}
