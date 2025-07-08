//! Lock-free concurrent data structures for high-performance packet processing
//!
//! # Safety
//!
//! This module contains several unsafe blocks that are necessary for implementing
//! lock-free data structures. Each unsafe block is carefully documented with its
//! safety invariants.

use std::sync::atomic::{AtomicUsize, Ordering};
use std::ptr;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::alloc::{alloc, dealloc, Layout};
use crossbeam_epoch::{self as epoch, Atomic, Owned};
use crossbeam_utils::CachePadded;

/// Lock-free stack using Treiber's algorithm
pub struct LockFreeStack<T> {
    head: Atomic<Node<T>>,
    _marker: PhantomData<T>,
}

struct Node<T> {
    data: T,
    next: Atomic<Node<T>>,
}

impl<T> LockFreeStack<T> {
    /// Create a new empty stack
    pub fn new() -> Self {
        Self {
            head: Atomic::null(),
            _marker: PhantomData,
        }
    }
    
    /// Push a value onto the stack
    pub fn push(&self, value: T) {
        let guard = &epoch::pin();
        let mut new_node = Owned::new(Node {
            data: value,
            next: Atomic::null(),
        });
        
        loop {
            let head = self.head.load(Ordering::Acquire, guard);
            new_node.next.store(head, Ordering::Relaxed);
            
            match self.head.compare_exchange(
                head,
                new_node,
                Ordering::Release,
                Ordering::Acquire,
                guard,
            ) {
                Ok(_) => break,
                Err(e) => new_node = e.new,
            }
        }
    }
    
    /// Pop a value from the stack
    pub fn pop(&self) -> Option<T> {
        let guard = &epoch::pin();
        
        loop {
            let head = self.head.load(Ordering::Acquire, guard);
            
            // SAFETY: The epoch guard ensures the pointer remains valid during this operation.
            // We only dereference the pointer within the guard's scope.
            match unsafe { head.as_ref() } {
                None => return None,
                Some(node) => {
                    let next = node.next.load(Ordering::Acquire, guard);
                    
                    if self.head.compare_exchange(
                        head,
                        next,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    ).is_ok() {
                        // SAFETY: After a successful CAS, we have exclusive ownership of the old head node.
                        // No other thread can access it because we've atomically removed it from the stack.
                        // We use defer_destroy to ensure the node is not freed until all threads have
                        // finished their current epoch operations.
                        unsafe {
                            guard.defer_destroy(head);
                            return Some(ptr::read(&node.data));
                        }
                    }
                }
            }
        }
    }
    
    /// Check if the stack is empty
    pub fn is_empty(&self) -> bool {
        let guard = &epoch::pin();
        self.head.load(Ordering::Acquire, guard).is_null()
    }
}

unsafe impl<T: Send> Send for LockFreeStack<T> {}
unsafe impl<T: Send> Sync for LockFreeStack<T> {}

/// Lock-free queue using Michael & Scott algorithm
pub struct LockFreeQueue<T> {
    head: CachePadded<Atomic<Node<T>>>,
    tail: CachePadded<Atomic<Node<T>>>,
    _marker: PhantomData<T>,
}

impl<T> LockFreeQueue<T> {
    /// Create a new empty queue
    pub fn new() -> Self {
        // SAFETY: The sentinel node's data field is never accessed, so it's safe to leave it
        // uninitialized. The sentinel is just a placeholder to simplify the queue logic.
        let sentinel = Owned::new(Node {
            data: unsafe { MaybeUninit::uninit().assume_init() },
            next: Atomic::null(),
        });
        
        let guard = &epoch::pin();
        let sentinel = sentinel.into_shared(guard);
        
        Self {
            head: CachePadded::new(Atomic::from(sentinel)),
            tail: CachePadded::new(Atomic::from(sentinel)),
            _marker: PhantomData,
        }
    }
    
    /// Enqueue a value
    pub fn enqueue(&self, value: T) {
        let guard = &epoch::pin();
        let new_node = Owned::new(Node {
            data: value,
            next: Atomic::null(),
        }).into_shared(guard);
        
        loop {
            let tail = self.tail.load(Ordering::Acquire, guard);
            // SAFETY: The tail pointer is protected by the epoch guard and cannot be freed
            // while we hold the guard.
            let tail_node = unsafe { tail.deref() };
            let next = tail_node.next.load(Ordering::Acquire, guard);
            
            if tail == self.tail.load(Ordering::Acquire, guard) {
                if next.is_null() {
                    // Try to link new node at the end
                    if tail_node.next.compare_exchange(
                        next,
                        new_node,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    ).is_ok() {
                        // Try to move tail forward
                        let _ = self.tail.compare_exchange(
                            tail,
                            new_node,
                            Ordering::Release,
                            Ordering::Acquire,
                            guard,
                        );
                        break;
                    }
                } else {
                    // Help move tail forward
                    let _ = self.tail.compare_exchange(
                        tail,
                        next,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    );
                }
            }
        }
    }
    
    /// Dequeue a value
    pub fn dequeue(&self) -> Option<T> {
        let guard = &epoch::pin();
        
        loop {
            let head = self.head.load(Ordering::Acquire, guard);
            let tail = self.tail.load(Ordering::Acquire, guard);
            let head_node = unsafe { head.deref() };
            let next = head_node.next.load(Ordering::Acquire, guard);
            
            if head == self.head.load(Ordering::Acquire, guard) {
                if head == tail {
                    if next.is_null() {
                        return None;
                    }
                    // Help move tail forward
                    let _ = self.tail.compare_exchange(
                        tail,
                        next,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    );
                } else {
                    // SAFETY: The next pointer is valid and protected by the epoch guard.
                    // We read the value before the CAS to avoid accessing freed memory.
                    let next_node = unsafe { next.deref() };
                    let value = unsafe { ptr::read(&next_node.data) };
                    
                    if self.head.compare_exchange(
                        head,
                        next,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    ).is_ok() {
                        // SAFETY: After successful CAS, we own the old head (sentinel) node
                        // and can safely schedule it for destruction.
                        unsafe { guard.defer_destroy(head); }
                        return Some(value);
                    }
                }
            }
        }
    }
    
    /// Check if the queue is empty
    pub fn is_empty(&self) -> bool {
        let guard = &epoch::pin();
        let head = self.head.load(Ordering::Acquire, guard);
        let head_node = unsafe { head.deref() };
        head_node.next.load(Ordering::Acquire, guard).is_null()
    }
}

unsafe impl<T: Send> Send for LockFreeQueue<T> {}
unsafe impl<T: Send> Sync for LockFreeQueue<T> {}

/// Bounded lock-free queue using a circular buffer
pub struct BoundedQueue<T> {
    buffer: *mut MaybeUninit<T>,
    capacity: usize,
    mask: usize,
    head: CachePadded<AtomicUsize>,
    tail: CachePadded<AtomicUsize>,
    _marker: PhantomData<T>,
}

impl<T> BoundedQueue<T> {
    /// Create a new bounded queue with given capacity (must be power of 2)
    pub fn new(capacity: usize) -> Self {
        assert!(capacity.is_power_of_two(), "Capacity must be a power of 2");
        
        let layout = Layout::array::<MaybeUninit<T>>(capacity).unwrap();
        // SAFETY: We're allocating memory for an array of MaybeUninit<T>, which doesn't
        // require initialization. The allocated memory is properly aligned for T.
        let buffer = unsafe { alloc(layout) as *mut MaybeUninit<T> };
        
        Self {
            buffer,
            capacity,
            mask: capacity - 1,
            head: CachePadded::new(AtomicUsize::new(0)),
            tail: CachePadded::new(AtomicUsize::new(0)),
            _marker: PhantomData,
        }
    }
    
    /// Try to push a value (returns false if full)
    pub fn try_push(&self, value: T) -> bool {
        let mut tail = self.tail.load(Ordering::Relaxed);
        
        loop {
            let head = self.head.load(Ordering::Acquire);
            
            if tail.wrapping_sub(head) >= self.capacity {
                return false; // Queue is full
            }
            
            match self.tail.compare_exchange_weak(
                tail,
                tail.wrapping_add(1),
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    // SAFETY: After successful CAS, we have exclusive access to the slot at
                    // index (tail & mask). The mask ensures the index is within bounds.
                    // No other thread can write to this slot until the head advances past it.
                    unsafe {
                        let slot = self.buffer.add(tail & self.mask);
                        (*slot).write(value);
                    }
                    return true;
                }
                Err(t) => tail = t,
            }
        }
    }
    
    /// Try to pop a value (returns None if empty)
    pub fn try_pop(&self) -> Option<T> {
        let mut head = self.head.load(Ordering::Relaxed);
        
        loop {
            let tail = self.tail.load(Ordering::Acquire);
            
            if head >= tail {
                return None; // Queue is empty
            }
            
            match self.head.compare_exchange_weak(
                head,
                head.wrapping_add(1),
                Ordering::Release,
                Ordering::Relaxed,
            ) {
                Ok(_) => {
                    // SAFETY: After successful CAS, we have exclusive access to read from
                    // the slot at index (head & mask). The value was previously written
                    // by try_push, so it's initialized. The mask ensures bounds safety.
                    unsafe {
                        let slot = self.buffer.add(head & self.mask);
                        return Some((*slot).assume_init_read());
                    }
                }
                Err(h) => head = h,
            }
        }
    }
    
    /// Get the current size (approximate)
    pub fn len(&self) -> usize {
        let tail = self.tail.load(Ordering::Relaxed);
        let head = self.head.load(Ordering::Relaxed);
        tail.wrapping_sub(head)
    }
    
    /// Check if empty (approximate)
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    /// Check if full (approximate)
    pub fn is_full(&self) -> bool {
        self.len() >= self.capacity
    }
}

impl<T> Drop for BoundedQueue<T> {
    fn drop(&mut self) {
        // Clean up any remaining items
        while self.try_pop().is_some() {}
        
        // Deallocate buffer
        let layout = Layout::array::<MaybeUninit<T>>(self.capacity).unwrap();
        // SAFETY: The buffer was allocated with the same layout, and we've ensured
        // all items have been consumed by calling try_pop in a loop.
        unsafe {
            dealloc(self.buffer as *mut u8, layout);
        }
    }
}

unsafe impl<T: Send> Send for BoundedQueue<T> {}
unsafe impl<T: Send> Sync for BoundedQueue<T> {}

/// Work-stealing deque for task scheduling
pub struct WorkStealingDeque<T> {
    buffer: *mut MaybeUninit<T>,
    capacity: usize,
    mask: usize,
    top: AtomicUsize,
    bottom: AtomicUsize,
    _marker: PhantomData<T>,
}

impl<T> WorkStealingDeque<T> {
    /// Create a new work-stealing deque
    pub fn new(capacity: usize) -> Self {
        assert!(capacity.is_power_of_two(), "Capacity must be a power of 2");
        
        let layout = Layout::array::<MaybeUninit<T>>(capacity).unwrap();
        // SAFETY: We're allocating memory for an array of MaybeUninit<T>, which doesn't
        // require initialization. The allocated memory is properly aligned for T.
        let buffer = unsafe { alloc(layout) as *mut MaybeUninit<T> };
        
        Self {
            buffer,
            capacity,
            mask: capacity - 1,
            top: AtomicUsize::new(0),
            bottom: AtomicUsize::new(0),
            _marker: PhantomData,
        }
    }
    
    /// Push work to the bottom (owner only)
    /// Returns false if the deque is full
    pub fn push(&self, value: T) -> bool {
        let bottom = self.bottom.load(Ordering::Relaxed);
        let top = self.top.load(Ordering::Acquire);
        
        if bottom.wrapping_sub(top) >= self.capacity {
            return false; // Deque is full
        }
        
        // SAFETY: We checked that the deque has space, and only the owner thread
        // can push to the bottom, so we have exclusive access to this slot.
        // The mask ensures the index is within bounds.
        unsafe {
            let slot = self.buffer.add(bottom & self.mask);
            (*slot).write(value);
        }
        
        self.bottom.store(bottom.wrapping_add(1), Ordering::Release);
        true
    }
    
    /// Pop work from the bottom (owner only)
    pub fn pop(&self) -> Option<T> {
        let bottom = self.bottom.load(Ordering::Relaxed);
        if bottom == 0 {
            return None;
        }
        
        let new_bottom = bottom - 1;
        self.bottom.store(new_bottom, Ordering::Relaxed);
        
        let top = self.top.load(Ordering::Relaxed);
        
        if new_bottom < top {
            self.bottom.store(bottom, Ordering::Relaxed);
            return None;
        }
        
        // SAFETY: Only the owner can pop from bottom, and we've verified the deque
        // isn't empty. The slot contains initialized data from a previous push.
        // The mask ensures bounds safety.
        unsafe {
            let slot = self.buffer.add(new_bottom & self.mask);
            let value = (*slot).assume_init_read();
            
            if new_bottom == top {
                // Last item - need to synchronize with stealers
                if self.top.compare_exchange(
                    top,
                    top.wrapping_add(1),
                    Ordering::SeqCst,
                    Ordering::Relaxed,
                ).is_err() {
                    // Failed - a stealer got it
                    self.bottom.store(bottom, Ordering::Relaxed);
                    return None;
                }
                self.bottom.store(bottom, Ordering::Relaxed);
            }
            
            Some(value)
        }
    }
    
    /// Steal work from the top (other threads)
    pub fn steal(&self) -> Option<T> {
        let top = self.top.load(Ordering::Acquire);
        let bottom = self.bottom.load(Ordering::Acquire);
        
        if top >= bottom {
            return None;
        }
        
        // SAFETY: We've verified the deque isn't empty. Multiple threads may steal
        // concurrently, but the CAS below ensures only one succeeds. The slot contains
        // initialized data from a previous push. The mask ensures bounds safety.
        unsafe {
            let slot = self.buffer.add(top & self.mask);
            let value = (*slot).assume_init_read();
            
            if self.top.compare_exchange(
                top,
                top.wrapping_add(1),
                Ordering::SeqCst,
                Ordering::Relaxed,
            ).is_ok() {
                Some(value)
            } else {
                None
            }
        }
    }
    
    /// Check if empty
    pub fn is_empty(&self) -> bool {
        let top = self.top.load(Ordering::Relaxed);
        let bottom = self.bottom.load(Ordering::Relaxed);
        bottom <= top
    }
}

impl<T> Drop for WorkStealingDeque<T> {
    fn drop(&mut self) {
        // Clean up remaining items
        while self.pop().is_some() {}
        
        // Deallocate buffer
        let layout = Layout::array::<MaybeUninit<T>>(self.capacity).unwrap();
        // SAFETY: The buffer was allocated with the same layout in new(), and we've
        // ensured all items have been consumed by calling pop in a loop. We have
        // exclusive access to the deque in drop.
        unsafe {
            dealloc(self.buffer as *mut u8, layout);
        }
    }
}

unsafe impl<T: Send> Send for WorkStealingDeque<T> {}
unsafe impl<T: Send> Sync for WorkStealingDeque<T> {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::thread;
    use std::sync::Arc;
    
    #[test]
    fn test_lock_free_stack() {
        let stack = Arc::new(LockFreeStack::new());
        let stack2 = stack.clone();
        
        // Push from one thread
        let handle = thread::spawn(move || {
            for i in 0..100 {
                stack2.push(i);
            }
        });
        
        handle.join().unwrap();
        
        // Pop from main thread
        let mut count = 0;
        while stack.pop().is_some() {
            count += 1;
        }
        
        assert_eq!(count, 100);
    }
    
    #[test]
    fn test_lock_free_queue() {
        let queue = Arc::new(LockFreeQueue::new());
        let queue2 = queue.clone();
        
        // Enqueue from one thread
        let handle = thread::spawn(move || {
            for i in 0..100 {
                queue2.enqueue(i);
            }
        });
        
        handle.join().unwrap();
        
        // Dequeue from main thread
        let mut values = vec![];
        while let Some(v) = queue.dequeue() {
            values.push(v);
        }
        
        assert_eq!(values.len(), 100);
        // Queue maintains FIFO order
        for (i, &v) in values.iter().enumerate() {
            assert_eq!(v, i as i32);
        }
    }
    
    #[test]
    fn test_work_stealing_deque() {
        let deque = WorkStealingDeque::new(16);
        
        // Test that push returns false when full
        for i in 0..16 {
            assert!(deque.push(i), "Failed to push item {}", i);
        }
        assert!(!deque.push(100), "Push should fail when deque is full");
        
        // Pop all items
        for i in (0..16).rev() {
            assert_eq!(deque.pop(), Some(i));
        }
        
        assert!(deque.is_empty());
    }
    
    #[test]
    fn test_bounded_queue() {
        let queue = BoundedQueue::new(16);
        
        // Fill the queue
        for i in 0..16 {
            assert!(queue.try_push(i));
        }
        
        // Should be full
        assert!(!queue.try_push(100));
        
        // Pop half
        for i in 0..8 {
            assert_eq!(queue.try_pop(), Some(i));
        }
        
        // Push more
        for i in 16..24 {
            assert!(queue.try_push(i));
        }
        
        // Pop remaining
        for i in 8..24 {
            assert_eq!(queue.try_pop(), Some(i));
        }
        
        assert!(queue.is_empty());
    }
}