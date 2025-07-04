//! Lock-free concurrent data structures for high-performance packet processing

use std::sync::atomic::{AtomicPtr, AtomicUsize, Ordering};
use std::ptr::{self, NonNull};
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::alloc::{alloc, dealloc, Layout};
use crossbeam_epoch::{self as epoch, Atomic, Owned, Shared, Guard};
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
                        // Safety: we have exclusive access to this node
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
                    // Read value before CAS
                    let next_node = unsafe { next.deref() };
                    let value = unsafe { ptr::read(&next_node.data) };
                    
                    if self.head.compare_exchange(
                        head,
                        next,
                        Ordering::Release,
                        Ordering::Acquire,
                        guard,
                    ).is_ok() {
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
    pub fn push(&self, value: T) {
        let bottom = self.bottom.load(Ordering::Relaxed);
        let top = self.top.load(Ordering::Acquire);
        
        if bottom.wrapping_sub(top) >= self.capacity {
            panic!("Deque is full");
        }
        
        unsafe {
            let slot = self.buffer.add(bottom & self.mask);
            (*slot).write(value);
        }
        
        self.bottom.store(bottom.wrapping_add(1), Ordering::Release);
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