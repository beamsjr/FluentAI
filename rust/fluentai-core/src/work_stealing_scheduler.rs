//! Work-stealing scheduler for efficient multi-threaded task execution
//!
//! This module provides a work-stealing scheduler that distributes tasks across
//! multiple worker threads. Each worker has its own deque of tasks, and idle
//! workers can steal tasks from busy workers to ensure load balancing.

use crossbeam_deque::{Injector, Stealer, Worker as WorkerDeque};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread;
use std::time::Duration;

use crate::thread_pool::{CpuAffinity, PanicHandler, ThreadPriority};

/// Configuration for the work-stealing scheduler
#[derive(Debug, Clone)]
pub struct WorkStealingConfig {
    /// Number of worker threads (None = number of CPU cores)
    pub num_threads: Option<usize>,
    
    /// Thread name prefix
    pub thread_name: String,
    
    /// Stack size for worker threads (None = system default)
    pub stack_size: Option<usize>,
    
    /// Keep-alive time for idle threads
    pub keep_alive: Duration,
    
    /// Thread priority (platform-specific)
    pub thread_priority: ThreadPriority,
    
    /// CPU affinity settings
    pub cpu_affinity: CpuAffinity,
    
    /// Panic handler
    pub panic_handler: PanicHandler,
}

impl Default for WorkStealingConfig {
    fn default() -> Self {
        Self {
            num_threads: None,
            thread_name: "fluentai-ws-worker".to_string(),
            stack_size: None,
            keep_alive: Duration::from_secs(60),
            thread_priority: ThreadPriority::Normal,
            cpu_affinity: CpuAffinity::None,
            panic_handler: PanicHandler::Restart,
        }
    }
}

/// A work-stealing task scheduler
pub struct WorkStealingScheduler {
    #[allow(dead_code)]
    config: WorkStealingConfig,
    workers: Vec<WorkerThread>,
    injector: Arc<Injector<Job>>,
    #[allow(dead_code)]
    stealers: Arc<Vec<Stealer<Job>>>,
    shutdown: Arc<AtomicBool>,
    active_count: Arc<AtomicUsize>,
    completed_count: Arc<AtomicUsize>,
}

type Job = Box<dyn FnOnce() + Send + 'static>;

struct WorkerThread {
    #[allow(dead_code)]
    id: usize,
    thread: Option<thread::JoinHandle<()>>,
}

impl WorkStealingScheduler {
    /// Create a new work-stealing scheduler with the given configuration
    pub fn new(config: WorkStealingConfig) -> Self {
        let num_threads = config.num_threads.unwrap_or_else(num_cpus::get);
        
        let injector = Arc::new(Injector::new());
        let shutdown = Arc::new(AtomicBool::new(false));
        let active_count = Arc::new(AtomicUsize::new(0));
        let completed_count = Arc::new(AtomicUsize::new(0));
        
        let mut workers = Vec::with_capacity(num_threads);
        let mut stealers = Vec::with_capacity(num_threads);
        let mut worker_deques = Vec::with_capacity(num_threads);
        
        // Create worker deques and collect stealers
        for _ in 0..num_threads {
            let worker = WorkerDeque::new_fifo();
            stealers.push(worker.stealer());
            worker_deques.push(worker);
        }
        
        let stealers = Arc::new(stealers);
        
        // Create worker threads
        for (id, worker_deque) in worker_deques.into_iter().enumerate() {
            workers.push(WorkerThread::new(
                id,
                config.clone(),
                worker_deque,
                Arc::clone(&injector),
                Arc::clone(&stealers),
                Arc::clone(&shutdown),
                Arc::clone(&active_count),
                Arc::clone(&completed_count),
            ));
        }
        
        WorkStealingScheduler {
            config,
            workers,
            injector,
            stealers,
            shutdown,
            active_count,
            completed_count,
        }
    }
    
    /// Submit a job to the scheduler
    pub fn execute<F>(&self, job: F) -> Result<(), &'static str>
    where
        F: FnOnce() + Send + 'static,
    {
        if self.shutdown.load(Ordering::Relaxed) {
            return Err("Scheduler is shutting down");
        }
        
        self.injector.push(Box::new(job));
        
        // Wake up workers by yielding - this helps ensure tasks are picked up
        thread::yield_now();
        
        Ok(())
    }
    
    /// Get the number of active threads
    pub fn active_count(&self) -> usize {
        self.active_count.load(Ordering::Relaxed)
    }
    
    /// Get the number of completed jobs
    pub fn completed_count(&self) -> usize {
        self.completed_count.load(Ordering::Relaxed)
    }
    
    /// Wait for all submitted tasks to complete
    pub fn wait_for_completion(&self) {
        // Wait until the injector is empty and all workers are idle
        loop {
            if self.injector.is_empty() && self.active_count() == 0 {
                // Give a tiny bit more time for the last completion count update
                thread::sleep(Duration::from_millis(1));
                break;
            }
            thread::yield_now();
        }
    }
    
    /// Shutdown the scheduler and wait for all threads to finish
    pub fn shutdown(self) {
        // First ensure all work is done
        self.wait_for_completion();
        
        self.shutdown.store(true, Ordering::Relaxed);
        
        // Wait for all threads to finish
        for worker in self.workers {
            if let Some(thread) = worker.thread {
                let _ = thread.join();
            }
        }
    }
}

impl WorkerThread {
    fn new(
        id: usize,
        config: WorkStealingConfig,
        worker: WorkerDeque<Job>,
        injector: Arc<Injector<Job>>,
        stealers: Arc<Vec<Stealer<Job>>>,
        shutdown: Arc<AtomicBool>,
        active_count: Arc<AtomicUsize>,
        completed_count: Arc<AtomicUsize>,
    ) -> Self {
        let thread_name = format!("{}-{}", config.thread_name, id);
        let mut builder = thread::Builder::new().name(thread_name);
        
        if let Some(stack_size) = config.stack_size {
            builder = builder.stack_size(stack_size);
        }
        
        let thread = builder
            .spawn(move || {
                worker_loop(
                    id,
                    worker,
                    injector,
                    stealers,
                    shutdown,
                    active_count,
                    completed_count,
                    config.keep_alive,
                );
            })
            .ok();
        
        WorkerThread {
            id,
            thread,
        }
    }
}

fn worker_loop(
    id: usize,
    worker: WorkerDeque<Job>,
    injector: Arc<Injector<Job>>,
    stealers: Arc<Vec<Stealer<Job>>>,
    shutdown: Arc<AtomicBool>,
    active_count: Arc<AtomicUsize>,
    completed_count: Arc<AtomicUsize>,
    _keep_alive: Duration,
) {
    
    while !shutdown.load(Ordering::Relaxed) {
        // Try to get work in this order:
        // 1. From our own deque
        // 2. From the global injector
        // 3. By stealing from other workers
        
        let mut job = worker.pop();
        
        if job.is_none() {
            // Try the global injector
            loop {
                match injector.steal() {
                    crossbeam_deque::Steal::Success(j) => {
                        job = Some(j);
                        break;
                    }
                    crossbeam_deque::Steal::Empty => break,
                    crossbeam_deque::Steal::Retry => continue,
                }
            }
        }
        
        if job.is_none() {
            // Try stealing from other workers
            for (i, stealer) in stealers.iter().enumerate() {
                if i == id {
                    continue; // Don't steal from ourselves
                }
                
                loop {
                    match stealer.steal() {
                        crossbeam_deque::Steal::Success(j) => {
                            job = Some(j);
                            break;
                        }
                        crossbeam_deque::Steal::Empty => break,
                        crossbeam_deque::Steal::Retry => continue,
                    }
                }
                
                if job.is_some() {
                    break;
                }
            }
        }
        
        if let Some(job) = job {
            active_count.fetch_add(1, Ordering::Relaxed);
            job();
            active_count.fetch_sub(1, Ordering::Relaxed);
            completed_count.fetch_add(1, Ordering::Relaxed);
        } else {
            // No work available
            if shutdown.load(Ordering::Relaxed) {
                // During shutdown, check if there's really no more work
                let really_empty = injector.is_empty() && 
                    stealers.iter().enumerate().all(|(i, s)| i == id || s.is_empty());
                if really_empty {
                    break;
                }
            }
            
            // Brief sleep to avoid busy-waiting
            thread::yield_now();
        }
        
        // Periodically try to move tasks from injector to local queue
        // This helps with locality
        if worker.len() < 32 {
            loop {
                match injector.steal_batch(&worker) {
                    crossbeam_deque::Steal::Success(_) => break,
                    crossbeam_deque::Steal::Empty => break,
                    crossbeam_deque::Steal::Retry => continue,
                }
            }
        }
    }
}

/// Builder for creating a work-stealing scheduler
pub struct WorkStealingBuilder {
    config: WorkStealingConfig,
}

impl WorkStealingBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            config: WorkStealingConfig::default(),
        }
    }
    
    /// Set the number of worker threads
    pub fn num_threads(mut self, num: usize) -> Self {
        self.config.num_threads = Some(num);
        self
    }
    
    /// Set the thread name prefix
    pub fn thread_name(mut self, name: impl Into<String>) -> Self {
        self.config.thread_name = name.into();
        self
    }
    
    /// Set the stack size for worker threads
    pub fn stack_size(mut self, size: usize) -> Self {
        self.config.stack_size = Some(size);
        self
    }
    
    /// Set the keep-alive duration for idle threads
    pub fn keep_alive(mut self, duration: Duration) -> Self {
        self.config.keep_alive = duration;
        self
    }
    
    /// Set the thread priority
    pub fn thread_priority(mut self, priority: ThreadPriority) -> Self {
        self.config.thread_priority = priority;
        self
    }
    
    /// Set the CPU affinity
    pub fn cpu_affinity(mut self, affinity: CpuAffinity) -> Self {
        self.config.cpu_affinity = affinity;
        self
    }
    
    /// Set the panic handler
    pub fn panic_handler(mut self, handler: PanicHandler) -> Self {
        self.config.panic_handler = handler;
        self
    }
    
    /// Build the work-stealing scheduler
    pub fn build(self) -> WorkStealingScheduler {
        WorkStealingScheduler::new(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicU32;
    
    #[test]
    fn test_work_stealing_basic() {
        let scheduler = WorkStealingBuilder::new()
            .num_threads(4)
            .build();
        
        let counter = Arc::new(AtomicU32::new(0));
        
        // Submit many tasks
        for _ in 0..1000 {
            let counter = Arc::clone(&counter);
            scheduler.execute(move || {
                counter.fetch_add(1, Ordering::Relaxed);
            }).unwrap();
        }
        
        // Wait for all tasks to complete
        scheduler.wait_for_completion();
        
        let final_count = counter.load(Ordering::Relaxed);
        let completed = scheduler.completed_count();
        
        println!("Counter: {}, Completed: {}", final_count, completed);
        
        assert_eq!(final_count, 1000);
        assert_eq!(completed, 1000);
        
        scheduler.shutdown();
    }
    
    #[test]
    fn test_work_stealing_distribution() {
        let scheduler = WorkStealingBuilder::new()
            .num_threads(4)
            .build();
        
        // Submit tasks that take varying amounts of time
        // This should trigger work stealing
        for i in 0..100 {
            scheduler.execute(move || {
                // Some tasks take longer than others
                if i % 10 == 0 {
                    thread::sleep(Duration::from_micros(100));
                }
            }).unwrap();
        }
        
        // Wait for all tasks to complete
        scheduler.wait_for_completion();
        
        assert_eq!(scheduler.completed_count(), 100);
        
        scheduler.shutdown();
    }
}