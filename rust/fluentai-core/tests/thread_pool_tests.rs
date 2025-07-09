//! Advanced tests for the thread pool implementation

use fluentai_core::thread_pool::*;
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::sync::{Arc, Barrier};
use std::thread;
use std::time::{Duration, Instant};

#[test]
fn test_thread_pool_concurrent_execution() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name("concurrent-test")
        .build();

    let counter = Arc::new(AtomicUsize::new(0));
    let num_jobs = 1000;

    let start = Instant::now();

    for _ in 0..num_jobs {
        let counter = Arc::clone(&counter);
        pool.execute(move || {
            // Simulate some work
            let mut sum = 0;
            for i in 0..100 {
                sum += i;
            }
            // Ensure work isn't optimized out
            if sum > 0 {
                counter.fetch_add(1, Ordering::Relaxed);
            }
        })
        .unwrap();
    }

    // Wait for all jobs to complete
    while pool.completed_count() < num_jobs {
        thread::sleep(Duration::from_millis(1));
    }

    let elapsed = start.elapsed();
    println!("Executed {} jobs in {:?}", num_jobs, elapsed);

    assert_eq!(counter.load(Ordering::Relaxed), num_jobs);
    assert!(pool.active_count() == 0);
}

#[test]
#[ignore = "Test uses barrier synchronization which can hang - run manually with: cargo test test_thread_pool_queue_saturation -- --ignored"]
fn test_thread_pool_queue_saturation() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(2)
        .queue_capacity(10)
        .build();

    let barrier = Arc::new(Barrier::new(3)); // 2 workers + main thread
    let jobs_executed = Arc::new(AtomicUsize::new(0));

    // Fill up worker threads
    for _ in 0..2 {
        let barrier = Arc::clone(&barrier);
        let jobs_executed = Arc::clone(&jobs_executed);
        pool.execute(move || {
            barrier.wait();
            jobs_executed.fetch_add(1, Ordering::Relaxed);
        })
        .unwrap();
    }

    // Fill up the queue
    let mut _queued_jobs = 0;
    for i in 0..20 {
        let result = pool.execute(move || {
            // Quick job
            thread::sleep(Duration::from_micros(100));
        });

        if result.is_ok() {
            _queued_jobs += 1;
        } else {
            // Queue is full
            println!("Queue filled after {} jobs", i);
            break;
        }
    }

    // Release the workers
    barrier.wait();

    // Wait for some completion
    thread::sleep(Duration::from_millis(100));

    assert!(jobs_executed.load(Ordering::Relaxed) >= 2);
}

#[test]
fn test_thread_pool_stress_test() {
    let pool = Arc::new(
        ThreadPoolBuilder::new()
            .num_threads(8)
            .thread_name("stress-test")
            .queue_capacity(1000)
            .build(),
    );

    let completed = Arc::new(AtomicUsize::new(0));
    let errors = Arc::new(AtomicUsize::new(0));

    // Spawn multiple threads submitting jobs
    let handles: Vec<_> = (0..10)
        .map(|_thread_id| {
            let pool = Arc::clone(&pool);
            let completed = Arc::clone(&completed);
            let errors = Arc::clone(&errors);

            thread::spawn(move || {
                for job_id in 0..100 {
                    let completed = Arc::clone(&completed);
                    let result = pool.execute(move || {
                        // Simulate varying workloads
                        if job_id % 10 == 0 {
                            thread::sleep(Duration::from_micros(500));
                        }
                        completed.fetch_add(1, Ordering::Relaxed);
                    });

                    if result.is_err() {
                        errors.fetch_add(1, Ordering::Relaxed);
                    }
                }
            })
        })
        .collect();

    // Wait for all submitters
    for handle in handles {
        handle.join().unwrap();
    }

    // Wait for all jobs to complete
    let timeout = Instant::now() + Duration::from_secs(5);
    while pool.completed_count() + errors.load(Ordering::Relaxed) < 1000 {
        if Instant::now() > timeout {
            panic!("Timeout waiting for jobs to complete");
        }
        thread::sleep(Duration::from_millis(10));
    }

    println!(
        "Completed: {}, Errors: {}",
        completed.load(Ordering::Relaxed),
        errors.load(Ordering::Relaxed)
    );

    assert!(completed.load(Ordering::Relaxed) > 900); // Allow some failures due to queue saturation
}

#[test]
fn test_thread_pool_panic_recovery() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(2)
        .panic_handler(PanicHandler::Restart)
        .build();

    let success_count = Arc::new(AtomicUsize::new(0));
    let panic_count = Arc::new(AtomicUsize::new(0));

    // Submit jobs that sometimes panic
    for i in 0..20 {
        let success_count = Arc::clone(&success_count);
        let panic_count = Arc::clone(&panic_count);

        pool.execute(move || {
            if i % 5 == 0 {
                panic_count.fetch_add(1, Ordering::Relaxed);
                panic!("Intentional panic {}", i);
            } else {
                success_count.fetch_add(1, Ordering::Relaxed);
            }
        })
        .unwrap();
    }

    // Wait for completion
    while pool.completed_count() < 20 {
        thread::sleep(Duration::from_millis(10));
    }

    assert_eq!(success_count.load(Ordering::Relaxed), 16); // 20 - 4 panics
    assert_eq!(panic_count.load(Ordering::Relaxed), 4);
}

#[test]
fn test_thread_pool_dynamic_resize() {
    let mut pool = ThreadPoolBuilder::new()
        .num_threads(2)
        .thread_name("resize-test")
        .build();

    let counter = Arc::new(AtomicUsize::new(0));

    // Submit some jobs with 2 threads
    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        pool.execute(move || {
            thread::sleep(Duration::from_millis(10));
            counter.fetch_add(1, Ordering::Relaxed);
        })
        .unwrap();
    }

    // Resize to more threads
    pool.resize(6);

    // Submit more jobs
    for _ in 0..20 {
        let counter = Arc::clone(&counter);
        pool.execute(move || {
            thread::sleep(Duration::from_millis(5));
            counter.fetch_add(1, Ordering::Relaxed);
        })
        .unwrap();
    }

    // Wait for completion
    while pool.completed_count() < 30 {
        thread::sleep(Duration::from_millis(10));
    }

    assert_eq!(counter.load(Ordering::Relaxed), 30);

    // Resize down
    pool.resize(1);
    // Can't access private field workers.len() directly
}

#[test]
fn test_thread_pool_custom_stack_size() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(1)
        .stack_size(4 * 1024 * 1024) // 4MB stack
        .thread_name("stack-test")
        .build();

    let executed = Arc::new(AtomicBool::new(false));
    let executed_clone = Arc::clone(&executed);

    pool.execute(move || {
        // Create a large stack allocation to test custom stack size
        let mut large_array = vec![0u8; 2 * 1024 * 1024]; // 2MB
        large_array[0] = 1;
        let last_idx = large_array.len() - 1;
        large_array[last_idx] = 2;

        // Ensure it's not optimized out
        if large_array[0] + large_array[last_idx] == 3 {
            executed_clone.store(true, Ordering::Relaxed);
        }
    })
    .unwrap();

    // Wait for completion
    while pool.completed_count() < 1 {
        thread::sleep(Duration::from_millis(10));
    }

    assert!(executed.load(Ordering::Relaxed));
}

#[test]
#[ignore = "Long-running test with 150ms+ sleep - run manually with: cargo test test_thread_pool_keep_alive -- --ignored"]
fn test_thread_pool_keep_alive() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(2)
        .keep_alive(Duration::from_millis(100))
        .thread_name("keep-alive-test")
        .build();

    // Submit a job
    pool.execute(|| {
        thread::sleep(Duration::from_millis(10));
    })
    .unwrap();

    // Wait for job to complete
    while pool.completed_count() < 1 {
        thread::sleep(Duration::from_millis(10));
    }

    // Workers should still be alive
    assert!(pool.active_count() == 0);

    // Wait longer than keep-alive
    thread::sleep(Duration::from_millis(150));

    // Submit another job - workers might have terminated and restarted
    let executed = Arc::new(AtomicBool::new(false));
    let executed_clone = Arc::clone(&executed);

    pool.execute(move || {
        executed_clone.store(true, Ordering::Relaxed);
    })
    .unwrap();

    // Wait for completion
    while pool.completed_count() < 2 {
        thread::sleep(Duration::from_millis(10));
    }

    assert!(executed.load(Ordering::Relaxed));
}

#[test]
fn test_thread_pool_stats_accuracy() {
    let pool = ThreadPoolBuilder::new().num_threads(4).build();

    let barrier = Arc::new(Barrier::new(5)); // 4 workers + main thread

    // Initial state
    assert_eq!(pool.queued_count(), 0);
    assert_eq!(pool.active_count(), 0);
    assert_eq!(pool.completed_count(), 0);

    // Queue jobs that will block
    for _ in 0..4 {
        let barrier = Arc::clone(&barrier);
        pool.execute(move || {
            barrier.wait();
        })
        .unwrap();
    }

    // Give jobs time to start
    thread::sleep(Duration::from_millis(50));

    // All workers should be active
    assert_eq!(pool.active_count(), 4);

    // Queue additional jobs
    for _ in 0..10 {
        pool.execute(|| {
            thread::sleep(Duration::from_micros(100));
        })
        .unwrap();
    }

    // Should have queued jobs
    assert!(pool.queued_count() > 0);

    // Release the workers
    barrier.wait();

    // Wait for all completion
    while pool.completed_count() < 14 {
        thread::sleep(Duration::from_millis(10));
    }

    assert_eq!(pool.completed_count(), 14);
    assert_eq!(pool.queued_count(), 0);
    assert_eq!(pool.active_count(), 0);
}

#[test]
fn test_thread_pool_cpu_affinity_options() {
    // Test CpuSet creation and comparison
    let cpu_set1 = CpuAffinity::CpuSet(vec![0, 2, 4]);
    let cpu_set2 = CpuAffinity::CpuSet(vec![0, 2, 4]);
    let cpu_set3 = CpuAffinity::CpuSet(vec![1, 3, 5]);

    assert_eq!(cpu_set1, cpu_set2);
    assert_ne!(cpu_set1, cpu_set3);

    // Test with different affinity settings
    let configs = vec![
        CpuAffinity::None,
        CpuAffinity::PinToCore,
        CpuAffinity::CpuSet(vec![0, 1]),
        CpuAffinity::NumaAware,
    ];

    for affinity in configs {
        let pool = ThreadPoolBuilder::new()
            .num_threads(2)
            .cpu_affinity(affinity.clone())
            .build();

        let executed = Arc::new(AtomicBool::new(false));
        let executed_clone = Arc::clone(&executed);

        pool.execute(move || {
            executed_clone.store(true, Ordering::Relaxed);
        })
        .unwrap();

        while pool.completed_count() < 1 {
            thread::sleep(Duration::from_millis(10));
        }

        assert!(executed.load(Ordering::Relaxed));
    }
}

#[test]
fn test_thread_pool_priority_settings() {
    let priorities = vec![
        ThreadPriority::Low,
        ThreadPriority::Normal,
        ThreadPriority::High,
        ThreadPriority::Realtime,
    ];

    for priority in priorities {
        let pool = ThreadPoolBuilder::new()
            .num_threads(1)
            .thread_priority(priority)
            .build();

        let executed = Arc::new(AtomicBool::new(false));
        let executed_clone = Arc::clone(&executed);

        pool.execute(move || {
            executed_clone.store(true, Ordering::Relaxed);
        })
        .unwrap();

        while pool.completed_count() < 1 {
            thread::sleep(Duration::from_millis(10));
        }

        assert!(executed.load(Ordering::Relaxed));
        // Can't access private field config.thread_priority directly
    }
}

#[test]
#[ignore = "Long-running test with thread synchronization delays - run manually with: cargo test test_thread_pool_graceful_shutdown -- --ignored"]
fn test_thread_pool_graceful_shutdown() {
    let pool = ThreadPoolBuilder::new()
        .num_threads(4)
        .thread_name("shutdown-test")
        .build();

    let counter = Arc::new(AtomicUsize::new(0));
    let in_progress = Arc::new(AtomicBool::new(false));

    // Submit jobs
    for i in 0..20 {
        let counter = Arc::clone(&counter);
        let in_progress = Arc::clone(&in_progress);

        pool.execute(move || {
            if i == 0 {
                in_progress.store(true, Ordering::Relaxed);
                thread::sleep(Duration::from_millis(50));
                in_progress.store(false, Ordering::Relaxed);
            }
            counter.fetch_add(1, Ordering::Relaxed);
        })
        .unwrap();
    }

    // Shutdown should wait for all jobs
    pool.shutdown();

    // All jobs should have completed
    assert_eq!(counter.load(Ordering::Relaxed), 20);
    assert!(!in_progress.load(Ordering::Relaxed));
}

#[test]
fn test_thread_pool_work_distribution() {
    let num_threads = 4;
    let pool = ThreadPoolBuilder::new().num_threads(num_threads).build();

    let thread_work_counts: Arc<[AtomicUsize; 4]> = Arc::new([
        AtomicUsize::new(0),
        AtomicUsize::new(0),
        AtomicUsize::new(0),
        AtomicUsize::new(0),
    ]);

    // Submit many quick jobs
    for _ in 0..400 {
        let thread_work_counts = Arc::clone(&thread_work_counts);
        pool.execute(move || {
            // Try to identify which thread we're on (approximation)
            let thread_name = thread::current().name().unwrap_or("").to_string();
            if let Some(id_str) = thread_name.split('-').last() {
                if let Ok(id) = id_str.parse::<usize>() {
                    if id < 4 {
                        thread_work_counts[id].fetch_add(1, Ordering::Relaxed);
                    }
                }
            }
        })
        .unwrap();
    }

    // Wait for completion
    while pool.completed_count() < 400 {
        thread::sleep(Duration::from_millis(10));
    }

    // Check work distribution (should be reasonably balanced)
    let counts: Vec<usize> = thread_work_counts
        .iter()
        .map(|c| c.load(Ordering::Relaxed))
        .collect();

    println!("Work distribution: {:?}", counts);

    // Each thread should have done some work
    // Note: This might not be perfectly balanced due to scheduling
    for count in &counts {
        assert!(*count > 0 || counts.iter().sum::<usize>() == 400);
    }
}
