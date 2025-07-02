//! Benchmark infrastructure for tracking performance improvements

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

/// Performance snapshot for a specific benchmark
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub name: String,
    pub timestamp: DateTime<Utc>,
    pub metrics: Metrics,
    pub environment: Environment,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Metrics {
    pub mean_ns: f64,
    pub median_ns: f64,
    pub min_ns: f64,
    pub max_ns: f64,
    pub std_dev_ns: f64,
    pub throughput: Option<Throughput>,
    pub memory_bytes: Option<u64>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Throughput {
    pub value: f64,
    pub unit: String, // e.g., "ops/sec", "MB/s"
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Environment {
    pub rust_version: String,
    pub python_version: Option<String>,
    pub os: String,
    pub cpu: String,
    pub commit_hash: Option<String>,
}

/// Comparison between Python and Rust implementations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceComparison {
    pub benchmark_name: String,
    pub python_baseline: Metrics,
    pub rust_implementation: Metrics,
    pub speedup: f64,
    pub timestamp: DateTime<Utc>,
}

/// Performance tracking across versions
pub struct PerformanceTracker {
    results_dir: String,
}

impl PerformanceTracker {
    pub fn new(results_dir: impl Into<String>) -> Self {
        Self {
            results_dir: results_dir.into(),
        }
    }

    /// Record a benchmark result
    pub fn record_result(&self, result: BenchmarkResult) -> Result<()> {
        let dir_path = Path::new(&self.results_dir);
        fs::create_dir_all(dir_path)?;

        let filename = format!(
            "{}_{}_{}.json",
            result.name.replace(' ', "_"),
            result.timestamp.format("%Y%m%d_%H%M%S"),
            result.environment.commit_hash.as_ref().unwrap_or(&"unknown".to_string())[..8].to_string()
        );

        let path = dir_path.join(filename);
        let json = serde_json::to_string_pretty(&result)?;
        fs::write(path, json)?;

        Ok(())
    }

    /// Load historical results for a benchmark
    pub fn load_history(&self, benchmark_name: &str) -> Result<Vec<BenchmarkResult>> {
        let dir_path = Path::new(&self.results_dir);
        let mut results = Vec::new();

        if !dir_path.exists() {
            return Ok(results);
        }

        for entry in fs::read_dir(dir_path)? {
            let entry = entry?;
            let path = entry.path();
            
            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                let contents = fs::read_to_string(&path)?;
                if let Ok(result) = serde_json::from_str::<BenchmarkResult>(&contents) {
                    if result.name == benchmark_name {
                        results.push(result);
                    }
                }
            }
        }

        // Sort by timestamp
        results.sort_by_key(|r| r.timestamp);
        Ok(results)
    }

    /// Compare Python and Rust implementations
    pub fn compare_implementations(
        &self,
        benchmark_name: &str,
        python_metrics: Metrics,
        rust_metrics: Metrics,
    ) -> PerformanceComparison {
        let speedup = python_metrics.mean_ns / rust_metrics.mean_ns;
        
        PerformanceComparison {
            benchmark_name: benchmark_name.to_string(),
            python_baseline: python_metrics,
            rust_implementation: rust_metrics,
            speedup,
            timestamp: Utc::now(),
        }
    }

    /// Generate performance report
    pub fn generate_report(&self) -> Result<PerformanceReport> {
        let mut report = PerformanceReport::new();

        // Group results by benchmark name
        let mut all_results: HashMap<String, Vec<BenchmarkResult>> = HashMap::new();
        
        let dir_path = Path::new(&self.results_dir);
        if dir_path.exists() {
            for entry in fs::read_dir(dir_path)? {
                let entry = entry?;
                let path = entry.path();
                
                if path.extension().and_then(|s| s.to_str()) == Some("json") {
                    let contents = fs::read_to_string(&path)?;
                    if let Ok(result) = serde_json::from_str::<BenchmarkResult>(&contents) {
                        all_results.entry(result.name.clone()).or_default().push(result);
                    }
                }
            }
        }

        // Calculate improvements for each benchmark
        for (name, mut results) in all_results {
            results.sort_by_key(|r| r.timestamp);
            
            if results.len() >= 2 {
                let first = &results[0];
                let latest = &results[results.len() - 1];
                
                let improvement = first.metrics.mean_ns / latest.metrics.mean_ns;
                report.improvements.insert(name.clone(), improvement);
                
                // Track progression
                let progression: Vec<(DateTime<Utc>, f64)> = results
                    .iter()
                    .map(|r| (r.timestamp, r.metrics.mean_ns))
                    .collect();
                report.progressions.insert(name, progression);
            }
        }

        Ok(report)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PerformanceReport {
    pub generated_at: DateTime<Utc>,
    pub improvements: HashMap<String, f64>, // benchmark_name -> speedup factor
    pub progressions: HashMap<String, Vec<(DateTime<Utc>, f64)>>, // benchmark_name -> (time, mean_ns)
}

impl PerformanceReport {
    fn new() -> Self {
        Self {
            generated_at: Utc::now(),
            improvements: HashMap::new(),
            progressions: HashMap::new(),
        }
    }

    pub fn print_summary(&self) {
        println!("Performance Report - Generated at: {}", self.generated_at);
        println!("{:-<80}", "");
        
        println!("\nOverall Improvements:");
        for (benchmark, improvement) in &self.improvements {
            println!("  {:<40} {:>6.1}x faster", benchmark, improvement);
        }
        
        println!("\nProgression Summary:");
        for (benchmark, progression) in &self.progressions {
            if let (Some(first), Some(last)) = (progression.first(), progression.last()) {
                let total_improvement = first.1 / last.1;
                println!(
                    "  {:<40} {} -> {} ({:.1}x improvement)",
                    benchmark,
                    format_time(first.1),
                    format_time(last.1),
                    total_improvement
                );
            }
        }
    }
}

fn format_time(ns: f64) -> String {
    if ns < 1_000.0 {
        format!("{:.1}ns", ns)
    } else if ns < 1_000_000.0 {
        format!("{:.1}µs", ns / 1_000.0)
    } else if ns < 1_000_000_000.0 {
        format!("{:.1}ms", ns / 1_000_000.0)
    } else {
        format!("{:.1}s", ns / 1_000_000_000.0)
    }
}

/// Macro for creating benchmark comparisons
#[macro_export]
macro_rules! compare_benchmark {
    ($name:expr, $python_fn:expr, $rust_fn:expr) => {{
        use criterion::black_box;
        use std::time::Instant;

        // Measure Python baseline
        let python_start = Instant::now();
        for _ in 0..1000 {
            black_box($python_fn());
        }
        let python_elapsed = python_start.elapsed();
        let python_mean_ns = python_elapsed.as_nanos() as f64 / 1000.0;

        // Measure Rust implementation
        let rust_start = Instant::now();
        for _ in 0..1000 {
            black_box($rust_fn());
        }
        let rust_elapsed = rust_start.elapsed();
        let rust_mean_ns = rust_elapsed.as_nanos() as f64 / 1000.0;

        let speedup = python_mean_ns / rust_mean_ns;
        
        println!(
            "{}: Python {:.1}µs, Rust {:.1}µs, Speedup: {:.1}x",
            $name,
            python_mean_ns / 1000.0,
            rust_mean_ns / 1000.0,
            speedup
        );

        speedup
    }};
}