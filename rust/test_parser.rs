// Quick test of the Rust parser
use claudelang_parser::parse;

fn main() {
    println!("Testing ClaudeLang Rust Parser\n");
    
    let test_cases = vec![
        ("Simple", "(+ 1 2)"),
        ("Nested", "(* (+ 1 2) (- 4 3))"),
        ("Lambda", "(lambda (x y) (+ x y))"),
        ("Let", "(let ((x 10)) x)"),
        ("List", "[1 2 3]"),
    ];
    
    for (name, code) in test_cases {
        print!("{}: ", name);
        match parse(code) {
            Ok(graph) => {
                println!("✓ Parsed successfully (nodes: {})", graph.nodes.len());
            }
            Err(e) => {
                println!("✗ Parse error: {}", e);
            }
        }
    }
    
    // Benchmark a simple expression
    println!("\nBenchmarking parser...");
    let start = std::time::Instant::now();
    let iterations = 10_000;
    
    for _ in 0..iterations {
        let _ = parse("(+ (* 2 3) (* 4 5))");
    }
    
    let elapsed = start.elapsed();
    let per_iteration = elapsed.as_nanos() as f64 / iterations as f64;
    
    println!("Average parse time: {:.1} ns ({:.1} µs)", per_iteration, per_iteration / 1000.0);
    println!("\nCompare to Python baseline: ~50 µs");
    println!("Expected speedup: {:.1}x", 50_000.0 / per_iteration);
}