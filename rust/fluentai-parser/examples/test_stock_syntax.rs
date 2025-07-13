use fluentai_parser::parse_flc;

fn main() {
    println!("Testing stock rebalancer syntax...\n");

    // Test some key syntax from the stock rebalancer
    let test_cases = vec![
        // Map literal
        (r#"{"symbol": "AAPL", "shares": 100}"#, "map literal"),
        // Function with map return
        (
            r#"private function create_stock(symbol, shares) { {"symbol": symbol, "shares": shares} }"#,
            "function with map",
        ),
        // Method calls on objects
        (r#"portfolio.stocks"#, "object field access"),
        // Array methods
        (r#"stocks.filter(s => s.symbol != "AAPL")"#, "array filter"),
        // F-string printing
        (
            r#"$(f"Portfolio Value: ${value}").print()"#,
            "f-string with print",
        ),
        // Object field access
        (r#"stock.symbol"#, "nested field access"),
        // Conditionals
        (
            r#"if (percentage < 0.0) { false } else { true }"#,
            "if-else expression",
        ),
        // Array push
        (r#"stocks.push(new_stock)"#, "array push method"),
        // Array reduce
        (
            r#"stocks.reduce(0.0, (sum, stock) => sum + stock.value)"#,
            "array reduce",
        ),
        // Find method
        (r#"stocks.find(s => s.symbol == "AAPL")"#, "array find"),
        // Is some check
        (r#"result.is_some()"#, "option is_some"),
        // Unwrap
        (r#"result.unwrap()"#, "option unwrap"),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (test, desc) in test_cases {
        match parse_flc(test) {
            Ok(_) => {
                println!("✓ {}: {}", desc, test);
                passed += 1;
            }
            Err(e) => {
                println!("✗ {} failed: {}", desc, e);
                println!("  Input: {}", test);
                failed += 1;
            }
        }
    }

    println!("\nSummary: {} passed, {} failed", passed, failed);
}
