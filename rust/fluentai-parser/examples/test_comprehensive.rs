use fluentai_parser::parse_flc;

fn main() {
    println!("Testing comprehensive FLC syntax...\n");

    let test_cases = vec![
        // Previously working features
        ("1 + 2", "arithmetic"),
        ("x == y", "comparison"),
        ("private function add(x, y) { x + y }", "function"),
        ("x => x * 2", "lambda"),
        ("let x = 5; x", "let binding"),
        ("[1, 2, 3]", "list literal"),
        ("if (x > 0) { x } else { 0 }", "if expression"),
        // Features we discovered were already implemented
        ("{\"key\": \"value\"}", "map literal"),
        ("#{1, 2, 3}", "set literal"),
        ("use std::io as io;", "import alias"),
        ("let {x, y} = point; x", "destructuring"),
        ("obj.?method()", "optional chaining"),
        // Features we just implemented
        ("for x in list { x * 2 }", "for loop"),
        ("while x > 0 { x - 1 }", "while loop"),
        ("private const MAX = 100", "const definition"),
        (
            "private struct Point { x: int }.derive(Debug)",
            "derive attribute",
        ),
    ];

    let mut passed = 0;
    let mut failed = 0;

    for (input, desc) in test_cases {
        match parse_flc(input) {
            Ok(_) => {
                println!("✓ {} works", desc);
                passed += 1;
            }
            Err(e) => {
                println!("✗ {} failed: {}", desc, e);
                failed += 1;
            }
        }
    }

    println!("\nSummary: {} passed, {} failed", passed, failed);
}
