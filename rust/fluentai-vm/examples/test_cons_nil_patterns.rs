use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse_flc;
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    VM,
};

fn main() {
    println!("=== Testing Cons/Nil Pattern Matching ===\n");

    // Test 1: Uppercase Cons pattern
    test_pattern(
        "Uppercase Cons pattern",
        r#"
(define lst (list 1 2 3))
(match lst
  (Nil "empty")
  ((Cons x xs) x))
"#,
    );

    // Test 2: Lowercase cons pattern
    test_pattern(
        "Lowercase cons pattern",
        r#"
(define lst (list 1 2 3))
(match lst
  (nil "empty")
  ((cons x xs) x))
"#,
    );

    // Test 3: Uppercase Nil pattern
    test_pattern(
        "Uppercase Nil pattern",
        r#"
(define lst (list))
(match lst
  (Nil "empty list")
  ((Cons x xs) "not empty"))
"#,
    );

    // Test 4: Extract both head and tail
    test_pattern(
        "Extract head and tail",
        r#"
(define lst (list 10 20 30))
(match lst
  (Nil 0)
  ((Cons head tail) (+ head (length tail))))
"#,
    );
}

fn test_pattern(name: &str, code: &str) {
    println!("Test: {}", name);
    println!("Code: {}", code.trim());

    match parse_flc(code) {
        Ok(graph) => {
            let options = CompilerOptions {
                optimization_level: OptimizationLevel::None,
                ..Default::default()
            };
            let compiler = Compiler::with_options(options);

            match compiler.compile(&graph) {
                Ok(bytecode) => {
                    let mut vm = VM::new(bytecode);
                    match vm.run() {
                        Ok(result) => {
                            println!("✓ Result: {:?}", result);
                        }
                        Err(e) => {
                            println!("✗ Runtime error: {:?}", e);
                        }
                    }
                }
                Err(e) => {
                    println!("✗ Compilation error: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("✗ Parse error: {:?}", e);
        }
    }
    println!();
}
