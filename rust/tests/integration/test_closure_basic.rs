use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, VM};

fn main() {
    // Test 1: Simple closure capture
    let code = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) (+ x y))))
            (add-x 5)))
    "#;
    
    println!("Test 1: Simple closure capture");
    println!("Code: {}", code);
    
    let graph = parse(code).expect("Failed to parse");
    let mut compiler = Compiler::new();
    let chunk = compiler.compile(&graph).expect("Failed to compile");
    
    let mut vm = VM::new(chunk);
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 15)\n", result),
        Err(e) => println!("Error: {}\n", e),
    }
    
    // Test 2: Multiple captures
    let code2 = r#"
        (let ((x 10) (y 20))
          (let ((sum-xy (lambda (z) (+ x (+ y z)))))
            (sum-xy 5)))
    "#;
    
    println!("Test 2: Multiple captures");
    println!("Code: {}", code2);
    
    let graph2 = parse(code2).expect("Failed to parse");
    let mut compiler2 = Compiler::new();
    let chunk2 = compiler2.compile(&graph2).expect("Failed to compile");
    
    let mut vm2 = VM::new(chunk2);
    match vm2.run() {
        Ok(result) => println!("Result: {:?} (expected 35)\n", result),
        Err(e) => println!("Error: {}\n", e),
    }
    
    // Test 3: Nested closures
    let code3 = r#"
        (let ((x 10))
          (let ((make-adder (lambda (y) 
                              (lambda (z) (+ x (+ y z))))))
            (let ((add-15 (make-adder 5)))
              (add-15 3))))
    "#;
    
    println!("Test 3: Nested closures");
    println!("Code: {}", code3);
    
    let graph3 = parse(code3).expect("Failed to parse");
    let mut compiler3 = Compiler::new();
    let chunk3 = compiler3.compile(&graph3).expect("Failed to compile");
    
    let mut vm3 = VM::new(chunk3);
    match vm3.run() {
        Ok(result) => println!("Result: {:?} (expected 18)", result),
        Err(e) => println!("Error: {}", e),
    }
}