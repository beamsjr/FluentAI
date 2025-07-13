
fn main() -> anyhow::Result<()> {
    // Test parsing nested closures
    let code = r#"
        (let ((x 10))
          (let ((make-adder (lambda (n)
                              (lambda (m) (+ (+ n m) x))))))
            (let ((add5 (make-adder 5)))
              (add5 7))))
    "#;
    
    println!("Parsing nested closure code:");
    match parse(code) {
        Ok(ast) => println!("Success: {:?}", ast),
        Err(e) => println!("Error: {}", e),
    }
    
    // Simpler version
    let code2 = r#"
        (lambda (n) (lambda (m) (+ n m)))
    "#;
    
    println!("\nParsing simple nested lambda:");
    match parse(code2) {
        Ok(ast) => println!("Success: {:?}", ast),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}