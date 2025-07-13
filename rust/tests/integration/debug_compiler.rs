use fluentai_vm::compiler::Compiler;

fn main() -> anyhow::Result<()> {
    let code = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) (+ x y))))
            (add-x 5)))
    "#;
    
    // Parse
    let ast = parse(code)?;
    
    // We need to create a custom compiler with debug prints
    println!("Can't easily debug internal compiler state without modifying the compiler");
    println!("The issue is likely in how we track stack positions vs local indices");
    
    Ok(())
}