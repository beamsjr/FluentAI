#[test]
fn test_handler_in_let_binding() {
    use fluentai_vm::{VM, compiler::Compiler};
    use fluentai_parser::parse;
    
    let code = r#"
(let ((result
        (handler
            ((error (lambda (e) 42)))
            (effect error:raise "test"))))
    result)
"#;
    
    let ast = parse(code).expect("Failed to parse");
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).expect("Failed to compile");
    
    let mut vm = VM::new(bytecode);
    let result = vm.run();
    
    // The handler should catch the error and return 42
    match result {
        Ok(value) => {
            assert_eq!(value, fluentai_vm::Value::Integer(42));
        }
        Err(e) => {
            panic!("Unexpected error: {:?}", e);
        }
    }
}