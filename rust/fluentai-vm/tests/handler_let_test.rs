// Test handler expressions in let bindings using FLC syntax
#[test]
fn test_handler_in_let_binding() {
    use fluentai_parser::parse;
    use fluentai_vm::{compiler::Compiler, VM};

    // Using FLC handler expression syntax
    let code = r#"
let result = handle {
    perform Error.raise("test")
} with {
    Error.raise(e) => 42
};
result
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
