//! Tests for list method operations

#[cfg(test)]
mod tests {
    use crate::{Compiler, VM};
    use fluentai_core::value::Value;
    use fluentai_parser::parse_flc;

    #[test]
    fn test_list_map() {
        let source = r#"
            let nums = [1, 2, 3];
            nums.map(x => x * 2)
        "#;
        
        // Parse the source
        let graph = parse_flc(source).expect("Failed to parse");
        
        // Compile to bytecode
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        
        // Create and run VM
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        match result {
            Value::List(values) => {
                assert_eq!(values.len(), 3);
                assert_eq!(values[0], Value::Integer(2));
                assert_eq!(values[1], Value::Integer(4));
                assert_eq!(values[2], Value::Integer(6));
            }
            _ => panic!("Expected list result, got: {:?}", result),
        }
    }

    #[test]
    fn test_list_filter() {
        let source = r#"
            let nums = [1, 2, 3, 4, 5];
            nums.filter(x => x > 2)
        "#;
        
        let graph = parse_flc(source).expect("Failed to parse");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        match result {
            Value::List(values) => {
                assert_eq!(values.len(), 3);
                assert_eq!(values[0], Value::Integer(3));
                assert_eq!(values[1], Value::Integer(4));
                assert_eq!(values[2], Value::Integer(5));
            }
            _ => panic!("Expected list result, got: {:?}", result),
        }
    }

    #[test]
    fn test_list_reduce() {
        let source = r#"
            let nums = [1, 2, 3, 4];
            nums.reduce(0, (acc, x) => acc + x)
        "#;
        
        let graph = parse_flc(source).expect("Failed to parse");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        match result {
            Value::Integer(10) => {},
            _ => panic!("Expected integer 10, got: {:?}", result),
        }
    }

    #[test]
    fn test_list_length() {
        let source = r#"
            let nums = [1, 2, 3, 4, 5];
            nums.length()
        "#;
        
        let graph = parse_flc(source).expect("Failed to parse");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        match result {
            Value::Integer(5) => {},
            _ => panic!("Expected integer 5, got: {:?}", result),
        }
    }

    #[test]
    fn test_list_append() {
        let source = r#"
            let nums = [1, 2, 3];
            nums.append(4)
        "#;
        
        let graph = parse_flc(source).expect("Failed to parse");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        match result {
            Value::List(values) => {
                assert_eq!(values.len(), 4);
                assert_eq!(values[0], Value::Integer(1));
                assert_eq!(values[1], Value::Integer(2));
                assert_eq!(values[2], Value::Integer(3));
                assert_eq!(values[3], Value::Integer(4));
            }
            _ => panic!("Expected list result, got: {:?}", result),
        }
    }

    #[test]
    fn test_chained_list_operations() {
        let source = r#"
            let nums = [1, 2, 3, 4, 5];
            nums
                .filter(x => x > 2)
                .map(x => x * 2)
                .reduce(0, (acc, x) => acc + x)
        "#;
        
        let graph = parse_flc(source).expect("Failed to parse");
        let mut compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).expect("Failed to compile");
        let mut vm = VM::new(bytecode);
        let result = vm.run().expect("VM execution failed");
        
        // filter: [3, 4, 5]
        // map: [6, 8, 10]
        // reduce: 6 + 8 + 10 = 24
        match result {
            Value::Integer(24) => {},
            _ => panic!("Expected integer 24, got: {:?}", result),
        }
    }
}