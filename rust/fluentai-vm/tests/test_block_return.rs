#[cfg(test)]
mod tests {
    use fluentai_core::value::Value;
    use fluentai_parser::parse_flc;
    use fluentai_vm::{Compiler, CompilerOptions, OptimizationLevel, VM};
    
    #[test]
    fn test_block_return_debug() {
        let code = "{ let x = 10; x }";
        
        println!("Testing code: {}", code);
        
        let graph = parse_flc(code).unwrap();
        println!("Parsed graph root: {:?}", graph.root_id);
        
        if let Some(root_id) = graph.root_id {
            let root_node = graph.get_node(root_id);
            println!("Root node: {:?}", root_node);
            
            // Print the whole graph structure
            for (id, node) in &graph.nodes {
                println!("Node {}: {:?}", id, node);
            }
        }
        
        let options = CompilerOptions {
            optimization_level: OptimizationLevel::None,
            ..Default::default()
        };
        let compiler = Compiler::with_options(options);
        let bytecode = compiler.compile(&graph).unwrap();
        
        println!("Bytecode instructions:");
        if let Some(chunk) = bytecode.chunks.get(bytecode.main_chunk) {
            for (i, instr) in chunk.instructions.iter().enumerate() {
                println!("  {}: {:?}", i, instr);
            }
        }
        
        let mut vm = VM::new(bytecode);
        let result = vm.run().unwrap();
        
        println!("Result: {:?}", result);
        assert_eq!(result, Value::Integer(10), "Block should return last expression");
    }
}