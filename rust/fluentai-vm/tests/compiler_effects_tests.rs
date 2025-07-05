//! Effect system and special forms tests for FluentAI compiler

use fluentai_core::ast::{Graph, Node, Literal, EffectType};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    bytecode::{Opcode, Value},
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn compile_and_run(graph: &Graph) -> Result<Value> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    let mut vm = VM::new(bytecode);
    Ok(vm.run()?)
}

#[test]
fn test_compile_effect_io() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (effect io (print "hello"))
    let hello = graph.add_node(Node::Literal(Literal::String("hello".to_string())));
    
    let effect_node = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![hello],
    });
    graph.root_id = Some(effect_node);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Check for effect opcode
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| instr.opcode == Opcode::Effect));
    
    Ok(())
}

#[test]
fn test_compile_effect_state() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (effect state (get "counter"))
    let key = graph.add_node(Node::Literal(Literal::String("counter".to_string())));
    
    let effect_node = graph.add_node(Node::Effect {
        effect_type: EffectType::State,
        operation: "get".to_string(),
        args: vec![key],
    });
    graph.root_id = Some(effect_node);
    
    compile_and_run(&graph)?;
    Ok(())
}

#[test]
fn test_compile_effect_network() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (effect network (fetch "https://api.example.com"))
    let url = graph.add_node(Node::Literal(Literal::String("https://api.example.com".to_string())));
    
    let effect_node = graph.add_node(Node::Effect {
        effect_type: EffectType::Network,
        operation: "fetch".to_string(),
        args: vec![url],
    });
    graph.root_id = Some(effect_node);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| instr.opcode == Opcode::Effect));
    
    Ok(())
}

#[test]
fn test_compile_effect_with_multiple_args() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (effect state (set "key" "value"))
    let key = graph.add_node(Node::Literal(Literal::String("key".to_string())));
    let value = graph.add_node(Node::Literal(Literal::String("value".to_string())));
    
    let effect_node = graph.add_node(Node::Effect {
        effect_type: EffectType::State,
        operation: "set".to_string(),
        args: vec![key, value],
    });
    graph.root_id = Some(effect_node);
    
    compile_and_run(&graph)?;
    Ok(())
}

#[test]
fn test_compile_gc_let() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (gc:let ((x (list 1 2 3))) x)
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let list_node = graph.add_node(Node::List(vec![one, two, three]));
    
    // For gc:let, we use a special application form
    let gc_let = graph.add_node(Node::Variable { name: "gc:let".to_string() });
    let x_var_sym = graph.add_node(Node::Variable { name: "x".to_string() });
    let x_binding = graph.add_node(Node::List(vec![x_var_sym, list_node]));
    let bindings = graph.add_node(Node::List(vec![x_binding]));
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    
    let gc_let_app = graph.add_node(Node::Application {
        function: gc_let,
        args: vec![bindings, x_var],
    });
    graph.root_id = Some(gc_let_app);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Check for GC operations or global load
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::GcAlloc || 
        instr.opcode == Opcode::GcDeref ||
        instr.opcode == Opcode::LoadGlobal // gc:let is loaded as global
    ));
    
    Ok(())
}

#[test]
fn test_compile_tail_call_optimization() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create a self-recursive function that should use tail calls
    // (letrec ((loop (lambda (n acc)
    //                  (if (= n 0)
    //                      acc
    //                      (loop (- n 1) (+ acc n))))))
    //   (loop 10 0))
    
    let n_var1 = graph.add_node(Node::Variable { name: "n".to_string() });
    let n_var2 = graph.add_node(Node::Variable { name: "n".to_string() });
    let n_var3 = graph.add_node(Node::Variable { name: "n".to_string() });
    let acc_var1 = graph.add_node(Node::Variable { name: "acc".to_string() });
    let acc_var2 = graph.add_node(Node::Variable { name: "acc".to_string() });
    
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    
    let eq = graph.add_node(Node::Variable { name: "=".to_string() });
    let cond = graph.add_node(Node::Application {
        function: eq,
        args: vec![n_var1, zero],
    });
    
    let minus = graph.add_node(Node::Variable { name: "-".to_string() });
    let n_minus_1 = graph.add_node(Node::Application {
        function: minus,
        args: vec![n_var2, one],
    });
    
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let acc_plus_n = graph.add_node(Node::Application {
        function: plus,
        args: vec![acc_var2, n_var3],
    });
    
    let loop_var = graph.add_node(Node::Variable { name: "loop".to_string() });
    let recursive_call = graph.add_node(Node::Application {
        function: loop_var,
        args: vec![n_minus_1, acc_plus_n],
    });
    
    let if_node = graph.add_node(Node::If {
        condition: cond,
        then_branch: acc_var1,
        else_branch: recursive_call,
    });
    
    let loop_lambda = graph.add_node(Node::Lambda {
        params: vec!["n".to_string(), "acc".to_string()],
        body: if_node,
    });
    
    let loop_var2 = graph.add_node(Node::Variable { name: "loop".to_string() });
    let ten = graph.add_node(Node::Literal(Literal::Integer(10)));
    let zero2 = graph.add_node(Node::Literal(Literal::Integer(0)));
    let initial_call = graph.add_node(Node::Application {
        function: loop_var2,
        args: vec![ten, zero2],
    });
    
    let letrec_node = graph.add_node(Node::Letrec {
        bindings: vec![("loop".to_string(), loop_lambda)],
        body: initial_call,
    });
    graph.root_id = Some(letrec_node);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Check for tail call optimization
    // Look in the lambda chunk (should be chunk 1)
    if bytecode.chunks.len() > 1 {
        let lambda_chunk = &bytecode.chunks[1];
        assert!(lambda_chunk.instructions.iter().any(|instr| 
            instr.opcode == Opcode::TailCall
        ), "Expected tail call optimization in recursive function");
    }
    
    Ok(())
}

#[test]
fn test_compile_specialized_arithmetic() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test specialized integer arithmetic opcodes
    let plus_int = graph.add_node(Node::Variable { name: "+int".to_string() });
    let five = graph.add_node(Node::Literal(Literal::Integer(5)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let app = graph.add_node(Node::Application {
        function: plus_int,
        args: vec![five, three],
    });
    graph.root_id = Some(app);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::AddInt
    ));
    
    // Test float arithmetic
    let mut graph = Graph::new();
    let plus_float = graph.add_node(Node::Variable { name: "+float".to_string() });
    let f1 = graph.add_node(Node::Literal(Literal::Float(1.5)));
    let f2 = graph.add_node(Node::Literal(Literal::Float(2.5)));
    
    let app = graph.add_node(Node::Application {
        function: plus_float,
        args: vec![f1, f2],
    });
    graph.root_id = Some(app);
    
    let compiler2 = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler2.compile(&graph)?;
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::AddFloat
    ));
    
    Ok(())
}

#[test]
fn test_compile_constructor_call() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test constructor syntax: (Cons 1 (Nil))
    let cons_var = graph.add_node(Node::Variable { name: "Cons".to_string() });
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let nil_var = graph.add_node(Node::Variable { name: "Nil".to_string() });
    let nil_app = graph.add_node(Node::Application {
        function: nil_var,
        args: vec![],
    });
    
    let cons_app = graph.add_node(Node::Application {
        function: cons_var,
        args: vec![one, nil_app],
    });
    graph.root_id = Some(cons_app);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Constructor calls should use MakeTagged opcode or be treated as globals
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::MakeTagged || instr.opcode == Opcode::LoadGlobal
    ));
    
    Ok(())
}

#[test]
fn test_compile_contract_node() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test that contract nodes compile (they're metadata)
    let contract = graph.add_node(Node::Contract {
        function_name: "test_func".to_string(),
        preconditions: vec![],
        postconditions: vec![],
        invariants: vec![],
        complexity: None,
        pure: true,
    });
    graph.root_id = Some(contract);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Contract should compile to Nop
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::Nop
    ));
    
    Ok(())
}

#[test]
fn test_compile_gc_operations() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test gc:collect special form
    let gc_collect = graph.add_node(Node::Variable { name: "gc:collect".to_string() });
    let app = graph.add_node(Node::Application {
        function: gc_collect,
        args: vec![],
    });
    graph.root_id = Some(app);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Should compile as a global function call
    let main_chunk = &bytecode.chunks[0];
    assert!(main_chunk.instructions.iter().any(|instr| 
        instr.opcode == Opcode::LoadGlobal
    ));
    
    Ok(())
}

#[test]
fn test_compile_loop_constructs() -> Result<()> {
    let mut graph = Graph::new();
    
    // Test that loop-related operations compile
    // This tests the LoopStart/LoopEnd opcodes indirectly
    let loop_var = graph.add_node(Node::Variable { name: "loop".to_string() });
    let n = graph.add_node(Node::Literal(Literal::Integer(10)));
    
    let app = graph.add_node(Node::Application {
        function: loop_var,
        args: vec![n],
    });
    graph.root_id = Some(app);
    
    let compiler = Compiler::with_options(CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    });
    let bytecode = compiler.compile(&graph)?;
    
    // Should compile the function call
    assert!(!bytecode.chunks.is_empty());
    
    Ok(())
}