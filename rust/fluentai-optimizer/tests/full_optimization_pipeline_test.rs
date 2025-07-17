//! Integration tests for the full optimization pipeline with all 5 phases

use fluentai_optimizer::pipeline::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_optimizer::passes::{
    OptimizationPass,
    code_layout::{CodeLayoutConfig, CodeLayoutPass},
    effect_reordering::{EffectReorderingConfig, EffectReorderingPass},
    memory_aware::{MemoryAwareConfig, MemoryAwarePass},
    memoization::{AdaptiveMemoizationPass, MemoizationConfig},
    subgraph_fusion::{SubgraphFusionConfig, SubgraphFusionPass},
};
use fluentai_optimizer::runtime_guided::{
    HotPathInliningPass, ValueSpecializationPass, AdaptiveLoopUnrollingPass,
    RuntimeGuidedConfig,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal, EffectType};
use fluentai_core::profiling::{FunctionProfileData, ValueProfileData, LoopProfileData};

/// Create a complex AST that exercises all optimization phases
fn create_complex_ast() -> Graph {
    let mut graph = Graph::new();
    
    // Create a hot function that will be inlined
    let hot_func_body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let hot_func = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: hot_func_body,
    }).unwrap();
    
    // Create a loop that will be unrolled
    let loop_counter = graph.add_node(Node::Variable { name: "i".to_string() }).unwrap();
    let _loop_body = graph.add_node(Node::Application {
        function: hot_func,
        args: vec![loop_counter],
    }).unwrap();
    
    // Create multiple IO effects that can be reordered
    let print1 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    let print2 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "print".to_string(),
        args: vec![],
    }).unwrap();
    
    // Create a pure computation between effects
    let arg_node = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    let pure_calc = graph.add_node(Node::Application {
        function: hot_func,
        args: vec![arg_node],
    }).unwrap();
    
    // Create a list that will trigger memory optimization
    let item1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    let item2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let item3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
    let list = graph.add_node(Node::List(vec![item1, item2, item3])).unwrap();
    
    // Create a sequence with effects
    let effect_sequence = graph.add_node(Node::Begin {
        exprs: vec![print1, pure_calc, print2, list],
    }).unwrap();
    
    // Create a memoizable pure function
    let cond_node = graph.add_node(Node::Literal(Literal::Boolean(true))).unwrap();
    let then_node = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    let else_node = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
    let fib_body = graph.add_node(Node::If {
        condition: cond_node,
        then_branch: then_node,
        else_branch: else_node,
    }).unwrap();
    let fib_func = graph.add_node(Node::Lambda {
        params: vec!["n".to_string()],
        body: fib_body,
    }).unwrap();
    
    // Create the main program
    let main_body = graph.add_node(Node::Let {
        bindings: vec![
            ("hot_func".to_string(), hot_func),
            ("fib".to_string(), fib_func),
        ],
        body: effect_sequence,
    }).unwrap();
    
    graph.root_id = Some(main_body);
    graph
}

/// Create profiling data to guide runtime optimizations
fn create_profiling_data(graph: &Graph) -> RuntimeGuidedConfig {
    // Find the hot function
    let mut hot_funcs = Vec::new();
    for (_node_id, node) in graph.nodes() {
        if let Node::Lambda { .. } = node {
            hot_funcs.push(FunctionProfileData {
                chunk_id: 1, // Placeholder chunk_id
                name: Some("hot_func".to_string()),
                call_count: 10000,
                total_time_ns: 500_000_000, // 500ms in nanoseconds
                avg_time_ns: 50_000, // 50us average
            });
            break;
        }
    }
    
    // Create value specialization data
    let skewed_values = vec![
        ValueProfileData {
            node_id: NodeId(std::num::NonZeroU32::new(1).unwrap()),
            total_count: 10000,
            most_common: Some(("42".to_string(), 8000)),
            skew_percentage: 0.8,
        }
    ];
    
    // Create loop profiling data
    let hot_loops = vec![
        LoopProfileData {
            loop_id: NodeId(std::num::NonZeroU32::new(2).unwrap()),
            execution_count: 1000,
            avg_iterations: 10.0,
            max_iterations: 20,
        }
    ];
    
    RuntimeGuidedConfig {
        hot_functions: hot_funcs,
        skewed_values,
        hot_loops,
        hot_function_threshold: 1000,
        value_specialization_threshold: 0.7,
        loop_unroll_threshold: 5.0,
    }
}

#[test]
fn test_full_optimization_pipeline() {
    let graph = create_complex_ast();
    let profiling_data = create_profiling_data(&graph);
    
    // Create all optimization passes in order
    let mut passes: Vec<Box<dyn fluentai_optimizer::passes::OptimizationPass>> = vec![
        // Phase 1: Profiling is done separately (in VM)
        
        // Phase 2: Runtime-guided optimizations
        Box::new(HotPathInliningPass::new(profiling_data.clone())),
        Box::new(ValueSpecializationPass::new(profiling_data.clone())),
        Box::new(AdaptiveLoopUnrollingPass::new(profiling_data)),
        
        // Phase 3: Adaptive memoization
        Box::new(AdaptiveMemoizationPass::new(MemoizationConfig::default())),
        
        // Phase 4: AI-driven optimizations
        Box::new(EffectReorderingPass::new(EffectReorderingConfig::default())),
        Box::new(SubgraphFusionPass::new(SubgraphFusionConfig::default())),
        
        // Phase 5: Memory-aware and code layout
        Box::new(MemoryAwarePass::new(MemoryAwareConfig::default())),
        Box::new(CodeLayoutPass::new(CodeLayoutConfig::default())),
    ];
    
    // Run all passes
    let mut result = graph.clone();
    let mut total_stats = Vec::new();
    
    for pass in &mut passes {
        println!("Running: {}", pass.name());
        match pass.run(&result) {
            Ok(optimized) => {
                result = optimized;
                total_stats.push(pass.stats());
            }
            Err(e) => {
                panic!("Pass {} failed: {}", pass.name(), e);
            }
        }
    }
    
    // Verify the graph is still valid
    assert!(result.root_id.is_some());
    
    // Print optimization statistics
    println!("\n=== Optimization Pipeline Results ===");
    for stat in total_stats {
        println!("{}", stat);
    }
}

#[test]
fn test_phase_isolation() {
    let graph = create_complex_ast();
    
    // Test that each phase can run independently
    
    // Phase 2: Runtime-guided
    let profiling_data = create_profiling_data(&graph);
    let mut hot_path_pass = HotPathInliningPass::new(profiling_data.clone());
    let result = hot_path_pass.run(&graph);
    assert!(result.is_ok());
    
    // Phase 3: Memoization
    let mut memo_pass = AdaptiveMemoizationPass::new(MemoizationConfig::default());
    let result = memo_pass.run(&graph);
    assert!(result.is_ok());
    
    // Phase 4: AI-driven
    let mut effect_pass = EffectReorderingPass::new(EffectReorderingConfig::default());
    let result = effect_pass.run(&graph);
    assert!(result.is_ok());
    
    // Phase 5: Memory-aware
    let mut memory_pass = MemoryAwarePass::new(MemoryAwareConfig::default());
    let result = memory_pass.run(&graph);
    assert!(result.is_ok());
}

#[test]
fn test_optimization_levels() {
    let graph = create_complex_ast();
    
    // Test different optimization levels
    let levels = vec![
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ];
    
    for level in levels {
        let config = OptimizationConfig::for_level(level);
        
        let mut pipeline = OptimizationPipeline::new(config);
        let result = pipeline.optimize(&graph);
        
        match result {
            Ok(_) => println!("Level {:?} succeeded", level),
            Err(e) => println!("Level {:?} failed: {}", level, e),
        }
    }
}

#[test]
fn test_combined_effect_and_memory_optimization() {
    let mut graph = Graph::new();
    
    // Create a scenario where both effect reordering and memory optimization apply
    let list_items: Vec<NodeId> = (0..100).map(|i| {
        graph.add_node(Node::Literal(Literal::Integer(i))).unwrap()
    }).collect();
    let large_list = graph.add_node(Node::List(list_items)).unwrap();
    
    let io_effect1 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![large_list],
    }).unwrap();
    
    let pure_body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let pure_transform = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: pure_body,
    }).unwrap();
    
    let io_effect2 = graph.add_node(Node::Effect {
        effect_type: EffectType::IO,
        operation: "write".to_string(),
        args: vec![],
    }).unwrap();
    
    let main = graph.add_node(Node::Begin {
        exprs: vec![io_effect1, pure_transform, io_effect2],
    }).unwrap();
    
    graph.root_id = Some(main);
    
    // Run both optimization phases
    let mut effect_pass = EffectReorderingPass::new(EffectReorderingConfig::default());
    let mut memory_pass = MemoryAwarePass::new(MemoryAwareConfig::default());
    
    let result1 = effect_pass.run(&graph).unwrap();
    let result2 = memory_pass.run(&result1).unwrap();
    
    assert!(result2.root_id.is_some());
    println!("Effect pass: {}", effect_pass.stats());
    println!("Memory pass: {}", memory_pass.stats());
}