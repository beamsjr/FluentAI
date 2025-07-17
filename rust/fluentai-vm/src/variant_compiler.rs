//! Simple variant compiler for learning mode
//! 
//! This module provides basic variant compilation without requiring
//! the full optimizer dependency, avoiding circular dependencies.

use crate::{Compiler, CompilerOptions, OptimizationLevel, OptimizationStrategy, VMResult};
use fluentai_bytecode::Bytecode;
use fluentai_core::ast::Graph;
use std::sync::Arc;

/// Variant compiler for creating optimized function variants
pub struct VariantCompiler {
    base_compiler: Compiler,
}

impl VariantCompiler {
    /// Create a new variant compiler
    pub fn new() -> Self {
        Self {
            base_compiler: Compiler::new(),
        }
    }
    
    /// Compile a function variant with a specific optimization strategy
    pub fn compile_variant(
        &mut self,
        ast: &Arc<Graph>,
        function_name: &str,
        strategy: OptimizationStrategy,
    ) -> VMResult<Bytecode> {
        // Create compiler options based on the strategy
        let options = match strategy {
            OptimizationStrategy::None => CompilerOptions {
                optimization_level: OptimizationLevel::None,
                debug_info: false,
                #[cfg(feature = "ai-analysis")]
                ai_optimization: false,
                #[cfg(feature = "ai-analysis")]
                hybrid_optimization: false,
            },
            OptimizationStrategy::Basic => CompilerOptions {
                optimization_level: OptimizationLevel::Basic,
                debug_info: false,
                #[cfg(feature = "ai-analysis")]
                ai_optimization: false,
                #[cfg(feature = "ai-analysis")]
                hybrid_optimization: false,
            },
            OptimizationStrategy::Standard => CompilerOptions {
                optimization_level: OptimizationLevel::Standard,
                debug_info: false,
                #[cfg(feature = "ai-analysis")]
                ai_optimization: false,
                #[cfg(feature = "ai-analysis")]
                hybrid_optimization: false,
            },
            OptimizationStrategy::Aggressive => CompilerOptions {
                optimization_level: OptimizationLevel::Aggressive,
                debug_info: false,
                #[cfg(feature = "ai-analysis")]
                ai_optimization: false,
                #[cfg(feature = "ai-analysis")]
                hybrid_optimization: false,
            },
            OptimizationStrategy::Custom(mask) => {
                // Map custom masks to appropriate optimization levels
                let optimization_level = match mask.count_ones() {
                    0..=1 => OptimizationLevel::None,      // Single optimization
                    2..=3 => OptimizationLevel::Basic,     // Few optimizations
                    4..=6 => OptimizationLevel::Standard,  // Several optimizations
                    _ => OptimizationLevel::Aggressive,    // Many optimizations
                };
                
                CompilerOptions {
                    optimization_level,
                    debug_info: false,
                    #[cfg(feature = "ai-analysis")]
                    ai_optimization: false,
                    #[cfg(feature = "ai-analysis")]
                    hybrid_optimization: false,
                }
            },
        };
        
        // Clone the AST (in a real implementation, we might transform it based on the strategy)
        let optimized_ast = ast.clone();
        
        // Create a new compiler with the specific options
        let compiler = Compiler::with_options(options);
        
        // Compile the AST
        compiler.compile(&optimized_ast)
            .map_err(|e| crate::error::VMError::RuntimeError { 
                message: format!("Failed to compile variant: {}", e),
                stack_trace: None,
            })
    }
    
    /// Get a simple hash to identify a variant
    pub fn variant_id(function_name: &str, strategy: OptimizationStrategy) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        
        let mut hasher = DefaultHasher::new();
        function_name.hash(&mut hasher);
        // Hash the strategy discriminant
        match strategy {
            OptimizationStrategy::None => 0u32.hash(&mut hasher),
            OptimizationStrategy::Basic => 1u32.hash(&mut hasher),
            OptimizationStrategy::Standard => 2u32.hash(&mut hasher),
            OptimizationStrategy::Aggressive => 3u32.hash(&mut hasher),
            OptimizationStrategy::Custom(flags) => {
                4u32.hash(&mut hasher);
                flags.hash(&mut hasher);
            }
        }
        hasher.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, Node, Literal};
    
    #[test]
    fn test_variant_compilation() {
        let mut compiler = VariantCompiler::new();
        
        // Create a simple AST
        let mut graph = Graph::new();
        let root = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        graph.root_id = Some(root);
        let ast = Arc::new(graph);
        
        // Compile with different strategies
        let strategies = vec![
            OptimizationStrategy::None,
            OptimizationStrategy::Basic,
            OptimizationStrategy::Standard,
            OptimizationStrategy::Aggressive,
        ];
        
        for strategy in strategies {
            let result = compiler.compile_variant(&ast, "test_func", strategy);
            assert!(result.is_ok());
            
            let chunk = result.unwrap();
            assert!(!chunk.instructions.is_empty());
        }
    }
    
    #[test]
    fn test_variant_id() {
        let id1 = VariantCompiler::variant_id("func1", OptimizationStrategy::Basic);
        let id2 = VariantCompiler::variant_id("func1", OptimizationStrategy::Aggressive);
        let id3 = VariantCompiler::variant_id("func2", OptimizationStrategy::Basic);
        
        // Different strategies should produce different IDs
        assert_ne!(id1, id2);
        // Different functions should produce different IDs
        assert_ne!(id1, id3);
        // Same function and strategy should produce same ID
        assert_eq!(id1, VariantCompiler::variant_id("func1", OptimizationStrategy::Basic));
    }
}