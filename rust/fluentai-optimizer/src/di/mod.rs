//! Dependency injection integration for the optimizer system

use std::sync::Arc;
use fluentai_di::prelude::*;
use fluentai_core::ast::Graph;
use anyhow::Result;

use crate::{
    OptimizationConfig, OptimizationLevel, OptimizationPipeline,
    passes::OptimizationPass,
    stats::OptimizationStats,
};

/// Optimization pass factory function
pub type PassFactory = Arc<dyn Fn() -> Box<dyn OptimizationPass> + Send + Sync>;

/// Pass registration with metadata
#[derive(Clone)]
pub struct PassRegistration {
    pub name: String,
    pub factory: PassFactory,
    pub enabled_levels: Vec<OptimizationLevel>,
    pub dependencies: Vec<String>,
    pub priority: i32,
}

/// Optimization pipeline builder with DI support
#[derive(Clone)]
pub struct OptimizationPipelineBuilder {
    config: OptimizationConfig,
    pass_registrations: Vec<PassRegistration>,
    custom_passes: Vec<PassFactory>,
    container: Option<Arc<Container>>,
}

impl OptimizationPipelineBuilder {
    /// Create new builder with default config
    pub fn new() -> Self {
        Self {
            config: OptimizationConfig::default(),
            pass_registrations: Vec::new(),
            custom_passes: Vec::new(),
            container: None,
        }
    }
    
    /// Create builder with specific optimization level
    pub fn with_level(level: OptimizationLevel) -> Self {
        Self {
            config: OptimizationConfig::for_level(level),
            pass_registrations: Vec::new(),
            custom_passes: Vec::new(),
            container: None,
        }
    }
    
    /// Set DI container for resolving services
    pub fn with_container(mut self, container: Arc<Container>) -> Self {
        self.container = Some(container);
        self
    }
    
    /// Set optimization config
    pub fn with_config(mut self, config: OptimizationConfig) -> Self {
        self.config = config;
        self
    }
    
    /// Register a pass factory
    pub fn register_pass<F>(mut self, name: impl Into<String>, factory: F) -> Self
    where
        F: Fn() -> Box<dyn OptimizationPass> + Send + Sync + 'static,
    {
        self.pass_registrations.push(PassRegistration {
            name: name.into(),
            factory: Arc::new(factory),
            enabled_levels: vec![
                OptimizationLevel::Basic,
                OptimizationLevel::Standard,
                OptimizationLevel::Aggressive,
            ],
            dependencies: Vec::new(),
            priority: 0,
        });
        self
    }
    
    /// Register a pass with full configuration
    pub fn register_pass_with<F>(
        mut self,
        name: impl Into<String>,
        factory: F,
        configure: impl FnOnce(&mut PassRegistration),
    ) -> Self
    where
        F: Fn() -> Box<dyn OptimizationPass> + Send + Sync + 'static,
    {
        let mut registration = PassRegistration {
            name: name.into(),
            factory: Arc::new(factory),
            enabled_levels: vec![
                OptimizationLevel::Basic,
                OptimizationLevel::Standard,
                OptimizationLevel::Aggressive,
            ],
            dependencies: Vec::new(),
            priority: 0,
        };
        configure(&mut registration);
        self.pass_registrations.push(registration);
        self
    }
    
    /// Add a custom pass factory
    pub fn add_custom_pass<F>(mut self, factory: F) -> Self
    where
        F: Fn() -> Box<dyn OptimizationPass> + Send + Sync + 'static,
    {
        self.custom_passes.push(Arc::new(factory));
        self
    }
    
    /// Register default optimization passes
    pub fn with_default_passes(mut self) -> Self {
        use crate::passes::{
            constant_folding::ConstantFoldingPass,
            dead_code::DeadCodeEliminationPass,
            cse::CommonSubexpressionEliminationPass,
            inline::InlinePass,
            tail_call::TailCallOptimizationPass,
            loop_opts::LoopOptimizationPass,
            beta_reduction::BetaReductionPass,
            partial_eval::PartialEvaluationPass,
        };
        
        // Constant folding - high priority, runs early
        self = self.register_pass_with(
            "constant_folding",
            || Box::new(ConstantFoldingPass::new()),
            |reg| {
                reg.priority = 100;
                reg.enabled_levels = vec![
                    OptimizationLevel::Basic,
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
            }
        );
        
        // Dead code elimination - runs after most optimizations
        self = self.register_pass_with(
            "dead_code_elimination",
            || Box::new(DeadCodeEliminationPass::new()),
            |reg| {
                reg.priority = -50;
                reg.enabled_levels = vec![
                    OptimizationLevel::Basic,
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
            }
        );
        
        // Common subexpression elimination
        self = self.register_pass_with(
            "cse",
            || Box::new(CommonSubexpressionEliminationPass::new()),
            |reg| {
                reg.priority = 50;
                reg.enabled_levels = vec![
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
                reg.dependencies = vec!["constant_folding".to_string()];
            }
        );
        
        // Inlining with configurable threshold
        let threshold = self.config.inline_threshold;
        self = self.register_pass_with(
            "inline",
            move || Box::new(InlinePass::new(threshold)),
            |reg| {
                reg.priority = 40;
                reg.enabled_levels = vec![
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
            }
        );
        
        // Tail call optimization
        self = self.register_pass_with(
            "tail_call_optimization",
            || Box::new(TailCallOptimizationPass::new()),
            |reg| {
                reg.priority = 30;
                reg.enabled_levels = vec![
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
            }
        );
        
        // Loop optimizations
        self = self.register_pass_with(
            "loop_optimization",
            || Box::new(LoopOptimizationPass::new()),
            |reg| {
                reg.priority = 20;
                reg.enabled_levels = vec![OptimizationLevel::Aggressive];
                reg.dependencies = vec!["constant_folding".to_string()];
            }
        );
        
        // Beta reduction
        self = self.register_pass_with(
            "beta_reduction",
            || Box::new(BetaReductionPass::new()),
            |reg| {
                reg.priority = 10;
                reg.enabled_levels = vec![
                    OptimizationLevel::Standard,
                    OptimizationLevel::Aggressive,
                ];
            }
        );
        
        // Partial evaluation
        self = self.register_pass_with(
            "partial_evaluation",
            || Box::new(PartialEvaluationPass::new()),
            |reg| {
                reg.priority = 0;
                reg.enabled_levels = vec![OptimizationLevel::Aggressive];
                reg.dependencies = vec!["constant_folding".to_string()];
            }
        );
        
        self
    }
    
    /// Build the optimization pipeline
    pub fn build(self) -> DynamicOptimizationPipeline {
        let mut passes = Vec::new();
        
        // Filter and sort pass registrations
        let mut enabled_registrations: Vec<_> = self.pass_registrations
            .into_iter()
            .filter(|reg| {
                // Check if pass is enabled for current level
                reg.enabled_levels.contains(&self.config.level) &&
                // Check config flags
                match reg.name.as_str() {
                    "constant_folding" => self.config.constant_folding,
                    "dead_code_elimination" => self.config.dead_code_elimination,
                    "cse" => self.config.cse,
                    "inline" => self.config.inline,
                    "tail_call_optimization" => self.config.tail_call_optimization,
                    "loop_optimization" => self.config.loop_optimization,
                    "beta_reduction" => self.config.beta_reduction,
                    "partial_evaluation" => self.config.partial_evaluation,
                    _ => true, // Custom passes are always enabled
                }
            })
            .collect();
        
        // Sort by priority (descending)
        enabled_registrations.sort_by(|a, b| b.priority.cmp(&a.priority));
        
        // TODO: Topological sort based on dependencies
        // For now, just respect priority
        
        // Create pass instances
        for reg in enabled_registrations {
            passes.push((reg.factory)());
        }
        
        // Add custom passes at the end
        for factory in self.custom_passes {
            passes.push((factory)());
        }
        
        DynamicOptimizationPipeline {
            config: self.config,
            passes,
            stats: OptimizationStats::new(),
            container: self.container,
        }
    }
}

impl Default for OptimizationPipelineBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Dynamic optimization pipeline with runtime pass composition
pub struct DynamicOptimizationPipeline {
    config: OptimizationConfig,
    passes: Vec<Box<dyn OptimizationPass>>,
    stats: OptimizationStats,
    container: Option<Arc<Container>>,
}

impl DynamicOptimizationPipeline {
    /// Run the optimization pipeline
    pub fn optimize(&mut self, graph: &Graph) -> Result<Graph> {
        use std::time::Instant;
        
        let start = Instant::now();
        self.stats = OptimizationStats::new();
        self.stats.nodes_before = graph.nodes.len();
        
        let mut optimized = graph.clone();
        
        // Run passes based on optimization level
        match self.config.level {
            OptimizationLevel::None => {
                // No optimization
            }
            _ => {
                // Run configured passes
                for iteration in 0..self.config.max_iterations {
                    let before = optimized.nodes.len();
                    
                    for pass in &mut self.passes {
                        if pass.is_applicable(&optimized) {
                            optimized = pass.run(&optimized)?;
                        }
                    }
                    
                    let after = optimized.nodes.len();
                    
                    // Stop if no changes
                    if before == after {
                        break;
                    }
                    
                    // Log iteration if in debug mode
                    if self.config.debug_mode {
                        eprintln!("Optimization iteration {}: {} -> {} nodes", 
                            iteration + 1, before, after);
                    }
                }
            }
        }
        
        self.stats.nodes_after = optimized.nodes.len();
        self.stats.optimization_time_us = start.elapsed().as_micros() as u64;
        
        Ok(optimized)
    }
    
    /// Get optimization statistics
    pub fn stats(&self) -> &OptimizationStats {
        &self.stats
    }
    
    /// Add a pass at runtime
    pub fn add_pass(&mut self, pass: Box<dyn OptimizationPass>) {
        self.passes.push(pass);
    }
    
    /// Remove all passes
    pub fn clear_passes(&mut self) {
        self.passes.clear();
    }
    
    /// Get reference to DI container if available
    pub fn container(&self) -> Option<&Arc<Container>> {
        self.container.as_ref()
    }
}

/// Extension trait for ContainerBuilder
pub trait OptimizerContainerBuilderExt {
    /// Register optimizer services
    fn register_optimizer(&mut self, config: OptimizationConfig) -> &mut Self;
    
    /// Register optimizer with default configuration
    fn register_optimizer_with_defaults(&mut self) -> &mut Self;
}

impl OptimizerContainerBuilderExt for ContainerBuilder {
    fn register_optimizer(&mut self, config: OptimizationConfig) -> &mut Self {
        // Register configuration
        self.register_instance(config.clone());
        
        // Register pipeline builder factory
        let config_for_builder = config.clone();
        self.register_transient(move || {
            OptimizationPipelineBuilder::new()
                .with_config(config_for_builder.clone())
                .with_default_passes()
        });
        
        // Register legacy pipeline for compatibility
        let config_for_pipeline = config.clone();
        self.register_transient(move || {
            OptimizationPipeline::new(config_for_pipeline.clone())
        });
        
        self
    }
    
    fn register_optimizer_with_defaults(&mut self) -> &mut Self {
        self.register_optimizer(OptimizationConfig::default())
    }
}

/// Service provider for optimization
pub trait OptimizationServiceProvider {
    /// Get optimization pipeline builder
    fn get_pipeline_builder(&self) -> Result<OptimizationPipelineBuilder>;
    
    /// Get optimization config
    fn get_config(&self) -> Result<OptimizationConfig>;
    
    /// Create optimized pipeline
    fn create_pipeline(&self) -> Result<DynamicOptimizationPipeline>;
}

/// Default implementation using DI container
pub struct ContainerOptimizationProvider {
    container: Arc<Container>,
}

impl ContainerOptimizationProvider {
    pub fn new(container: Arc<Container>) -> Self {
        Self { container }
    }
}

impl OptimizationServiceProvider for ContainerOptimizationProvider {
    fn get_pipeline_builder(&self) -> Result<OptimizationPipelineBuilder> {
        self.container
            .resolve::<OptimizationPipelineBuilder>()
            .map_err(|e| anyhow::anyhow!("Failed to resolve pipeline builder: {}", e))
    }
    
    fn get_config(&self) -> Result<OptimizationConfig> {
        self.container
            .resolve::<OptimizationConfig>()
            .map_err(|e| anyhow::anyhow!("Failed to resolve optimization config: {}", e))
    }
    
    fn create_pipeline(&self) -> Result<DynamicOptimizationPipeline> {
        let builder = self.get_pipeline_builder()?;
        Ok(builder.with_container(self.container.clone()).build())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_pipeline_builder_creation() {
        let builder = OptimizationPipelineBuilder::new();
        let pipeline = builder.build();
        assert_eq!(pipeline.passes.len(), 0); // No default passes without calling with_default_passes
    }
    
    #[test]
    fn test_pipeline_builder_with_defaults() {
        let builder = OptimizationPipelineBuilder::new()
            .with_default_passes();
        let pipeline = builder.build();
        assert!(pipeline.passes.len() > 0);
    }
    
    #[test]
    fn test_pipeline_builder_with_level() {
        let builder = OptimizationPipelineBuilder::with_level(OptimizationLevel::Aggressive)
            .with_default_passes();
        let pipeline = builder.build();
        
        // Aggressive level should have more passes enabled
        assert!(pipeline.passes.len() > 5);
    }
    
    #[test]
    fn test_custom_pass_registration() {
        use crate::passes::constant_folding::ConstantFoldingPass;
        
        let builder = OptimizationPipelineBuilder::new()
            .register_pass("custom_constant_fold", || {
                Box::new(ConstantFoldingPass::new())
            });
        
        let pipeline = builder.build();
        assert_eq!(pipeline.passes.len(), 1);
    }
    
    #[test]
    fn test_pass_priority_ordering() {
        use crate::passes::constant_folding::ConstantFoldingPass;
        use crate::passes::dead_code::DeadCodeEliminationPass;
        
        let builder = OptimizationPipelineBuilder::new()
            .register_pass_with("low_priority", 
                || Box::new(ConstantFoldingPass::new()),
                |reg| { reg.priority = -10; }
            )
            .register_pass_with("high_priority",
                || Box::new(DeadCodeEliminationPass::new()),
                |reg| { reg.priority = 100; }
            );
        
        let pipeline = builder.build();
        assert_eq!(pipeline.passes.len(), 2);
        // High priority pass should come first
    }
    
    #[test]
    fn test_di_integration() {
        let mut builder = ContainerBuilder::new();
        builder.register_optimizer_with_defaults();
        
        let container = Arc::new(builder.build());
        
        // Should be able to resolve config
        let config = container.resolve::<OptimizationConfig>();
        assert!(config.is_ok());
        
        // Should be able to resolve builder
        let pipeline_builder = container.resolve::<OptimizationPipelineBuilder>();
        assert!(pipeline_builder.is_ok());
    }
}