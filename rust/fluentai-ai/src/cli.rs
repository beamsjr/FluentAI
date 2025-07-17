//! CLI interface for AI analysis

use crate::analyzer::{AiAnalyzerBuilder, AiAnalyzer};
use crate::error::Result;
use crate::metadata::{MetadataInjector, InjectorConfig};
use fluentai_core::ast::Graph;
use std::path::Path;
use tracing::{info, error};

/// CLI configuration for AI analysis
#[derive(Debug, Clone)]
pub struct AiCliConfig {
    /// Enable optimization detection
    pub detect_optimizations: bool,
    /// Enable pattern detection
    pub detect_patterns: bool,
    /// Enable embedding generation
    pub generate_embeddings: bool,
    /// Inject metadata back into AST
    pub inject_metadata: bool,
    /// Output format
    pub output_format: OutputFormat,
    /// Verbosity level
    pub verbose: bool,
}

impl Default for AiCliConfig {
    fn default() -> Self {
        Self {
            detect_optimizations: true,
            detect_patterns: true,
            generate_embeddings: false,
            inject_metadata: false,
            output_format: OutputFormat::Human,
            verbose: false,
        }
    }
}

/// Output format for analysis results
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OutputFormat {
    /// Human-readable format
    Human,
    /// JSON format
    Json,
    /// Compact format
    Compact,
}

/// CLI handler for AI analysis
pub struct AiCli {
    analyzer: AiAnalyzer,
    config: AiCliConfig,
}

impl AiCli {
    /// Create a new CLI handler
    pub fn new(config: AiCliConfig) -> Self {
        let analyzer = AiAnalyzerBuilder::new()
            .detect_optimizations(config.detect_optimizations)
            .detect_patterns(config.detect_patterns)
            .generate_embeddings(config.generate_embeddings)
            .build();
        
        Self { analyzer, config }
    }
    
    /// Analyze an AST graph and output results
    pub fn analyze_and_output(&self, graph: &mut Graph) -> Result<()> {
        info!("Starting AI analysis");
        
        // Perform analysis
        let analysis = self.analyzer.analyze_graph(graph)?;
        
        // Inject metadata if requested
        if self.config.inject_metadata {
            let injector = MetadataInjector::new(InjectorConfig::default());
            injector.inject(graph, &analysis)?;
            info!("Metadata injected into AST");
        }
        
        // Output results
        match self.config.output_format {
            OutputFormat::Human => self.output_human(&analysis),
            OutputFormat::Json => self.output_json(&analysis)?,
            OutputFormat::Compact => self.output_compact(&analysis),
        }
        
        // Output cache statistics if verbose
        if self.config.verbose {
            let stats = self.analyzer.cache_stats();
            println!("\nCache Statistics:");
            println!("  Hits: {}", stats.hits);
            println!("  Misses: {}", stats.misses);
            println!("  Hit rate: {:.1}%", 
                if stats.hits + stats.misses > 0 {
                    (stats.hits as f64 / (stats.hits + stats.misses) as f64) * 100.0
                } else {
                    0.0
                }
            );
        }
        
        Ok(())
    }
    
    /// Output results in human-readable format
    fn output_human(&self, analysis: &crate::analysis::AiAnalysisResult) {
        println!("=== AI Analysis Results ===\n");
        
        println!("Performance Score: {:.2}", analysis.performance_score);
        println!("Confidence: {:.2}", analysis.confidence);
        
        if !analysis.optimizations.is_empty() {
            println!("\nOptimization Opportunities:");
            for (i, opt) in analysis.optimizations.iter().enumerate() {
                println!("  {}. {:?}", i + 1, opt.optimization_type);
                println!("     Target nodes: {:?}", opt.target_nodes);
                println!("     Expected improvement: {:.1}%", opt.expected_improvement * 100.0);
                println!("     {}", opt.description);
            }
        }
        
        if !analysis.patterns.is_empty() {
            println!("\nDetected Patterns:");
            for pattern in &analysis.patterns {
                println!("  - {:?} (confidence: {:.2})", pattern.pattern_type, pattern.confidence);
                println!("    Nodes: {:?}", pattern.nodes);
            }
        }
        
        if self.config.generate_embeddings && !analysis.embeddings.is_empty() {
            println!("\nGenerated embeddings for {} nodes", analysis.embeddings.len());
        }
    }
    
    /// Output results in JSON format
    fn output_json(&self, analysis: &crate::analysis::AiAnalysisResult) -> Result<()> {
        let json = serde_json::to_string_pretty(analysis)
            .map_err(|e| crate::error::AiError::Other(e.into()))?;
        println!("{}", json);
        Ok(())
    }
    
    /// Output results in compact format
    fn output_compact(&self, analysis: &crate::analysis::AiAnalysisResult) {
        println!("Score: {:.2} | Confidence: {:.2} | Optimizations: {} | Patterns: {}",
            analysis.performance_score,
            analysis.confidence,
            analysis.optimizations.len(),
            analysis.patterns.len()
        );
    }
}

/// Parse CLI arguments into configuration
pub fn parse_cli_args(args: Vec<String>) -> Result<AiCliConfig> {
    let mut config = AiCliConfig::default();
    
    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--no-optimizations" => config.detect_optimizations = false,
            "--no-patterns" => config.detect_patterns = false,
            "--embeddings" => config.generate_embeddings = true,
            "--inject-metadata" => config.inject_metadata = true,
            "--json" => config.output_format = OutputFormat::Json,
            "--compact" => config.output_format = OutputFormat::Compact,
            "--verbose" | "-v" => config.verbose = true,
            "--help" | "-h" => {
                print_help();
                std::process::exit(0);
            }
            arg if arg.starts_with("--") => {
                error!("Unknown argument: {}", arg);
                return Err(crate::error::AiError::Other(
                    anyhow::anyhow!("Unknown argument: {}", arg)
                ));
            }
            _ => {}
        }
        i += 1;
    }
    
    Ok(config)
}

/// Print help message
fn print_help() {
    println!("FluentAI AST Analysis Tool");
    println!("\nUsage: fluentai-analyze [OPTIONS]");
    println!("\nOptions:");
    println!("  --no-optimizations    Disable optimization detection");
    println!("  --no-patterns        Disable pattern detection");
    println!("  --embeddings         Generate node embeddings");
    println!("  --inject-metadata    Inject analysis results back into AST");
    println!("  --json               Output results as JSON");
    println!("  --compact            Output results in compact format");
    println!("  -v, --verbose        Enable verbose output");
    println!("  -h, --help           Show this help message");
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_cli_args() {
        let args = vec![
            "--embeddings".to_string(),
            "--json".to_string(),
            "--verbose".to_string(),
        ];
        
        let config = parse_cli_args(args).unwrap();
        assert!(config.generate_embeddings);
        assert_eq!(config.output_format, OutputFormat::Json);
        assert!(config.verbose);
    }
    
    #[test]
    fn test_cli_output() {
        use fluentai_core::ast::{Node, Literal};
        
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        
        let cli = AiCli::new(AiCliConfig::default());
        // This should not panic
        cli.analyze_and_output(&mut graph).unwrap();
    }
}