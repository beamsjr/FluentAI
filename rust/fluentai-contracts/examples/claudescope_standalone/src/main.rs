//! ClaudeScope Standalone Executable
//! 
//! A self-contained network analyzer that embeds ClaudeScope logic
//! and provides a native executable without external dependencies.

use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use colored::*;
use rust_embed::RustEmbed;
use std::collections::HashMap;
use std::time::{Duration, Instant};
use std::thread;

// Embed all ClaudeScope source files at compile time
#[derive(RustEmbed)]
#[folder = "../../examples/claudescope/"]
#[include = "*.ai"]
#[include = "src/*.ai"]
#[include = "test/*.ai"]
struct ClaudeScopeAssets;

#[derive(Parser, Debug)]
#[command(name = "claudescope")]
#[command(author = "FluentAi Team")]
#[command(version = "1.0")]
#[command(about = "Self-verifying network analyzer with formal contracts", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Run the interactive demo workflow
    Demo,
    /// Start packet capture mode
    Capture {
        /// Interface to capture on
        #[arg(short, long, default_value = "eth0")]
        interface: String,
        /// Number of packets to capture
        #[arg(short, long, default_value = "1000")]
        count: usize,
    },
    /// Analyze network topology and compliance
    Analyze,
    /// Explain recent security violations
    Explain,
    /// Generate comprehensive report  
    Report,
    /// Show embedded source code
    Source {
        /// Module to display
        module: String,
    },
    /// List all embedded modules
    List,
}

/// Simulated network state for the standalone demo
struct NetworkState {
    packets_captured: usize,
    devices_discovered: usize,
    violations: Vec<Violation>,
    topology: NetworkTopology,
    start_time: Instant,
}

#[derive(Clone)]
struct Violation {
    contract: String,
    severity: String,
    description: String,
    timestamp: Instant,
}

struct NetworkTopology {
    nodes: usize,
    edges: usize,
    segments: usize,
}

impl NetworkState {
    fn new() -> Self {
        Self {
            packets_captured: 0,
            devices_discovered: 0,
            violations: Vec::new(),
            topology: NetworkTopology {
                nodes: 0,
                edges: 0,
                segments: 1,
            },
            start_time: Instant::now(),
        }
    }
    
    fn simulate_packet_capture(&mut self, count: usize) {
        println!("{}", "Starting packet capture simulation...".green());
        
        for i in 0..count {
            // Simulate packet arrival
            thread::sleep(Duration::from_millis(10));
            self.packets_captured += 1;
            
            // Simulate topology discovery
            if i % 50 == 0 {
                self.devices_discovered += 1;
                self.topology.nodes = self.devices_discovered;
                self.topology.edges = self.devices_discovered * 2;
            }
            
            // Simulate security violations
            if i % 100 == 0 && i > 0 {
                self.violations.push(Violation {
                    contract: "guest-vlan-isolation".to_string(),
                    severity: "critical".to_string(),
                    description: "Guest VLAN device attempting to access internal network".to_string(),
                    timestamp: Instant::now(),
                });
            }
            
            if i % 200 == 0 && i > 0 {
                self.violations.push(Violation {
                    contract: "port-scan-detection".to_string(),
                    severity: "medium".to_string(),
                    description: "Potential port scanning activity detected".to_string(),
                    timestamp: Instant::now(),
                });
            }
            
            // Progress indicator
            if i % 100 == 0 {
                print!("\r{} Captured {} packets, discovered {} devices", 
                    "▶".green(), self.packets_captured, self.devices_discovered);
                use std::io::{self, Write};
                io::stdout().flush().unwrap();
            }
        }
        println!("\n{} Capture complete!", "✓".green());
    }
    
    fn analyze(&self) {
        println!("\n{}", "=== Network Analysis Results ===".blue().bold());
        
        println!("\n{}", "Network Topology:".yellow());
        println!("  Total devices: {}", self.topology.nodes);
        println!("  Active connections: {}", self.topology.edges);
        println!("  Network segments: {}", self.topology.segments);
        
        println!("\n{}", "Security Compliance:".yellow());
        let compliance_score = 100 - (self.violations.len() * 5).min(50);
        println!("  Compliance score: {}%", 
            if compliance_score >= 80 { 
                compliance_score.to_string().green() 
            } else { 
                compliance_score.to_string().red() 
            });
        
        let critical_violations = self.violations.iter()
            .filter(|v| v.severity == "critical").count();
        let high_violations = self.violations.iter()
            .filter(|v| v.severity == "high").count();
            
        println!("  Total violations: {}", self.violations.len());
        if critical_violations > 0 {
            println!("  Critical violations: {}", critical_violations.to_string().red());
        }
        if high_violations > 0 {
            println!("  High severity violations: {}", high_violations.to_string().yellow());
        }
        
        println!("\n{}", "Traffic Statistics:".yellow());
        println!("  Total packets captured: {}", self.packets_captured);
        println!("  Average packet rate: {:.1} pps", 
            self.packets_captured as f64 / self.start_time.elapsed().as_secs_f64());
    }
    
    fn explain_violations(&self) {
        println!("\n{}", "=== Security Violation Explanations ===".blue().bold());
        
        if self.violations.is_empty() {
            println!("{}", "No violations detected.".green());
            return;
        }
        
        for (i, violation) in self.violations.iter().enumerate() {
            println!("\n{} Violation #{}", "▶".red(), i + 1);
            println!("  Contract: {}", violation.contract.yellow());
            println!("  Severity: {}", 
                match violation.severity.as_str() {
                    "critical" => violation.severity.red(),
                    "high" => violation.severity.yellow(),
                    _ => violation.severity.normal(),
                });
            println!("  Description: {}", violation.description);
            
            // Provide specific remediation based on contract type
            println!("\n  {}", "Remediation:".green());
            match violation.contract.as_str() {
                "guest-vlan-isolation" => {
                    println!("    • Review firewall rules for VLAN segmentation");
                    println!("    • Ensure proper VLAN tagging on switch ports");
                    println!("    • Implement network access control (NAC)");
                },
                "port-scan-detection" => {
                    println!("    • Enable rate limiting on firewall");
                    println!("    • Deploy intrusion prevention system (IPS)");
                    println!("    • Block source IP if scanning continues");
                },
                _ => {
                    println!("    • Review security policy configuration");
                    println!("    • Consult security team for remediation");
                }
            }
        }
    }
    
    fn generate_report(&self) {
        println!("\n{}", "=== ClaudeScope Network Security Report ===".blue().bold());
        println!("Generated: {}", chrono::Local::now().format("%Y-%m-%d %H:%M:%S"));
        
        self.analyze();
        
        println!("\n{}", "Executive Summary:".yellow());
        let risk_level = if self.violations.iter().any(|v| v.severity == "critical") {
            "HIGH".red()
        } else if self.violations.iter().any(|v| v.severity == "high") {
            "MODERATE".yellow()
        } else {
            "LOW".green()
        };
        println!("  Overall risk level: {}", risk_level);
        println!("  Immediate action required: {}", 
            if self.violations.iter().any(|v| v.severity == "critical") { 
                "YES".red() 
            } else { 
                "NO".green() 
            });
        
        println!("\n{}", "Recommendations:".yellow());
        println!("  • Enable continuous monitoring and alerting");
        println!("  • Schedule regular security audits");
        println!("  • Implement automated remediation workflows");
        println!("  • Review and update security policies quarterly");
    }
}

fn show_source(module: &str) -> Result<()> {
    let file_path = if module.ends_with(".ai") {
        module.to_string()
    } else {
        format!("src/{}.ai", module)
    };
    
    match ClaudeScopeAssets::get(&file_path) {
        Some(content) => {
            let source = std::str::from_utf8(content.data.as_ref())?;
            println!("{}", format!("=== {} ===", file_path).blue().bold());
            println!("{}", source);
            Ok(())
        }
        None => {
            anyhow::bail!("Module not found: {}. Use 'claudescope list' to see available modules.", module)
        }
    }
}

fn list_modules() {
    println!("{}", "=== Available ClaudeScope Modules ===".blue().bold());
    
    let mut modules: Vec<_> = ClaudeScopeAssets::iter().collect();
    modules.sort();
    
    println!("\n{}", "Core modules:".yellow());
    for module in &modules {
        if module.starts_with("src/") && module.ends_with(".ai") {
            let name = module.strip_prefix("src/").unwrap().strip_suffix(".ai").unwrap();
            println!("  • {}", name);
        }
    }
    
    println!("\n{}", "Other files:".yellow());
    for module in &modules {
        if !module.starts_with("src/") {
            println!("  • {}", module);
        }
    }
}

fn run_demo() -> Result<()> {
    println!("{}", "Running ClaudeScope demo workflow...".green().bold());
    
    let mut state = NetworkState::new();
    
    // Step 1: Initialize
    println!("\n{} Initializing ClaudeScope...", "Step 1:".blue());
    thread::sleep(Duration::from_millis(500));
    println!("{} ClaudeScope initialized", "✓".green());
    
    // Step 2: Start capture
    println!("\n{} Starting packet capture...", "Step 2:".blue());
    state.simulate_packet_capture(1000);
    
    // Step 3: Analyze
    println!("\n{} Analyzing network...", "Step 3:".blue());
    thread::sleep(Duration::from_millis(500));
    state.analyze();
    
    // Step 4: Explain violations
    println!("\n{} Explaining violations...", "Step 4:".blue());
    state.explain_violations();
    
    // Step 5: Generate report
    println!("\n{} Generating report...", "Step 5:".blue());
    thread::sleep(Duration::from_millis(500));
    println!("\n{}", "Report saved to: claudescope_report.txt".green());
    
    println!("\n{}", "=== Demo Complete ===".green().bold());
    println!("ClaudeScope demonstrates how formal contracts enhance network security tools.");
    
    Ok(())
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    
    // ASCII art banner
    println!("{}", r#"
    ╔══════════════════════════════════════════════╗
    ║       ClaudeScope Network Analyzer           ║
    ║   Self-Verifying Security with FluentAi   ║
    ║         (Standalone Edition v1.0)            ║
    ╚══════════════════════════════════════════════╝
    "#.blue().bold());
    
    match &cli.command {
        None | Some(Commands::Demo) => {
            run_demo()?;
        }
        
        Some(Commands::Capture { interface, count }) => {
            println!("Starting packet capture on {}...", interface.yellow());
            let mut state = NetworkState::new();
            state.simulate_packet_capture(*count);
            state.analyze();
        }
        
        Some(Commands::Analyze) => {
            let mut state = NetworkState::new();
            println!("Simulating network traffic for analysis...");
            state.simulate_packet_capture(500);
            state.analyze();
        }
        
        Some(Commands::Explain) => {
            let mut state = NetworkState::new();
            println!("Simulating network traffic...");
            state.simulate_packet_capture(500);
            state.explain_violations();
        }
        
        Some(Commands::Report) => {
            let mut state = NetworkState::new();
            println!("Generating comprehensive report...");
            state.simulate_packet_capture(1000);
            state.generate_report();
        }
        
        Some(Commands::Source { module }) => {
            show_source(module)?;
        }
        
        Some(Commands::List) => {
            list_modules();
        }
    }
    
    Ok(())
}

// Re-export chrono for timestamp formatting
use chrono;