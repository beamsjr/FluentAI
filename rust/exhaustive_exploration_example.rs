//! Example showing exhaustive exploration mode
//! 
//! This demonstrates how the VM can test all 8192 possible optimization combinations
//! when time is not a constraint and you want to find the absolute best strategy.

use fluentai_vm::{VM, VMConfig};
use fluentai_vm::learning_mode::{LearningModeConfig, ExplorationMode};

fn main() {
    println!("=== Exhaustive Exploration Mode Example ===\n");
    
    println!("Scenario: Critical financial calculation function");
    println!("Goal: Find the absolute best optimization combination");
    println!("Time constraint: None - we can afford to test everything\n");
    
    // Configure VM with exhaustive exploration
    let mut config = VMConfig::default();
    let mut learning_config = LearningModeConfig::default();
    learning_config.exploration_mode = ExplorationMode::Exhaustive;
    learning_config.hot_threshold = 10; // Lower threshold for demo
    
    println!("Configuration:");
    println!("  Exploration mode: Exhaustive");
    println!("  Hot threshold: 10 executions");
    println!("  Max strategies: Unlimited (will test all 8192+ combinations)");
    
    // Example exploration phases
    println!("\n=== Exploration Phases ===\n");
    
    println!("Phase 1: Testing baseline (strategy 0/8195)");
    println!("  OptimizationStrategy::None");
    println!("  Result: 1000ms baseline\n");
    
    println!("Phase 2: Testing all individual optimizations (strategies 1-13)");
    println!("  Custom(0x001) - ConstFold only: 980ms");
    println!("  Custom(0x002) - DeadCode only: 990ms");
    println!("  Custom(0x004) - CSE only: 950ms");
    println!("  Custom(0x008) - Inline only: 850ms");
    println!("  ... (testing all 13 individual optimizations)\n");
    
    println!("Phase 3: Testing all 2-optimization combinations (strategies 14-91)");
    println!("  Custom(0x003) - ConstFold + DeadCode: 970ms");
    println!("  Custom(0x005) - ConstFold + CSE: 930ms");
    println!("  Custom(0x009) - ConstFold + Inline: 830ms");
    println!("  ... (testing all 78 combinations)\n");
    
    println!("Phase 4: Testing all 3-optimization combinations (strategies 92-377)");
    println!("  Custom(0x007) - ConstFold + DeadCode + CSE: 920ms");
    println!("  Custom(0x00B) - ConstFold + DeadCode + Inline: 810ms");
    println!("  ... (testing all 286 combinations)\n");
    
    println!("... continuing through all possible combinations ...\n");
    
    println!("Phase N: Testing all optimizations enabled (strategy 8191)");
    println!("  Custom(0x1FFF) - All 13 optimizations: 750ms (slower due to overhead)\n");
    
    println!("Phase N+1: Testing predefined strategies (strategies 8192-8195)");
    println!("  Basic: 970ms");
    println!("  Standard: 850ms");
    println!("  Aggressive: 780ms\n");
    
    println!("=== Results After Exhaustive Testing ===\n");
    
    println!("Total strategies tested: 8195");
    println!("Total execution time: ~6.8 hours (3 seconds per test)\n");
    
    println!("Top 5 best performing combinations:");
    println!("1. Custom(0x018C) - Inline + CSE + StrengthRed + AlgebraicSimp: 720ms");
    println!("2. Custom(0x01CC) - Inline + CSE + BetaRed + StrengthRed + AlgebraicSimp: 725ms");
    println!("3. Custom(0x008C) - Inline + CSE + StrengthRed: 730ms");
    println!("4. Custom(0x018E) - Inline + CSE + DeadCode + StrengthRed + AlgebraicSimp: 735ms");
    println!("5. Custom(0x00CC) - Inline + CSE + BetaRed + StrengthRed: 740ms\n");
    
    println!("🎯 Optimal strategy discovered: Custom(0x018C)");
    println!("   28% performance improvement (vs 22% with Aggressive)");
    println!("   Optimizations: Inline + CSE + StrengthRed + AlgebraicSimp");
    println!("   Key insight: This specific combination avoids overhead from");
    println!("                loop optimizations that don't benefit this function\n");
    
    println!("=== Comparison of Exploration Modes ===\n");
    
    println!("Quick Mode (4 tests, ~12 seconds):");
    println!("  Best found: Aggressive (780ms)");
    println!("  Improvement: 22%\n");
    
    println!("Smart Mode (18 tests, ~54 seconds):");
    println!("  Best found: Custom(0x008C) (730ms)");
    println!("  Improvement: 27%\n");
    
    println!("Exhaustive Mode (8195 tests, ~6.8 hours):");
    println!("  Best found: Custom(0x018C) (720ms)");
    println!("  Improvement: 28%\n");
    
    println!("=== When to Use Each Mode ===\n");
    
    println!("Quick Mode:");
    println!("  ✓ Development and testing");
    println!("  ✓ Non-critical code paths");
    println!("  ✓ When compilation time matters\n");
    
    println!("Smart Mode:");
    println!("  ✓ Production code");
    println!("  ✓ Hot functions");
    println!("  ✓ Good balance of exploration vs time\n");
    
    println!("Exhaustive Mode:");
    println!("  ✓ Mission-critical functions");
    println!("  ✓ Performance benchmarking");
    println!("  ✓ One-time optimization discovery");
    println!("  ✓ Research and analysis");
    println!("  ✗ Not for regular compilation\n");
    
    println!("=== Implementation Notes ===\n");
    
    println!("1. Results can be cached and reused:");
    println!("   - Once exhaustive search finds optimal strategy");
    println!("   - Save mapping: function_pattern -> optimal_strategy");
    println!("   - Reuse without re-exploration\n");
    
    println!("2. Can be run offline:");
    println!("   - Run exhaustive search on representative workloads");
    println!("   - Build optimization database");
    println!("   - Ship with pre-discovered optimal strategies\n");
    
    println!("3. Parallelization potential:");
    println!("   - Test multiple strategies concurrently");
    println!("   - Reduce 6.8 hours to ~1 hour with 8 cores");
}

// Example of how to enable exhaustive mode programmatically
fn enable_exhaustive_mode(vm: &mut VM) {
    let mut config = LearningModeConfig::default();
    config.exploration_mode = ExplorationMode::Exhaustive;
    config.hot_threshold = 10; // Lower threshold for critical functions
    
    // Enable learning mode with exhaustive exploration
    vm.enable_learning_mode(config);
}

// Example of caching discovered optimal strategies
struct OptimizationCache {
    // Map from function characteristics to optimal strategy
    optimal_strategies: std::collections::HashMap<FunctionFingerprint, OptimizationStrategy>,
}

#[derive(Hash, Eq, PartialEq)]
struct FunctionFingerprint {
    has_loops: bool,
    has_recursion: bool,
    has_effects: bool,
    node_count: usize,
    depth: usize,
}

use fluentai_vm::learning_mode::OptimizationStrategy;