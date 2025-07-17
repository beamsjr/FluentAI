//! Simple test to show all optimization strategies

fn main() {
    // Import the strategy formatting function inline
    fn format_strategy(mask: u32) -> String {
        let mut opts = Vec::new();
        if mask & 0x001 != 0 { opts.push("ConstFold"); }
        if mask & 0x002 != 0 { opts.push("DeadCode"); }
        if mask & 0x004 != 0 { opts.push("CSE"); }
        if mask & 0x008 != 0 { opts.push("Inline"); }
        if mask & 0x010 != 0 { opts.push("TailCall"); }
        if mask & 0x020 != 0 { opts.push("LoopOpt"); }
        if mask & 0x040 != 0 { opts.push("BetaRed"); }
        if mask & 0x080 != 0 { opts.push("PartialEval"); }
        if mask & 0x100 != 0 { opts.push("StrengthRed"); }
        if mask & 0x200 != 0 { opts.push("AlgebraicSimp"); }
        if mask & 0x400 != 0 { opts.push("LoopInvariant"); }
        if mask & 0x800 != 0 { opts.push("FuncSpec"); }
        if mask & 0x1000 != 0 { opts.push("Memoization"); }
        
        if opts.is_empty() {
            format!("Custom(0x{:03x})", mask)
        } else {
            format!("Custom[{}]", opts.join("+"))
        }
    }
    
    println!("=== Learning Mode Optimization Strategies ===\n");
    
    println!("Basic strategies:");
    println!("  - None");
    println!("  - Basic");
    println!("  - Standard");
    println!("  - Aggressive");
    
    println!("\nCustom single-optimization strategies:");
    println!("  - {} (Constant folding only)", format_strategy(0x001));
    println!("  - {} (Dead code elimination only)", format_strategy(0x002));
    println!("  - {} (Common subexpression elimination)", format_strategy(0x004));
    println!("  - {} (Function inlining)", format_strategy(0x008));
    println!("  - {} (Loop optimization)", format_strategy(0x020));
    println!("  - {} (Effect reordering/Partial evaluation)", format_strategy(0x080));
    println!("  - {} (Strength reduction)", format_strategy(0x100));
    println!("  - {} (Loop invariant code motion)", format_strategy(0x400));
    println!("  - {} (Function specialization)", format_strategy(0x800));
    println!("  - {} (Memoization - cache pure function results)", format_strategy(0x1000));
    
    println!("\nCombination strategies:");
    println!("  - {} (Common pair)", format_strategy(0x003));
    println!("  - {} (Loop optimizations combo)", format_strategy(0x420));
    println!("  - {} (Effect optimization combo)", format_strategy(0x084));
    println!("  - {} (Memoization + specialization)", format_strategy(0x1800));
    
    println!("\nTotal strategies available: 18");
    println!("\nWith these strategies, the learning mode will:");
    println!("1. Try each optimization in isolation to measure its impact");
    println!("2. Try common combinations that work well together");
    println!("3. Use reinforcement learning to select the best strategy for each function");
    println!("4. Adapt optimization choices based on runtime performance metrics");
}