//! REPL demonstration

use fluentai_repl::{Repl, ReplConfig};

fn main() -> anyhow::Result<()> {
    println!("Starting FluentAi REPL demo...");

    // Create custom config
    let mut config = ReplConfig::default();
    config.show_banner = true;
    config.prompt = "cl> ".to_string();

    // Create and run REPL
    let mut repl = Repl::new(config)?;
    repl.run()?;

    Ok(())
}
