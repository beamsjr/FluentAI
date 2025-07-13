//! Test interpreter with a file

use fluentai_interpreter::{Interpreter, InterpreterOptions};
use fluentai_parser::parse_flc;
use std::fs;

fn main() -> anyhow::Result<()> {
    let content = fs::read_to_string("test_repl.ai")?;
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    // Split by lines and execute each non-empty, non-comment line
    for line in content.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with(";;") {
            continue;
        }

        println!("\n> {}", line);
        match parse_flc(line) {
            Ok(graph) => match interpreter.interpret(&graph) {
                Ok(result) => println!("=> {}", result),
                Err(e) => println!("Error: {}", e),
            },
            Err(e) => println!("Parse error: {}", e),
        }
    }

    Ok(())
}
