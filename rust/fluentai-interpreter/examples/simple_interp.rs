//! Simple interpreter example

use fluentai_interpreter::{Interpreter, InterpreterOptions};
use fluentai_parser::parse_flc;

fn main() -> anyhow::Result<()> {
    // Create interpreter
    let mut interpreter = Interpreter::new(InterpreterOptions::default());

    // Test cases
    let tests = vec![
        ("42", "Integer literal"),
        ("(+ 1 2)", "Simple addition"),
        ("(* 3 4)", "Multiplication"),
        ("(- 10 3)", "Subtraction"),
        ("(if (> 5 3) \"yes\" \"no\")", "If expression"),
        ("(let ((x 10)) (+ x 5))", "Let binding"),
        ("((lambda (x) (* x 2)) 5)", "Lambda application"),
        ("[1 2 3]", "List literal"),
    ];

    for (code, desc) in tests {
        println!("\n{}: {}", desc, code);

        match parse_flc(code) {
            Ok(graph) => match interpreter.interpret(&graph) {
                Ok(result) => println!("=> {}", result),
                Err(e) => println!("Error: {}", e),
            },
            Err(e) => println!("Parse error: {}", e),
        }
    }

    Ok(())
}
