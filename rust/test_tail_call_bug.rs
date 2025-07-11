use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_vm::{Compiler, CompilerOptions, VM};

fn main() -> Result<()> {
    // Test case: Define a recursive function using 'define'
    // (define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))
    let mut graph = Graph::new();

    // Build the recursive function
    let n_var = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)))?;
    let one = graph.add_node(Node::Literal(Literal::Integer(1)))?;

    // Build (= n 0)
    let eq = graph.add_node(Node::Variable { name: "=".to_string() })?;
    let condition = graph.add_node(Node::Application {
        function: eq,
        args: vec![n_var, zero],
    })?;

    // Build (- n 1)
    let minus = graph.add_node(Node::Variable { name: "-".to_string() })?;
    let n_var2 = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let n_minus_1 = graph.add_node(Node::Application {
        function: minus,
        args: vec![n_var2, one],
    })?;

    // Build recursive call (factorial (- n 1))
    let factorial_var = graph.add_node(Node::Variable { name: "factorial".to_string() })?;
    let recursive_call = graph.add_node(Node::Application {
        function: factorial_var,
        args: vec![n_minus_1],
    })?;

    // Build (* n (factorial (- n 1)))
    let mult = graph.add_node(Node::Variable { name: "*".to_string() })?;
    let n_var3 = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let multiply_expr = graph.add_node(Node::Application {
        function: mult,
        args: vec![n_var3, recursive_call],
    })?;

    // Build if expression
    let if_expr = graph.add_node(Node::If {
        condition,
        then_branch: one,
        else_branch: multiply_expr,
    })?;

    // Build lambda
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["n".to_string()],
        body: if_expr,
    })?;

    // Build define
    let define = graph.add_node(Node::Define {
        name: "factorial".to_string(),
        value: lambda,
    })?;

    // Build a tail-recursive version for comparison
    // (define factorial-tail (lambda (n acc) (if (= n 0) acc (factorial-tail (- n 1) (* n acc)))))
    let acc_var = graph.add_node(Node::Variable { name: "acc".to_string() })?;
    
    // Build condition (= n 0)
    let n_var4 = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let eq2 = graph.add_node(Node::Variable { name: "=".to_string() })?;
    let condition2 = graph.add_node(Node::Application {
        function: eq2,
        args: vec![n_var4, zero],
    })?;

    // Build (- n 1)
    let minus2 = graph.add_node(Node::Variable { name: "-".to_string() })?;
    let n_var5 = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let n_minus_1_tail = graph.add_node(Node::Application {
        function: minus2,
        args: vec![n_var5, one],
    })?;

    // Build (* n acc)
    let mult2 = graph.add_node(Node::Variable { name: "*".to_string() })?;
    let n_var6 = graph.add_node(Node::Variable { name: "n".to_string() })?;
    let acc_var2 = graph.add_node(Node::Variable { name: "acc".to_string() })?;
    let mult_expr = graph.add_node(Node::Application {
        function: mult2,
        args: vec![n_var6, acc_var2],
    })?;

    // Build tail recursive call (factorial-tail (- n 1) (* n acc))
    let factorial_tail_var = graph.add_node(Node::Variable { name: "factorial-tail".to_string() })?;
    let tail_recursive_call = graph.add_node(Node::Application {
        function: factorial_tail_var,
        args: vec![n_minus_1_tail, mult_expr],
    })?;

    // Build if expression
    let if_tail = graph.add_node(Node::If {
        condition: condition2,
        then_branch: acc_var,
        else_branch: tail_recursive_call,
    })?;

    // Build lambda
    let lambda_tail = graph.add_node(Node::Lambda {
        params: vec!["n".to_string(), "acc".to_string()],
        body: if_tail,
    })?;

    // Build define
    let define_tail = graph.add_node(Node::Define {
        name: "factorial-tail".to_string(),
        value: lambda_tail,
    })?;

    // Test both functions
    let test_program = graph.add_node(Node::Begin {
        exprs: vec![define, define_tail],
    })?;
    
    graph.root_id = Some(test_program);

    // Compile and examine bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    println!("Bytecode for factorial functions:");
    println!("{:#?}", bytecode);

    // Look for TailCall instructions
    let mut has_tail_call = false;
    for chunk in &bytecode.chunks {
        for inst in &chunk.instructions {
            if inst.opcode == fluentai_vm::bytecode::Opcode::TailCall {
                has_tail_call = true;
                println!("Found TailCall instruction!");
            }
        }
    }

    if !has_tail_call {
        println!("WARNING: No TailCall instructions found in bytecode!");
        println!("This suggests tail call detection is not working for functions defined with 'define'");
    }

    Ok(())
}