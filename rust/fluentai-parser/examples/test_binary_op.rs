use fluentai_core::ast::Node;
use fluentai_parser::flc_parser::Parser;

fn main() {
    println!("=== Testing Binary Operation Parsing ===");

    // Test parsing "1 + 2"
    test_parse("1 + 2", "simple addition");

    // Test other operations
    test_parse("10 - 5", "subtraction");
    test_parse("3 * 4", "multiplication");
    test_parse("1 + 2 * 3", "precedence");
}

fn test_parse(input: &str, description: &str) {
    println!("\nTesting: {} - Input: {}", description, input);

    let parser = Parser::new(input);
    match parser.parse() {
        Ok(graph) => {
            println!("✓ Parse successful!");

            if let Some(root_id) = graph.root_id {
                println!("Root node ID: {:?}", root_id);

                // Analyze the root node
                if let Some(root_node) = graph.get_node(root_id) {
                    analyze_node(&graph, root_id, root_node, 0);
                }
            } else {
                println!("No root node found!");
            }
        }
        Err(e) => {
            println!("✗ Parse failed: {}", e);
        }
    }
}

fn analyze_node(
    graph: &fluentai_core::ast::Graph,
    _node_id: fluentai_core::ast::NodeId,
    node: &Node,
    indent: usize,
) {
    let prefix = "  ".repeat(indent);

    match node {
        Node::Application { function, args } => {
            println!("{}Application {{", prefix);
            println!("{}  function: {:?}", prefix, function);
            if let Some(func_node) = graph.get_node(*function) {
                println!("{}  function node:", prefix);
                analyze_node(graph, *function, func_node, indent + 2);
            }
            println!("{}  args: {:?}", prefix, args);
            for (i, arg_id) in args.iter().enumerate() {
                if let Some(arg_node) = graph.get_node(*arg_id) {
                    println!("{}  arg[{}]:", prefix, i);
                    analyze_node(graph, *arg_id, arg_node, indent + 2);
                }
            }
            println!("{}}}", prefix);
        }
        Node::Variable { name } => {
            println!("{}Variable {{ name: \"{}\" }}", prefix, name);
        }
        Node::Literal(lit) => {
            println!("{}Literal({:?})", prefix, lit);
        }
        _ => {
            println!("{}{:?}", prefix, node);
        }
    }
}
