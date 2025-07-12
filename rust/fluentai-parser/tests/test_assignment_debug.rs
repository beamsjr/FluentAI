#[cfg(test)]
mod assignment_debug_tests {
    use fluentai_core::ast::Node;
    use fluentai_parser::flc_parser::Parser;

    #[test]
    fn test_simple_assignment_parsing() {
        let code = "x := 42";
        let parser = Parser::new(code);
        let graph = parser.parse().unwrap();

        println!("\n=== Parsing '{}' ===", code);
        println!("Root ID: {:?}", graph.root_id);
        println!("Total nodes: {}", graph.nodes.len());

        // List all nodes
        let mut node_ids: Vec<_> = graph.nodes.keys().cloned().collect();
        node_ids.sort_by_key(|id| id.0);

        for id in &node_ids {
            let node = &graph.nodes[id];
            println!("  {:?}: {:?}", id, node);

            // Check assignment nodes
            if let Node::Assignment { target, value } = node {
                let target_exists = graph.nodes.contains_key(target);
                let value_exists = graph.nodes.contains_key(value);
                println!("    Assignment check:");
                println!("      target {:?} exists: {}", target, target_exists);
                println!("      value {:?} exists: {}", value, value_exists);

                if !target_exists || !value_exists {
                    panic!("Assignment references non-existent nodes!");
                }
            }
        }

        // Verify we have the expected structure
        assert_eq!(
            graph.nodes.len(),
            3,
            "Expected 3 nodes: Variable, Literal, Assignment"
        );
    }

    #[test]
    fn test_assignment_in_let() {
        let code = "let x = 10; x := 42";
        let parser = Parser::new(code);
        let graph = parser.parse().unwrap();

        println!("\n=== Parsing '{}' ===", code);
        println!("Root ID: {:?}", graph.root_id);
        println!("Total nodes: {}", graph.nodes.len());

        // List all nodes
        for (id, node) in &graph.nodes {
            println!("  {:?}: {:?}", id, node);
        }
    }
}
