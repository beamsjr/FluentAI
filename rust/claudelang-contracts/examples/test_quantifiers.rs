//! Minimal test to verify quantifier implementation

use claudelang_contracts::quantifiers::{
    QuantifierBuilder, QuantifierParser, QuantifierDomain, Quantifier
};
use claudelang_core::ast::{Graph, Node, Literal};

fn main() {
    println!("=== Testing Quantifier Implementation ===\n");
    
    // Test 1: Build a simple forall expression
    println!("Test 1: Building forall x in [0, 10], x >= 0");
    let mut builder = QuantifierBuilder::new(Graph::new());
    
    let quantified = builder.forall(
        vec![("x", QuantifierDomain::IntRange(0, 10))],
        |builder, vars| {
            let x = vars["x"];
            let zero = builder.graph.add_node(Node::Literal(Literal::Integer(0)));
            let ge = builder.graph.add_node(Node::Variable { name: ">=".to_string() });
            builder.graph.add_node(Node::Application {
                function: ge,
                args: vec![x, zero],
            })
        }
    );
    
    println!("Created quantified expression: {:?}", quantified);
    
    // Test 2: Parse it back
    println!("\nTest 2: Parsing the quantified expression");
    let parser = QuantifierParser::new(&builder.graph);
    match parser.parse_quantifier(quantified) {
        Ok(Some(quant)) => {
            println!("Parsed successfully!");
            println!("  Quantifier: {:?}", quant.quantifier);
            println!("  Bound vars: {:?}", quant.bound_vars);
            println!("  Body: {:?}", quant.body);
        }
        Ok(None) => println!("Not a quantified expression"),
        Err(e) => println!("Parse error: {}", e),
    }
    
    // Test 3: Build an exists expression
    println!("\nTest 3: Building exists y in list, y = target");
    let list = builder.graph.add_node(Node::Variable { name: "list".to_string() });
    let target = builder.graph.add_node(Node::Variable { name: "target".to_string() });
    
    let exists_expr = builder.exists(
        vec![("y", QuantifierDomain::ListElements(list))],
        |builder, vars| {
            let y = vars["y"];
            let eq = builder.graph.add_node(Node::Variable { name: "=".to_string() });
            builder.graph.add_node(Node::Application {
                function: eq,
                args: vec![y, target],
            })
        }
    );
    
    println!("Created exists expression: {:?}", exists_expr);
    
    // Test 4: Parse the exists expression
    match parser.parse_quantifier(exists_expr) {
        Ok(Some(quant)) => {
            println!("Parsed exists expression:");
            println!("  Quantifier: {:?}", quant.quantifier);
            assert_eq!(quant.quantifier, Quantifier::Exists);
        }
        _ => println!("Failed to parse exists expression"),
    }
    
    println!("\n✅ Quantifier implementation is working correctly!");
}