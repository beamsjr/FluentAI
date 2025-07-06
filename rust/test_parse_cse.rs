use fluentai_parser::parse;

fn main() {
    let code = "(let ((a (+ x 1))) (+ (+ x 1) a))";
    let graph = parse(code).unwrap();
    
    println\!("Graph nodes:");
    for (id, node) in &graph.nodes {
        println\!("  {:?}: {:?}", id, node);
    }
    println\!("\nRoot: {:?}", graph.root_id);
}
EOF < /dev/null