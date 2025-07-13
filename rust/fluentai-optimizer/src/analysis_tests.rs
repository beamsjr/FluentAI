#[cfg(test)]
mod tests {
    use crate::analysis::{
        AliasAnalysis, ControlFlowGraph,
        DataFlowAnalysis, EffectAnalysis, TypeAnalysis,
    };
    use fluentai_core::ast::{EffectType, Graph, Literal, Node, NodeId};
    use fluentai_parser::parse_flc;
    use std::num::NonZeroU32;

    // ===== Helper Functions =====

    fn create_node_id(n: u32) -> NodeId {
        NodeId(NonZeroU32::new(n).unwrap())
    }

    // ===== ControlFlowGraph Tests =====

    #[test]
    fn test_cfg_empty_graph() {
        let graph = Graph::new();
        let cfg = ControlFlowGraph::build(&graph);

        assert!(cfg.entries.is_empty());
        assert!(cfg.exits.is_empty());
        assert!(cfg.predecessors.is_empty());
        assert!(cfg.successors.is_empty());
        assert!(cfg.loop_headers.is_empty());
    }

    #[test]
    fn test_cfg_single_node() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");
        graph.root_id = Some(node_id);

        let cfg = ControlFlowGraph::build(&graph);

        assert!(cfg.entries.contains(&node_id));
        assert!(cfg.exits.contains(&node_id));
        assert!(cfg
            .predecessors
            .get(&node_id)
            .map_or(true, |p| p.is_empty()));
        assert!(cfg.successors.get(&node_id).map_or(true, |s| s.is_empty()));
    }

    #[test]
    fn test_cfg_if_expression() {
        let code = "if (x > 0) { x + 1 } else { x - 1 }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        // Should have one entry (the if node)
        assert_eq!(cfg.entries.len(), 1);
        // Multiple exits (the then and else branches)
        assert!(cfg.exits.len() >= 2);
    }

    #[test]
    fn test_cfg_let_expression() {
        let code = "{ let x = 1; let y = 2; x + y }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        assert_eq!(cfg.entries.len(), 1);
        assert!(cfg.exits.len() >= 1);
    }

    #[test]
    fn test_cfg_lambda() {
        let code = "(x) => x + 1";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        assert_eq!(cfg.entries.len(), 1);
        // Lambda body should be analyzed
        assert!(!cfg.predecessors.is_empty() || !cfg.successors.is_empty());
    }

    #[test]
    fn test_cfg_application() {
        let code = "f(a, b, c)";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        assert_eq!(cfg.entries.len(), 1);
        // Function and arguments should create control flow
        assert!(!cfg.successors.is_empty());
    }

    #[test]
    fn test_cfg_match_expression() {
        let code = "match(x) { 0 => \"zero\", 1 => \"one\", _ => \"other\" }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        assert_eq!(cfg.entries.len(), 1);
        // Multiple branches should create multiple exits
        assert!(cfg.exits.len() >= 1);
    }

    #[test]
    fn test_cfg_nested_control_flow() {
        let code = "if (if (a) { b } else { c }) { if (d) { e } else { f } } else { g }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        assert_eq!(cfg.entries.len(), 1);
        // Nested control flow should create complex graph
        assert!(cfg.predecessors.len() > 1);
        assert!(cfg.successors.len() > 1);
    }

    #[test]
    fn test_cfg_list_nodes() {
        let code = "[1 2 3 4 5]";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);

        // List elements should be analyzed
        assert_eq!(cfg.entries.len(), 1);
    }

    // ===== DataFlowAnalysis Tests =====

    #[test]
    fn test_dataflow_empty_graph() {
        let graph = Graph::new();
        let cfg = ControlFlowGraph::build(&graph);
        let dataflow = DataFlowAnalysis::analyze(&graph, &cfg);

        assert!(dataflow.definitions.is_empty());
        assert!(dataflow.uses.is_empty());
        assert!(dataflow.live_in.is_empty());
        assert!(dataflow.live_out.is_empty());
    }

    #[test]
    fn test_dataflow_variable_use() {
        let code = "x + y";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);
        let dataflow = DataFlowAnalysis::analyze(&graph, &cfg);

        // Should track uses of x and y
        assert!(!dataflow.uses.is_empty());
    }

    #[test]
    fn test_dataflow_let_binding() {
        let code = "{ let x = 1; x + 2 }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);
        let dataflow = DataFlowAnalysis::analyze(&graph, &cfg);

        // Should track definition and use of x
        assert!(!dataflow.definitions.is_empty());
        assert!(!dataflow.uses.is_empty());
    }

    #[test]
    fn test_dataflow_lambda_params() {
        let code = "(x, y) => x + y";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);
        let dataflow = DataFlowAnalysis::analyze(&graph, &cfg);

        // Lambda parameters are definitions
        assert!(!dataflow.definitions.is_empty());
    }

    #[test]
    fn test_dataflow_shadowing() {
        let code = "{ let x = 1; { let x = 2; x } }";
        let graph = parse_flc(code).unwrap();
        let cfg = ControlFlowGraph::build(&graph);
        let dataflow = DataFlowAnalysis::analyze(&graph, &cfg);

        // Should have multiple definitions of x
        let x_defs = dataflow
            .definitions
            .values()
            .flat_map(|vars| vars.iter())
            .filter(|var| var == &"x")
            .count();
        assert_eq!(x_defs, 2);
    }

    // ===== EffectAnalysis Tests =====

    #[test]
    fn test_effect_analysis_pure() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();
        let effects = EffectAnalysis::analyze(&graph);

        // Pure arithmetic should have no effects
        let root_id = graph.root_id.unwrap();
        assert!(effects.pure_nodes.contains(&root_id));
    }

    #[test]
    fn test_effect_analysis_io() {
        let code = "perform IO.print(\"hello\")";
        let graph = parse_flc(code).unwrap();
        let effects = EffectAnalysis::analyze(&graph);

        // Should detect IO effect
        let root_id = graph.root_id.unwrap();
        assert!(!effects.pure_nodes.contains(&root_id));
        let root_effects = &effects.node_effects[&root_id];
        assert!(root_effects.contains(&EffectType::IO));
    }

    #[test]
    #[ignore = "Effect propagation through let bindings not fully implemented"]
    fn test_effect_analysis_propagation() {
        let code = "{ let x = perform IO.read(); x + 1 }";
        let graph = parse_flc(code).unwrap();
        let effects = EffectAnalysis::analyze(&graph);

        // Effects should propagate through let binding
        let root_id = graph.root_id.unwrap();
        assert!(!effects.pure_nodes.contains(&root_id));
    }

    #[test]
    #[ignore = "State effect type not yet supported in parser"]
    fn test_effect_analysis_multiple() {
        let code =
            "{ perform IO.print(\"start\"); perform State.set(\"x\", 1); perform IO.print(\"end\") }";
        let graph = parse_flc(code).unwrap();
        let effects = EffectAnalysis::analyze(&graph);

        // Should have both IO and State effects
        let mut all_effects: std::collections::HashSet<EffectType> =
            std::collections::HashSet::new();
        for effects_set in effects.node_effects.values() {
            all_effects.extend(effects_set);
        }
        assert!(all_effects.contains(&EffectType::IO));
        assert!(all_effects.contains(&EffectType::State));
    }

    #[test]
    fn test_effect_analysis_lambda() {
        let code = "(x) => perform IO.print(x)";
        let graph = parse_flc(code).unwrap();
        let effects = EffectAnalysis::analyze(&graph);

        // Lambda itself might be pure, but body has effects
        let root_id = graph.root_id.unwrap();
        if let Some(Node::Lambda { body, .. }) = graph.get_node(root_id) {
            assert!(!effects.pure_nodes.contains(body));
        }
    }

    // ===== AliasAnalysis Tests =====

    #[test]
    fn test_alias_analysis_empty() {
        let graph = Graph::new();
        let alias = AliasAnalysis::analyze(&graph);

        assert!(alias.alias_sets.is_empty());
    }

    #[test]
    fn test_alias_analysis_simple_binding() {
        let code = "{ let x = y; x }";
        let graph = parse_flc(code).unwrap();
        let alias = AliasAnalysis::analyze(&graph);

        // x and y might be aliases
        assert!(!alias.alias_sets.is_empty());
    }

    #[test]
    fn test_alias_analysis_multiple() {
        let code = "{ let a = b; let c = a; a + c }";
        let graph = parse_flc(code).unwrap();
        let alias = AliasAnalysis::analyze(&graph);

        // Should detect transitive aliases
        assert!(!alias.alias_sets.is_empty());
    }

    // ===== TypeAnalysis Tests =====

    #[test]
    fn test_type_analysis_empty() {
        let graph = Graph::new();
        let type_info = TypeAnalysis::analyze(&graph);

        assert!(type_info.node_types.is_empty());
    }

    #[test]
    fn test_type_analysis_literals() {
        let code = "list(42, 3.14, \"hello\", true)";
        let graph = parse_flc(code).unwrap();
        let type_info = TypeAnalysis::analyze(&graph);

        // Should infer types for literals
        assert!(!type_info.node_types.is_empty());
    }

    #[test]
    fn test_type_analysis_function() {
        let code = "(x) => x + 1";
        let graph = parse_flc(code).unwrap();
        let type_info = TypeAnalysis::analyze(&graph);

        // Should infer function type
        assert!(!type_info.node_types.is_empty());
    }

    #[test]
    fn test_type_analysis_propagation() {
        let code = "{ let x = 42; x + 1 }";
        let graph = parse_flc(code).unwrap();
        let type_info = TypeAnalysis::analyze(&graph);

        // Type info should propagate through bindings
        assert!(!type_info.node_types.is_empty());
    }
}
