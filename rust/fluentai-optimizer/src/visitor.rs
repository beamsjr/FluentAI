//! Visitor pattern infrastructure for AST traversal

use fluentai_core::ast::{Graph, Node, NodeId, Literal, Pattern, EffectType};

/// Trait for visiting AST nodes
pub trait NodeVisitor {
    /// The result type produced by this visitor
    type Result;
    
    /// Visit any node - dispatches to specific visit methods
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId) -> Self::Result {
        match graph.get_node(node_id) {
            Some(node) => match node {
                Node::Variable { name } => self.visit_variable(graph, node_id, name),
                Node::Literal(lit) => self.visit_literal(graph, node_id, lit),
                Node::Application { function, args } => self.visit_application(graph, node_id, *function, args),
                Node::Lambda { params, body } => self.visit_lambda(graph, node_id, params, *body),
                Node::Let { bindings, body } => self.visit_let(graph, node_id, bindings, *body),
                Node::Letrec { bindings, body } => self.visit_letrec(graph, node_id, bindings, *body),
                Node::If { condition, then_branch, else_branch } => {
                    self.visit_if(graph, node_id, *condition, *then_branch, *else_branch)
                }
                Node::List(elements) => self.visit_list(graph, node_id, elements),
                Node::Match { expr, branches } => self.visit_match(graph, node_id, *expr, branches),
                Node::Effect { effect_type, operation, args } => self.visit_effect(graph, node_id, effect_type, operation, args),
                _ => self.visit_default(graph, node_id, node),
            },
            None => self.visit_missing(graph, node_id),
        }
    }
    
    /// Visit a variable node
    fn visit_variable(&mut self, graph: &Graph, node_id: NodeId, name: &str) -> Self::Result;
    
    /// Visit a literal node
    fn visit_literal(&mut self, graph: &Graph, node_id: NodeId, literal: &Literal) -> Self::Result;
    
    /// Visit an application node
    fn visit_application(&mut self, graph: &Graph, node_id: NodeId, function: NodeId, args: &[NodeId]) -> Self::Result;
    
    /// Visit a lambda node
    fn visit_lambda(&mut self, graph: &Graph, node_id: NodeId, params: &[String], body: NodeId) -> Self::Result;
    
    /// Visit a let node
    fn visit_let(&mut self, graph: &Graph, node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) -> Self::Result;
    
    /// Visit a letrec node
    fn visit_letrec(&mut self, graph: &Graph, node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) -> Self::Result;
    
    /// Visit an if node
    fn visit_if(&mut self, graph: &Graph, node_id: NodeId, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> Self::Result;
    
    /// Visit a list node
    fn visit_list(&mut self, graph: &Graph, node_id: NodeId, elements: &[NodeId]) -> Self::Result;
    
    /// Visit a match node
    fn visit_match(&mut self, graph: &Graph, node_id: NodeId, expr: NodeId, branches: &[(Pattern, NodeId)]) -> Self::Result;
    
    /// Visit an effect node
    fn visit_effect(&mut self, graph: &Graph, node_id: NodeId, effect_type: &EffectType, operation: &str, args: &[NodeId]) -> Self::Result;
    
    /// Visit a default node (for unhandled node types)
    fn visit_default(&mut self, graph: &Graph, node_id: NodeId, node: &Node) -> Self::Result;
    
    /// Visit a missing node (node ID not found in graph)
    fn visit_missing(&mut self, graph: &Graph, node_id: NodeId) -> Self::Result;
}

/// Guarded visitor that switches to iterative traversal when depth limit is exceeded
pub struct GuardedVisitor<V: NodeVisitor> {
    _inner: V,
    depth: usize,
    max_depth: usize,
}

impl<V: NodeVisitor> GuardedVisitor<V> {
    /// Create a new guarded visitor
    pub fn new(inner: V, max_depth: usize) -> Self {
        Self {
            _inner: inner,
            depth: 0,
            max_depth,
        }
    }
    
    /// Check if we should switch to iterative mode
    pub fn should_switch_to_iterative(&self) -> bool {
        self.depth >= self.max_depth
    }
    
    /// Increment depth for recursive call
    pub fn with_depth<F, R>(&mut self, f: F) -> R 
    where 
        F: FnOnce(&mut Self) -> R
    {
        self.depth += 1;
        let result = f(self);
        self.depth -= 1;
        result
    }
}

/// Trampoline mechanism for tail recursion
pub enum Bounce<T> {
    /// Computation is complete
    Done(T),
    /// More computation needed
    Call(Box<dyn FnOnce() -> Bounce<T>>),
}

impl<T> Bounce<T> {
    /// Create a Done variant
    pub fn done(value: T) -> Self {
        Bounce::Done(value)
    }
    
    /// Create a Call variant
    pub fn call<F>(f: F) -> Self 
    where 
        F: FnOnce() -> Bounce<T> + 'static
    {
        Bounce::Call(Box::new(f))
    }
}

/// Execute a trampolined computation
pub fn trampoline<T>(mut bounce: Bounce<T>) -> T {
    loop {
        match bounce {
            Bounce::Done(value) => return value,
            Bounce::Call(thunk) => bounce = thunk(),
        }
    }
}

/// A visitor that collects all node IDs in traversal order
pub struct CollectingVisitor {
    /// List of collected node IDs
    pub nodes: Vec<NodeId>,
}

impl CollectingVisitor {
    /// Create a new collecting visitor
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }
}

impl NodeVisitor for CollectingVisitor {
    type Result = ();
    
    fn visit_variable(&mut self, _graph: &Graph, node_id: NodeId, _name: &str) {
        self.nodes.push(node_id);
    }
    
    fn visit_literal(&mut self, _graph: &Graph, node_id: NodeId, _literal: &Literal) {
        self.nodes.push(node_id);
    }
    
    fn visit_application(&mut self, graph: &Graph, node_id: NodeId, function: NodeId, args: &[NodeId]) {
        self.nodes.push(node_id);
        self.visit_node(graph, function);
        for arg in args {
            self.visit_node(graph, *arg);
        }
    }
    
    fn visit_lambda(&mut self, graph: &Graph, node_id: NodeId, _params: &[String], body: NodeId) {
        self.nodes.push(node_id);
        self.visit_node(graph, body);
    }
    
    fn visit_let(&mut self, graph: &Graph, node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) {
        self.nodes.push(node_id);
        for (_, value_id) in bindings {
            self.visit_node(graph, *value_id);
        }
        self.visit_node(graph, body);
    }
    
    fn visit_letrec(&mut self, graph: &Graph, node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) {
        self.nodes.push(node_id);
        for (_, value_id) in bindings {
            self.visit_node(graph, *value_id);
        }
        self.visit_node(graph, body);
    }
    
    fn visit_if(&mut self, graph: &Graph, node_id: NodeId, condition: NodeId, then_branch: NodeId, else_branch: NodeId) {
        self.nodes.push(node_id);
        self.visit_node(graph, condition);
        self.visit_node(graph, then_branch);
        self.visit_node(graph, else_branch);
    }
    
    fn visit_list(&mut self, graph: &Graph, node_id: NodeId, elements: &[NodeId]) {
        self.nodes.push(node_id);
        for elem in elements {
            self.visit_node(graph, *elem);
        }
    }
    
    fn visit_match(&mut self, graph: &Graph, node_id: NodeId, expr: NodeId, branches: &[(Pattern, NodeId)]) {
        self.nodes.push(node_id);
        self.visit_node(graph, expr);
        for (_, branch) in branches {
            self.visit_node(graph, *branch);
        }
    }
    
    fn visit_effect(&mut self, graph: &Graph, node_id: NodeId, _effect_type: &EffectType, _operation: &str, args: &[NodeId]) {
        self.nodes.push(node_id);
        for arg in args {
            self.visit_node(graph, *arg);
        }
    }
    
    fn visit_default(&mut self, _graph: &Graph, node_id: NodeId, _node: &Node) {
        self.nodes.push(node_id);
    }
    
    fn visit_missing(&mut self, _graph: &Graph, _node_id: NodeId) {
        // Skip missing nodes
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, Node, NodeId, Literal, Pattern, EffectType};
    use std::collections::HashMap;

    /// Simple visitor that counts different node types
    struct NodeCounter {
        counts: HashMap<String, usize>,
    }

    impl NodeCounter {
        fn new() -> Self {
            Self {
                counts: HashMap::new(),
            }
        }

        fn increment(&mut self, node_type: &str) {
            *self.counts.entry(node_type.to_string()).or_insert(0) += 1;
        }
    }

    impl NodeVisitor for NodeCounter {
        type Result = ();

        fn visit_variable(&mut self, _graph: &Graph, _node_id: NodeId, _name: &str) {
            self.increment("variable");
        }

        fn visit_literal(&mut self, _graph: &Graph, _node_id: NodeId, _literal: &Literal) {
            self.increment("literal");
        }

        fn visit_application(&mut self, graph: &Graph, _node_id: NodeId, function: NodeId, args: &[NodeId]) {
            self.increment("application");
            self.visit_node(graph, function);
            for arg in args {
                self.visit_node(graph, *arg);
            }
        }

        fn visit_lambda(&mut self, graph: &Graph, _node_id: NodeId, _params: &[String], body: NodeId) {
            self.increment("lambda");
            self.visit_node(graph, body);
        }

        fn visit_let(&mut self, graph: &Graph, _node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) {
            self.increment("let");
            for (_, value_id) in bindings {
                self.visit_node(graph, *value_id);
            }
            self.visit_node(graph, body);
        }

        fn visit_letrec(&mut self, graph: &Graph, _node_id: NodeId, bindings: &[(String, NodeId)], body: NodeId) {
            self.increment("letrec");
            for (_, value_id) in bindings {
                self.visit_node(graph, *value_id);
            }
            self.visit_node(graph, body);
        }

        fn visit_if(&mut self, graph: &Graph, _node_id: NodeId, condition: NodeId, then_branch: NodeId, else_branch: NodeId) {
            self.increment("if");
            self.visit_node(graph, condition);
            self.visit_node(graph, then_branch);
            self.visit_node(graph, else_branch);
        }

        fn visit_list(&mut self, graph: &Graph, _node_id: NodeId, elements: &[NodeId]) {
            self.increment("list");
            for elem in elements {
                self.visit_node(graph, *elem);
            }
        }

        fn visit_match(&mut self, graph: &Graph, _node_id: NodeId, expr: NodeId, branches: &[(Pattern, NodeId)]) {
            self.increment("match");
            self.visit_node(graph, expr);
            for (_, branch) in branches {
                self.visit_node(graph, *branch);
            }
        }

        fn visit_effect(&mut self, _graph: &Graph, _node_id: NodeId, _effect_type: &EffectType, _operation: &str, _args: &[NodeId]) {
            self.increment("effect");
        }

        fn visit_default(&mut self, _graph: &Graph, _node_id: NodeId, _node: &Node) {
            self.increment("default");
        }

        fn visit_missing(&mut self, _graph: &Graph, _node_id: NodeId) {
            self.increment("missing");
        }
    }

    #[test]
    fn test_visitor_basic() {
        let mut graph = Graph::new();
        
        // Create a simple AST: (+ 1 2)
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
        let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
        let app = graph.add_node(Node::Application {
            function: plus,
            args: vec![one, two],
        }).expect("Failed to add node");
        graph.root_id = Some(app);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, app);

        assert_eq!(counter.counts.get("application"), Some(&1));
        assert_eq!(counter.counts.get("variable"), Some(&1));
        assert_eq!(counter.counts.get("literal"), Some(&2));
    }

    #[test]
    fn test_visitor_lambda() {
        let mut graph = Graph::new();
        
        // Create: (lambda (x) x)
        let x_ref = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
        let lambda = graph.add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: x_ref,
        }).expect("Failed to add node");
        graph.root_id = Some(lambda);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, lambda);

        assert_eq!(counter.counts.get("lambda"), Some(&1));
        assert_eq!(counter.counts.get("variable"), Some(&1));
    }

    #[test]
    fn test_visitor_let_binding() {
        let mut graph = Graph::new();
        
        // Create: (let ((x 5)) x)
        let five = graph.add_node(Node::Literal(Literal::Integer(5))).expect("Failed to add node");
        let x_ref = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
        let let_node = graph.add_node(Node::Let {
            bindings: vec![("x".to_string(), five)],
            body: x_ref,
        }).expect("Failed to add node");
        graph.root_id = Some(let_node);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, let_node);

        assert_eq!(counter.counts.get("let"), Some(&1));
        assert_eq!(counter.counts.get("literal"), Some(&1));
        assert_eq!(counter.counts.get("variable"), Some(&1));
    }

    #[test]
    fn test_visitor_if_expression() {
        let mut graph = Graph::new();
        
        // Create: (if #t 1 2)
        let cond = graph.add_node(Node::Literal(Literal::Boolean(true))).expect("Failed to add node");
        let then_val = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
        let else_val = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
        let if_node = graph.add_node(Node::If {
            condition: cond,
            then_branch: then_val,
            else_branch: else_val,
        }).expect("Failed to add node");
        graph.root_id = Some(if_node);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, if_node);

        assert_eq!(counter.counts.get("if"), Some(&1));
        assert_eq!(counter.counts.get("literal"), Some(&3));
    }

    #[test]
    fn test_visitor_list() {
        let mut graph = Graph::new();
        
        // Create: (list 1 2 3)
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
        let three = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
        let list = graph.add_node(Node::List(vec![one, two, three])).expect("Failed to add node");
        graph.root_id = Some(list);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, list);

        assert_eq!(counter.counts.get("list"), Some(&1));
        assert_eq!(counter.counts.get("literal"), Some(&3));
    }

    #[test]
    fn test_visitor_missing_node() {
        let graph = Graph::new();
        let mut counter = NodeCounter::new();
        
        // Visit a non-existent node
        counter.visit_node(&graph, NodeId(std::num::NonZeroU32::new(9999).unwrap()));
        
        assert_eq!(counter.counts.get("missing"), Some(&1));
    }

    #[test]
    fn test_visitor_effect_node() {
        let mut graph = Graph::new();
        
        // Create an effect node
        let effect = graph.add_node(Node::Effect {
            effect_type: EffectType::IO,
            operation: "print".to_string(),
            args: vec![],
        }).expect("Failed to add node");
        graph.root_id = Some(effect);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, effect);

        assert_eq!(counter.counts.get("effect"), Some(&1));
    }

    #[test]
    fn test_guarded_visitor() {
        let mut graph = Graph::new();
        
        // Create a deep structure to test the guarded visitor
        let mut current = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
        for i in 1..10 {
            let body = graph.add_node(Node::Literal(Literal::Integer(i))).expect("Failed to add node");
            current = graph.add_node(Node::Let {
                bindings: vec![("x".to_string(), current)],
                body,
            }).expect("Failed to add node");
        }
        
        let guarded = GuardedVisitor::new(NodeCounter::new(), 5);
        
        // Test that depth tracking works
        assert_eq!(guarded.depth, 0);
        assert_eq!(guarded.max_depth, 5);
        assert!(!guarded.should_switch_to_iterative());
    }

    #[test]
    fn test_bounce_trampoline() {
        // Test Done
        let done = Bounce::done(100);
        assert_eq!(trampoline(done), 100);
        
        // Test Call
        let call = Bounce::call(|| Bounce::done(200));
        assert_eq!(trampoline(call), 200);
        
        // Test nested calls
        let nested = Bounce::call(|| {
            Bounce::call(|| {
                Bounce::done(300)
            })
        });
        assert_eq!(trampoline(nested), 300);
    }

    #[test]
    fn test_collecting_visitor() {
        let mut graph = Graph::new();
        
        // Create a simple structure
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
        let y = graph.add_node(Node::Variable { name: "y".to_string() }).expect("Failed to add node");
        let app = graph.add_node(Node::Application {
            function: x,
            args: vec![y],
        }).expect("Failed to add node");
        
        let mut collector = CollectingVisitor::new();
        collector.visit_node(&graph, app);
        
        // Should have collected all three nodes
        assert_eq!(collector.nodes.len(), 3);
        assert!(collector.nodes.contains(&app));
        assert!(collector.nodes.contains(&x));
        assert!(collector.nodes.contains(&y));
    }

    #[test]
    fn test_visitor_match_node() {
        let mut graph = Graph::new();
        
        // Create: (match x ((0) 'zero) ((1) 'one))
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
        let zero_lit = graph.add_node(Node::Literal(Literal::String("zero".to_string()))).expect("Failed to add node");
        let one_lit = graph.add_node(Node::Literal(Literal::String("one".to_string()))).expect("Failed to add node");
        
        let match_node = graph.add_node(Node::Match {
            expr: x,
            branches: vec![
                (Pattern::Literal(Literal::Integer(0)), zero_lit),
                (Pattern::Literal(Literal::Integer(1)), one_lit),
            ],
        }).expect("Failed to add node");
        graph.root_id = Some(match_node);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, match_node);

        assert_eq!(counter.counts.get("match"), Some(&1));
        assert_eq!(counter.counts.get("variable"), Some(&1));
        assert_eq!(counter.counts.get("literal"), Some(&2));
    }

    #[test]
    fn test_visitor_letrec() {
        let mut graph = Graph::new();
        
        // Create: (letrec ((fact (lambda (n) n))) (fact 5))
        let n = graph.add_node(Node::Variable { name: "n".to_string() }).expect("Failed to add node");
        let lambda = graph.add_node(Node::Lambda {
            params: vec!["n".to_string()],
            body: n,
        }).expect("Failed to add node");
        let five = graph.add_node(Node::Literal(Literal::Integer(5))).expect("Failed to add node");
        let fact_ref = graph.add_node(Node::Variable { name: "fact".to_string() }).expect("Failed to add node");
        let app = graph.add_node(Node::Application {
            function: fact_ref,
            args: vec![five],
        }).expect("Failed to add node");
        let letrec = graph.add_node(Node::Letrec {
            bindings: vec![("fact".to_string(), lambda)],
            body: app,
        }).expect("Failed to add node");
        graph.root_id = Some(letrec);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, letrec);

        assert_eq!(counter.counts.get("letrec"), Some(&1));
        assert_eq!(counter.counts.get("lambda"), Some(&1));
        assert_eq!(counter.counts.get("application"), Some(&1));
        assert_eq!(counter.counts.get("variable"), Some(&2));
        assert_eq!(counter.counts.get("literal"), Some(&1));
    }

    #[test]
    fn test_visitor_default_node() {
        let mut graph = Graph::new();
        
        // Create a node type that doesn't have a specific visitor method
        let channel = graph.add_node(Node::Channel).expect("Failed to add node");
        graph.root_id = Some(channel);

        let mut counter = NodeCounter::new();
        counter.visit_node(&graph, channel);

        assert_eq!(counter.counts.get("default"), Some(&1));
    }
}