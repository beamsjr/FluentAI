//! Program analysis infrastructure for optimizations

use fluentai_core::ast::{Graph, Node, NodeId, EffectType};
use rustc_hash::{FxHashMap, FxHashSet};

/// Control flow graph representation
pub struct ControlFlowGraph {
    /// Entry nodes
    pub entries: FxHashSet<NodeId>,
    /// Exit nodes  
    pub exits: FxHashSet<NodeId>,
    /// Predecessors for each node
    pub predecessors: FxHashMap<NodeId, FxHashSet<NodeId>>,
    /// Successors for each node
    pub successors: FxHashMap<NodeId, FxHashSet<NodeId>>,
    /// Loop headers
    pub loop_headers: FxHashSet<NodeId>,
    /// Dominators
    pub dominators: FxHashMap<NodeId, FxHashSet<NodeId>>,
}

impl ControlFlowGraph {
    /// Build control flow graph from AST
    pub fn build(graph: &Graph) -> Self {
        let mut cfg = Self {
            entries: FxHashSet::default(),
            exits: FxHashSet::default(),
            predecessors: FxHashMap::default(),
            successors: FxHashMap::default(),
            loop_headers: FxHashSet::default(),
            dominators: FxHashMap::default(),
        };

        // Start from root
        if let Some(root) = graph.root_id {
            cfg.entries.insert(root);
            cfg.analyze_node(graph, root, None);
            cfg.compute_dominators(graph);
            cfg.find_loops();
        }

        cfg
    }

    fn analyze_node(&mut self, graph: &Graph, node_id: NodeId, pred: Option<NodeId>) {
        // Add predecessor relationship
        if let Some(p) = pred {
            self.predecessors.entry(node_id).or_default().insert(p);
            self.successors.entry(p).or_default().insert(node_id);
        }

        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::If { condition, then_branch, else_branch } => {
                    // Condition is evaluated first
                    self.analyze_node(graph, *condition, Some(node_id));
                    // Then both branches
                    self.analyze_node(graph, *then_branch, Some(node_id));
                    self.analyze_node(graph, *else_branch, Some(node_id));
                }
                Node::Application { function, args } => {
                    // Function evaluated first
                    self.analyze_node(graph, *function, Some(node_id));
                    // Then arguments in order
                    for arg in args {
                        self.analyze_node(graph, *arg, Some(node_id));
                    }
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    // Bindings first
                    for (_, value_id) in bindings {
                        self.analyze_node(graph, *value_id, Some(node_id));
                    }
                    // Then body
                    self.analyze_node(graph, *body, Some(node_id));
                }
                Node::Lambda { body, .. } => {
                    self.analyze_node(graph, *body, Some(node_id));
                }
                Node::Match { expr, branches } => {
                    self.analyze_node(graph, *expr, Some(node_id));
                    for (_, branch_body) in branches {
                        self.analyze_node(graph, *branch_body, Some(node_id));
                    }
                }
                Node::List(items) => {
                    // Analyze each item in the list
                    for item in items {
                        self.analyze_node(graph, *item, Some(node_id));
                    }
                }
                _ => {
                    // Leaf nodes are potential exits
                    if self.successors.get(&node_id).map_or(true, |s| s.is_empty()) {
                        self.exits.insert(node_id);
                    }
                }
            }
        }
    }

    fn compute_dominators(&mut self, graph: &Graph) {
        // Simple dominator computation
        for node_id in graph.nodes.keys() {
            let mut doms = FxHashSet::default();
            doms.insert(*node_id); // Node dominates itself
            self.dominators.insert(*node_id, doms);
        }
    }

    fn find_loops(&mut self) {
        // Simple loop detection - mark nodes with back edges
        for (node, preds) in &self.predecessors {
            for pred in preds {
                if self.dominators.get(pred)
                    .map_or(false, |doms| doms.contains(node)) {
                    self.loop_headers.insert(*node);
                }
            }
        }
    }
}

/// Data flow analysis results
pub struct DataFlowAnalysis {
    /// Variables defined at each node
    pub definitions: FxHashMap<NodeId, FxHashSet<String>>,
    /// Variables used at each node
    pub uses: FxHashMap<NodeId, FxHashSet<String>>,
    /// Live variables at each node
    pub live_in: FxHashMap<NodeId, FxHashSet<String>>,
    /// Live variables after each node
    pub live_out: FxHashMap<NodeId, FxHashSet<String>>,
    /// Reaching definitions
    pub reaching_defs: FxHashMap<NodeId, FxHashSet<(String, NodeId)>>,
}

impl DataFlowAnalysis {
    /// Perform data flow analysis
    pub fn analyze(graph: &Graph, cfg: &ControlFlowGraph) -> Self {
        let mut analysis = Self {
            definitions: FxHashMap::default(),
            uses: FxHashMap::default(),
            live_in: FxHashMap::default(),
            live_out: FxHashMap::default(),
            reaching_defs: FxHashMap::default(),
        };

        // First pass: collect definitions and uses
        for (node_id, node) in &graph.nodes {
            analysis.analyze_node(*node_id, node);
        }

        // Compute liveness
        analysis.compute_liveness(cfg);

        // Compute reaching definitions
        analysis.compute_reaching_definitions(cfg);

        analysis
    }

    fn analyze_node(&mut self, node_id: NodeId, node: &Node) {
        let mut defs = FxHashSet::default();
        let mut uses = FxHashSet::default();

        match node {
            Node::Variable { name } => {
                uses.insert(name.clone());
            }
            Node::Let { bindings, .. } => {
                for (name, _) in bindings {
                    defs.insert(name.clone());
                }
            }
            Node::Letrec { bindings, .. } => {
                for (name, _) in bindings {
                    defs.insert(name.clone());
                }
            }
            Node::Lambda { params, .. } => {
                for param in params {
                    defs.insert(param.clone());
                }
            }
            _ => {}
        }

        self.definitions.insert(node_id, defs);
        self.uses.insert(node_id, uses);
    }

    fn compute_liveness(&mut self, cfg: &ControlFlowGraph) {
        // Backward data flow analysis for liveness
        let mut changed = true;
        while changed {
            changed = false;

            for node_id in cfg.exits.iter() {
                // Exit nodes have empty live_out
                self.live_out.entry(*node_id).or_default();
            }

            // Process nodes in reverse topological order
            for (node_id, _) in &self.definitions {
                let mut new_live_in = self.uses.get(node_id).cloned()
                    .unwrap_or_default();

                if let Some(live_out) = self.live_out.get(node_id) {
                    for var in live_out {
                        if !self.definitions.get(node_id)
                            .map_or(false, |defs| defs.contains(var)) {
                            new_live_in.insert(var.clone());
                        }
                    }
                }

                let old_size = self.live_in.get(node_id).map_or(0, |s| s.len());
                self.live_in.insert(*node_id, new_live_in.clone());
                if new_live_in.len() != old_size {
                    changed = true;
                }

                // Update live_out of predecessors
                if let Some(preds) = cfg.predecessors.get(node_id) {
                    for pred in preds {
                        let live_out = self.live_out.entry(*pred).or_default();
                        let old_size = live_out.len();
                        live_out.extend(new_live_in.iter().cloned());
                        if live_out.len() != old_size {
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    fn compute_reaching_definitions(&mut self, cfg: &ControlFlowGraph) {
        // Forward data flow analysis for reaching definitions
        let mut changed = true;
        while changed {
            changed = false;

            for (node_id, defs) in &self.definitions {
                let mut reaching = FxHashSet::default();

                // Collect reaching definitions from predecessors
                if let Some(preds) = cfg.predecessors.get(node_id) {
                    for pred in preds {
                        if let Some(pred_reaching) = self.reaching_defs.get(pred) {
                            reaching.extend(pred_reaching.iter().cloned());
                        }
                    }
                }

                // Kill definitions of variables defined here
                reaching.retain(|(var, _)| !defs.contains(var));

                // Gen new definitions
                for var in defs {
                    reaching.insert((var.clone(), *node_id));
                }

                let old_size = self.reaching_defs.get(node_id).map_or(0, |s| s.len());
                self.reaching_defs.insert(*node_id, reaching);
                if self.reaching_defs[node_id].len() != old_size {
                    changed = true;
                }
            }
        }
    }
}

/// Effect analysis for optimization
pub struct EffectAnalysis {
    /// Effects for each node
    pub node_effects: FxHashMap<NodeId, FxHashSet<EffectType>>,
    /// Pure nodes (no side effects)
    pub pure_nodes: FxHashSet<NodeId>,
    /// Nodes that can be evaluated at compile time
    pub const_evaluable: FxHashSet<NodeId>,
}

impl EffectAnalysis {
    /// Analyze effects in the graph
    pub fn analyze(graph: &Graph) -> Self {
        let mut analysis = Self {
            node_effects: FxHashMap::default(),
            pure_nodes: FxHashSet::default(),
            const_evaluable: FxHashSet::default(),
        };

        // Analyze each node
        for (node_id, node) in &graph.nodes {
            let mut visited = FxHashSet::default();
            let effects = analysis.analyze_node_effects_with_visited(graph, *node_id, node, &mut visited);
            
            if effects.is_empty() || (effects.len() == 1 && effects.contains(&EffectType::Pure)) {
                analysis.pure_nodes.insert(*node_id);
                
                // Check if const evaluable
                if analysis.is_const_evaluable(graph, *node_id, node) {
                    analysis.const_evaluable.insert(*node_id);
                }
            }
            
            analysis.node_effects.insert(*node_id, effects);
        }

        analysis
    }

    fn analyze_node_effects(&self, graph: &Graph, node_id: NodeId, node: &Node) -> FxHashSet<EffectType> {
        let mut visited = FxHashSet::default();
        self.analyze_node_effects_with_visited(graph, node_id, node, &mut visited)
    }

    fn analyze_node_effects_with_visited(&self, graph: &Graph, node_id: NodeId, node: &Node, visited: &mut FxHashSet<NodeId>) -> FxHashSet<EffectType> {
        // Check for cycles
        if !visited.insert(node_id) {
            // We've already visited this node - assume pure to break the cycle
            let mut effects = FxHashSet::default();
            effects.insert(EffectType::Pure);
            return effects;
        }

        let mut effects = FxHashSet::default();

        match node {
            Node::Literal(_) => {
                effects.insert(EffectType::Pure);
            }
            Node::Variable { .. } => {
                effects.insert(EffectType::Pure);
            }
            Node::Lambda { .. } => {
                effects.insert(EffectType::Pure);
            }
            Node::Effect { effect_type, .. } => {
                effects.insert(*effect_type);
            }
            Node::Application { function, args } => {
                // Collect effects from function and arguments
                if let Some(func_node) = graph.get_node(*function) {
                    effects.extend(self.analyze_node_effects_with_visited(graph, *function, func_node, visited));
                }
                for arg in args {
                    if let Some(arg_node) = graph.get_node(*arg) {
                        effects.extend(self.analyze_node_effects_with_visited(graph, *arg, arg_node, visited));
                    }
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                if let Some(cond) = graph.get_node(*condition) {
                    effects.extend(self.analyze_node_effects_with_visited(graph, *condition, cond, visited));
                }
                if let Some(then_n) = graph.get_node(*then_branch) {
                    effects.extend(self.analyze_node_effects_with_visited(graph, *then_branch, then_n, visited));
                }
                if let Some(else_n) = graph.get_node(*else_branch) {
                    effects.extend(self.analyze_node_effects_with_visited(graph, *else_branch, else_n, visited));
                }
            }
            Node::List(items) => {
                // Lists are pure, but analyze contained items
                effects.insert(EffectType::Pure);
                for item in items {
                    if let Some(item_node) = graph.get_node(*item) {
                        effects.extend(self.analyze_node_effects_with_visited(graph, *item, item_node, visited));
                    }
                }
            }
            _ => {
                // Default to pure for other nodes
                effects.insert(EffectType::Pure);
            }
        }

        effects
    }

    fn is_const_evaluable(&self, graph: &Graph, _node_id: NodeId, node: &Node) -> bool {
        match node {
            Node::Literal(_) => true,
            Node::Application { function, args } => {
                // Check if function is a known pure primitive
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    if is_pure_primitive(name) {
                        // Check if all arguments are const evaluable
                        return args.iter().all(|arg_id| {
                            self.const_evaluable.contains(arg_id) ||
                            graph.get_node(*arg_id).map_or(false, |n| {
                                matches!(n, Node::Literal(_))
                            })
                        });
                    }
                }
                false
            }
            _ => false,
        }
    }
}

/// Check if a function name is a pure primitive
fn is_pure_primitive(name: &str) -> bool {
    matches!(name,
        "+" | "-" | "*" | "/" | "mod" |
        "<" | ">" | "<=" | ">=" | "=" | "!=" |
        "and" | "or" | "not" |
        "car" | "cdr" | "cons" | "list" |
        "list-len" | "list-empty?" |
        "str-len" | "str-concat" | "str-upper" | "str-lower"
    )
}

/// Alias analysis for optimization
pub struct AliasAnalysis {
    /// Alias sets - nodes that may refer to the same value
    pub alias_sets: Vec<FxHashSet<NodeId>>,
    /// Node to alias set mapping
    pub node_to_set: FxHashMap<NodeId, usize>,
}

impl AliasAnalysis {
    /// Perform alias analysis
    pub fn analyze(graph: &Graph) -> Self {
        let mut analysis = Self {
            alias_sets: Vec::new(),
            node_to_set: FxHashMap::default(),
        };

        // Simple alias analysis - group nodes by value equality
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Variable { name } => {
                    // Variables with same name may alias
                    let mut found_set = None;
                    for (set_idx, set) in analysis.alias_sets.iter().enumerate() {
                        for other_id in set {
                            if let Some(Node::Variable { name: other_name }) = graph.get_node(*other_id) {
                                if name == other_name {
                                    found_set = Some(set_idx);
                                    break;
                                }
                            }
                        }
                        if found_set.is_some() {
                            break;
                        }
                    }

                    if let Some(set_idx) = found_set {
                        analysis.alias_sets[set_idx].insert(*node_id);
                        analysis.node_to_set.insert(*node_id, set_idx);
                    } else {
                        let mut new_set = FxHashSet::default();
                        new_set.insert(*node_id);
                        let set_idx = analysis.alias_sets.len();
                        analysis.alias_sets.push(new_set);
                        analysis.node_to_set.insert(*node_id, set_idx);
                    }
                }
                _ => {
                    // Each other node is in its own alias set
                    let mut new_set = FxHashSet::default();
                    new_set.insert(*node_id);
                    let set_idx = analysis.alias_sets.len();
                    analysis.alias_sets.push(new_set);
                    analysis.node_to_set.insert(*node_id, set_idx);
                }
            }
        }

        analysis
    }

    /// Check if two nodes may alias
    pub fn may_alias(&self, node1: NodeId, node2: NodeId) -> bool {
        if let (Some(set1), Some(set2)) = (self.node_to_set.get(&node1), self.node_to_set.get(&node2)) {
            set1 == set2
        } else {
            false
        }
    }
}

/// Calculate the size of a node (for inlining decisions)
pub fn calculate_node_size(graph: &Graph, node_id: NodeId) -> usize {
    let mut size = 0;
    let mut visited = FxHashSet::default();
    calculate_node_size_helper(graph, node_id, &mut size, &mut visited);
    size
}

fn calculate_node_size_helper(graph: &Graph, node_id: NodeId, size: &mut usize, visited: &mut FxHashSet<NodeId>) {
    if !visited.insert(node_id) {
        return; // Already visited
    }
    
    *size += 1;
    
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Application { function, args } => {
                calculate_node_size_helper(graph, *function, size, visited);
                for arg in args {
                    calculate_node_size_helper(graph, *arg, size, visited);
                }
            }
            Node::Lambda { body, .. } => {
                calculate_node_size_helper(graph, *body, size, visited);
            }
            Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                for (_, value) in bindings {
                    calculate_node_size_helper(graph, *value, size, visited);
                }
                calculate_node_size_helper(graph, *body, size, visited);
            }
            Node::If { condition, then_branch, else_branch } => {
                calculate_node_size_helper(graph, *condition, size, visited);
                calculate_node_size_helper(graph, *then_branch, size, visited);
                calculate_node_size_helper(graph, *else_branch, size, visited);
            }
            Node::Match { expr, branches } => {
                calculate_node_size_helper(graph, *expr, size, visited);
                for (_, branch) in branches {
                    calculate_node_size_helper(graph, *branch, size, visited);
                }
            }
            Node::List(items) => {
                // Count nodes within the list
                for item in items {
                    calculate_node_size_helper(graph, *item, size, visited);
                }
            }
            _ => {} // Leaf nodes
        }
    }
}

/// Check if a function is recursive
pub fn is_recursive_function(graph: &Graph, func_id: NodeId) -> bool {
    if let Some(Node::Lambda { body, .. }) = graph.get_node(func_id) {
        let mut visited = FxHashSet::default();
        contains_reference_to(graph, *body, func_id, &mut visited)
    } else {
        false
    }
}

fn contains_reference_to(graph: &Graph, node_id: NodeId, target_id: NodeId, visited: &mut FxHashSet<NodeId>) -> bool {
    if !visited.insert(node_id) {
        return false; // Already visited
    }
    
    if node_id == target_id {
        return true;
    }
    
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Application { function, args } => {
                if contains_reference_to(graph, *function, target_id, visited) {
                    return true;
                }
                for arg in args {
                    if contains_reference_to(graph, *arg, target_id, visited) {
                        return true;
                    }
                }
            }
            Node::Lambda { body, .. } => {
                return contains_reference_to(graph, *body, target_id, visited);
            }
            Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                for (_, value) in bindings {
                    if contains_reference_to(graph, *value, target_id, visited) {
                        return true;
                    }
                }
                return contains_reference_to(graph, *body, target_id, visited);
            }
            Node::If { condition, then_branch, else_branch } => {
                return contains_reference_to(graph, *condition, target_id, visited) ||
                       contains_reference_to(graph, *then_branch, target_id, visited) ||
                       contains_reference_to(graph, *else_branch, target_id, visited);
            }
            Node::Match { expr, branches } => {
                if contains_reference_to(graph, *expr, target_id, visited) {
                    return true;
                }
                for (_, branch) in branches {
                    if contains_reference_to(graph, *branch, target_id, visited) {
                        return true;
                    }
                }
            }
            Node::List(items) => {
                // Check for references within the list
                for item in items {
                    if contains_reference_to(graph, *item, target_id, visited) {
                        return true;
                    }
                }
            }
            _ => {} // Leaf nodes
        }
    }
    
    false
}

/// Type-based analysis for optimization
pub struct TypeAnalysis {
    /// Type information for each node
    pub node_types: FxHashMap<NodeId, TypeInfo>,
    /// Nodes that can be specialized
    pub specializable: FxHashSet<NodeId>,
    /// Polymorphic functions
    pub polymorphic_functions: FxHashSet<NodeId>,
}

/// Type information for optimization
#[derive(Debug, Clone)]
pub enum TypeInfo {
    /// Known concrete type
    Concrete(ConcreteType),
    /// Polymorphic type
    Polymorphic,
    /// Unknown type
    Unknown,
}

/// Concrete types we can optimize for
#[derive(Debug, Clone, PartialEq)]
pub enum ConcreteType {
    Integer,
    Float,
    Boolean,
    String,
    List(Box<ConcreteType>),
    Function(Vec<ConcreteType>, Box<ConcreteType>),
}

impl TypeAnalysis {
    /// Perform type analysis on the graph
    pub fn analyze(graph: &Graph) -> Self {
        let mut analysis = Self {
            node_types: FxHashMap::default(),
            specializable: FxHashSet::default(),
            polymorphic_functions: FxHashSet::default(),
        };

        // First pass: infer types from literals and built-ins
        for (node_id, node) in &graph.nodes {
            if let Some(type_info) = analysis.infer_node_type(graph, node) {
                analysis.node_types.insert(*node_id, type_info);
            }
        }

        // Second pass: propagate types through the graph
        analysis.propagate_types(graph);

        // Identify specialization opportunities
        analysis.find_specializable_nodes(graph);

        analysis
    }

    /// Infer type from a node
    fn infer_node_type(&self, _graph: &Graph, node: &Node) -> Option<TypeInfo> {
        match node {
            Node::Literal(lit) => {
                use fluentai_core::ast::Literal::*;
                let concrete_type = match lit {
                    Integer(_) => ConcreteType::Integer,
                    Float(_) => ConcreteType::Float,
                    Boolean(_) => ConcreteType::Boolean,
                    String(_) => ConcreteType::String,
                    Nil => return Some(TypeInfo::Unknown),
                };
                Some(TypeInfo::Concrete(concrete_type))
            }
            Node::Variable { name } => {
                // Type inference for built-in functions
                match name.as_str() {
                    "+" | "-" | "*" | "/" => Some(TypeInfo::Concrete(ConcreteType::Function(
                        vec![ConcreteType::Integer, ConcreteType::Integer],
                        Box::new(ConcreteType::Integer),
                    ))),
                    "<" | ">" | "=" | "<=" | ">=" => Some(TypeInfo::Concrete(ConcreteType::Function(
                        vec![ConcreteType::Integer, ConcreteType::Integer],
                        Box::new(ConcreteType::Boolean),
                    ))),
                    "and" | "or" => Some(TypeInfo::Concrete(ConcreteType::Function(
                        vec![ConcreteType::Boolean, ConcreteType::Boolean],
                        Box::new(ConcreteType::Boolean),
                    ))),
                    "not" => Some(TypeInfo::Concrete(ConcreteType::Function(
                        vec![ConcreteType::Boolean],
                        Box::new(ConcreteType::Boolean),
                    ))),
                    _ => None,
                }
            }
            Node::List(items) => {
                if items.is_empty() {
                    Some(TypeInfo::Unknown)
                } else {
                    // For now, assume homogeneous lists
                    Some(TypeInfo::Polymorphic)
                }
            }
            _ => None,
        }
    }

    /// Propagate types through the graph
    fn propagate_types(&mut self, graph: &Graph) {
        let mut changed = true;
        while changed {
            changed = false;
            
            for (node_id, node) in &graph.nodes {
                if self.node_types.contains_key(node_id) {
                    continue;
                }

                match node {
                    Node::Application { function, args } => {
                        // Try to infer result type from function type
                        if let Some(TypeInfo::Concrete(ConcreteType::Function(_, result))) = 
                            self.node_types.get(function) {
                            self.node_types.insert(*node_id, TypeInfo::Concrete((**result).clone()));
                            changed = true;
                        }
                    }
                    Node::If { condition: _, then_branch, else_branch } => {
                        // If both branches have the same type, the if expression has that type
                        if let (Some(then_type), Some(else_type)) = 
                            (self.node_types.get(then_branch), self.node_types.get(else_branch)) {
                            if matches!((then_type, else_type), 
                                       (TypeInfo::Concrete(t1), TypeInfo::Concrete(t2)) if t1 == t2) {
                                self.node_types.insert(*node_id, then_type.clone());
                                changed = true;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    /// Find nodes that can be specialized
    fn find_specializable_nodes(&mut self, graph: &Graph) {
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Lambda { .. } => {
                    // Lambda functions can potentially be specialized
                    if let Some(TypeInfo::Concrete(_)) = self.node_types.get(node_id) {
                        self.specializable.insert(*node_id);
                    } else {
                        self.polymorphic_functions.insert(*node_id);
                    }
                }
                Node::Application { function, .. } => {
                    // Applications of polymorphic functions can be specialized
                    if self.polymorphic_functions.contains(function) {
                        self.specializable.insert(*node_id);
                    }
                }
                _ => {}
            }
        }
    }

    /// Check if a node has a concrete type
    pub fn has_concrete_type(&self, node_id: NodeId) -> bool {
        matches!(self.node_types.get(&node_id), Some(TypeInfo::Concrete(_)))
    }

    /// Get the concrete type of a node if available
    pub fn get_concrete_type(&self, node_id: NodeId) -> Option<&ConcreteType> {
        match self.node_types.get(&node_id) {
            Some(TypeInfo::Concrete(t)) => Some(t),
            _ => None,
        }
    }
}

#[cfg(test)]
#[path = "analysis_tests.rs"]
mod analysis_tests;