//! AST representation using an efficient graph structure

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt;
use std::num::NonZeroU32;
use crate::documentation::{DocumentedNode, Documentation, DocumentationCategory, DocumentationVisibility};

/// Node identifier in the AST graph
/// 
/// Uses NonZeroU32 internally to enable null pointer optimization for Option<NodeId>.
/// NodeId(0) is reserved as an invalid/null node.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub NonZeroU32);

impl NodeId {
    /// Creates a new NodeId from a u32.
    /// Returns None if the value is 0.
    pub fn new(value: u32) -> Option<Self> {
        NonZeroU32::new(value).map(NodeId)
    }
    
    /// Gets the inner u32 value
    pub fn get(&self) -> u32 {
        self.0.get()
    }
}

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

/// Node metadata for analysis and optimization
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct NodeMetadata {
    /// Source location information
    pub span: Option<(usize, usize)>,
    /// Inferred type information
    pub type_info: Option<String>,
    /// Purity flag for optimization
    pub is_pure: Option<bool>,
    /// Custom annotations
    pub annotations: Vec<String>,
}

/// AST graph representation
/// 
/// # Invariants
/// - `next_id` monotonically increases and is never reused
/// - NodeIds are unique within a graph
/// - All NodeId references in nodes must point to valid nodes in the graph
/// - The root_id, if present, must point to a valid node
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph {
    pub nodes: FxHashMap<NodeId, Node>,
    pub root_id: Option<NodeId>,
    /// Next ID to assign. Starts at 1 and monotonically increases.
    /// IDs are never reused, even after node deletion.
    next_id: u32,
    /// Optional metadata for nodes
    pub metadata: FxHashMap<NodeId, NodeMetadata>,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: FxHashMap::default(),
            root_id: None,
            next_id: 1, // Start at 1 since 0 is reserved for null
            metadata: FxHashMap::default(),
        }
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        // SAFETY: next_id starts at 1 and only increments, so it's always non-zero
        let id = NodeId(NonZeroU32::new(self.next_id).expect("NodeId overflow"));
        self.next_id += 1;
        self.nodes.insert(id, node);
        id
    }

    pub fn get_node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(&id)
    }
    
    /// Gets mutable reference to a node
    pub fn get_node_mut(&mut self, id: NodeId) -> Option<&mut Node> {
        self.nodes.get_mut(&id)
    }
    
    /// Gets metadata for a node
    pub fn get_metadata(&self, id: NodeId) -> Option<&NodeMetadata> {
        self.metadata.get(&id)
    }
    
    /// Gets or creates metadata for a node
    pub fn metadata_mut(&mut self, id: NodeId) -> &mut NodeMetadata {
        self.metadata.entry(id).or_default()
    }
    
    /// Sets metadata for a node
    pub fn set_metadata(&mut self, id: NodeId, metadata: NodeMetadata) {
        self.metadata.insert(id, metadata);
    }
    
    /// Returns an iterator over all node IDs in the graph
    pub fn node_ids(&self) -> impl Iterator<Item = NodeId> + '_ {
        self.nodes.keys().copied()
    }
    
    /// Returns an iterator over all nodes in the graph
    pub fn nodes(&self) -> impl Iterator<Item = (&NodeId, &Node)> + '_ {
        self.nodes.iter()
    }
    
    /// Performs a depth-first traversal starting from the given node
    pub fn dfs_from(&self, start: NodeId, mut visitor: impl FnMut(NodeId, &Node)) {
        let mut visited = std::collections::HashSet::new();
        self.dfs_helper(start, &mut visited, &mut visitor);
    }
    
    fn dfs_helper(
        &self,
        node_id: NodeId,
        visited: &mut std::collections::HashSet<NodeId>,
        visitor: &mut impl FnMut(NodeId, &Node),
    ) {
        if !visited.insert(node_id) {
            return; // Already visited
        }
        
        if let Some(node) = self.get_node(node_id) {
            visitor(node_id, node);
            
            // Visit child nodes
            match node {
                Node::Lambda { body, .. } => {
                    self.dfs_helper(*body, visited, visitor);
                }
                Node::Application { function, args } => {
                    self.dfs_helper(*function, visited, visitor);
                    for arg in args {
                        self.dfs_helper(*arg, visited, visitor);
                    }
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value) in bindings {
                        self.dfs_helper(*value, visited, visitor);
                    }
                    self.dfs_helper(*body, visited, visitor);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.dfs_helper(*condition, visited, visitor);
                    self.dfs_helper(*then_branch, visited, visitor);
                    self.dfs_helper(*else_branch, visited, visitor);
                }
                Node::Match { expr, branches } => {
                    self.dfs_helper(*expr, visited, visitor);
                    for (_, branch) in branches {
                        self.dfs_helper(*branch, visited, visitor);
                    }
                }
                Node::List(items) => {
                    for item in items {
                        self.dfs_helper(*item, visited, visitor);
                    }
                }
                Node::Effect { args, .. } => {
                    for arg in args {
                        self.dfs_helper(*arg, visited, visitor);
                    }
                }
                _ => {} // Leaf nodes
            }
        }
    }
    
    /// Collects all child node IDs of a given node
    pub fn children(&self, node_id: NodeId) -> Vec<NodeId> {
        let mut children = Vec::new();
        if let Some(node) = self.get_node(node_id) {
            match node {
                Node::Lambda { body, .. } => {
                    children.push(*body);
                }
                Node::Application { function, args } => {
                    children.push(*function);
                    children.extend(args);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    children.extend(bindings.iter().map(|(_, v)| v));
                    children.push(*body);
                }
                Node::If { condition, then_branch, else_branch } => {
                    children.push(*condition);
                    children.push(*then_branch);
                    children.push(*else_branch);
                }
                Node::Match { expr, branches } => {
                    children.push(*expr);
                    children.extend(branches.iter().map(|(_, b)| b));
                }
                Node::List(items) => {
                    children.extend(items);
                }
                Node::Effect { args, .. } => {
                    children.extend(args);
                }
                _ => {} // Leaf nodes have no children
            }
        }
        children
    }
}

/// AST node types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Node {
    // Literals
    Literal(Literal),
    
    // Variables and bindings
    Variable { name: String },
    Lambda {
        params: Vec<String>,
        body: NodeId,
    },
    Let {
        bindings: Vec<(String, NodeId)>,
        body: NodeId,
    },
    Letrec {
        bindings: Vec<(String, NodeId)>,
        body: NodeId,
    },
    
    // Control flow
    If {
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    },
    
    // Function application
    Application {
        function: NodeId,
        args: Vec<NodeId>,
    },
    
    // Effects
    Effect {
        effect_type: EffectType,
        operation: String,
        args: Vec<NodeId>,
    },
    
    // Data structures
    List(Vec<NodeId>),
    
    // Pattern matching
    Match {
        expr: NodeId,
        branches: Vec<(Pattern, NodeId)>,
    },
    
    // Module system
    Module {
        name: String,
        exports: Vec<String>,
        body: NodeId,
    },
    Import {
        module_path: String,
        import_list: Vec<ImportItem>,
        import_all: bool,
    },
    Export {
        export_list: Vec<ExportItem>,
    },
    QualifiedVariable {
        module_name: String,
        variable_name: String,
    },
    
    // Async/concurrent constructs
    Async {
        body: NodeId,
    },
    Await {
        expr: NodeId,
    },
    Spawn {
        expr: NodeId,
    },
    Channel,
    Send {
        channel: NodeId,
        value: NodeId,
    },
    Receive {
        channel: NodeId,
    },
    
    // Contract specification
    Contract {
        function_name: String,
        preconditions: Vec<NodeId>,
        postconditions: Vec<NodeId>,
        invariants: Vec<NodeId>,
        complexity: Option<String>,
        pure: bool,
    },
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Pattern {
    Variable(String),
    Literal(Literal),
    Constructor {
        name: String,
        patterns: Vec<Pattern>,
    },
    Wildcard,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum EffectType {
    Pure,
    IO,
    State,
    Error,
    Time,
    Network,
    Random,
    Dom,
    Async,
    Concurrent,
}

impl fmt::Display for EffectType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EffectType::Pure => write!(f, "Pure"),
            EffectType::IO => write!(f, "IO"),
            EffectType::State => write!(f, "State"),
            EffectType::Error => write!(f, "Error"),
            EffectType::Time => write!(f, "Time"),
            EffectType::Network => write!(f, "Network"),
            EffectType::Random => write!(f, "Random"),
            EffectType::Dom => write!(f, "Dom"),
            EffectType::Async => write!(f, "Async"),
            EffectType::Concurrent => write!(f, "Concurrent"),
        }
    }
}

impl DocumentedNode for Literal {
    fn name() -> &'static str {
        "Literal"
    }
    
    fn syntax() -> &'static str {
        "<literal>"
    }
    
    fn description() -> &'static str {
        "Literal values in FluentAi"
    }
    
    fn examples() -> &'static [&'static str] {
        &["42", "3.14", "\"hello\"", "true", "nil"]
    }
    
    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImportItem {
    pub name: String,
    pub alias: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExportItem {
    pub name: String,
    pub alias: Option<String>,
}

impl DocumentedNode for Node {
    fn name() -> &'static str {
        "AST Node"
    }
    
    fn syntax() -> &'static str {
        "Various - see specific node types"
    }
    
    fn description() -> &'static str {
        "Abstract Syntax Tree node representing a FluentAi construct"
    }
    
    fn examples() -> &'static [&'static str] {
        &[]
    }
    
    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }
    
    fn visibility() -> DocumentationVisibility {
        DocumentationVisibility::Internal
    }
    
    fn get_docs() -> Documentation {
        // For the enum itself, we return generic documentation
        // The real documentation comes from get_node_docs() below
        Documentation {
            name: Self::name().to_string(),
            syntax: Self::syntax().to_string(),
            description: Self::description().to_string(),
            examples: Self::examples().iter().map(|s| s.to_string()).collect(),
            category: Self::category(),
            see_also: vec![],
            visibility: Self::visibility(),
        }
    }
}

impl Node {
    /// Get documentation specific to this node variant
    pub fn get_node_docs(&self) -> Documentation {
        match self {
            Node::Literal(lit) => match lit {
                Literal::Integer(_) => Documentation {
                    name: "Integer".to_string(),
                    syntax: "<integer>".to_string(),
                    description: "Integer literals represent whole numbers. FluentAi supports 64-bit signed integers.".to_string(),
                    examples: vec!["42".to_string(), "-17".to_string(), "0".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Float(_) => Documentation {
                    name: "Float".to_string(),
                    syntax: "<float>".to_string(),
                    description: "Floating-point literals represent decimal numbers. FluentAi uses 64-bit double precision.".to_string(),
                    examples: vec!["3.14".to_string(), "-2.5".to_string(), "1.23e-4".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::String(_) => Documentation {
                    name: "String".to_string(),
                    syntax: "\"<text>\"".to_string(),
                    description: "String literals represent text data. Strings are enclosed in double quotes and support escape sequences.".to_string(),
                    examples: vec!["\"Hello, World!\"".to_string(), "\"Line 1\\nLine 2\"".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Boolean(_) => Documentation {
                    name: "Boolean".to_string(),
                    syntax: "true | false".to_string(),
                    description: "Boolean literals represent truth values. Only 'true' and 'false' are valid boolean values.".to_string(),
                    examples: vec!["true".to_string(), "false".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
                Literal::Nil => Documentation {
                    name: "Nil".to_string(),
                    syntax: "nil".to_string(),
                    description: "Nil represents the absence of a value. It is the only value of its type.".to_string(),
                    examples: vec!["nil".to_string()],
                    category: DocumentationCategory::Literal,
                    see_also: vec![],
                    visibility: DocumentationVisibility::Public,
                },
            },
            Node::Variable { name: _ } => Documentation {
                name: "Variable".to_string(),
                syntax: "<identifier>".to_string(),
                description: "Variables reference values bound in the current scope. Variable names must start with a letter or underscore.".to_string(),
                examples: vec!["x".to_string(), "count".to_string(), "my_variable".to_string()],
                category: DocumentationCategory::Variable,
                see_also: vec!["Let".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Lambda { .. } => Documentation {
                name: "Lambda".to_string(),
                syntax: "(lambda (<params>) <body>)".to_string(),
                description: "Lambda creates anonymous functions. Parameters are bound in the function body scope.".to_string(),
                examples: vec!["(lambda (x) (+ x 1))".to_string(), "(lambda (x y) (* x y))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Application".to_string(), "Let".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Let { .. } => Documentation {
                name: "Let".to_string(),
                syntax: "(let ((<var> <expr>) ...) <body>)".to_string(),
                description: "Let creates local variable bindings. Bindings are evaluated sequentially and are available in the body.".to_string(),
                examples: vec!["(let ((x 5)) (+ x 1))".to_string(), "(let ((x 10) (y 20)) (+ x y))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Letrec".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Letrec { .. } => Documentation {
                name: "Letrec".to_string(),
                syntax: "(letrec ((<var> <expr>) ...) <body>)".to_string(),
                description: "Letrec creates recursive local bindings. All bindings are in scope for all binding expressions, enabling mutual recursion.".to_string(),
                examples: vec!["(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Let".to_string(), "Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::If { .. } => Documentation {
                name: "If".to_string(),
                syntax: "(if <condition> <then> <else>)".to_string(),
                description: "Conditional expression. Evaluates condition, then evaluates and returns either the then or else branch.".to_string(),
                examples: vec!["(if true \"yes\" \"no\")".to_string(), "(if (> x 0) \"positive\" \"non-positive\")".to_string()],
                category: DocumentationCategory::ControlFlow,
                see_also: vec!["Match".to_string(), "Boolean".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Application { .. } => Documentation {
                name: "Application".to_string(),
                syntax: "(<function> <arg1> <arg2> ...)".to_string(),
                description: "Function application. Applies a function to zero or more arguments.".to_string(),
                examples: vec!["(+ 1 2)".to_string(), "(print \"Hello\")".to_string(), "(map square [1 2 3])".to_string()],
                category: DocumentationCategory::Function,
                see_also: vec!["Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Effect { .. } => Documentation {
                name: "Effect".to_string(),
                syntax: "(effect <type> <operation> <args>...) | (<type>:<operation> <args>...)".to_string(),
                description: "Performs an effectful operation. Effects are tracked by the type system and handled by effect handlers. Can be called using explicit effect syntax or shorthand notation with colon.".to_string(),
                examples: vec!["(effect IO print \"Hello\")".to_string(), "(io:print \"Hello\")".to_string()],
                category: DocumentationCategory::Effect,
                see_also: vec!["Async".to_string(), "IO".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::List(_) => Documentation {
                name: "List".to_string(),
                syntax: "[<expr1> <expr2> ...]".to_string(),
                description: "List literal. Creates a list containing the evaluated expressions.".to_string(),
                examples: vec!["[1 2 3]".to_string(), "[\"a\" \"b\" \"c\"]".to_string(), "[]".to_string()],
                category: DocumentationCategory::DataStructure,
                see_also: vec!["cons".to_string(), "car".to_string(), "cdr".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Match { .. } => Documentation {
                name: "Match".to_string(),
                syntax: "(match <expr> (<pattern> <body>) ...)".to_string(),
                description: "Pattern matching expression. Matches the expression against patterns and evaluates the corresponding body.".to_string(),
                examples: vec!["(match x (0 \"zero\") (1 \"one\") (_ \"other\"))".to_string()],
                category: DocumentationCategory::PatternMatching,
                see_also: vec!["If".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Module { .. } => Documentation {
                name: "Module".to_string(),
                syntax: "(module <name> <exports> <body>)".to_string(),
                description: "Defines a module with a name, list of exports, and body. Modules provide namespace isolation.".to_string(),
                examples: vec!["(module math [add subtract] (let ((add +) (subtract -)) ...))".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Import".to_string(), "Export".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Import { .. } => Documentation {
                name: "Import".to_string(),
                syntax: "(import <module-path> [<items>...] | *)".to_string(),
                description: "Imports items from a module. Can import specific items or all exports with *.".to_string(),
                examples: vec!["(import \"std/math\" [sin cos])".to_string(), "(import \"utils\" *)".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Module".to_string(), "Export".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Export { .. } => Documentation {
                name: "Export".to_string(),
                syntax: "(export <name1> <name2> ... | <name> as <alias> ...)".to_string(),
                description: "Exports items from the current module. Can optionally rename exports with aliases.".to_string(),
                examples: vec!["(export add subtract multiply)".to_string()],
                category: DocumentationCategory::Module,
                see_also: vec!["Module".to_string(), "Import".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::QualifiedVariable { .. } => Documentation {
                name: "QualifiedVariable".to_string(),
                syntax: "<module>.<variable>".to_string(),
                description: "References a variable from a specific module namespace.".to_string(),
                examples: vec!["math.pi".to_string(), "std.print".to_string()],
                category: DocumentationCategory::Variable,
                see_also: vec!["Variable".to_string(), "Import".to_string(), "Module".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Async { .. } => Documentation {
                name: "Async".to_string(),
                syntax: "(async <body>)".to_string(),
                description: "Creates an asynchronous computation that can be awaited.".to_string(),
                examples: vec!["(async (http-get \"https://api.example.com\"))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Await".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Await { .. } => Documentation {
                name: "Await".to_string(),
                syntax: "(await <async-expr>)".to_string(),
                description: "Waits for an asynchronous computation to complete and returns its result.".to_string(),
                examples: vec!["(await (async (+ 1 2)))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Async".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Spawn { .. } => Documentation {
                name: "Spawn".to_string(),
                syntax: "(spawn <expr>)".to_string(),
                description: "Spawns a new concurrent task. Returns immediately without waiting for completion.".to_string(),
                examples: vec!["(spawn (process-data data))".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Async".to_string(), "Channel".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Channel => Documentation {
                name: "Channel".to_string(),
                syntax: "(chan)".to_string(),
                description: "Creates a new communication channel for sending values between concurrent tasks.".to_string(),
                examples: vec!["(let ((ch (chan))) ...)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Send".to_string(), "Receive".to_string(), "Spawn".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Send { .. } => Documentation {
                name: "Send".to_string(),
                syntax: "(send! <channel> <value>)".to_string(),
                description: "Sends a value through a channel. May block if the channel is full.".to_string(),
                examples: vec!["(send! ch 42)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Receive".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Receive { .. } => Documentation {
                name: "Receive".to_string(),
                syntax: "(recv! <channel>)".to_string(),
                description: "Receives a value from a channel. Blocks until a value is available.".to_string(),
                examples: vec!["(recv! ch)".to_string()],
                category: DocumentationCategory::Async,
                see_also: vec!["Channel".to_string(), "Send".to_string()],
                visibility: DocumentationVisibility::Public,
            },
            Node::Contract { .. } => Documentation {
                name: "Contract".to_string(),
                syntax: "(spec:contract <function-name> :requires [...] :ensures [...] :invariant [...] :complexity \"...\" :pure <bool>)".to_string(),
                description: "Specifies formal contracts for functions including preconditions, postconditions, invariants, complexity bounds, and purity constraints.".to_string(),
                examples: vec![
                    "(spec:contract factorial :requires [(>= n 0)] :ensures [(>= result 1)] :complexity \"O(n)\" :pure true)".to_string(),
                    "(spec:contract sort :requires [(list? input)] :ensures [(sorted? result) (= (length result) (length input))] :pure true)".to_string()
                ],
                category: DocumentationCategory::Verification,
                see_also: vec!["Lambda".to_string()],
                visibility: DocumentationVisibility::Public,
            },
        }
    }
}