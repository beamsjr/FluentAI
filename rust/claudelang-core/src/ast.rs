//! AST representation using an efficient graph structure

use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::fmt;

/// Node identifier in the AST graph
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct NodeId(pub u32);

impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "n{}", self.0)
    }
}

/// AST graph representation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Graph {
    pub nodes: FxHashMap<NodeId, Node>,
    pub root_id: Option<NodeId>,
    next_id: u32,
}

impl Graph {
    pub fn new() -> Self {
        Self {
            nodes: FxHashMap::default(),
            root_id: None,
            next_id: 0,
        }
    }

    pub fn add_node(&mut self, node: Node) -> NodeId {
        let id = NodeId(self.next_id);
        self.next_id += 1;
        self.nodes.insert(id, node);
        id
    }

    pub fn get_node(&self, id: NodeId) -> Option<&Node> {
        self.nodes.get(&id)
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
}

#[derive(Debug, Clone, Serialize, Deserialize)]
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