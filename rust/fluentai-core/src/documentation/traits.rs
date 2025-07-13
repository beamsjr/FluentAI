//! Documentation traits and types for FluentAi

use serde::{Deserialize, Serialize};

/// Visibility level for documentation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DocumentationVisibility {
    /// Public API - visible to end users
    Public,
    /// Internal implementation detail - hidden from end users
    Internal,
    /// Advanced feature - shown with a warning
    Advanced,
}

/// Documentation for a language construct
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Documentation {
    /// Name of the documented construct
    pub name: String,
    /// Syntax pattern for the construct
    pub syntax: String,
    /// Human-readable description
    pub description: String,
    /// Usage examples
    pub examples: Vec<String>,
    /// Category for organization
    pub category: DocumentationCategory,
    /// Related constructs
    pub see_also: Vec<String>,
    /// Visibility level
    pub visibility: DocumentationVisibility,
}

/// Category of documented construct
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DocumentationCategory {
    /// Literal values (numbers, strings, etc.)
    Literal,
    /// Variable references and bindings
    Variable,
    /// Functions and function application
    Function,
    /// Control flow constructs (if, match, etc.)
    ControlFlow,
    /// Data structures (lists, maps, etc.)
    DataStructure,
    /// Pattern matching constructs
    PatternMatching,
    /// Module system constructs
    Module,
    /// Asynchronous programming constructs
    Async,
    /// Effect system constructs
    Effect,
    /// Operators
    Operator,
    /// Language keywords
    Keyword,
    /// Verification and contract constructs
    Verification,
}

/// Trait that all AST nodes must implement for documentation
pub trait DocumentedNode {
    /// Returns the name of the construct
    fn name() -> &'static str;

    /// Returns the syntax pattern
    fn syntax() -> &'static str;

    /// Returns a human-readable description
    fn description() -> &'static str;

    /// Returns example usage
    fn examples() -> &'static [&'static str];

    /// Returns the documentation category
    fn category() -> DocumentationCategory;

    /// Returns the visibility level (defaults to Public)
    fn visibility() -> DocumentationVisibility {
        DocumentationVisibility::Public
    }

    /// Returns related constructs (defaults to empty)
    fn see_also() -> &'static [&'static str] {
        &[]
    }

    /// Builds a complete Documentation struct
    fn get_docs() -> Documentation {
        Documentation {
            name: Self::name().to_string(),
            syntax: Self::syntax().to_string(),
            description: Self::description().to_string(),
            examples: Self::examples().iter().map(|s| s.to_string()).collect(),
            category: Self::category(),
            see_also: Self::see_also().iter().map(|s| s.to_string()).collect(),
            visibility: Self::visibility(),
        }
    }
}

/// Documentation for operators
#[derive(Debug, Clone)]
pub struct OperatorDoc {
    /// The operator symbol
    pub symbol: &'static str,
    /// Human-readable name
    pub name: &'static str,
    /// Operator precedence (higher binds tighter)
    pub precedence: u8,
    /// Associativity direction
    pub associativity: Associativity,
    /// Description of the operator
    pub description: &'static str,
    /// Usage examples
    pub examples: &'static [&'static str],
}

/// Operator associativity
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Associativity {
    /// Left-associative (a + b + c = (a + b) + c)
    Left,
    /// Right-associative (a ^ b ^ c = a ^ (b ^ c))
    Right,
    /// Non-associative
    None,
}

/// Documentation for keywords
#[derive(Debug, Clone)]
pub struct KeywordDoc {
    /// The keyword
    pub keyword: &'static str,
    /// Description of the keyword
    pub description: &'static str,
    /// Syntax pattern
    pub syntax: &'static str,
    /// Usage examples
    pub examples: &'static [&'static str],
}

/// Documentation for built-in functions
#[derive(Debug, Clone)]
pub struct BuiltinDoc {
    pub name: &'static str,
    pub signature: &'static str,
    pub description: &'static str,
    pub examples: &'static [&'static str],
    pub module: &'static str,
}

/// Helper function to create operator documentation
impl OperatorDoc {
    pub const fn new(
        symbol: &'static str,
        name: &'static str,
        precedence: u8,
        associativity: Associativity,
        description: &'static str,
        examples: &'static [&'static str],
    ) -> Self {
        Self {
            symbol,
            name,
            precedence,
            associativity,
            description,
            examples,
        }
    }

    /// Converts to a general Documentation struct
    pub fn to_documentation(&self) -> Documentation {
        Documentation {
            name: self.name.to_string(),
            syntax: self.symbol.to_string(),
            description: self.description.to_string(),
            examples: self.examples.iter().map(|s| s.to_string()).collect(),
            category: DocumentationCategory::Operator,
            see_also: vec![],
            visibility: DocumentationVisibility::Public,
        }
    }
}

/// Helper function to create keyword documentation
impl KeywordDoc {
    pub const fn new(
        keyword: &'static str,
        description: &'static str,
        syntax: &'static str,
        examples: &'static [&'static str],
    ) -> Self {
        Self {
            keyword,
            description,
            syntax,
            examples,
        }
    }

    /// Converts to a general Documentation struct
    pub fn to_documentation(&self) -> Documentation {
        Documentation {
            name: self.keyword.to_string(),
            syntax: self.syntax.to_string(),
            description: self.description.to_string(),
            examples: self.examples.iter().map(|s| s.to_string()).collect(),
            category: DocumentationCategory::Keyword,
            see_also: vec![],
            visibility: DocumentationVisibility::Public,
        }
    }
}

/// Trait for user-facing language features that must be documented
/// This is used to enforce documentation at compile time for features users can directly use
pub trait UserFacingFeature {
    /// Get the documentation for this feature
    fn documentation() -> Documentation;

    /// Validate that documentation is complete and appropriate for users
    fn validate_documentation() -> Result<(), &'static str> {
        let doc = Self::documentation();

        // Ensure visibility is Public
        if doc.visibility != DocumentationVisibility::Public {
            return Err("User-facing features must have Public visibility");
        }

        // Ensure required fields are non-empty
        if doc.name.is_empty() {
            return Err("Documentation must have a name");
        }
        if doc.syntax.is_empty() {
            return Err("Documentation must have syntax");
        }
        if doc.description.is_empty() {
            return Err("Documentation must have a description");
        }
        if doc.examples.is_empty() {
            return Err("Documentation must have at least one example");
        }

        Ok(())
    }
}

/// Helper function to create builtin documentation
impl BuiltinDoc {
    /// Creates a new builtin function documentation
    pub const fn new(
        name: &'static str,
        signature: &'static str,
        description: &'static str,
        examples: &'static [&'static str],
        module: &'static str,
    ) -> Self {
        Self {
            name,
            signature,
            description,
            examples,
            module,
        }
    }

    /// Converts to a general Documentation struct
    pub fn to_documentation(&self) -> Documentation {
        Documentation {
            name: self.name.to_string(),
            syntax: self.signature.to_string(),
            description: self.description.to_string(),
            examples: self.examples.iter().map(|s| s.to_string()).collect(),
            category: DocumentationCategory::Function,
            see_also: vec![],
            visibility: DocumentationVisibility::Public,
        }
    }
}
