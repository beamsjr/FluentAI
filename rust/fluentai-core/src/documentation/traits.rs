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
    pub name: String,
    pub syntax: String,
    pub description: String,
    pub examples: Vec<String>,
    pub category: DocumentationCategory,
    pub see_also: Vec<String>,
    pub visibility: DocumentationVisibility,
}

/// Category of documented construct
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DocumentationCategory {
    Literal,
    Variable,
    Function,
    ControlFlow,
    DataStructure,
    PatternMatching,
    Module,
    Async,
    Effect,
    Operator,
    Keyword,
    Verification,
}

/// Trait that all AST nodes must implement for documentation
pub trait DocumentedNode {
    fn name() -> &'static str;

    fn syntax() -> &'static str;

    fn description() -> &'static str;

    fn examples() -> &'static [&'static str];

    fn category() -> DocumentationCategory;

    fn visibility() -> DocumentationVisibility {
        DocumentationVisibility::Public
    }

    fn see_also() -> &'static [&'static str] {
        &[]
    }

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
    pub symbol: &'static str,
    pub name: &'static str,
    pub precedence: u8,
    pub associativity: Associativity,
    pub description: &'static str,
    pub examples: &'static [&'static str],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Associativity {
    Left,
    Right,
    None,
}

/// Documentation for keywords
#[derive(Debug, Clone)]
pub struct KeywordDoc {
    pub keyword: &'static str,
    pub description: &'static str,
    pub syntax: &'static str,
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
