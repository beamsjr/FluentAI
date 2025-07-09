//! Diagnostic types and utilities for the linter

use fluentai_core::ast::NodeId;
use miette::{Diagnostic, SourceSpan};
use serde::{Deserialize, Serialize};
use std::fmt;

/// A lint diagnostic with location and severity information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LintDiagnostic {
    /// Rule that generated this diagnostic
    pub rule_id: String,
    /// Diagnostic message
    pub message: String,
    /// Kind of diagnostic
    pub kind: DiagnosticKind,
    /// Location in the source
    pub location: Location,
    /// Optional suggestions for fixing
    pub suggestions: Vec<Suggestion>,
    /// Additional notes
    pub notes: Vec<String>,
}

/// Diagnostic severity/kind
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticKind {
    Error,
    Warning,
    Note,
    Help,
}

/// Location information for diagnostics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Location {
    /// File path
    pub file: Option<String>,
    /// Start position
    pub start: Position,
    /// End position
    pub end: Position,
    /// Node ID if available
    pub node_id: Option<NodeId>,
}

/// Position in source
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

/// A suggestion for fixing an issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Suggestion {
    /// Description of the suggestion
    pub message: String,
    /// Code changes to apply
    pub replacements: Vec<Replacement>,
}

/// A code replacement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Replacement {
    /// Location to replace
    pub location: Location,
    /// New text
    pub new_text: String,
}

impl LintDiagnostic {
    /// Create a new error diagnostic
    pub fn error(rule_id: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            rule_id: rule_id.into(),
            message: message.into(),
            kind: DiagnosticKind::Error,
            location: Location::unknown(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Create a new warning diagnostic
    pub fn warning(rule_id: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            rule_id: rule_id.into(),
            message: message.into(),
            kind: DiagnosticKind::Warning,
            location: Location::unknown(),
            suggestions: Vec::new(),
            notes: Vec::new(),
        }
    }

    /// Set the location
    pub fn with_location(mut self, location: Location) -> Self {
        self.location = location;
        self
    }

    /// Add a suggestion
    pub fn with_suggestion(mut self, suggestion: Suggestion) -> Self {
        self.suggestions.push(suggestion);
        self
    }

    /// Add a note
    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }
}

impl Location {
    /// Create an unknown location
    pub fn unknown() -> Self {
        Self {
            file: None,
            start: Position {
                line: 0,
                column: 0,
                offset: 0,
            },
            end: Position {
                line: 0,
                column: 0,
                offset: 0,
            },
            node_id: None,
        }
    }

    /// Create from line/column positions
    pub fn from_positions(
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    ) -> Self {
        Self {
            file: None,
            start: Position {
                line: start_line,
                column: start_col,
                offset: 0,
            },
            end: Position {
                line: end_line,
                column: end_col,
                offset: 0,
            },
            node_id: None,
        }
    }

    /// Convert to miette source span
    pub fn to_source_span(&self) -> SourceSpan {
        SourceSpan::from((self.start.offset, self.end.offset - self.start.offset))
    }
}

impl fmt::Display for DiagnosticKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DiagnosticKind::Error => write!(f, "error"),
            DiagnosticKind::Warning => write!(f, "warning"),
            DiagnosticKind::Note => write!(f, "note"),
            DiagnosticKind::Help => write!(f, "help"),
        }
    }
}

/// Convert to miette diagnostic for pretty printing
impl From<LintDiagnostic> for MietteDiagnostic {
    fn from(diag: LintDiagnostic) -> Self {
        MietteDiagnostic(diag)
    }
}

/// Wrapper for miette compatibility
pub struct MietteDiagnostic(pub LintDiagnostic);

impl fmt::Display for MietteDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.message)
    }
}

impl fmt::Debug for MietteDiagnostic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

impl std::error::Error for MietteDiagnostic {}

impl Diagnostic for MietteDiagnostic {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        Some(Box::new(&self.0.rule_id))
    }

    fn severity(&self) -> Option<miette::Severity> {
        match self.0.kind {
            DiagnosticKind::Error => Some(miette::Severity::Error),
            DiagnosticKind::Warning => Some(miette::Severity::Warning),
            DiagnosticKind::Note => Some(miette::Severity::Advice),
            DiagnosticKind::Help => Some(miette::Severity::Advice),
        }
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        if self.0.suggestions.is_empty() {
            None
        } else {
            Some(Box::new(self.0.suggestions[0].message.clone()))
        }
    }
}
