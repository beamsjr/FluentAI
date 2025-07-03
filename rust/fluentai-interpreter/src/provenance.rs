//! Provenance tracking for debugging and error reporting

use std::fmt;
use fluentai_core::ast::NodeId;

/// Source location information
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// File name or module name
    pub file: String,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
    /// Optional end position for ranges
    pub end: Option<(usize, usize)>,
}

impl SourceLocation {
    /// Create a new source location
    pub fn new(file: String, line: usize, column: usize) -> Self {
        Self {
            file,
            line,
            column,
            end: None,
        }
    }

    /// Create a source location with a range
    pub fn with_range(
        file: String,
        line: usize,
        column: usize,
        end_line: usize,
        end_column: usize,
    ) -> Self {
        Self {
            file,
            line,
            column,
            end: Some((end_line, end_column)),
        }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.column)?;
        if let Some((end_line, end_col)) = self.end {
            write!(f, "-{}:{}", end_line, end_col)?;
        }
        Ok(())
    }
}

/// Provenance information for values and errors
#[derive(Debug, Clone)]
pub struct ProvenanceInfo {
    /// The node that produced this value
    pub node_id: NodeId,
    /// Source location if available
    pub location: Option<SourceLocation>,
    /// Parent provenance (for tracking through transformations)
    pub parent: Option<Box<ProvenanceInfo>>,
    /// Additional context
    pub context: Option<String>,
}

impl ProvenanceInfo {
    /// Create basic provenance info
    pub fn new(node_id: NodeId) -> Self {
        Self {
            node_id,
            location: None,
            parent: None,
            context: None,
        }
    }

    /// Add source location
    pub fn with_location(mut self, location: SourceLocation) -> Self {
        self.location = Some(location);
        self
    }

    /// Add parent provenance
    pub fn with_parent(mut self, parent: ProvenanceInfo) -> Self {
        self.parent = Some(Box::new(parent));
        self
    }

    /// Add context information
    pub fn with_context(mut self, context: String) -> Self {
        self.context = Some(context);
        self
    }

    /// Get the full chain of provenance
    pub fn chain(&self) -> Vec<&ProvenanceInfo> {
        let mut chain = vec![self];
        let mut current = self;
        while let Some(parent) = &current.parent {
            chain.push(parent);
            current = parent;
        }
        chain
    }
}

/// Trait for types that carry provenance
pub trait HasProvenance {
    /// Get provenance information
    fn provenance(&self) -> Option<&ProvenanceInfo>;
    
    /// Set provenance information
    fn set_provenance(&mut self, provenance: ProvenanceInfo);
}

/// Stack trace entry
#[derive(Debug, Clone)]
pub struct StackFrame {
    /// Function name
    pub function_name: String,
    /// Arguments passed
    pub arguments: Vec<String>,
    /// Source location
    pub location: Option<SourceLocation>,
    /// Local variables at this frame
    pub locals: Vec<(String, String)>,
}

impl fmt::Display for StackFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "  at {}(", self.function_name)?;
        for (i, arg) in self.arguments.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", arg)?;
        }
        write!(f, ")")?;
        if let Some(loc) = &self.location {
            write!(f, " [{}]", loc)?;
        }
        Ok(())
    }
}

/// Execution trace for debugging
#[derive(Debug, Default)]
pub struct ExecutionTrace {
    /// Stack frames
    pub frames: Vec<StackFrame>,
    /// Maximum depth to track
    pub max_depth: usize,
}

impl ExecutionTrace {
    /// Create a new execution trace
    pub fn new(max_depth: usize) -> Self {
        Self {
            frames: Vec::new(),
            max_depth,
        }
    }

    /// Push a new frame
    pub fn push_frame(&mut self, frame: StackFrame) {
        if self.frames.len() < self.max_depth {
            self.frames.push(frame);
        }
    }

    /// Pop a frame
    pub fn pop_frame(&mut self) {
        self.frames.pop();
    }

    /// Get current depth
    pub fn depth(&self) -> usize {
        self.frames.len()
    }

    /// Format as a stack trace
    pub fn format_trace(&self) -> String {
        let mut trace = String::from("Stack trace (most recent call last):\n");
        for frame in self.frames.iter().rev() {
            trace.push_str(&format!("{}\n", frame));
        }
        trace
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_source_location() {
        let loc = SourceLocation::new("test.cl".to_string(), 10, 5);
        assert_eq!(loc.to_string(), "test.cl:10:5");

        let loc = SourceLocation::with_range("test.cl".to_string(), 10, 5, 10, 15);
        assert_eq!(loc.to_string(), "test.cl:10:5-10:15");
    }

    #[test]
    fn test_provenance_chain() {
        use std::num::NonZeroU32;
        let p1 = ProvenanceInfo::new(NodeId(NonZeroU32::new(1).unwrap()));
        let p2 = ProvenanceInfo::new(NodeId(NonZeroU32::new(2).unwrap())).with_parent(p1);
        let p3 = ProvenanceInfo::new(NodeId(NonZeroU32::new(3).unwrap())).with_parent(p2);

        let chain = p3.chain();
        assert_eq!(chain.len(), 3);
        assert_eq!(chain[0].node_id, NodeId(NonZeroU32::new(3).unwrap()));
        assert_eq!(chain[1].node_id, NodeId(NonZeroU32::new(2).unwrap()));
        assert_eq!(chain[2].node_id, NodeId(NonZeroU32::new(1).unwrap()));
    }

    #[test]
    fn test_stack_frame() {
        let frame = StackFrame {
            function_name: "factorial".to_string(),
            arguments: vec!["5".to_string()],
            location: Some(SourceLocation::new("test.cl".to_string(), 10, 5)),
            locals: vec![],
        };

        assert_eq!(frame.to_string(), "  at factorial(5) [test.cl:10:5]");
    }
}