//! Source mapping for bytecode instructions
//! 
//! This module provides source location tracking for bytecode instructions,
//! enabling better error messages and debugging capabilities.

use fluentai_core::ast::NodeId;
use std::collections::HashMap;

/// Represents a location in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceLocation {
    /// Start byte offset in the source
    pub start: usize,
    /// End byte offset in the source
    pub end: usize,
    /// Optional line number (1-based)
    pub line: Option<u32>,
    /// Optional column number (1-based)
    pub column: Option<u32>,
}

impl SourceLocation {
    /// Create a new source location from byte offsets
    pub fn new(start: usize, end: usize) -> Self {
        Self {
            start,
            end,
            line: None,
            column: None,
        }
    }
    
    /// Create a new source location with line and column information
    pub fn with_line_col(start: usize, end: usize, line: u32, column: u32) -> Self {
        Self {
            start,
            end,
            line: Some(line),
            column: Some(column),
        }
    }
}

/// Maps bytecode instructions to their source locations
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    /// Maps instruction offset to source location
    instruction_map: HashMap<usize, SourceLocation>,
    /// Maps instruction offset to the AST node that generated it
    node_map: HashMap<usize, NodeId>,
    /// Optional source file name
    pub filename: Option<String>,
    /// Optional source text for error reporting
    pub source_text: Option<String>,
}

impl SourceMap {
    /// Create a new empty source map
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Create a source map with filename
    pub fn with_filename(filename: String) -> Self {
        Self {
            filename: Some(filename),
            ..Default::default()
        }
    }
    
    /// Add a mapping from instruction offset to source location
    pub fn add_instruction_location(&mut self, offset: usize, location: SourceLocation) {
        self.instruction_map.insert(offset, location);
    }
    
    /// Add a mapping from instruction offset to AST node
    pub fn add_instruction_node(&mut self, offset: usize, node_id: NodeId) {
        self.node_map.insert(offset, node_id);
    }
    
    /// Get the source location for an instruction
    pub fn get_location(&self, offset: usize) -> Option<&SourceLocation> {
        self.instruction_map.get(&offset)
    }
    
    /// Get the AST node for an instruction
    pub fn get_node(&self, offset: usize) -> Option<NodeId> {
        self.node_map.get(&offset).copied()
    }
    
    /// Format an error message with source location
    pub fn format_error(&self, offset: usize, message: &str) -> String {
        if let Some(location) = self.get_location(offset) {
            if let (Some(filename), Some(line), Some(column)) = 
                (&self.filename, location.line, location.column) {
                format!("{}:{}:{}: {}", filename, line, column, message)
            } else if let Some(filename) = &self.filename {
                format!("{}:{}-{}: {}", filename, location.start, location.end, message)
            } else {
                format!("at {}-{}: {}", location.start, location.end, message)
            }
        } else {
            message.to_string()
        }
    }
    
    /// Get a snippet of source code around the given location
    pub fn get_source_snippet(&self, location: &SourceLocation) -> Option<String> {
        let source = self.source_text.as_ref()?;
        
        // Ensure bounds are valid
        if location.start >= source.len() || location.end > source.len() {
            return None;
        }
        
        // Find line boundaries
        let line_start = source[..location.start]
            .rfind('\n')
            .map(|i| i + 1)
            .unwrap_or(0);
        
        let line_end = source[location.end..]
            .find('\n')
            .map(|i| location.end + i)
            .unwrap_or(source.len());
        
        Some(source[line_start..line_end].to_string())
    }
}

/// Source map for an entire bytecode module
#[derive(Debug, Clone, Default)]
pub struct ModuleSourceMap {
    /// Source maps for each chunk
    pub chunk_maps: Vec<SourceMap>,
}

impl ModuleSourceMap {
    /// Create a new module source map
    pub fn new() -> Self {
        Self::default()
    }
    
    /// Add a source map for a chunk
    pub fn add_chunk_map(&mut self, map: SourceMap) -> usize {
        self.chunk_maps.push(map);
        self.chunk_maps.len() - 1
    }
    
    /// Get the source map for a chunk
    pub fn get_chunk_map(&self, chunk_id: usize) -> Option<&SourceMap> {
        self.chunk_maps.get(chunk_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_source_location() {
        let loc = SourceLocation::new(10, 20);
        assert_eq!(loc.start, 10);
        assert_eq!(loc.end, 20);
        assert_eq!(loc.line, None);
        assert_eq!(loc.column, None);
        
        let loc_with_line = SourceLocation::with_line_col(30, 40, 5, 10);
        assert_eq!(loc_with_line.line, Some(5));
        assert_eq!(loc_with_line.column, Some(10));
    }
    
    #[test]
    fn test_source_map() {
        let mut map = SourceMap::with_filename("test.flc".to_string());
        map.source_text = Some("let x = 42\nlet y = x + 1".to_string());
        
        let loc1 = SourceLocation::with_line_col(4, 5, 1, 5);
        let loc2 = SourceLocation::with_line_col(16, 17, 2, 5);
        
        map.add_instruction_location(0, loc1);
        map.add_instruction_location(10, loc2);
        
        assert_eq!(map.get_location(0), Some(&loc1));
        assert_eq!(map.get_location(10), Some(&loc2));
        assert_eq!(map.get_location(5), None);
        
        let error_msg = map.format_error(0, "undefined variable");
        assert_eq!(error_msg, "test.flc:1:5: undefined variable");
        
        let snippet = map.get_source_snippet(&loc1).unwrap();
        assert_eq!(snippet, "let x = 42");
    }
}