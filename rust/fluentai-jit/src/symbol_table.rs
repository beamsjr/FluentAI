//! Symbol interning table for JIT compilation
//! 
//! This module provides a thread-safe symbol interning system that maps
//! string symbols to unique 32-bit IDs for efficient storage and comparison.

use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use once_cell::sync::Lazy;

/// Global symbol table instance
pub static SYMBOL_TABLE: Lazy<Arc<SymbolTable>> = Lazy::new(|| {
    Arc::new(SymbolTable::new())
});

/// Thread-safe symbol interning table
pub struct SymbolTable {
    /// Map from string to symbol ID
    string_to_id: RwLock<HashMap<String, u32>>,
    /// Map from ID to string
    id_to_string: RwLock<HashMap<u32, String>>,
    /// Next available ID
    next_id: RwLock<u32>,
}

impl SymbolTable {
    /// Create a new symbol table
    pub fn new() -> Self {
        // Start IDs at 1 to avoid 0 which might have special meaning
        Self {
            string_to_id: RwLock::new(HashMap::new()),
            id_to_string: RwLock::new(HashMap::new()),
            next_id: RwLock::new(1),
        }
    }
    
    /// Intern a symbol and return its ID
    pub fn intern(&self, symbol: &str) -> u32 {
        // Fast path: check if already interned
        {
            let string_to_id = self.string_to_id.read().unwrap();
            if let Some(&id) = string_to_id.get(symbol) {
                return id;
            }
        }
        
        // Slow path: intern the symbol
        let mut string_to_id = self.string_to_id.write().unwrap();
        let mut id_to_string = self.id_to_string.write().unwrap();
        let mut next_id = self.next_id.write().unwrap();
        
        // Double-check in case another thread interned while we waited
        if let Some(&id) = string_to_id.get(symbol) {
            return id;
        }
        
        // Intern the new symbol
        let id = *next_id;
        string_to_id.insert(symbol.to_string(), id);
        id_to_string.insert(id, symbol.to_string());
        *next_id += 1;
        
        id
    }
    
    /// Look up a symbol by ID
    pub fn lookup(&self, id: u32) -> Option<String> {
        let id_to_string = self.id_to_string.read().unwrap();
        id_to_string.get(&id).cloned()
    }
    
    /// Get the number of interned symbols
    pub fn len(&self) -> usize {
        let string_to_id = self.string_to_id.read().unwrap();
        string_to_id.len()
    }
    
    /// Check if the table is empty
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    
    /// Clear all interned symbols (useful for testing)
    #[cfg(test)]
    pub fn clear(&self) {
        let mut string_to_id = self.string_to_id.write().unwrap();
        let mut id_to_string = self.id_to_string.write().unwrap();
        let mut next_id = self.next_id.write().unwrap();
        
        string_to_id.clear();
        id_to_string.clear();
        *next_id = 1;
    }
}

/// Intern a symbol using the global table
pub fn intern(symbol: &str) -> u32 {
    SYMBOL_TABLE.intern(symbol)
}

/// Look up a symbol using the global table
pub fn lookup(id: u32) -> Option<String> {
    SYMBOL_TABLE.lookup(id)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_symbol_interning() {
        let table = SymbolTable::new();
        
        // Intern some symbols
        let id1 = table.intern("hello");
        let id2 = table.intern("world");
        let id3 = table.intern("hello"); // Same as id1
        
        assert_eq!(id1, id3);
        assert_ne!(id1, id2);
        
        // Look up symbols
        assert_eq!(table.lookup(id1), Some("hello".to_string()));
        assert_eq!(table.lookup(id2), Some("world".to_string()));
        assert_eq!(table.lookup(999), None);
    }
    
    #[test]
    fn test_global_interning() {
        // Clear for test isolation
        #[cfg(test)]
        SYMBOL_TABLE.clear();
        
        let id1 = intern("test_symbol");
        let id2 = intern("another_symbol");
        let id3 = intern("test_symbol");
        
        assert_eq!(id1, id3);
        assert_ne!(id1, id2);
        
        assert_eq!(lookup(id1), Some("test_symbol".to_string()));
        assert_eq!(lookup(id2), Some("another_symbol".to_string()));
    }
    
    #[test]
    fn test_thread_safety() {
        use std::thread;
        
        let table = Arc::new(SymbolTable::new());
        let mut handles = vec![];
        
        // Spawn multiple threads that intern symbols
        for i in 0..10 {
            let table_clone = Arc::clone(&table);
            let handle = thread::spawn(move || {
                let symbol = format!("thread_{}", i);
                let id1 = table_clone.intern(&symbol);
                let id2 = table_clone.intern(&symbol);
                assert_eq!(id1, id2);
                id1
            });
            handles.push(handle);
        }
        
        // Wait for all threads
        let ids: Vec<u32> = handles.into_iter()
            .map(|h| h.join().unwrap())
            .collect();
        
        // All IDs should be unique
        let mut unique_ids = ids.clone();
        unique_ids.sort();
        unique_ids.dedup();
        assert_eq!(ids.len(), unique_ids.len());
    }
}