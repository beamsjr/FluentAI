//! Command history management for the REPL

use std::path::PathBuf;
use crate::error::{ReplError, ReplResult};

/// History manager for the REPL
pub struct HistoryManager {
    /// Path to history file
    history_file: PathBuf,
    /// Maximum number of entries to keep
    max_entries: usize,
    /// Whether history is enabled
    enabled: bool,
}

impl HistoryManager {
    /// Create a new history manager
    pub fn new() -> ReplResult<Self> {
        let history_file = Self::default_history_file()?;
        Ok(Self {
            history_file,
            max_entries: 10000,
            enabled: true,
        })
    }

    /// Get the default history file path
    fn default_history_file() -> ReplResult<PathBuf> {
        let home = dirs::home_dir()
            .ok_or_else(|| ReplError::History("Could not find home directory".to_string()))?;
        let fluentai_dir = home.join(".claudelang");
        
        // Create directory if it doesn't exist
        if !fluentai_dir.exists() {
            std::fs::create_dir_all(&fluentai_dir)?;
        }
        
        Ok(fluentai_dir.join("repl_history"))
    }

    /// Get the history file path
    pub fn history_file(&self) -> &PathBuf {
        &self.history_file
    }

    /// Set a custom history file path
    pub fn set_history_file(&mut self, path: PathBuf) {
        self.history_file = path;
    }

    /// Get maximum number of entries
    pub fn max_entries(&self) -> usize {
        self.max_entries
    }

    /// Set maximum number of entries
    pub fn set_max_entries(&mut self, max: usize) {
        self.max_entries = max;
    }

    /// Check if history is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Enable or disable history
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Load history from file
    pub fn load(&self) -> ReplResult<Vec<String>> {
        if !self.enabled || !self.history_file.exists() {
            return Ok(Vec::new());
        }

        let content = std::fs::read_to_string(&self.history_file)?;
        let entries: Vec<String> = content
            .lines()
            .map(|s| s.to_string())
            .collect();
        
        Ok(entries)
    }

    /// Save history to file
    pub fn save(&self, entries: &[String]) -> ReplResult<()> {
        if !self.enabled {
            return Ok(());
        }

        // Take only the last max_entries
        let start = entries.len().saturating_sub(self.max_entries);
        let content = entries[start..].join("\n");
        
        std::fs::write(&self.history_file, content)?;
        Ok(())
    }

    /// Append a single entry to history file
    pub fn append(&self, entry: &str) -> ReplResult<()> {
        if !self.enabled {
            return Ok(());
        }

        use std::io::Write;
        let mut file = std::fs::OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.history_file)?;
        
        writeln!(file, "{}", entry)?;
        Ok(())
    }

    /// Clear history
    pub fn clear(&self) -> ReplResult<()> {
        if self.history_file.exists() {
            std::fs::remove_file(&self.history_file)?;
        }
        Ok(())
    }
}

impl Default for HistoryManager {
    fn default() -> Self {
        Self::new().expect("Failed to create default history manager")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_history_manager() {
        let temp_dir = TempDir::new().unwrap();
        let history_file = temp_dir.path().join("test_history");
        
        let mut manager = HistoryManager::new().unwrap();
        manager.set_history_file(history_file.clone());
        
        // Test append
        manager.append("command 1").unwrap();
        manager.append("command 2").unwrap();
        
        // Test load
        let entries = manager.load().unwrap();
        assert_eq!(entries.len(), 2);
        assert_eq!(entries[0], "command 1");
        assert_eq!(entries[1], "command 2");
        
        // Test save
        let new_entries = vec![
            "command 3".to_string(),
            "command 4".to_string(),
            "command 5".to_string(),
        ];
        manager.save(&new_entries).unwrap();
        
        let loaded = manager.load().unwrap();
        assert_eq!(loaded.len(), 3);
        assert_eq!(loaded[2], "command 5");
        
        // Test clear
        manager.clear().unwrap();
        assert!(!history_file.exists());
    }
}