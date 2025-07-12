//! FluentAI Examples
//! 
//! This crate contains example FluentAI programs demonstrating various language features.
//! The actual examples are in .flc files in the parent directory.

#![doc = include_str!("../README.md")]

#[cfg(test)]
mod tests {
    #[test]
    fn examples_readme_exists() {
        assert!(std::path::Path::new("README.md").exists() || 
                std::path::Path::new("../README.md").exists());
    }
}