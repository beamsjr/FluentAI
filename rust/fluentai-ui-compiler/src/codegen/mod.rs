//! Code generation modules for different output formats

pub mod react;
pub mod vanilla_js;
pub mod vue;
pub mod web_component;

// Common utilities for code generation
pub mod utils;

pub use utils::{indent, JsBuilder};
