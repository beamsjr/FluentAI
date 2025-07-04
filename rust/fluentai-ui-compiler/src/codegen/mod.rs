//! Code generation modules for different output formats

pub mod vanilla_js;
pub mod react;
pub mod vue;
pub mod web_component;

// Common utilities for code generation
pub mod utils;

pub use utils::{JsBuilder, indent};