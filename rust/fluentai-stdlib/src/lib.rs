//! FluentAi Standard Library
//!
//! This crate provides the standard library functions for FluentAi,
//! implementing all the built-in functions available to FluentAi programs.

#![warn(missing_docs)]

pub mod chars;
pub mod collections;
pub mod core;
pub mod datetime;
pub mod functional;
pub mod io;
pub mod io_effects;
pub mod logger;
pub mod math;
pub mod registry;
pub mod strings;
pub mod strings_extended;
pub mod test_support;
pub mod value;
pub mod vm_bridge;
pub mod method_adapters;

// Re-export the registry for convenience
pub use registry::{StdlibFunction, StdlibRegistry};

use crate::value::Value;
use anyhow::Result;

/// Type alias for standard library function signatures
pub type StdlibFn = fn(&[Value]) -> Result<Value>;

/// Initialize the standard library and return a populated registry
pub fn init_stdlib() -> StdlibRegistry {
    let mut registry = StdlibRegistry::new();

    // Register all modules
    core::register(&mut registry);
    strings::register(&mut registry);
    strings_extended::register(&mut registry);
    chars::register(&mut registry);
    collections::register(&mut registry);
    math::register(&mut registry);
    io::register(&mut registry);
    functional::register(&mut registry);
    datetime::register(&mut registry);
    logger::register(&mut registry);
    test_support::register(&mut registry);
    method_adapters::register(&mut registry);

    registry
}
