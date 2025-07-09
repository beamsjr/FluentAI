//! FluentAI SDK - High-level API for embedding FluentAI
//!
//! This crate provides a simplified, high-level API for embedding
//! FluentAI into applications. It wraps the runtime crate with
//! additional conveniences and utilities.

pub mod builder;
pub mod error;
pub mod script;
pub mod session;
pub mod stdlib;
pub mod value;

pub use builder::FluentAIBuilder;
pub use error::{Error, Result};
pub use script::Script;
pub use session::{Session, SessionOptions};
pub use value::{FromValue, IntoValue};

// Re-export important types
pub use fluentai_core::value::Value;
pub use fluentai_core_lib::{HostFunction, RuntimeConfig};

/// SDK version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize the FluentAI SDK
///
/// This should be called once at application startup to initialize
/// global resources and register standard library functions.
pub fn init() -> Result<()> {
    // Any global initialization
    Ok(())
}

/// Create a new FluentAI session with default settings
pub fn new_session() -> Result<Session> {
    Session::new(SessionOptions::default())
}

/// Create a new FluentAI session with custom options
pub fn new_session_with_options(options: SessionOptions) -> Result<Session> {
    Session::new(options)
}

/// Evaluate a FluentAI expression
///
/// This is a convenience function for simple evaluations.
/// For more complex scenarios, use a Session.
pub fn eval(code: &str) -> Result<Value> {
    let mut session = new_session()?;
    session.eval(code)
}

/// Load and execute a FluentAI script file
pub fn run_file(path: impl AsRef<std::path::Path>) -> Result<Value> {
    let mut session = new_session()?;
    session.run_file(path)
}

/// Prelude module for common imports
pub mod prelude {
    pub use crate::{
        eval, run_file, Error, FromValue, IntoValue, Result, Script, Session, SessionOptions, Value,
    };
}
