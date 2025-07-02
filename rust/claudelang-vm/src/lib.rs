//! ClaudeLang Virtual Machine
//!
//! High-performance register-based VM for executing ClaudeLang bytecode

pub mod bytecode;
pub mod compiler;
pub mod vm;
pub mod stdlib_bridge;

pub use vm::VM;
pub use bytecode::{Bytecode, Opcode};
pub use compiler::Compiler;