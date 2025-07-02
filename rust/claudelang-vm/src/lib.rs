//! ClaudeLang Virtual Machine
//!
//! High-performance register-based VM for executing ClaudeLang bytecode

pub mod bytecode;
pub mod vm;

pub use vm::VM;
pub use bytecode::{Bytecode, Opcode};