//! ClaudeLang Virtual Machine
//!
//! High-performance register-based VM for executing ClaudeLang bytecode

pub mod bytecode;
pub mod compiler;
pub mod vm;
pub mod stdlib_bridge;
pub mod builder;
pub mod di;
pub mod debug;

pub use vm::VM;
pub use bytecode::{Bytecode, Opcode};
pub use compiler::Compiler;
pub use builder::{VMBuilder, VMConfig};
pub use di::{VMContainerBuilderExt, VMServiceProvider, ContainerVMProvider, VMFactory};
pub use debug::{VMDebugEvent, DebugConfig, StepMode};