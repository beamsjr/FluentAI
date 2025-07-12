//! Modular opcode handlers for the FluentAI VM
//! 
//! This module organizes opcode execution into logical groups,
//! making the VM easier to maintain and test.

use fluentai_bytecode::Instruction;
use crate::error::VMResult;
use crate::vm::{VM, VMState};

pub mod arithmetic;
pub mod stack;
pub mod control_flow;
pub mod memory;
pub mod collections;
pub mod concurrent;
pub mod effects;
pub mod logical;
pub mod comparison;
pub mod string;

/// Trait for opcode handlers
pub trait OpcodeHandler {
    /// Execute the given instruction
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState>;
}

/// Re-export handler implementations
pub use arithmetic::ArithmeticHandler;
pub use stack::StackHandler;
pub use control_flow::ControlFlowHandler;
pub use memory::MemoryHandler;
pub use collections::CollectionsHandler;
pub use concurrent::ConcurrentHandler;
pub use effects::EffectsHandler;
pub use logical::LogicalHandler;
pub use comparison::ComparisonHandler;
pub use string::StringHandler;
