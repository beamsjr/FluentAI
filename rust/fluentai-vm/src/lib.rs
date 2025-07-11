//! FluentAi Virtual Machine
//!
//! High-performance register-based VM for executing FluentAi bytecode

pub mod builder;
pub mod bytecode;
pub mod compiler;
pub mod compiler_builtins;
pub mod concurrent;
pub mod concurrent_gc;
pub mod debug;
pub mod di;
pub mod error;
pub mod fast_channel;
pub mod free_var_analysis;
pub mod gc;
pub mod memory_pool;
pub mod opcode_handlers;
pub mod optimization;
pub mod safety;
pub mod security;
pub mod simd;
pub mod stack_effect;
pub mod stdlib_bridge;
pub mod typed_stack;
pub mod unboxed;
pub mod usage_tracker;
pub mod vm;
pub mod vm_builder;
pub mod async_vm;
pub mod cow_globals;

pub use builder::{VMBuilder as VMBuilderLegacy, VMConfig};
pub use bytecode::{Bytecode, Opcode};
pub use compiler::{Compiler, CompilerOptions};
pub use concurrent::{BoundedQueue, LockFreeQueue, LockFreeStack, WorkStealingDeque};
pub use concurrent_gc::{ConcurrentGc, ConcurrentGcConfig};
pub use debug::{DebugConfig, StepMode, VMDebugEvent};
pub use di::{ContainerVMProvider, VMContainerBuilderExt, VMFactory, VMServiceProvider};
pub use error::VMError;
pub use fast_channel::{channel, ChannelMode, FastChannel, Receiver, Sender};
pub use fluentai_core::value::Value;
pub use fluentai_optimizer::OptimizationLevel;
pub use free_var_analysis::{FreeVarAnalyzer, VarInfo};
pub use gc::{GarbageCollector, GcConfig, GcHandle, GcScope};
pub use memory_pool::{MemoryPool, ObjectPool, PoolConfig, SlabAllocator};
pub use optimization::{CachedValue, FusedOpcode, InlineCache, InstructionFusion, ProfileInfo};
pub use security::{Capability, SecurityManager, SecurityPolicy, TaintLevel};
pub use simd::{PortableSimd, SimdOp, SimdOps};
pub use typed_stack::{TypeTag, TypedStack};
pub use unboxed::{BoxedValue, UnboxedValue};
pub use usage_tracker::{UsageStats, UsageTracker};
pub use vm::{CallFrame, VM, VMState};
pub use vm_builder::VMBuilder;

// Test modules
#[cfg(test)]
mod builder_tests;
#[cfg(test)]
mod bytecode_tests;
#[cfg(test)]
mod debug_tests;
#[cfg(test)]
mod error_tests;
#[cfg(test)]
mod gc_tests;
#[cfg(test)]
mod memory_pool_tests;
#[cfg(test)]
mod security_tests;
#[cfg(test)]
mod stdlib_bridge_tests;
#[cfg(test)]
mod unboxed_tests;
#[cfg(test)]
mod vm_integration_tests;
#[cfg(test)]
mod vm_simple_coverage_tests;
#[cfg(test)]
mod vm_tests;
