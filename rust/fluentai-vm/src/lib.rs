//! FluentAi Virtual Machine
//!
//! High-performance register-based VM for executing FluentAi bytecode

#![warn(missing_docs)]

#[cfg(feature = "std")]
pub mod builder;
pub mod compiler;
pub mod compiler_builtins;
pub mod concurrent;
pub mod concurrent_gc;
pub mod debug;
#[cfg(feature = "std")]
pub mod di;
pub mod error;
pub mod fast_channel;
pub mod free_var_analysis;
pub mod gc;
#[cfg(feature = "jit")]
pub mod jit_integration;
pub mod memory_pool;
pub mod module_loader;
pub mod module_registry;
pub mod opcode_handlers;
pub mod optimization;
#[cfg(feature = "std")]
pub mod promise_manager;
pub mod safety;
pub mod security;
pub mod simd;
pub mod stack_effect;
#[cfg(feature = "std")]
pub mod stdlib_bridge;
pub mod typed_stack;
pub mod unboxed;
pub mod usage_tracker;
pub mod vm;
#[cfg(feature = "std")]
pub mod vm_builder;
#[cfg(feature = "std")]
pub mod async_vm;
pub mod cow_globals;
pub mod continuation;
#[cfg(feature = "std")]
pub mod promise_executor;

#[cfg(feature = "std")]
pub use builder::{VMBuilder as VMBuilderLegacy, VMConfig};
pub use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
pub use compiler::{Compiler, CompilerOptions};
pub use concurrent::{BoundedQueue, LockFreeQueue, LockFreeStack, WorkStealingDeque};
pub use concurrent_gc::{ConcurrentGc, ConcurrentGcConfig};
pub use debug::{DebugConfig, StepMode, VMDebugEvent};
#[cfg(feature = "std")]
pub use di::{ContainerVMProvider, VMContainerBuilderExt, VMFactory, VMServiceProvider};
pub use error::VMError;
pub use fast_channel::{channel, ChannelMode, FastChannel, Receiver, Sender};
pub use fluentai_core::value::Value;
pub use fluentai_optimizer::OptimizationLevel;
pub use free_var_analysis::{FreeVarAnalyzer, VarInfo};
pub use gc::{GarbageCollector, GcConfig, GcHandle, GcScope};
pub use memory_pool::{MemoryPool, ObjectPool, PoolConfig, SlabAllocator};
pub use module_loader::{ModuleLoader, ModuleLoaderConfig};
pub use module_registry::{ModuleInfo, ModuleRegistry};
pub use optimization::{CachedValue, FusedOpcode, InlineCache, InstructionFusion, ProfileInfo};
pub use security::{Capability, SecurityManager, SecurityPolicy, TaintLevel};
pub use simd::{PortableSimd, SimdOp, SimdOps};
pub use typed_stack::{TypeTag, TypedStack};
pub use unboxed::{BoxedValue, UnboxedValue};
pub use usage_tracker::{UsageStats, UsageTracker};
pub use vm::{CallFrame, VM, VMState};
#[cfg(feature = "std")]
pub use vm_builder::VMBuilder;

// Test modules
#[cfg(all(test, feature = "std"))]
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
#[cfg(all(test, feature = "std"))]
mod stdlib_bridge_tests;
#[cfg(test)]
mod unboxed_tests;
#[cfg(test)]
mod vm_integration_tests;
#[cfg(test)]
mod vm_simple_coverage_tests;
#[cfg(test)]
mod vm_tests;
