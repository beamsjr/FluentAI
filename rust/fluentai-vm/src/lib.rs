//! FluentAi Virtual Machine
//!
//! High-performance register-based VM for executing FluentAi bytecode

pub mod bytecode;
pub mod compiler;
pub mod vm;
pub mod stdlib_bridge;
pub mod builder;
pub mod di;
pub mod debug;
pub mod safety;
pub mod security;
pub mod error;
pub mod gc;
pub mod unboxed;
pub mod typed_stack;
pub mod memory_pool;
pub mod concurrent;
pub mod fast_channel;
pub mod optimization;
pub mod simd;
pub mod concurrent_gc;

pub use vm::VM;
pub use bytecode::{Bytecode, Opcode};
pub use compiler::{Compiler, CompilerOptions};
pub use builder::{VMBuilder, VMConfig};
pub use di::{VMContainerBuilderExt, VMServiceProvider, ContainerVMProvider, VMFactory};
pub use debug::{VMDebugEvent, DebugConfig, StepMode};
pub use security::{SecurityManager, SecurityPolicy, Capability, TaintLevel};
pub use gc::{GarbageCollector, GcHandle, GcScope, GcConfig};
pub use unboxed::{UnboxedValue, BoxedValue};
pub use typed_stack::{TypedStack, TypeTag};
pub use memory_pool::{MemoryPool, PoolConfig, ObjectPool, SlabAllocator};
pub use concurrent::{LockFreeStack, LockFreeQueue, BoundedQueue, WorkStealingDeque};
pub use fast_channel::{FastChannel, ChannelMode, channel, Sender, Receiver};
pub use optimization::{InstructionFusion, InlineCache, ProfileInfo, FusedOpcode};
pub use simd::{SimdOps, SimdOp, PortableSimd};
pub use concurrent_gc::{ConcurrentGc, ConcurrentGcConfig};