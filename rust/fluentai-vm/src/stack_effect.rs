//! Stack effect management for bytecode instructions
//!
//! This module provides a declarative way to describe how each opcode affects the stack,
//! making stack depth tracking more maintainable and less error-prone.

use fluentai_bytecode::{Instruction, Opcode};

/// Bit masks for MakeClosure instruction unpacking (must match compiler and VM)
const MAKECLOSURE_CAPTURE_COUNT_MASK: u32 = 0xFFFF;

/// Describes how an instruction affects the stack
#[derive(Debug, Clone, Copy)]
pub struct StackEffect {
    /// Number of values consumed from the stack
    pub pop: usize,
    /// Number of values pushed to the stack
    pub push: usize,
}

impl StackEffect {
    /// Create a new stack effect
    pub const fn new(pop: usize, push: usize) -> Self {
        Self { pop, push }
    }
    
    /// Net effect on stack depth (can be negative)
    pub fn net_effect(&self) -> isize {
        self.push as isize - self.pop as isize
    }
    
    /// Apply this effect to a stack depth, returning new depth
    pub fn apply(&self, depth: usize) -> usize {
        depth.saturating_sub(self.pop).saturating_add(self.push)
    }
}

/// Get the stack effect for an instruction
pub fn stack_effect(instruction: &Instruction) -> StackEffect {
    use Opcode::*;
    
    match instruction.opcode {
        // Stack manipulation
        Pop => StackEffect::new(1, 0),
        PopN => {
            // PopN pops N values beneath the top value, keeping the top
            // So it needs N+1 values initially, and leaves 1 value
            if instruction.arg > 0 {
                StackEffect::new(instruction.arg as usize + 1, 1)
            } else {
                StackEffect::new(0, 0) // No effect if arg is 0
            }
        }
        Dup => StackEffect::new(1, 2), // Duplicate top value
        Swap => StackEffect::new(2, 2), // Swap top two values
        
        // Push operations
        Push | PushConst | PushNil | PushTrue | PushFalse 
        | PushInt0 | PushInt1 | PushInt2 | PushIntSmall => StackEffect::new(0, 1),
        
        // Load operations
        Load | LoadGlobal | LoadCaptured | LoadLocal0 
        | LoadLocal1 | LoadLocal2 | LoadLocal3 => StackEffect::new(0, 1),
        
        // Store operations
        Store => StackEffect::new(1, 0),
        StoreGlobal => StackEffect::new(1, 0),
        UpdateLocal => StackEffect::new(1, 0),
        
        // Binary operations (consume 2, produce 1)
        Add | Sub | Mul | Div | Mod 
        | Eq | Ne | Lt | Le | Gt | Ge 
        | And | Or | StrConcat | ListCons => StackEffect::new(2, 1),
        
        // Unary operations (consume 1, produce 1)
        Not | Neg | ListHead | ListTail | ListEmpty | ListLen => StackEffect::new(1, 1),
        
        // Function operations
        Call => {
            // Call pops: function + args, pushes: result
            // Net effect: -(arg_count + 1) + 1 = -arg_count
            let arg_count = instruction.arg as usize;
            StackEffect::new(arg_count + 1, 1)
        }
        TailCall => {
            // Similar to Call
            let arg_count = instruction.arg as usize;
            StackEffect::new(arg_count + 1, 1)
        }
        Return => StackEffect::new(1, 0), // Consumes return value
        TailReturn => StackEffect::new(1, 0),
        
        // Function creation
        MakeFunc => StackEffect::new(0, 1),
        MakeClosure => {
            // MakeClosure consumes N captured values and produces 1 function
            let capture_count = (instruction.arg & MAKECLOSURE_CAPTURE_COUNT_MASK) as usize;
            StackEffect::new(capture_count, 1)
        }
        
        // List operations
        MakeList => {
            let count = instruction.arg as usize;
            StackEffect::new(count, 1)
        }
        
        // Tagged values
        MakeTagged => {
            let count = instruction.arg as usize;
            StackEffect::new(count + 1, 1) // +1 for the tag
        }
        IsTagged => StackEffect::new(1, 2), // Produces value and boolean
        
        // Cell operations
        MakeCell => StackEffect::new(1, 1), // Consumes value, produces cell
        CellGet => StackEffect::new(1, 1), // Consumes cell, produces value
        CellSet => StackEffect::new(2, 1), // Consumes cell and value, produces nil
        
        // GC operations
        GcAlloc => StackEffect::new(1, 1), // Consumes value, produces handle
        GcDeref => StackEffect::new(1, 1), // Consumes handle, produces value
        GcSet => StackEffect::new(2, 1), // Consumes handle and value, produces nil
        GcCollect => StackEffect::new(0, 1), // No args, produces nil
        
        // Control flow
        Jump | JumpIf => StackEffect::new(0, 0), // No stack effect
        JumpIfNot => StackEffect::new(1, 0), // Consumes condition
        
        // Channel operations
        Channel => StackEffect::new(0, 1), // Creates channel
        ChannelWithCapacity => StackEffect::new(1, 1), // Consumes capacity, produces channel
        Send => StackEffect::new(2, 1), // Consumes channel and value, produces nil
        Receive => StackEffect::new(1, 1), // Consumes channel, produces value
        TrySend => StackEffect::new(2, 1), // Consumes channel and value, produces bool
        TryReceive => StackEffect::new(1, 1), // Consumes channel, produces Option
        
        // Select operation
        Select => {
            // Complex - depends on branch count
            // For now, conservative estimate
            StackEffect::new(0, 1)
        }
        
        // Actor operations
        ActorSend => StackEffect::new(2, 1), // Consumes actor and message, produces nil
        
        // Handler operations
        MakeHandler => {
            // Consumes 3 values per handler (effect type, op filter, handler fn)
            let handler_count = instruction.arg as usize;
            StackEffect::new(3 * handler_count, 1)
        }
        InstallHandler => StackEffect::new(1, 0), // Consumes handler
        UninstallHandler => StackEffect::new(0, 0), // No stack effect
        
        // Error handling
        PushHandler => StackEffect::new(0, 0), // No immediate stack effect
        PopHandler => StackEffect::new(0, 0), // No immediate stack effect
        Throw => StackEffect::new(1, 0), // Consumes error (but unwinds stack)
        
        // Loop operations
        LoopStart | LoopEnd => StackEffect::new(0, 0),
        
        // Other
        Halt => StackEffect::new(0, 0),
        Become => StackEffect::new(1, 1), // Consumes new state, produces nil
        
        // Effect operations
        Effect => {
            // Consumes effect type, operation, and N args; produces result
            let arg_count = instruction.arg as usize;
            StackEffect::new(2 + arg_count, 1)
        }
        
        // Additional operations
        MakeFuture => StackEffect::new(1, 1), // Consumes function, produces future
        
        // Qualified name loading
        LoadQualified => StackEffect::new(0, 1), // Pushes value
        
        // Specialized arithmetic operations (same as their generic counterparts)
        AddInt | SubInt | MulInt | DivInt 
        | AddFloat | SubFloat | MulFloat | DivFloat => StackEffect::new(2, 1),
        
        // Specialized comparison operations
        LtInt | LeInt | GtInt | GeInt => StackEffect::new(2, 1),
        
        // Fast local store operations
        StoreLocal0 | StoreLocal1 | StoreLocal2 | StoreLocal3 => StackEffect::new(1, 0),
        
        // String operations
        StrLen => StackEffect::new(1, 1), // Consumes string, produces int
        StrUpper | StrLower => StackEffect::new(1, 1), // Consumes string, produces string
        
        // Tagged value operations
        GetTag => StackEffect::new(1, 1), // Consumes tagged value, produces tag
        GetTaggedField => StackEffect::new(1, 1), // Consumes tagged value, produces field
        
        // Module operations
        LoadModule => StackEffect::new(0, 1), // Pushes module
        ImportBinding => StackEffect::new(1, 0), // Consumes module name
        ImportAll => StackEffect::new(1, 0), // Consumes module
        BeginModule | EndModule => StackEffect::new(0, 0), // No stack effect
        ExportBinding => StackEffect::new(0, 0), // No stack effect
        
        // Environment operations
        MakeEnv | PopEnv => StackEffect::new(0, 0), // No direct stack effect
        
        // Promise operations
        PromiseNew => StackEffect::new(0, 1), // Creates new promise
        PromiseAll => StackEffect::new(1, 1), // Consumes list, produces promise
        PromiseRace => StackEffect::new(1, 1), // Consumes list, produces promise
        WithTimeout => StackEffect::new(2, 1), // Consumes promise and timeout, produces promise
        
        // Actor model operations
        CreateActor => StackEffect::new(2, 1), // Consumes state and handler, produces actor
        ActorReceive => StackEffect::new(0, 1), // Produces current message from context
        
        // Async operations
        Spawn => StackEffect::new(1, 1), // Consumes function, produces promise/future
        Await => StackEffect::new(1, 1), // Consumes promise/future, produces result
        
        // Effect operations
        EffectAsync => StackEffect::new(2, 1), // Like Effect but async
        
        // Error handling
        Try | Catch | Finally | EndFinally => StackEffect::new(0, 0), // Control flow
        PushFinally => StackEffect::new(0, 0), // No immediate stack effect
        
        // Special
        Nop => StackEffect::new(0, 0), // No operation
    }
}

// Stack depth guard removed - stack depth management is now centralized in emit()