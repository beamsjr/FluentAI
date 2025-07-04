//! Bytecode representation for FluentAi VM

use std::fmt;
use rustc_hash::FxHashMap;
use crate::safety::{PromiseId, ChannelId};
use crate::gc::GcHandle;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    // Stack manipulation
    Push,
    Pop,
    Dup,
    Swap,
    
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Neg,
    
    // Specialized unboxed arithmetic (faster paths)
    AddInt,          // Add two integers without boxing
    SubInt,          // Subtract two integers
    MulInt,          // Multiply two integers
    DivInt,          // Divide two integers
    AddFloat,        // Add two floats
    SubFloat,        // Subtract two floats  
    MulFloat,        // Multiply two floats
    DivFloat,        // Divide two floats
    
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    
    // Type-specialized comparison
    LtInt,
    LeInt,
    GtInt,
    GeInt,
    
    // Boolean
    And,
    Or,
    Not,
    
    // Control flow
    Jump,
    JumpIf,
    JumpIfNot,
    Call,
    Return,
    
    // Variables
    Load,
    Store,
    LoadGlobal,
    StoreGlobal,
    
    // Fast local variable access
    LoadLocal0,
    LoadLocal1,
    LoadLocal2,
    LoadLocal3,
    StoreLocal0,
    StoreLocal1,
    StoreLocal2,
    StoreLocal3,
    
    // Functions
    MakeFunc,
    MakeClosure,  // Like MakeFunc but captures N values from stack
    LoadCaptured, // Load value from captured environment
    MakeEnv,
    PopEnv,
    
    // Let binding cleanup
    // Removes N values from stack but preserves the top value
    PopN,
    
    // Lists
    MakeList,
    ListHead,
    ListTail,
    ListCons,
    ListLen,
    ListEmpty,
    
    // Strings
    StrLen,
    StrConcat,
    StrUpper,
    StrLower,
    
    // Specialized constants
    PushInt0,
    PushInt1,
    PushInt2,
    PushIntSmall, // for small integers that fit in arg
    PushTrue,
    PushFalse,
    PushNil,
    PushConst, // Push constant from constant pool
    
    // Effects
    Effect,
    EffectAsync,
    Await,
    Spawn,
    Channel,
    Send,
    Receive,
    
    // Mutable cells
    MakeCell,    // Create a cell with initial value
    CellGet,     // Get value from cell
    CellSet,     // Set value in cell
    
    // Tagged values (for constructor patterns)
    MakeTagged,      // Create tagged value: tag(string const), arity -> Tagged
    GetTag,          // Get tag from tagged value
    GetTaggedField,  // Get field N from tagged value
    IsTagged,        // Check if value is tagged with specific tag
    
    // Module operations
    LoadModule,      // Load module by name (string const)
    ImportBinding,   // Import specific binding from module
    LoadQualified,   // Load qualified variable (module.name)
    BeginModule,     // Mark beginning of module scope
    EndModule,       // Mark end of module scope
    ExportBinding,   // Export a binding from current module
    
    // GC operations
    GcAlloc,         // Allocate value with GC
    GcDeref,         // Dereference GC handle
    GcSet,           // Set value in GC handle
    GcCollect,       // Manually trigger collection
    
    // Tail call optimization
    TailCall,        // Tail call with frame reuse
    TailReturn,      // Return from tail-recursive function
    LoopStart,       // Mark start of tail-recursive loop
    LoopEnd,         // Mark end of tail-recursive loop
    UpdateLocal,     // Update local variable (for loop parameter updates)
    
    // Special
    Halt,
    Nop,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    pub opcode: Opcode,
    pub arg: u32,
}

impl Instruction {
    pub fn new(opcode: Opcode) -> Self {
        Self { opcode, arg: 0 }
    }
    
    pub fn with_arg(opcode: Opcode, arg: u32) -> Self {
        Self { opcode, arg }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Map(FxHashMap<String, Value>),
    Function {
        chunk_id: usize,
        env: Vec<Value>,
    },
    Promise(PromiseId), // Promise ID
    Channel(ChannelId), // Channel ID
    Cell(usize),     // Index into VM's cell storage
    Tagged {
        tag: String,
        values: Vec<Value>,
    },
    Module {
        name: String,
        exports: FxHashMap<String, Value>,
    },
    GcHandle(Box<GcHandle>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Map(map) => {
                write!(f, "{{")?;
                let mut first = true;
                for (k, v) in map.iter() {
                    if !first {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", k, v)?;
                    first = false;
                }
                write!(f, "}}")
            }
            Value::Function { .. } => write!(f, "<function>"),
            Value::Promise(id) => write!(f, "<{}>", id),
            Value::Channel(id) => write!(f, "<{}>", id),
            Value::Cell(idx) => write!(f, "<cell:{}>", idx),
            Value::Tagged { tag, values } => {
                write!(f, "{}", tag)?;
                if !values.is_empty() {
                    write!(f, "(")?;
                    for (i, val) in values.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", val)?;
                    }
                    write!(f, ")")?;
                }
                Ok(())
            }
            Value::Module { name, exports } => {
                write!(f, "<module {} with {} exports>", name, exports.len())
            }
            Value::GcHandle(_) => write!(f, "<gc-handle>"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Value>,
    pub name: Option<String>,
}

impl BytecodeChunk {
    pub fn new(name: Option<String>) -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            name,
        }
    }
    
    pub fn add_instruction(&mut self, instruction: Instruction) -> usize {
        self.instructions.push(instruction);
        self.instructions.len() - 1
    }
    
    pub fn add_constant(&mut self, value: Value) -> u32 {
        self.constants.push(value);
        (self.constants.len() - 1) as u32
    }
    
    pub fn patch_jump(&mut self, offset: usize, target: usize) {
        self.instructions[offset].arg = target as u32;
    }
}

#[derive(Debug, Clone)]
pub struct Bytecode {
    pub chunks: Vec<BytecodeChunk>,
    pub main_chunk: usize,
}

impl Bytecode {
    pub fn new() -> Self {
        Self {
            chunks: Vec::new(),
            main_chunk: 0,
        }
    }
    
    pub fn add_chunk(&mut self, chunk: BytecodeChunk) -> usize {
        self.chunks.push(chunk);
        self.chunks.len() - 1
    }
}