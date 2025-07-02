//! Bytecode representation for ClaudeLang VM

use std::fmt;
use std::collections::HashMap;

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
    
    // Type-specialized arithmetic (for performance)
    AddInt,
    SubInt,
    MulInt,
    DivInt,
    
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
    MakeEnv,
    PopEnv,
    
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

#[derive(Debug, Clone)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
    List(Vec<Value>),
    Map(HashMap<String, Value>),
    Function {
        chunk_id: usize,
        env: Vec<Value>,
    },
    Promise(String), // Promise ID
    Channel(String), // Channel ID
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
            Value::Promise(id) => write!(f, "<promise:{}>", id),
            Value::Channel(id) => write!(f, "<channel:{}>", id),
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