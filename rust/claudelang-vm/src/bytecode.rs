//! Bytecode representation for the VM

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(u8)]
pub enum Opcode {
    // Stack operations
    Push = 0x00,
    Pop = 0x01,
    Dup = 0x02,
    Swap = 0x03,
    
    // Arithmetic
    Add = 0x10,
    Sub = 0x11,
    Mul = 0x12,
    Div = 0x13,
    Mod = 0x14,
    Neg = 0x15,
    
    // Comparison
    Eq = 0x20,
    Ne = 0x21,
    Lt = 0x22,
    Le = 0x23,
    Gt = 0x24,
    Ge = 0x25,
    
    // Control flow
    Jump = 0x30,
    JumpIf = 0x31,
    JumpIfNot = 0x32,
    Call = 0x33,
    Return = 0x34,
    
    // Variables
    LoadLocal = 0x40,
    StoreLocal = 0x41,
    LoadGlobal = 0x42,
    StoreGlobal = 0x43,
    
    // Other
    Halt = 0xFF,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Instruction {
    pub opcode: Opcode,
    pub operand: Option<u32>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bytecode {
    pub instructions: Vec<Instruction>,
    pub constants: Vec<Constant>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constant {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    Nil,
}