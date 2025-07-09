//! Bytecode representation for FluentAi VM

use fluentai_core::value::Value;

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
    AddInt,   // Add two integers without boxing
    SubInt,   // Subtract two integers
    MulInt,   // Multiply two integers
    DivInt,   // Divide two integers
    AddFloat, // Add two floats
    SubFloat, // Subtract two floats
    MulFloat, // Multiply two floats
    DivFloat, // Divide two floats

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

    // Effect handlers
    MakeHandler,      // Create handler table from stack values
    InstallHandler,   // Install handler for dynamic scope
    UninstallHandler, // Uninstall handler, preserving result

    // Mutable cells
    MakeCell, // Create a cell with initial value
    CellGet,  // Get value from cell
    CellSet,  // Set value in cell

    // Tagged values (for constructor patterns)
    MakeTagged,     // Create tagged value: tag(string const), arity -> Tagged
    GetTag,         // Get tag from tagged value
    GetTaggedField, // Get field N from tagged value
    IsTagged,       // Check if value is tagged with specific tag

    // Module operations
    LoadModule,    // Load module by name (string const)
    ImportBinding, // Import specific binding from module
    ImportAll,     // Import all exports from module
    LoadQualified, // Load qualified variable (module.name)
    BeginModule,   // Mark beginning of module scope
    EndModule,     // Mark end of module scope
    ExportBinding, // Export a binding from current module

    // GC operations
    GcAlloc,   // Allocate value with GC
    GcDeref,   // Dereference GC handle
    GcSet,     // Set value in GC handle
    GcCollect, // Manually trigger collection

    // Tail call optimization
    TailCall,    // Tail call with frame reuse
    TailReturn,  // Return from tail-recursive function
    LoopStart,   // Mark start of tail-recursive loop
    LoopEnd,     // Mark end of tail-recursive loop
    UpdateLocal, // Update local variable (for loop parameter updates)

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

// Value type is now imported from fluentai_core::value::Value
// The core Value type includes all the variants needed by the VM

// Display implementation is provided by fluentai_core::value::Value

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
        assert!(offset < self.instructions.len(), "Invalid jump offset");
        assert!(target <= u32::MAX as usize, "Jump target overflow");
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
