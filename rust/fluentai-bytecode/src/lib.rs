//! Bytecode representation for FluentAi
//! 
//! This crate defines the bytecode format used by both the VM and JIT compiler.

#![warn(missing_docs)]

pub mod source_map;

use fluentai_core::value::Value;

/// Bytecode operation codes for the FluentAi VM
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opcode {
    // Stack manipulation
    /// Push a value onto the stack
    Push,
    /// Pop a value from the stack
    Pop,
    /// Duplicate the top stack value
    Dup,
    /// Swap the top two stack values
    Swap,

    // Arithmetic
    /// Add two values
    Add,
    /// Subtract two values
    Sub,
    /// Multiply two values
    Mul,
    /// Divide two values
    Div,
    /// Modulo operation
    Mod,
    /// Negate a value
    Neg,

    // Specialized unboxed arithmetic (faster paths)
    /// Add two integers without boxing
    AddInt,
    /// Subtract two integers without boxing
    SubInt,
    /// Multiply two integers without boxing
    MulInt,
    /// Divide two integers without boxing
    DivInt,
    /// Add two floats without boxing
    AddFloat,
    /// Subtract two floats without boxing
    SubFloat,
    /// Multiply two floats without boxing
    MulFloat,
    /// Divide two floats without boxing
    DivFloat,

    // Comparison
    /// Equal comparison
    Eq,
    /// Not equal comparison
    Ne,
    /// Less than comparison
    Lt,
    /// Less than or equal comparison
    Le,
    /// Greater than comparison
    Gt,
    /// Greater than or equal comparison
    Ge,

    // Type-specialized comparison
    /// Less than for integers
    LtInt,
    /// Less than or equal for integers
    LeInt,
    /// Greater than for integers
    GtInt,
    /// Greater than or equal for integers
    GeInt,

    // Boolean
    /// Logical AND operation
    And,
    /// Logical OR operation
    Or,
    /// Logical NOT operation
    Not,

    // Control flow
    /// Unconditional jump
    Jump,
    /// Jump if top of stack is truthy
    JumpIf,
    /// Jump if top of stack is falsy
    JumpIfNot,
    /// Call a function
    Call,
    /// Return from function
    Return,

    // Variables
    /// Load a variable
    Load,
    /// Store a variable
    Store,
    /// Load a local variable
    LoadLocal,
    /// Store a local variable
    StoreLocal,
    /// Load a global variable
    LoadGlobal,
    /// Store a global variable
    StoreGlobal,
    /// Define a new global variable
    DefineGlobal,

    // Fast local variable access
    /// Load local variable at index 0
    LoadLocal0,
    /// Load local variable at index 1
    LoadLocal1,
    /// Load local variable at index 2
    LoadLocal2,
    /// Load local variable at index 3
    LoadLocal3,
    /// Store to local variable at index 0
    StoreLocal0,
    /// Store to local variable at index 1
    StoreLocal1,
    /// Store to local variable at index 2
    StoreLocal2,
    /// Store to local variable at index 3
    StoreLocal3,

    // Upvalue operations
    /// Load a captured variable
    LoadUpvalue,
    /// Store to a captured variable
    StoreUpvalue,

    // Functions
    /// Create a function
    MakeFunc,
    /// Create a closure capturing N values from stack
    MakeClosure,
    /// Load value from captured environment
    LoadCaptured,
    /// Create a future from a function
    MakeFuture,
    /// Create a new environment
    MakeEnv,
    /// Pop the current environment
    PopEnv,

    // Let binding cleanup
    /// Removes N values from stack but preserves the top value
    PopN,

    // Lists
    /// Create a list from stack values
    MakeList,
    /// Get element from a list
    ListGet,
    /// Set element in a list
    ListSet,
    /// Get head of a list
    ListHead,
    /// Get tail of a list
    ListTail,
    /// Cons an element to a list
    ListCons,
    /// Get length of a list
    ListLen,
    /// Check if list is empty
    ListEmpty,

    // Maps
    /// Create a map from stack values
    MakeMap,
    /// Get value from a map
    MapGet,
    /// Set value in a map
    MapSet,

    // Strings
    /// Get string length
    StrLen,
    /// Concatenate strings
    StrConcat,
    /// Convert string to uppercase
    StrUpper,
    /// Convert string to lowercase
    StrLower,

    // Specialized constants
    /// Push integer 0
    PushInt0,
    /// Push integer 1
    PushInt1,
    /// Push integer 2
    PushInt2,
    /// Push small integer from instruction argument
    PushIntSmall,
    /// Push boolean true
    PushTrue,
    /// Push boolean false
    PushFalse,
    /// Push nil value
    PushNil,
    /// Push constant from constant pool
    PushConst,

    // Effects
    /// Execute an effect
    Effect,
    /// Execute an async effect
    EffectAsync,
    /// Perform an effect operation
    Perform,
    /// Resume from an effect
    Resume,
    /// Await an async result
    Await,
    /// Spawn a concurrent task
    Spawn,
    /// Create a channel
    Channel,
    /// Create a channel with capacity
    ChannelWithCapacity,
    /// Make a channel object
    MakeChannel,
    /// Send to a channel
    Send,
    /// Receive from a channel
    Receive,
    /// Try to send without blocking
    TrySend,
    /// Try to receive without blocking
    TryReceive,
    /// Select from multiple channels
    Select,
    
    // Actor model
    /// Create a new actor
    CreateActor,
    /// Make an actor from state and handler
    MakeActor,
    /// Send message to an actor
    ActorSend,
    /// Receive messages in an actor
    ActorReceive,
    /// Change actor state
    Become,
    
    // Error handling
    /// Begin try block
    Try,
    /// Start try block execution
    TryStart,
    /// Start try block with finally
    TryStartWithFinally,
    /// End try block
    TryEnd,
    /// Catch errors matching pattern
    Catch,
    /// Finally block start - saves state
    Finally,
    /// Start finally execution
    FinallyStart,
    /// End finally execution
    FinallyEnd,
    /// Finally block end - restores state
    EndFinally,
    /// Throw an error
    Throw,
    /// Push error handler onto stack (with catch IP)
    PushHandler,
    /// Push finally handler IP
    PushFinally,
    /// Pop error handler from stack
    PopHandler,
    
    // Promise operations
    /// Create a new promise
    PromiseNew,
    /// Wait for all promises
    PromiseAll,
    /// Race multiple promises
    PromiseRace,
    /// Add timeout to promise
    WithTimeout,

    // Effect handlers
    /// Create handler table from stack values
    MakeHandler,
    /// Install handler for dynamic scope
    InstallHandler,
    /// Uninstall handler, preserving result
    UninstallHandler,

    // Mutable cells
    /// Create a cell with initial value
    MakeCell,
    /// Load cell reference
    LoadCell,
    /// Store cell reference
    StoreCell,
    /// Get value from cell
    CellGet,
    /// Set value in cell
    CellSet,

    // Tagged values (for constructor patterns)
    /// Create tagged value: tag(string const), arity -> Tagged
    MakeTagged,
    /// Get tag from tagged value
    GetTag,
    /// Get field N from tagged value
    GetTaggedField,
    /// Check if value is tagged with specific tag
    IsTagged,

    // Module operations
    /// Load module by name (string const)
    LoadModule,
    /// Import specific binding from module
    ImportBinding,
    /// Import all exports from module
    ImportAll,
    /// Load qualified variable (module.name)
    LoadQualified,
    /// Mark beginning of module scope
    BeginModule,
    /// Mark end of module scope
    EndModule,
    /// Export a binding from current module
    ExportBinding,

    // GC operations
    /// Allocate value with GC
    GcAlloc,
    /// Dereference GC handle
    GcDeref,
    /// Set value in GC handle
    GcSet,
    /// Manually trigger collection
    GcCollect,

    // Tail call optimization
    /// Tail call with frame reuse
    TailCall,
    /// Return from tail-recursive function
    TailReturn,
    /// Mark start of tail-recursive loop
    LoopStart,
    /// Mark end of tail-recursive loop
    LoopEnd,
    /// Update local variable (for loop parameter updates)
    UpdateLocal,

    // Special
    /// Halt execution
    Halt,
    /// No operation
    Nop,
}

/// A bytecode instruction with optional argument
#[derive(Debug, Clone)]
pub struct Instruction {
    /// The operation code
    pub opcode: Opcode,
    /// Optional argument for the opcode
    pub arg: u32,
}

impl Instruction {
    /// Create a new instruction without an argument
    pub fn new(opcode: Opcode) -> Self {
        Self { opcode, arg: 0 }
    }

    /// Create a new instruction with an argument
    pub fn with_arg(opcode: Opcode, arg: u32) -> Self {
        Self { opcode, arg }
    }
}

/// A chunk of bytecode with associated metadata
#[derive(Debug, Clone)]
pub struct BytecodeChunk {
    /// The bytecode instructions
    pub instructions: Vec<Instruction>,
    /// Constant pool for this chunk
    pub constants: Vec<Value>,
    /// Optional name for debugging
    pub name: Option<String>,
    /// Line numbers for debugging (maps instruction index to line number)
    pub line_numbers: Vec<u32>,
    /// Optional source map for this chunk
    pub source_map: Option<source_map::SourceMap>,
}

impl BytecodeChunk {
    /// Create a new bytecode chunk
    pub fn new(name: Option<String>) -> Self {
        Self {
            instructions: Vec::new(),
            constants: Vec::new(),
            name,
            line_numbers: Vec::new(),
            source_map: None,
        }
    }

    /// Add an instruction and return its index
    pub fn add_instruction(&mut self, instruction: Instruction) -> usize {
        self.instructions.push(instruction);
        self.instructions.len() - 1
    }

    /// Add a constant and return its index
    pub fn add_constant(&mut self, value: Value) -> u32 {
        self.constants.push(value);
        (self.constants.len() - 1) as u32
    }

    /// Patch a jump instruction with the target address
    pub fn patch_jump(&mut self, offset: usize, target: usize) {
        assert!(offset < self.instructions.len(), "Invalid jump offset");
        assert!(target <= u32::MAX as usize, "Jump target overflow");
        self.instructions[offset].arg = target as u32;
    }
    
    /// Add line number information for debugging
    pub fn add_line(&mut self, line: u32) {
        self.line_numbers.push(line);
    }
}

/// Complete bytecode for a module or program
#[derive(Debug, Clone)]
pub struct Bytecode {
    /// All bytecode chunks in this module
    pub chunks: Vec<BytecodeChunk>,
    /// Index of the main chunk to execute
    pub main_chunk: usize,
    /// Optional module-wide source map
    pub module_source_map: Option<source_map::ModuleSourceMap>,
}

impl Bytecode {
    /// Create a new empty bytecode module
    pub fn new() -> Self {
        Self {
            chunks: Vec::new(),
            main_chunk: 0,
            module_source_map: None,
        }
    }

    /// Add a chunk and return its index
    pub fn add_chunk(&mut self, chunk: BytecodeChunk) -> usize {
        self.chunks.push(chunk);
        self.chunks.len() - 1
    }
}