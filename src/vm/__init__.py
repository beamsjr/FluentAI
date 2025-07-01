# VM module exports
from .bytecode import Opcode, Instruction, BytecodeChunk, BytecodeBuilder
from .compiler import BytecodeCompiler
from .vm import VM