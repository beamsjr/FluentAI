"""
x86-64 Code Generation for ClaudeLang JIT

This module provides a complete x86-64 assembler and code generator
for JIT compilation of ClaudeLang bytecode.
"""

import struct
from typing import List, Dict, Optional, Tuple
from enum import IntEnum
from dataclasses import dataclass

from ..vm.bytecode import Opcode


class Register(IntEnum):
    """x86-64 registers"""
    # 64-bit registers
    RAX = 0  # Return value, accumulator
    RCX = 1  # Counter
    RDX = 2  # Data
    RBX = 3  # Base
    RSP = 4  # Stack pointer
    RBP = 5  # Base pointer
    RSI = 6  # Source index
    RDI = 7  # Destination index
    R8 = 8
    R9 = 9
    R10 = 10
    R11 = 11
    R12 = 12
    R13 = 13
    R14 = 14
    R15 = 15
    
    # 8-bit low registers (for setcc)
    AL = 16
    CL = 17
    DL = 18
    BL = 19


class Condition(IntEnum):
    """x86-64 condition codes"""
    OVERFLOW = 0x0
    NO_OVERFLOW = 0x1
    BELOW = 0x2      # unsigned <
    ABOVE_EQUAL = 0x3  # unsigned >=
    EQUAL = 0x4      # ==
    NOT_EQUAL = 0x5  # !=
    BELOW_EQUAL = 0x6  # unsigned <=
    ABOVE = 0x7      # unsigned >
    SIGN = 0x8       # negative
    NO_SIGN = 0x9    # positive
    PARITY = 0xA
    NO_PARITY = 0xB
    LESS = 0xC       # signed <
    GREATER_EQUAL = 0xD  # signed >=
    LESS_EQUAL = 0xE    # signed <=
    GREATER = 0xF    # signed >


@dataclass
class Label:
    """Label for jump targets"""
    name: str
    offset: Optional[int] = None


class X86_64CodeGen:
    """Complete x86-64 code generator"""
    
    def __init__(self):
        self.code = bytearray()
        self.labels: Dict[str, int] = {}
        self.fixups: List[Tuple[int, str]] = []  # (offset, label_name)
        
    def get_code(self) -> bytes:
        """Get assembled code with fixups applied"""
        # Apply fixups
        for offset, label in self.fixups:
            if label not in self.labels:
                raise ValueError(f"Undefined label: {label}")
            
            target = self.labels[label]
            # Calculate relative offset from instruction end
            rel_offset = target - (offset + 4)
            
            # Write 32-bit relative offset
            self.code[offset:offset+4] = struct.pack('<i', rel_offset)
        
        return bytes(self.code)
    
    def label(self, name: str):
        """Define a label at current position"""
        self.labels[name] = len(self.code)
    
    # Basic encoding helpers
    
    def emit_byte(self, byte: int):
        """Emit a single byte"""
        self.code.append(byte & 0xFF)
    
    def emit_bytes(self, *bytes_vals: int):
        """Emit multiple bytes"""
        for b in bytes_vals:
            self.emit_byte(b)
    
    def emit_int32(self, value: int):
        """Emit 32-bit integer (little-endian)"""
        self.code.extend(struct.pack('<i', value))
    
    def emit_int64(self, value: int):
        """Emit 64-bit integer (little-endian)"""
        self.code.extend(struct.pack('<q', value))
    
    def emit_modrm(self, mod: int, reg: int, rm: int):
        """Emit ModR/M byte"""
        self.emit_byte((mod << 6) | ((reg & 7) << 3) | (rm & 7))
    
    def emit_rex(self, w: int = 0, r: int = 0, x: int = 0, b: int = 0):
        """Emit REX prefix if needed"""
        rex = 0x40 | (w << 3) | (r << 2) | (x << 1) | b
        if rex != 0x40:  # Only emit if needed
            self.emit_byte(rex)
    
    # Stack operations
    
    def push(self, reg: Register):
        """push reg"""
        if reg >= Register.R8:
            self.emit_rex(b=1)
        self.emit_byte(0x50 + (reg & 7))
    
    def pop(self, reg: Register):
        """pop reg"""
        if reg >= Register.R8:
            self.emit_rex(b=1)
        self.emit_byte(0x58 + (reg & 7))
    
    def push_imm32(self, value: int):
        """push imm32"""
        self.emit_byte(0x68)
        self.emit_int32(value)
    
    # Data movement
    
    def mov_reg_reg(self, dst: Register, src: Register):
        """mov dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x89)
        self.emit_modrm(3, src, dst)
    
    def mov_imm_reg(self, dst: Register, value: int):
        """mov dst, imm64"""
        self.emit_rex(w=1, b=(dst >> 3))
        self.emit_byte(0xB8 + (dst & 7))
        self.emit_int64(value)
    
    def mov_mem_reg(self, dst: Register, base: Register, offset: int = 0):
        """mov dst, [base + offset]"""
        self.emit_rex(w=1, r=(dst >> 3), b=(base >> 3))
        self.emit_byte(0x8B)
        
        if offset == 0 and base != Register.RBP:
            self.emit_modrm(0, dst, base)
        elif -128 <= offset <= 127:
            self.emit_modrm(1, dst, base)
            self.emit_byte(offset & 0xFF)
        else:
            self.emit_modrm(2, dst, base)
            self.emit_int32(offset)
    
    def mov_reg_mem(self, base: Register, offset: int, src: Register):
        """mov [base + offset], src"""
        self.emit_rex(w=1, r=(src >> 3), b=(base >> 3))
        self.emit_byte(0x89)
        
        if offset == 0 and base != Register.RBP:
            self.emit_modrm(0, src, base)
        elif -128 <= offset <= 127:
            self.emit_modrm(1, src, base)
            self.emit_byte(offset & 0xFF)
        else:
            self.emit_modrm(2, src, base)
            self.emit_int32(offset)
    
    def lea(self, dst: Register, base: Register, offset: int = 0):
        """lea dst, [base + offset]"""
        self.emit_rex(w=1, r=(dst >> 3), b=(base >> 3))
        self.emit_byte(0x8D)
        
        if offset == 0:
            self.emit_modrm(0, dst, base)
        elif -128 <= offset <= 127:
            self.emit_modrm(1, dst, base)
            self.emit_byte(offset & 0xFF)
        else:
            self.emit_modrm(2, dst, base)
            self.emit_int32(offset)
    
    # Arithmetic operations
    
    def add_reg_reg(self, dst: Register, src: Register):
        """add dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x01)
        self.emit_modrm(3, src, dst)
    
    def add_imm_reg(self, dst: Register, value: int):
        """add dst, imm32"""
        self.emit_rex(w=1, b=(dst >> 3))
        
        if dst == Register.RAX:
            self.emit_byte(0x05)
            self.emit_int32(value)
        else:
            self.emit_byte(0x81)
            self.emit_modrm(3, 0, dst)
            self.emit_int32(value)
    
    def sub_reg_reg(self, dst: Register, src: Register):
        """sub dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x29)
        self.emit_modrm(3, src, dst)
    
    def sub_imm_reg(self, dst: Register, value: int):
        """sub dst, imm32"""
        self.emit_rex(w=1, b=(dst >> 3))
        
        if dst == Register.RAX:
            self.emit_byte(0x2D)
            self.emit_int32(value)
        else:
            self.emit_byte(0x81)
            self.emit_modrm(3, 5, dst)
            self.emit_int32(value)
    
    def imul_reg_reg(self, dst: Register, src: Register):
        """imul dst, src"""
        self.emit_rex(w=1, r=(dst >> 3), b=(src >> 3))
        self.emit_bytes(0x0F, 0xAF)
        self.emit_modrm(3, dst, src)
    
    def idiv_reg(self, divisor: Register):
        """idiv divisor (RAX:RDX / divisor -> RAX quotient, RDX remainder)"""
        self.emit_rex(w=1, b=(divisor >> 3))
        self.emit_bytes(0xF7)
        self.emit_modrm(3, 7, divisor)
    
    def neg_reg(self, reg: Register):
        """neg reg"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xF7)
        self.emit_modrm(3, 3, reg)
    
    def inc_reg(self, reg: Register):
        """inc reg"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xFF)
        self.emit_modrm(3, 0, reg)
    
    def dec_reg(self, reg: Register):
        """dec reg"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xFF)
        self.emit_modrm(3, 1, reg)
    
    # Logical operations
    
    def and_reg_reg(self, dst: Register, src: Register):
        """and dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x21)
        self.emit_modrm(3, src, dst)
    
    def and_imm_reg(self, dst: Register, value: int):
        """and dst, imm32"""
        self.emit_rex(w=1, b=(dst >> 3))
        
        if dst == Register.RAX:
            self.emit_byte(0x25)
            self.emit_int32(value)
        else:
            self.emit_byte(0x81)
            self.emit_modrm(3, 4, dst)
            self.emit_int32(value)
    
    def or_reg_reg(self, dst: Register, src: Register):
        """or dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x09)
        self.emit_modrm(3, src, dst)
    
    def xor_reg_reg(self, dst: Register, src: Register):
        """xor dst, src"""
        self.emit_rex(w=1, r=(src >> 3), b=(dst >> 3))
        self.emit_byte(0x31)
        self.emit_modrm(3, src, dst)
    
    def not_reg(self, reg: Register):
        """not reg"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xF7)
        self.emit_modrm(3, 2, reg)
    
    def shl_reg_imm(self, reg: Register, count: int):
        """shl reg, imm8"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xC1)
        self.emit_modrm(3, 4, reg)
        self.emit_byte(count & 0xFF)
    
    def shr_reg_imm(self, reg: Register, count: int):
        """shr reg, imm8"""
        self.emit_rex(w=1, b=(reg >> 3))
        self.emit_byte(0xC1)
        self.emit_modrm(3, 5, reg)
        self.emit_byte(count & 0xFF)
    
    # Comparison
    
    def cmp_reg_reg(self, reg1: Register, reg2: Register):
        """cmp reg1, reg2"""
        self.emit_rex(w=1, r=(reg2 >> 3), b=(reg1 >> 3))
        self.emit_byte(0x39)
        self.emit_modrm(3, reg2, reg1)
    
    def cmp_imm_reg(self, reg: Register, value: int):
        """cmp reg, imm32"""
        self.emit_rex(w=1, b=(reg >> 3))
        
        if reg == Register.RAX:
            self.emit_byte(0x3D)
            self.emit_int32(value)
        else:
            self.emit_byte(0x81)
            self.emit_modrm(3, 7, reg)
            self.emit_int32(value)
    
    def test_reg_reg(self, reg1: Register, reg2: Register):
        """test reg1, reg2"""
        self.emit_rex(w=1, r=(reg2 >> 3), b=(reg1 >> 3))
        self.emit_byte(0x85)
        self.emit_modrm(3, reg2, reg1)
    
    # Control flow
    
    def jmp(self, label: str):
        """jmp label (32-bit relative)"""
        self.emit_byte(0xE9)
        # Record fixup for later
        self.fixups.append((len(self.code), label))
        self.emit_int32(0)  # Placeholder
    
    def jcc(self, condition: Condition, label: str):
        """Conditional jump (32-bit relative)"""
        self.emit_bytes(0x0F, 0x80 + condition)
        self.fixups.append((len(self.code), label))
        self.emit_int32(0)  # Placeholder
    
    def call(self, func_ptr: int):
        """call absolute address"""
        # mov rax, func_ptr
        self.mov_imm_reg(Register.RAX, func_ptr)
        # call rax
        self.emit_bytes(0xFF, 0xD0)
    
    def ret(self):
        """ret"""
        self.emit_byte(0xC3)
    
    # Extended operations for VM
    
    def cqo(self):
        """Sign extend RAX to RDX:RAX"""
        self.emit_rex(w=1)
        self.emit_byte(0x99)
    
    def setcc(self, condition: Condition, reg: Register):
        """Set byte on condition"""
        if reg >= Register.R8:
            self.emit_rex(b=1)
        self.emit_bytes(0x0F, 0x90 + condition)
        self.emit_modrm(3, 0, reg)
    
    def movzx_byte_reg(self, dst: Register, src: Register):
        """Zero extend byte to 64-bit"""
        self.emit_rex(w=1, r=(dst >> 3), b=(src >> 3))
        self.emit_bytes(0x0F, 0xB6)
        self.emit_modrm(3, dst, src)
    
    # Helper methods for common patterns
    
    def prologue(self):
        """Standard function prologue"""
        self.push(Register.RBP)
        self.mov_reg_reg(Register.RBP, Register.RSP)
    
    def epilogue(self):
        """Standard function epilogue"""
        self.mov_reg_reg(Register.RSP, Register.RBP)
        self.pop(Register.RBP)
        self.ret()
    
    def save_registers(self, registers: List[Register]):
        """Save multiple registers"""
        for reg in registers:
            self.push(reg)
    
    def restore_registers(self, registers: List[Register]):
        """Restore multiple registers (reverse order)"""
        for reg in reversed(registers):
            self.pop(reg)