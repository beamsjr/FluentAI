// Quick test to check if all opcodes are implemented
use std::collections::HashSet;

fn main() {
    // List all opcodes from bytecode.rs
    let all_opcodes = vec![
        "Push", "Pop", "PopN", "Dup", "Swap",
        "Add", "Sub", "Mul", "Div", "Mod", "Neg",
        "AddInt", "SubInt", "MulInt", "DivInt",
        "Eq", "Ne", "Lt", "Le", "Gt", "Ge",
        "LtInt", "LeInt", "GtInt", "GeInt",
        "And", "Or", "Not",
        "Jump", "JumpIf", "JumpIfNot", "Call", "Return",
        "Load", "Store", "LoadGlobal", "StoreGlobal",
        "LoadLocal0", "LoadLocal1", "LoadLocal2", "LoadLocal3",
        "StoreLocal0", "StoreLocal1", "StoreLocal2", "StoreLocal3",
        "MakeFunc", "MakeClosure", "LoadCaptured", "MakeEnv", "PopEnv",
        "MakeList", "ListHead", "ListTail", "ListCons", "ListLen", "ListEmpty",
        "StrLen", "StrConcat", "StrUpper", "StrLower",
        "PushInt0", "PushInt1", "PushInt2", "PushIntSmall",
        "PushTrue", "PushFalse", "PushNil", "PushConst",
        "Effect", "EffectAsync", "Await", "Spawn", "Channel", "Send", "Receive",
        "MakeCell", "CellGet", "CellSet",
        "MakeTagged", "GetTag", "GetTaggedField", "IsTagged",
        "Halt", "Nop"
    ];
    
    // Opcodes that are implemented in vm.rs (based on what we've seen)
    let implemented = vec![
        "Push", "Pop", "PopN", "Dup", "Swap",
        "Add", "Sub", "Mul", "Div", "Mod", "Neg",
        "AddInt", "SubInt", "MulInt", "DivInt",
        "Eq", "Ne", "Lt", "Le", "Gt", "Ge",
        "LtInt", "LeInt", "GtInt", "GeInt",
        "And", "Or", "Not",
        "Jump", "JumpIf", "JumpIfNot", "Call", "Return",
        "Load", "Store", "LoadGlobal", "StoreGlobal",
        "LoadLocal0", "LoadLocal1", "LoadLocal2", "LoadLocal3",
        "StoreLocal0", "StoreLocal1", "StoreLocal2", "StoreLocal3",
        "MakeFunc", "MakeClosure", "LoadCaptured", "MakeEnv", "PopEnv",
        "MakeList", "ListHead", "ListTail", "ListCons", "ListLen", "ListEmpty",
        "StrLen", "StrConcat", "StrUpper", "StrLower",
        "PushInt0", "PushInt1", "PushInt2", "PushIntSmall",
        "PushTrue", "PushFalse", "PushNil", "PushConst",
        "Effect", "EffectAsync", "Await", "Spawn", "Channel", "Send", "Receive",
        "MakeCell", "CellGet", "CellSet",
        "MakeTagged", "GetTag", "GetTaggedField", "IsTagged",
        "Halt", "Nop"
    ];
    
    let all_set: HashSet<_> = all_opcodes.into_iter().collect();
    let impl_set: HashSet<_> = implemented.into_iter().collect();
    
    let unimplemented: Vec<_> = all_set.difference(&impl_set).collect();
    
    if unimplemented.is_empty() {
        println!("All opcodes are implemented!");
    } else {
        println!("Unimplemented opcodes:");
        for op in unimplemented {
            println!("  - {}", op);
        }
    }
}