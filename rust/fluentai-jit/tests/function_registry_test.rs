//! Tests for function registry index mapping

use fluentai_jit::function_registry::{FunctionRegistry, JitFunctionInfo};
use fluentai_core::value::Value;
use cranelift::prelude::*;
use cranelift_codegen::isa::CallConv;
use cranelift_module::FuncId;
use std::sync::Arc;

#[test]
fn test_function_index_mapping() {
    let mut registry = FunctionRegistry::new();
    
    // Create dummy function info
    let dummy_func_id = FuncId::from_u32(1);
    let dummy_sig = Signature::new(CallConv::SystemV);
    
    let info1 = JitFunctionInfo {
        func_id: dummy_func_id,
        signature: dummy_sig.clone(),
        arity: 2,
        chunk_id: Some(10),
    };
    
    let info2 = JitFunctionInfo {
        func_id: dummy_func_id,
        signature: dummy_sig.clone(),
        arity: 1,
        chunk_id: None,
    };
    
    let info3 = JitFunctionInfo {
        func_id: dummy_func_id,
        signature: dummy_sig.clone(),
        arity: 3,
        chunk_id: Some(20),
    };
    
    // Register functions
    let ptr1 = 0x1000 as *const u8;
    let ptr2 = 0x2000 as *const u8;
    let ptr3 = 0x3000 as *const u8;
    
    registry.register_vm_function(10, info1, ptr1);
    registry.register_native_function("print".to_string(), info2, ptr2);
    registry.register_vm_function(20, info3, ptr3);
    
    // Test VM function index lookup
    let func1 = Value::Function { chunk_id: 10, env: vec![] };
    let index1 = registry.get_function_index(&func1);
    assert_eq!(index1, Some(0));
    
    // Test native function index lookup
    let func2 = Value::NativeFunction { 
        name: "print".to_string(), 
        arity: 1,
        function: Arc::new(|_| Ok(Value::Nil))
    };
    let index2 = registry.get_function_index(&func2);
    assert_eq!(index2, Some(1));
    
    // Test second VM function
    let func3 = Value::Function { chunk_id: 20, env: vec![] };
    let index3 = registry.get_function_index(&func3);
    assert_eq!(index3, Some(2));
    
    // Test non-existent function
    let func4 = Value::Function { chunk_id: 99, env: vec![] };
    let index4 = registry.get_function_index(&func4);
    assert_eq!(index4, None);
    
    // Test function pointer retrieval
    assert_eq!(registry.get_function_pointer(0), Some(ptr1));
    assert_eq!(registry.get_function_pointer(1), Some(ptr2));
    assert_eq!(registry.get_function_pointer(2), Some(ptr3));
    assert_eq!(registry.get_function_pointer(3), None);
}

#[test]
fn test_function_table() {
    let mut registry = FunctionRegistry::new();
    
    let dummy_func_id = FuncId::from_u32(1);
    let dummy_sig = Signature::new(CallConv::SystemV);
    
    // Register multiple functions
    for i in 0..5 {
        let info = JitFunctionInfo {
            func_id: dummy_func_id,
            signature: dummy_sig.clone(),
            arity: 1,
            chunk_id: Some(i * 10),
        };
        let ptr = (0x1000 + i * 0x1000) as *const u8;
        registry.register_vm_function(i * 10, info, ptr);
    }
    
    // Verify function table
    let table = registry.function_table();
    assert_eq!(table.len(), 5);
    
    for i in 0..5 {
        let expected_ptr = (0x1000 + i * 0x1000) as *const u8;
        assert_eq!(table[i], expected_ptr);
    }
}

#[test]
fn test_mixed_function_registration() {
    let mut registry = FunctionRegistry::new();
    
    let dummy_func_id = FuncId::from_u32(1);
    let dummy_sig = Signature::new(CallConv::SystemV);
    
    // Mix VM and native function registrations
    let info = JitFunctionInfo {
        func_id: dummy_func_id,
        signature: dummy_sig.clone(),
        arity: 1,
        chunk_id: Some(5),
    };
    registry.register_vm_function(5, info.clone(), 0x1000 as *const u8);
    
    let native_info = JitFunctionInfo {
        func_id: dummy_func_id,
        signature: dummy_sig.clone(),
        arity: 2,
        chunk_id: None,
    };
    registry.register_native_function("map".to_string(), native_info, 0x2000 as *const u8);
    
    registry.register_vm_function(15, info, 0x3000 as *const u8);
    
    // Verify indices are sequential
    let vm_func1 = Value::Function { chunk_id: 5, env: vec![] };
    let native_func = Value::NativeFunction {
        name: "map".to_string(),
        arity: 2,
        function: Arc::new(|_| Ok(Value::Nil))
    };
    let vm_func2 = Value::Function { chunk_id: 15, env: vec![] };
    
    assert_eq!(registry.get_function_index(&vm_func1), Some(0));
    assert_eq!(registry.get_function_index(&native_func), Some(1));
    assert_eq!(registry.get_function_index(&vm_func2), Some(2));
}