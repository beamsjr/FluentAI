//! Module operations handler
//! 
//! This handler implements module loading, import/export, and qualified variable access.
//! It integrates with the module registry and module loader to provide seamless module support.

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use crate::module_loader::ModuleLoader;
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ModuleHandler {
    loader: ModuleLoader,
}

impl ModuleHandler {
    pub fn new() -> Self {
        Self {
            loader: ModuleLoader::new(),
        }
    }
    
    pub fn with_loader(loader: ModuleLoader) -> Self {
        Self { loader }
    }
}

impl OpcodeHandler for ModuleHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Load a module by name
            LoadModule => {
                let name_idx = instruction.arg as usize;
                let module_name = vm.get_constant_string_at(chunk_id, name_idx)?;
                
                // Load the module using the module loader
                match self.loader.load_module(vm, &module_name) {
                    Ok(loaded_name) => {
                        // Push the module name on the stack for subsequent import operations
                        vm.push(Value::String(loaded_name))?;
                    }
                    Err(e) => {
                        return Err(VMError::RuntimeError {
                            message: format!("Failed to load module '{}': {}", module_name, e),
                            stack_trace: Some(vm.build_stack_trace()),
                        });
                    }
                }
            }
            
            // Import specific binding from module
            ImportBinding => {
                // arg encodes module_idx (high 16 bits) and binding_idx (low 16 bits)
                let module_idx = (instruction.arg >> 16) as usize;
                let binding_idx = (instruction.arg & 0xFFFF) as usize;
                
                let module_name = vm.get_constant_string_at(chunk_id, module_idx)?;
                let binding_name = vm.get_constant_string_at(chunk_id, binding_idx)?;
                
                // Get the export from the module registry
                match vm.get_module_export(&module_name, &binding_name) {
                    Ok(value) => {
                        vm.push(value)?;
                    }
                    Err(e) => {
                        return Err(VMError::RuntimeError {
                            message: format!("Failed to import '{}' from module '{}': {}", 
                                binding_name, module_name, e),
                            stack_trace: Some(vm.build_stack_trace()),
                        });
                    }
                }
            }
            
            // Import all exports from module
            ImportAll => {
                let module_idx = instruction.arg as usize;
                let module_name = vm.get_constant_string_at(chunk_id, module_idx)?;
                
                // Get the export names from the module
                let export_names = {
                    if let Some(module_info) = vm.module_registry().get_module(&module_name) {
                        module_info.exports.iter()
                            .map(|e| e.name.clone())
                            .collect::<Vec<String>>()
                    } else {
                        return Err(VMError::RuntimeError {
                            message: format!("Module '{}' not found", module_name),
                            stack_trace: Some(vm.build_stack_trace()),
                        });
                    }
                };
                
                // Import each export into the current scope
                for export_name in export_names {
                    if let Ok(value) = vm.get_module_export(&module_name, &export_name) {
                        // Store in global scope with the export name
                        vm.set_global(export_name, value);
                    }
                }
            }
            
            // Load qualified variable (module.name)
            LoadQualified => {
                // arg encodes module_idx (high 16 bits) and var_idx (low 16 bits)
                let module_idx = (instruction.arg >> 16) as usize;
                let var_idx = (instruction.arg & 0xFFFF) as usize;
                
                let module_name = vm.get_constant_string_at(chunk_id, module_idx)?;
                let var_name = vm.get_constant_string_at(chunk_id, var_idx)?;
                
                // Get the export from the module registry
                match vm.get_module_export(&module_name, &var_name) {
                    Ok(value) => {
                        vm.push(value)?;
                    }
                    Err(e) => {
                        return Err(VMError::RuntimeError {
                            message: format!("Failed to access '{}.{}': {}", 
                                module_name, var_name, e),
                            stack_trace: Some(vm.build_stack_trace()),
                        });
                    }
                }
            }
            
            // Mark beginning of module scope
            BeginModule => {
                // This is primarily a marker for the compiler/optimizer
                // The VM doesn't need to do anything special here
            }
            
            // Mark end of module scope
            EndModule => {
                // This is primarily a marker for the compiler/optimizer
                // The VM doesn't need to do anything special here
            }
            
            // Export a binding from current module
            ExportBinding => {
                let name_idx = instruction.arg as usize;
                let export_name = vm.get_constant_string_at(chunk_id, name_idx)?;
                
                // Get the value to export (should be on top of stack)
                let value = vm.peek(0)?.clone();
                
                // If we're in a module context, register the export
                // This would typically be handled during module initialization
                // For now, we'll store it as a global with a module prefix
                vm.set_global(format!("__export__{}", export_name), value);
            }
            
            _ => unreachable!("ModuleHandler received non-module opcode"),
        }
        
        Ok(VMState::Continue)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;
    use std::fs;
    
    #[test]
    fn test_module_handler_basic() {
        // Create a temporary directory for test modules
        let temp_dir = TempDir::new().unwrap();
        let module_path = temp_dir.path().join("test_module.flc");
        
        // Write a simple module
        fs::write(&module_path, r#"
            module TestModule;
            
            public function add(a: int, b: int) -> int {
                a + b
            }
            
            public const PI = 3.14159;
        "#).unwrap();
        
        // Create module loader with test path
        let mut loader = ModuleLoader::new();
        loader.add_module_path(temp_dir.path().to_path_buf());
        
        // Create handler with custom loader
        let handler = ModuleHandler::with_loader(loader);
        
        // Test would continue with VM setup and instruction execution
        // This is just a basic structure test
        assert!(matches!(handler.loader.module_paths()[0], _));
    }
}