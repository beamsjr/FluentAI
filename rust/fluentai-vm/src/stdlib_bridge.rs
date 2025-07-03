//! Bridge implementation for stdlib to call VM functions

use crate::vm::VM;
use crate::bytecode::Value as VMValue;
use fluentai_stdlib::value::Value as StdlibValue;
use fluentai_stdlib::vm_bridge::VMCallback;
use anyhow::Result;

/// Implementation of VMCallback for the VM
pub struct VMStdlibBridge<'a> {
    vm: &'a mut VM,
}

impl<'a> VMStdlibBridge<'a> {
    pub fn new(vm: &'a mut VM) -> Self {
        Self { vm }
    }
}

impl VMCallback for VMStdlibBridge<'_> {
    fn call_function(&mut self, func: &StdlibValue, args: &[StdlibValue]) -> Result<StdlibValue> {
        // Convert stdlib function value to VM value
        let vm_func = self.vm.stdlib_value_to_vm_value(func);
        
        // Convert arguments
        let vm_args: Vec<VMValue> = args.iter()
            .map(|arg| self.vm.stdlib_value_to_vm_value(arg))
            .collect();
        
        // Push function and arguments to stack
        self.vm.push(vm_func)?;
        for arg in vm_args {
            self.vm.push(arg)?;
        }
        
        // Call the function
        self.vm.call_value(args.len())?;
        
        // Get result and convert back
        let result = self.vm.pop()?;
        Ok(self.vm.vm_value_to_stdlib_value(&result))
    }
}

/// Extension trait for VM to support higher-order stdlib functions
pub trait VMStdlibExt {
    /// Execute a higher-order stdlib function that needs VM callback
    fn call_higher_order_stdlib(&mut self, func_name: &str, args: &[StdlibValue]) -> Result<StdlibValue>;
}

impl VMStdlibExt for VM {
    fn call_higher_order_stdlib(&mut self, func_name: &str, args: &[StdlibValue]) -> Result<StdlibValue> {
        match func_name {
            "map" => {
                if args.len() != 2 {
                    return Err(anyhow::anyhow!("map: expected 2 arguments"));
                }
                
                let func = &args[0];
                let list = match &args[1] {
                    StdlibValue::List(items) => items,
                    _ => return Err(anyhow::anyhow!("map: second argument must be a list")),
                };
                
                let mut result = Vec::with_capacity(list.len());
                
                // Create bridge for callbacks
                for item in list {
                    // Convert to VM values
                    let vm_func = self.stdlib_value_to_vm_value(func);
                    let vm_item = self.stdlib_value_to_vm_value(item);
                    
                    // Push and call
                    self.push(vm_func)?;
                    self.push(vm_item)?;
                    self.call_value(1)?;
                    
                    // Get result
                    let vm_result = self.pop()?;
                    result.push(self.vm_value_to_stdlib_value(&vm_result));
                }
                
                Ok(StdlibValue::List(result))
            }
            
            "filter" => {
                if args.len() != 2 {
                    return Err(anyhow::anyhow!("filter: expected 2 arguments"));
                }
                
                let pred = &args[0];
                let list = match &args[1] {
                    StdlibValue::List(items) => items,
                    _ => return Err(anyhow::anyhow!("filter: second argument must be a list")),
                };
                
                let mut result = Vec::new();
                
                for item in list {
                    // Convert to VM values
                    let vm_pred = self.stdlib_value_to_vm_value(pred);
                    let vm_item = self.stdlib_value_to_vm_value(item);
                    
                    // Push and call
                    self.push(vm_pred)?;
                    self.push(vm_item.clone())?;
                    self.call_value(1)?;
                    
                    // Check result
                    let vm_result = self.pop()?;
                    match vm_result {
                        VMValue::Bool(true) => {
                            result.push(item.clone());
                        }
                        VMValue::Bool(false) => {}
                        _ => return Err(anyhow::anyhow!("filter: predicate must return boolean")),
                    }
                }
                
                Ok(StdlibValue::List(result))
            }
            
            "fold" => {
                if args.len() != 3 {
                    return Err(anyhow::anyhow!("fold: expected 3 arguments"));
                }
                
                let func = &args[0];
                let mut acc = args[1].clone();
                let list = match &args[2] {
                    StdlibValue::List(items) => items,
                    _ => return Err(anyhow::anyhow!("fold: third argument must be a list")),
                };
                
                for item in list {
                    // Convert to VM values
                    let vm_func = self.stdlib_value_to_vm_value(func);
                    let vm_acc = self.stdlib_value_to_vm_value(&acc);
                    let vm_item = self.stdlib_value_to_vm_value(item);
                    
                    // Push and call with two arguments
                    self.push(vm_func)?;
                    self.push(vm_acc)?;
                    self.push(vm_item)?;
                    self.call_value(2)?;
                    
                    // Update accumulator
                    let vm_result = self.pop()?;
                    acc = self.vm_value_to_stdlib_value(&vm_result);
                }
                
                Ok(acc)
            }
            
            _ => Err(anyhow::anyhow!("Unknown higher-order function: {}", func_name)),
        }
    }
}