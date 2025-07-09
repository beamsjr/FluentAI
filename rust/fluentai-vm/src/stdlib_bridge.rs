//! Bridge implementation for stdlib to call VM functions

use crate::vm::VM;
use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_effects::EffectContext;
use fluentai_stdlib::vm_bridge::VMCallback;
use std::sync::Arc;

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
    fn call_function(&mut self, func: &Value, args: &[Value]) -> Result<Value> {
        // Push function and arguments to stack
        self.vm.push(func.clone())?;
        for arg in args {
            self.vm.push(arg.clone())?;
        }

        // Call the function
        self.vm.call_value(args.len())?;

        // Get result
        Ok(self.vm.pop()?)
    }

    fn effect_context(&self) -> Arc<EffectContext> {
        self.vm.get_effect_context()
    }

    fn call_function_with_context(
        &mut self,
        func: &Value,
        args: &[Value],
        context: Arc<EffectContext>,
    ) -> Result<Value> {
        // Save the current effect context
        let saved_context = self.vm.get_effect_context();

        // Set the new context
        self.vm.set_effect_context(context);

        // Call the function
        let result = self.call_function(func, args);

        // Restore the original context
        self.vm.set_effect_context(saved_context);

        result
    }
}

/// Extension trait for VM to support higher-order stdlib functions
pub trait VMStdlibExt {
    /// Execute a higher-order stdlib function that needs VM callback
    fn call_higher_order_stdlib(&mut self, func_name: &str, args: &[Value]) -> Result<Value>;
}

impl VMStdlibExt for VM {
    fn call_higher_order_stdlib(&mut self, func_name: &str, args: &[Value]) -> Result<Value> {
        match func_name {
            "map" => {
                if args.len() != 2 {
                    return Err(anyhow::anyhow!("map: expected 2 arguments"));
                }

                let func = &args[0];
                let list = match &args[1] {
                    Value::List(items) => items,
                    _ => return Err(anyhow::anyhow!("map: second argument must be a list")),
                };

                let mut result = Vec::with_capacity(list.len());

                // Create bridge for callbacks
                for item in list {
                    // Push and call
                    self.push(func.clone())?;
                    self.push(item.clone())?;
                    self.call_value(1)?;

                    // Get result
                    let vm_result = self.pop()?;
                    result.push(vm_result);
                }

                Ok(Value::List(result))
            }

            "filter" => {
                if args.len() != 2 {
                    return Err(anyhow::anyhow!("filter: expected 2 arguments"));
                }

                let pred = &args[0];
                let list = match &args[1] {
                    Value::List(items) => items,
                    _ => return Err(anyhow::anyhow!("filter: second argument must be a list")),
                };

                let mut result = Vec::new();

                for item in list {
                    // Push and call
                    self.push(pred.clone())?;
                    self.push(item.clone())?;
                    self.call_value(1)?;

                    // Check result
                    let vm_result = self.pop()?;
                    match vm_result {
                        Value::Boolean(true) => {
                            result.push(item.clone());
                        }
                        Value::Boolean(false) => {}
                        _ => return Err(anyhow::anyhow!("filter: predicate must return boolean")),
                    }
                }

                Ok(Value::List(result))
            }

            "fold" => {
                if args.len() != 3 {
                    return Err(anyhow::anyhow!("fold: expected 3 arguments"));
                }

                let func = &args[0];
                let mut acc = args[1].clone();
                let list = match &args[2] {
                    Value::List(items) => items,
                    _ => return Err(anyhow::anyhow!("fold: third argument must be a list")),
                };

                for item in list {
                    // Push and call with two arguments
                    self.push(func.clone())?;
                    self.push(acc)?;
                    self.push(item.clone())?;
                    self.call_value(2)?;

                    // Update accumulator
                    acc = self.pop()?;
                }

                Ok(acc)
            }

            _ => Err(anyhow::anyhow!(
                "Unknown higher-order function: {}",
                func_name
            )),
        }
    }
}
