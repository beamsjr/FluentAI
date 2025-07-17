//! Concurrent and async operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ConcurrentHandler;

impl ConcurrentHandler {
    // Helper method for type errors
    fn type_error(&self, operation: &str, expected: &str, got: &Value) -> VMError {
        VMError::TypeError {
            operation: operation.to_string(),
            expected: expected.to_string(),
            got: crate::error::value_type_name(got).to_string(),
            location: None,
            stack_trace: None,
        }
    }

    // Helper for std feature errors
    #[allow(dead_code)]
    fn std_feature_error(&self, operation: &str) -> VMError {
        VMError::RuntimeError {
            message: format!("{} operation requires std feature", operation),
            stack_trace: None,
        }
    }

    // Spawn handler
    fn handle_spawn(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let func = vm.pop()?;
            vm.spawn_task(func)?;
            Ok(())
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Spawn"))
        }
    }

    // Await handler
    fn handle_await(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let future = vm.pop()?;
            match future {
                Value::Promise(promise_id) => {
                    let pid = crate::safety::PromiseId(promise_id);
                    
                    // Check if the promise is already resolved
                    // First try a non-blocking check
                    if let Some(receiver) = vm.promises_mut().get_mut(&pid) {
                        match receiver.try_recv() {
                            Ok(Ok(value)) => {
                                // Promise is ready, push the result
                                vm.push(value)?;
                                vm.promises_mut().remove(&pid);
                                Ok(())
                            }
                            Ok(Err(e)) => {
                                // Promise rejected
                                vm.promises_mut().remove(&pid);
                                Err(e)
                            }
                            Err(_) => {
                                // Promise not ready - need to suspend
                                // For now in synchronous VM, we'll just return a placeholder
                                // The AsyncVM will handle actual suspension
                                vm.push(Value::Nil)?;
                                Ok(())
                            }
                        }
                    } else {
                        Err(VMError::AsyncError {
                            message: format!("Promise {:?} not found", pid),
                            stack_trace: None,
                        })
                    }
                }
                _ => Err(self.type_error("await", "promise", &future))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Await"))
        }
    }

    // Channel creation handler
    fn handle_channel(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let channel_id = vm.create_channel();
            vm.push(Value::Channel(channel_id.0))?;
            Ok(())
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Channel"))
        }
    }

    // Channel with capacity handler
    fn handle_channel_with_capacity(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let capacity = vm.pop()?;
            match capacity {
                Value::Integer(_n) => {
                    // For now, just create a regular channel
                    let channel_id = vm.create_channel();
                    vm.push(Value::Channel(channel_id.0))?;
                    Ok(())
                }
                _ => Err(self.type_error("channel_with_capacity", "integer", &capacity))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Channel"))
        }
    }

    // Send handler
    fn handle_send(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let value = vm.pop()?;
            let target = vm.pop()?;
            
            match target {
                Value::Channel(channel_id_raw) => {
                    vm.send_to_channel(crate::safety::ChannelId(channel_id_raw), value)?;
                    vm.push(Value::Nil)?;
                    Ok(())
                }
                Value::Actor(actor_id_raw) => {
                    vm.send_to_actor(crate::safety::ActorId(actor_id_raw), value)?;
                    vm.push(Value::Nil)?;
                    Ok(())
                }
                _ => Err(self.type_error("send", "channel or actor", &target))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Send"))
        }
    }

    // Receive handler
    fn handle_receive(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let channel = vm.pop()?;
            
            match channel {
                Value::Channel(channel_id_raw) => {
                    match vm.receive_from_channel(crate::safety::ChannelId(channel_id_raw)) {
                        Ok(value) => {
                            vm.push(value)?;
                            Ok(())
                        }
                        Err(e) => Err(e)
                    }
                }
                _ => Err(self.type_error("receive", "channel", &channel))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Receive"))
        }
    }

    // Try receive handler
    fn handle_try_receive(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let channel = vm.pop()?;
            
            match channel {
                Value::Channel(channel_id_raw) => {
                    let channel_id = crate::safety::ChannelId(channel_id_raw);
                    // Use get_channel_receiver_mut to try non-blocking receive
                    if let Some(receiver) = vm.get_channel_receiver_mut(&channel_id) {
                        match receiver.try_recv() {
                            Ok(value) => vm.push(value)?,
                            Err(_) => vm.push(Value::Nil)?, // Channel empty or closed
                        }
                        Ok(())
                    } else {
                        Err(VMError::RuntimeError {
                            message: format!("Channel not found"),
                            stack_trace: None,
                        })
                    }
                }
                _ => Err(self.type_error("try_receive", "channel", &channel))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("TryReceive"))
        }
    }

    // Select handler
    fn handle_select(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let channels = vm.pop()?;
            
            match channels {
                Value::List(ref _channels) => {
                    // TODO: Implement actual select logic
                    // For now, just return nil
                    vm.push(Value::Nil)?;
                    Ok(())
                }
                _ => Err(self.type_error("select", "list of channels", &channels))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Select"))
        }
    }

    // Create actor handler
    fn handle_create_actor(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let handler = vm.pop()?;
            let initial_state = vm.pop()?;
            let actor_id = vm.create_actor(initial_state, handler)?;
            vm.push(Value::Actor(actor_id.0))?;
            Ok(())
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("CreateActor"))
        }
    }

    // Actor send handler
    fn handle_actor_send(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let message = vm.pop()?;
            let actor = vm.pop()?;
            
            match actor {
                Value::Actor(actor_id_raw) => {
                    vm.send_to_actor(crate::safety::ActorId(actor_id_raw), message)?;
                    vm.push(Value::Nil)?;
                    Ok(())
                }
                _ => Err(self.type_error("actor_send", "actor", &actor))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("ActorSend"))
        }
    }

    // Try send handler
    fn handle_try_send(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let value = vm.pop()?;
            let channel = vm.pop()?;
            
            match channel {
                Value::Channel(channel_id_raw) => {
                    let channel_id = crate::safety::ChannelId(channel_id_raw);
                    // Use get_channel to access the sender for non-blocking send
                    if let Some((sender, _)) = vm.get_channel(&channel_id) {
                        match sender.try_send(value) {
                            Ok(()) => vm.push(Value::Boolean(true))?,
                            Err(_) => vm.push(Value::Boolean(false))?, // Channel full or closed
                        }
                        Ok(())
                    } else {
                        Err(VMError::RuntimeError {
                            message: format!("Channel not found"),
                            stack_trace: None,
                        })
                    }
                }
                _ => Err(self.type_error("try_send", "channel", &channel))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("TrySend"))
        }
    }

    // Actor receive handler
    fn handle_actor_receive(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            // Get the current actor's message if available
            if let Some(message) = vm.get_current_actor_message() {
                vm.push(message)?;
                Ok(())
            } else {
                Err(VMError::RuntimeError {
                    message: "No message available for current actor".to_string(),
                    stack_trace: None,
                })
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("ActorReceive"))
        }
    }

    // Become handler
    fn handle_become(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let new_behavior = vm.pop()?;
            // Store the new behavior for the current actor
            if let Some(actor_id) = vm.current_actor_context() {
                vm.update_actor_state(actor_id, new_behavior)?;
                Ok(())
            } else {
                Err(VMError::RuntimeError {
                    message: "Not in actor context".to_string(),
                    stack_trace: None,
                })
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("Become"))
        }
    }

    // Promise new handler
    fn handle_promise_new(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            use tokio::sync::oneshot;
            
            // Create a new promise ID
            let promise_id = vm.id_generator().next_promise_id();
            let (_tx, rx) = oneshot::channel();
            
            // Store the receiver
            vm.promises_mut().insert(promise_id, rx);
            
            vm.push(Value::Promise(promise_id.0))?;
            Ok(())
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("PromiseNew"))
        }
    }

    // Promise all handler
    fn handle_promise_all(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            use tokio::sync::oneshot;
            
            let promises = vm.pop()?;
            
            match promises {
                Value::List(ref _promise_list) => {
                    // TODO: Implement actual promise.all logic
                    // For now, create a new promise that resolves to a list
                    let result_promise = vm.id_generator().next_promise_id();
                    let (_tx, rx) = oneshot::channel();
                    vm.promises_mut().insert(result_promise, rx);
                    
                    vm.push(Value::Promise(result_promise.0))?;
                    Ok(())
                }
                _ => Err(self.type_error("promise_all", "list of promises", &promises))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("PromiseAll"))
        }
    }

    // Promise race handler
    fn handle_promise_race(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            use tokio::sync::oneshot;
            
            let promises = vm.pop()?;
            
            match promises {
                Value::List(ref _promise_list) => {
                    // TODO: Implement actual promise.race logic
                    // For now, create a new promise
                    let result_promise = vm.id_generator().next_promise_id();
                    let (_tx, rx) = oneshot::channel();
                    vm.promises_mut().insert(result_promise, rx);
                    
                    vm.push(Value::Promise(result_promise.0))?;
                    Ok(())
                }
                _ => Err(self.type_error("promise_race", "list of promises", &promises))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("PromiseRace"))
        }
    }

    // With timeout handler
    fn handle_with_timeout(&mut self, vm: &mut VM) -> VMResult<()> {
        #[cfg(feature = "std")]
        {
            let timeout_ms = vm.pop()?;
            let future = vm.pop()?;
            
            match timeout_ms {
                Value::Integer(_ms) => {
                    // TODO: Implement actual timeout logic
                    // For now, just return the future
                    vm.push(future)?;
                    Ok(())
                }
                _ => Err(self.type_error("with_timeout", "integer (milliseconds)", &timeout_ms))
            }
        }
        #[cfg(not(feature = "std"))]
        {
            Err(self.std_feature_error("WithTimeout"))
        }
    }
}

impl OpcodeHandler for ConcurrentHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            Spawn => self.handle_spawn(vm)?,
            Await => self.handle_await(vm)?,
            Channel => self.handle_channel(vm)?,
            ChannelWithCapacity => self.handle_channel_with_capacity(vm)?,
            Send => self.handle_send(vm)?,
            Receive => self.handle_receive(vm)?,
            TryReceive => self.handle_try_receive(vm)?,
            Select => self.handle_select(vm)?,
            CreateActor => self.handle_create_actor(vm)?,
            ActorSend => self.handle_actor_send(vm)?,
            TrySend => self.handle_try_send(vm)?,
            ActorReceive => self.handle_actor_receive(vm)?,
            Become => self.handle_become(vm)?,
            PromiseNew => self.handle_promise_new(vm)?,
            PromiseAll => self.handle_promise_all(vm)?,
            PromiseRace => self.handle_promise_race(vm)?,
            WithTimeout => self.handle_with_timeout(vm)?,
            
            _ => unreachable!("ConcurrentHandler received non-concurrent opcode"),
        }
        
        Ok(VMState::Continue)
    }
}