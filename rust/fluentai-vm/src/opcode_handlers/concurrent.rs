//! Concurrent and async operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ConcurrentHandler;

impl OpcodeHandler for ConcurrentHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Spawn a new concurrent task
            Spawn => {
                let func = vm.pop()?;
                vm.spawn_task(func)?;
            }
            
            // Await a promise/future
            Await => {
                let future = vm.pop()?;
                match future {
                    Value::Promise(promise_id) => {
                        let result = vm.await_promise(crate::safety::PromiseId(promise_id))?;
                        vm.push(result)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "await".to_string(),
                            expected: "future".to_string(),
                            got: vm.value_type_name(&future).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            // Channel operations
            Channel => {
                let channel_id = vm.create_channel();
                vm.push(Value::Channel(channel_id.0))?;
            }
            
            ChannelWithCapacity => {
                let capacity = vm.pop()?;
                match capacity {
                    Value::Integer(_n) => {
                        // For now, just create a regular channel
                        let channel_id = vm.create_channel();
                        vm.push(Value::Channel(channel_id.0))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "channel_with_capacity".to_string(),
                            expected: "integer".to_string(),
                            got: vm.value_type_name(&capacity).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            MakeChannel => {
                let channel_id = vm.create_channel();
                vm.push(Value::Channel(channel_id.0))?;
            }
            
            Send => {
                let value = vm.pop()?;
                let channel = vm.pop()?;
                
                match channel {
                    Value::Channel(channel_id_raw) => {
                        vm.send_to_channel(crate::safety::ChannelId(channel_id_raw), value)?;
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "send".to_string(),
                            expected: "channel".to_string(),
                            got: vm.value_type_name(&channel).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            Receive => {
                let channel = vm.pop()?;
                
                match channel {
                    Value::Channel(channel_id_raw) => {
                        let value = vm.receive_from_channel(crate::safety::ChannelId(channel_id_raw))?;
                        vm.push(value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "receive".to_string(),
                            expected: "channel".to_string(),
                            got: vm.value_type_name(&channel).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            // Actor operations
            CreateActor => {
                let handler = vm.pop()?;
                let initial_state = vm.pop()?;
                let actor_id = vm.create_actor(initial_state, handler)?;
                vm.push(Value::Actor(actor_id.0))?;
            }
            
            MakeActor => {
                let handler = vm.pop()?;
                let initial_state = vm.pop()?;
                let actor_id = vm.create_actor(initial_state, handler)?;
                vm.push(Value::Actor(actor_id.0))?;
            }
            
            ActorSend => {
                let message = vm.pop()?;
                let actor = vm.pop()?;
                
                match actor {
                    Value::Actor(actor_id_raw) => {
                        vm.send_to_actor(crate::safety::ActorId(actor_id_raw), message)?;
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "actor_send".to_string(),
                            expected: "actor".to_string(),
                            got: vm.value_type_name(&actor).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            SendToActor => {
                let message = vm.pop()?;
                let actor = vm.pop()?;
                
                match actor {
                    Value::Actor(actor_id_raw) => {
                        vm.send_to_actor(crate::safety::ActorId(actor_id_raw), message)?;
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "send_to_actor".to_string(),
                            expected: "actor".to_string(),
                            got: vm.value_type_name(&actor).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            // Additional async operations
            TrySend => {
                let value = vm.pop()?;
                let channel = vm.pop()?;
                
                match channel {
                    Value::Channel(_channel_id) => {
                        // Simplified - always succeed for now
                        vm.push(Value::Boolean(true))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "try_send".to_string(),
                            expected: "channel".to_string(),
                            got: vm.value_type_name(&channel).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            TryReceive => {
                let channel = vm.pop()?;
                
                match channel {
                    Value::Channel(_channel_id) => {
                        // Simplified - return None for now
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "try_receive".to_string(),
                            expected: "channel".to_string(),
                            got: vm.value_type_name(&channel).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            Select => {
                // Simplified implementation
                vm.push(Value::Nil)?;
            }
            
            ActorReceive => {
                // ActorReceive is used within actor handlers to pattern match on messages
                // The current implementation will be in the actor message processing logic
                // For now, this opcode should not be called directly
                return Err(VMError::RuntimeError {
                    message: "ActorReceive opcode should only be used within actor handlers".to_string(),
                    stack_trace: None,
                });
            }
            
            Become => {
                // Simplified implementation
                let _new_behavior = vm.pop()?;
            }
            
            PromiseNew => {
                // Simplified implementation
                vm.push(Value::Nil)?;
            }
            
            PromiseAll => {
                let promises = vm.pop()?;
                match promises {
                    Value::List(_) => {
                        // Simplified - return completed promise
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "promise_all".to_string(),
                            expected: "list of promises".to_string(),
                            got: vm.value_type_name(&promises).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            PromiseRace => {
                let promises = vm.pop()?;
                match promises {
                    Value::List(_) => {
                        // Simplified - return completed promise
                        vm.push(Value::Nil)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "promise_race".to_string(),
                            expected: "list of promises".to_string(),
                            got: vm.value_type_name(&promises).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            WithTimeout => {
                let _timeout = vm.pop()?;
                let _future = vm.pop()?;
                // Simplified - return the future unchanged
                vm.push(Value::Nil)?;
            }
            
            _ => unreachable!("ConcurrentHandler received non-concurrent opcode"),
        }
        
        Ok(VMState::Continue)
    }
}