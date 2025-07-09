//! Unit tests for debug support functionality

#[cfg(test)]
mod tests {
    use super::super::debug::*;
    use crate::bytecode::{Instruction, Opcode};
use fluentai_core::value::Value;
    use tokio::sync::mpsc;
    
    mod debug_config {
        use super::*;
        
        #[test]
        fn test_default_config() {
            let config = DebugConfig::default();
            assert!(!config.enabled);
            assert!(config.breakpoints.is_empty());
            assert_eq!(config.step_mode, StepMode::Run);
            assert!(config.event_sender.is_none());
        }
        
        #[test]
        fn test_with_events() {
            let (tx, mut _rx) = mpsc::unbounded_channel();
            let config = DebugConfig::with_events(tx);
            assert!(config.enabled);
            assert!(config.breakpoints.is_empty());
            assert_eq!(config.step_mode, StepMode::Run);
            assert!(config.event_sender.is_some());
        }
        
        #[test]
        fn test_add_breakpoint() {
            let mut config = DebugConfig::default();
            
            config.add_breakpoint(10);
            assert_eq!(config.breakpoints.len(), 1);
            assert!(config.breakpoints.contains(&10));
            
            // Adding same breakpoint again shouldn't duplicate
            config.add_breakpoint(10);
            assert_eq!(config.breakpoints.len(), 1);
            
            config.add_breakpoint(20);
            assert_eq!(config.breakpoints.len(), 2);
            assert!(config.breakpoints.contains(&20));
        }
        
        #[test]
        fn test_remove_breakpoint() {
            let mut config = DebugConfig::default();
            config.add_breakpoint(10);
            config.add_breakpoint(20);
            config.add_breakpoint(30);
            
            config.remove_breakpoint(20);
            assert_eq!(config.breakpoints.len(), 2);
            assert!(config.breakpoints.contains(&10));
            assert!(!config.breakpoints.contains(&20));
            assert!(config.breakpoints.contains(&30));
            
            // Removing non-existent breakpoint should do nothing
            config.remove_breakpoint(40);
            assert_eq!(config.breakpoints.len(), 2);
        }
        
        #[test]
        fn test_should_break() {
            let mut config = DebugConfig::default();
            config.add_breakpoint(10);
            config.add_breakpoint(20);
            
            assert!(config.should_break(10));
            assert!(config.should_break(20));
            assert!(!config.should_break(15));
            assert!(!config.should_break(0));
        }
        
        #[test]
        fn test_send_event() {
            let (tx, mut rx) = mpsc::unbounded_channel();
            let config = DebugConfig::with_events(tx);
            
            let event = VMDebugEvent::StackPush {
                value: Value::Integer(42),
            };
            config.send_event(event.clone());
            
            // Verify event was sent
            let received = rx.try_recv().unwrap();
            match received {
                VMDebugEvent::StackPush { value } => {
                    assert_eq!(value, Value::Integer(42));
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_send_event_no_sender() {
            let config = DebugConfig::default();
            
            // Should not panic even without sender
            config.send_event(VMDebugEvent::StackPush {
                value: Value::Integer(42),
            });
        }
    }
    
    mod step_mode {
        use super::*;
        
        #[test]
        fn test_step_mode_equality() {
            assert_eq!(StepMode::Run, StepMode::Run);
            assert_eq!(StepMode::Step, StepMode::Step);
            assert_eq!(StepMode::StepOver, StepMode::StepOver);
            assert_eq!(StepMode::StepOut, StepMode::StepOut);
            
            assert_ne!(StepMode::Run, StepMode::Step);
            assert_ne!(StepMode::Step, StepMode::StepOver);
        }
        
        #[test]
        fn test_step_mode_copy() {
            let mode1 = StepMode::Step;
            let mode2 = mode1; // Copy
            assert_eq!(mode1, mode2);
        }
    }
    
    mod debug_events {
        use super::*;
        
        #[test]
        fn test_pre_instruction_event() {
            let event = VMDebugEvent::PreInstruction {
                pc: 42,
                instruction: Instruction::new(Opcode::Add),
                stack_size: 5,
            };
            
            match event {
                VMDebugEvent::PreInstruction { pc, instruction, stack_size } => {
                    assert_eq!(pc, 42);
                    assert_eq!(instruction.opcode, Opcode::Add);
                    assert_eq!(stack_size, 5);
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_post_instruction_event() {
            let event = VMDebugEvent::PostInstruction {
                pc: 43,
                stack_size: 4,
                stack_top: Some(Value::Integer(100)),
            };
            
            match event {
                VMDebugEvent::PostInstruction { pc, stack_size, stack_top } => {
                    assert_eq!(pc, 43);
                    assert_eq!(stack_size, 4);
                    assert_eq!(stack_top, Some(Value::Integer(100)));
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_function_call_event() {
            let event = VMDebugEvent::FunctionCall {
                name: Some("factorial".to_string()),
                arg_count: 1,
                call_depth: 3,
            };
            
            match event {
                VMDebugEvent::FunctionCall { name, arg_count, call_depth } => {
                    assert_eq!(name, Some("factorial".to_string()));
                    assert_eq!(arg_count, 1);
                    assert_eq!(call_depth, 3);
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_function_return_event() {
            let event = VMDebugEvent::FunctionReturn {
                value: Value::Integer(120),
                call_depth: 2,
            };
            
            match event {
                VMDebugEvent::FunctionReturn { value, call_depth } => {
                    assert_eq!(value, Value::Integer(120));
                    assert_eq!(call_depth, 2);
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_variable_bind_event() {
            let event = VMDebugEvent::VariableBind {
                name: "x".to_string(),
                value: Value::String("hello".to_string()),
                is_global: true,
            };
            
            match event {
                VMDebugEvent::VariableBind { name, value, is_global } => {
                    assert_eq!(name, "x");
                    assert_eq!(value, Value::String("hello".to_string()));
                    assert!(is_global);
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_stack_push_event() {
            let event = VMDebugEvent::StackPush {
                value: Value::List(vec![Value::Integer(1), Value::Integer(2)]),
            };
            
            match event {
                VMDebugEvent::StackPush { value } => {
                    match value {
                        Value::List(items) => {
                            assert_eq!(items.len(), 2);
                            assert_eq!(items[0], Value::Integer(1));
                            assert_eq!(items[1], Value::Integer(2));
                        },
                        _ => panic!("Expected list value"),
                    }
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_stack_pop_event() {
            let event = VMDebugEvent::StackPop {
                value: Value::Boolean(true),
            };
            
            match event {
                VMDebugEvent::StackPop { value } => {
                    assert_eq!(value, Value::Boolean(true));
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_error_event() {
            let event = VMDebugEvent::Error {
                message: "Division by zero".to_string(),
                pc: Some(100),
            };
            
            match event {
                VMDebugEvent::Error { message, pc } => {
                    assert_eq!(message, "Division by zero");
                    assert_eq!(pc, Some(100));
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_breakpoint_event() {
            let event = VMDebugEvent::Breakpoint {
                pc: 50,
            };
            
            match event {
                VMDebugEvent::Breakpoint { pc } => {
                    assert_eq!(pc, 50);
                },
                _ => panic!("Wrong event type"),
            }
        }
        
        #[test]
        fn test_event_clone() {
            let events = vec![
                VMDebugEvent::PreInstruction {
                    pc: 1,
                    instruction: Instruction::new(Opcode::Push),
                    stack_size: 0,
                },
                VMDebugEvent::StackPush {
                    value: Value::Integer(42),
                },
                VMDebugEvent::Error {
                    message: "test error".to_string(),
                    pc: None,
                },
            ];
            
            for event in events {
                let cloned = event.clone();
                // Can't directly compare events due to lack of PartialEq
                // But clone should work without panicking
                match (&event, &cloned) {
                    (VMDebugEvent::PreInstruction { pc: pc1, .. }, 
                     VMDebugEvent::PreInstruction { pc: pc2, .. }) => {
                        assert_eq!(pc1, pc2);
                    },
                    (VMDebugEvent::StackPush { value: v1 },
                     VMDebugEvent::StackPush { value: v2 }) => {
                        assert_eq!(v1, v2);
                    },
                    (VMDebugEvent::Error { message: m1, pc: p1 },
                     VMDebugEvent::Error { message: m2, pc: p2 }) => {
                        assert_eq!(m1, m2);
                        assert_eq!(p1, p2);
                    },
                    _ => {},
                }
            }
        }
    }
    
    #[tokio::test]
    async fn test_event_channel_integration() {
        let (tx, mut rx) = mpsc::unbounded_channel();
        let config = DebugConfig::with_events(tx);
        
        // Send multiple events
        let events = vec![
            VMDebugEvent::PreInstruction {
                pc: 0,
                instruction: Instruction::with_arg(Opcode::Push, 0),
                stack_size: 0,
            },
            VMDebugEvent::StackPush {
                value: Value::Integer(42),
            },
            VMDebugEvent::PostInstruction {
                pc: 0,
                stack_size: 1,
                stack_top: Some(Value::Integer(42)),
            },
        ];
        
        for event in &events {
            config.send_event(event.clone());
        }
        
        // Receive and verify events
        for expected in events {
            let received = rx.recv().await.expect("Should receive event");
            // Compare key fields based on event type
            match (expected, received) {
                (VMDebugEvent::PreInstruction { pc: pc1, stack_size: s1, .. },
                 VMDebugEvent::PreInstruction { pc: pc2, stack_size: s2, .. }) => {
                    assert_eq!(pc1, pc2);
                    assert_eq!(s1, s2);
                },
                (VMDebugEvent::StackPush { value: v1 },
                 VMDebugEvent::StackPush { value: v2 }) => {
                    assert_eq!(v1, v2);
                },
                (VMDebugEvent::PostInstruction { pc: pc1, stack_size: s1, stack_top: t1 },
                 VMDebugEvent::PostInstruction { pc: pc2, stack_size: s2, stack_top: t2 }) => {
                    assert_eq!(pc1, pc2);
                    assert_eq!(s1, s2);
                    assert_eq!(t1, t2);
                },
                _ => panic!("Event type mismatch"),
            }
        }
    }
    
    #[test]
    fn test_debug_config_modifications() {
        let mut config = DebugConfig::default();
        
        // Test enabling/disabling
        assert!(!config.enabled);
        config.enabled = true;
        assert!(config.enabled);
        
        // Test changing step mode
        assert_eq!(config.step_mode, StepMode::Run);
        config.step_mode = StepMode::Step;
        assert_eq!(config.step_mode, StepMode::Step);
        
        // Test breakpoint operations
        config.add_breakpoint(10);
        config.add_breakpoint(20);
        config.add_breakpoint(30);
        assert_eq!(config.breakpoints.len(), 3);
        
        config.remove_breakpoint(20);
        assert_eq!(config.breakpoints.len(), 2);
        assert!(!config.should_break(20));
        assert!(config.should_break(10));
        assert!(config.should_break(30));
    }
}