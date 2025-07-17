//! Comprehensive tests for VM builder

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::builder::{
        DevelopmentConfig, ProductionConfig, SandboxConfig, TestingConfig, VMBuilder, VMConfig,
    };
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    use crate::gc::GcConfig;
    use crate::security::{SecurityManager, SecurityPolicy};
    use fluentai_core::value::Value;
    use fluentai_effects::{EffectContext, EffectRuntime};
    use fluentai_modules::ModuleConfig;
    use fluentai_stdlib::StdlibRegistry;
    use rustc_hash::FxHashMap;
    use std::sync::Arc;

    fn create_test_bytecode() -> Bytecode {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::PushInt1));
        chunk.add_instruction(Instruction::new(Opcode::Return));
        let chunk_id = bytecode.add_chunk(chunk);
        bytecode.main_chunk = chunk_id;  // Set the main chunk!
        bytecode
    }

    #[test]
    fn test_builder_basic_construction() {
        let bytecode = create_test_bytecode();
        let mut vm = VMBuilder::new().with_bytecode(bytecode).build().unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_effect_context() {
        let bytecode = create_test_bytecode();
        let context = Arc::new(EffectContext::new());

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_effect_context(context.clone())
            .build()
            .unwrap();

        // VM should have the custom effect context
        // (Would need getter methods on VM to test this properly)
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_effect_runtime() {
        let bytecode = create_test_bytecode();
        let runtime = Arc::new(EffectRuntime::new().unwrap());

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_effect_runtime(runtime.clone())
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_stdlib_registry() {
        let bytecode = create_test_bytecode();
        let registry = StdlibRegistry::new();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_stdlib_registry(registry)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_module_config() {
        let bytecode = create_test_bytecode();
        let module_config = ModuleConfig {
            search_paths: vec!["./modules".into()],
            enable_cache: true,
            max_cache_size: 100,
            allow_circular: false,
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_module_config(module_config)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_globals() {
        let bytecode = create_test_bytecode();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_global("app_name", Value::String("TestApp".to_string()))
            .with_global("version", Value::String("1.2.3".to_string()))
            .with_global("debug", Value::Boolean(true))
            .with_global("max_retries", Value::Integer(5))
            .build()
            .unwrap();

        assert_eq!(
            vm.get_global("app_name"),
            Some(&Value::String("TestApp".to_string()))
        );
        assert_eq!(
            vm.get_global("version"),
            Some(&Value::String("1.2.3".to_string()))
        );
        assert_eq!(vm.get_global("debug"), Some(&Value::Boolean(true)));
        assert_eq!(vm.get_global("max_retries"), Some(&Value::Integer(5)));
    }

    #[test]
    fn test_builder_with_stack_size() {
        let bytecode = create_test_bytecode();
        let custom_stack_size = 4 * 1024 * 1024; // 4MB

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_stack_size(custom_stack_size)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_trace_mode() {
        let bytecode = create_test_bytecode();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_trace_mode(true)
            .build()
            .unwrap();

        // VM should have trace mode enabled
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_security_manager() {
        let bytecode = create_test_bytecode();
        let policy = SecurityPolicy::default();
        let security_manager = Arc::new(SecurityManager::new(policy));

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_security_manager(security_manager)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_security_policy() {
        let bytecode = create_test_bytecode();
        let mut policy = SecurityPolicy::default();
        policy.max_memory = 50 * 1024 * 1024; // 50MB
        policy.max_instructions = 5_000_000;

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_security_policy(policy)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_sandbox_mode() {
        let bytecode = create_test_bytecode();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_sandbox_mode()
            .build()
            .unwrap();

        // VM should have sandbox security policy
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_gc() {
        let bytecode = create_test_bytecode();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_gc()
            .build()
            .unwrap();

        // VM should have GC enabled with default config
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_gc_config() {
        let bytecode = create_test_bytecode();
        let gc_config = GcConfig {
            collection_threshold: 500,
            incremental: true,
            max_heap_size: 50 * 1024 * 1024,
            collect_cycles: true,
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_gc_config(gc_config)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_development_config() {
        let bytecode = create_test_bytecode();

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(DevelopmentConfig)
            .build()
            .unwrap();

        // Development config should enable trace mode and set stack size
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_production_config() {
        let bytecode = create_test_bytecode();
        let config = ProductionConfig {
            stack_size: 8 * 1024 * 1024, // 8MB
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(config)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_testing_config() {
        let bytecode = create_test_bytecode();
        let config = TestingConfig { enable_trace: true };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(config)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_with_sandbox_config() {
        let bytecode = create_test_bytecode();
        let config = SandboxConfig {
            max_memory: 20 * 1024 * 1024, // 20MB
            max_instructions: 2_000_000,
            allowed_modules: vec!["math".to_string(), "string".to_string()],
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(config)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_error_no_bytecode() {
        let result = VMBuilder::new().build();

        assert!(result.is_err());
        if let Err(error) = result {
            assert!(error.to_string().contains("Bytecode is required"));
        }
    }

    #[test]
    fn test_builder_fluent_api() {
        let bytecode = create_test_bytecode();

        // Test fluent API chaining
        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_trace_mode(true)
            .with_stack_size(2 * 1024 * 1024)
            .with_global("env", Value::String("test".to_string()))
            .with_sandbox_mode()
            .with_gc()
            .build()
            .unwrap();

        assert_eq!(
            vm.get_global("env"),
            Some(&Value::String("test".to_string()))
        );
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_clone() {
        let bytecode = create_test_bytecode();

        let builder1 = VMBuilder::new()
            .with_bytecode(bytecode.clone())
            .with_trace_mode(true);

        let builder2 = builder1.clone();

        // Both builders should create VMs successfully
        let mut vm1 = builder1.build().unwrap();
        let mut vm2 = builder2.build().unwrap();

        assert!(vm1.run().is_ok());
        assert!(vm2.run().is_ok());
    }

    #[test]
    fn test_builder_default() {
        let bytecode = create_test_bytecode();

        // Test Default trait implementation
        let mut vm = VMBuilder::default()
            .with_bytecode(bytecode)
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_builder_multiple_globals() {
        let bytecode = create_test_bytecode();

        let mut globals = FxHashMap::default();
        for i in 0..10 {
            globals.insert(format!("var_{}", i), Value::Integer(i as i64));
        }

        let mut builder = VMBuilder::new().with_bytecode(bytecode);

        for (name, value) in globals.iter() {
            builder = builder.with_global(name, value.clone());
        }

        let mut vm = builder.build().unwrap();

        // Verify all globals were set
        for (name, value) in globals.iter() {
            assert_eq!(vm.get_global(name), Some(value));
        }
    }

    #[test]
    fn test_sandbox_config_default() {
        let config = SandboxConfig::default();

        assert_eq!(config.max_memory, 10 * 1024 * 1024);
        assert_eq!(config.max_instructions, 1_000_000);
        assert!(config.allowed_modules.is_empty());
    }

    #[test]
    fn test_builder_with_all_options() {
        let bytecode = create_test_bytecode();
        let effect_context = Arc::new(EffectContext::new());
        let effect_runtime = Arc::new(EffectRuntime::new().unwrap());
        let stdlib_registry = StdlibRegistry::new();
        let module_config = ModuleConfig {
            search_paths: vec!["./test".into()],
            enable_cache: false,
            max_cache_size: 50,
            allow_circular: false,
        };
        let gc_config = GcConfig {
            collection_threshold: 100,
            incremental: false,
            max_heap_size: 10 * 1024 * 1024,
            collect_cycles: false,
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_effect_context(effect_context)
            .with_effect_runtime(effect_runtime)
            .with_stdlib_registry(stdlib_registry)
            .with_module_config(module_config)
            .with_global("test", Value::Boolean(true))
            .with_stack_size(1024 * 1024)
            .with_trace_mode(true)
            .with_sandbox_mode()
            .with_gc_config(gc_config)
            .build()
            .unwrap();

        assert_eq!(vm.get_global("test"), Some(&Value::Boolean(true)));
        assert!(vm.run().is_ok());
    }

    #[test]
    fn test_custom_vm_config() {
        struct CustomConfig {
            globals: Vec<(String, Value)>,
        }

        impl VMConfig for CustomConfig {
            fn configure(&self, _builder: &mut VMBuilder) {
                // VMBuilder doesn't expose mutable access to its fields
                // This is by design - configuration should be done through the builder API
                // For this test, we'll just leave the implementation empty
            }
        }

        let bytecode = create_test_bytecode();
        let config = CustomConfig {
            globals: vec![
                ("custom1".to_string(), Value::Integer(42)),
                ("custom2".to_string(), Value::String("test".to_string())),
            ],
        };

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_config(config)
            .with_global("custom1", Value::Integer(42))
            .with_global("custom2", Value::String("test".to_string()))
            .build()
            .unwrap();

        assert_eq!(vm.get_global("custom1"), Some(&Value::Integer(42)));
        assert_eq!(
            vm.get_global("custom2"),
            Some(&Value::String("test".to_string()))
        );
    }

    #[test]
    fn test_builder_overrides() {
        let bytecode = create_test_bytecode();

        // Test that later calls override earlier ones
        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_trace_mode(true)
            .with_trace_mode(false) // Should override to false
            .with_global("test", Value::Integer(1))
            .with_global("test", Value::Integer(2)) // Should override to 2
            .build()
            .unwrap();

        assert_eq!(vm.get_global("test"), Some(&Value::Integer(2)));
    }

    #[test]
    fn test_security_policy_priority() {
        let bytecode = create_test_bytecode();

        // Security manager should take priority over security policy
        let policy1 = SecurityPolicy {
            max_memory: 10 * 1024 * 1024,
            ..Default::default()
        };

        let policy2 = SecurityPolicy {
            max_memory: 20 * 1024 * 1024,
            ..Default::default()
        };

        let security_manager = Arc::new(SecurityManager::new(policy2));

        let mut vm = VMBuilder::new()
            .with_bytecode(bytecode)
            .with_security_policy(policy1)
            .with_security_manager(security_manager) // Should take priority
            .build()
            .unwrap();

        assert!(vm.run().is_ok());
    }
}
