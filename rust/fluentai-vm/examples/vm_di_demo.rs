//! Example demonstrating VM dependency injection

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_di::prelude::*;
use fluentai_effects::provider::EffectHandlerProvider;
use fluentai_effects::{EffectHandler, EffectResult, EffectType};
use fluentai_vm::di::*;
use fluentai_vm::{
    bytecode::{BytecodeChunk, Instruction, Opcode},
    Bytecode,
};
use std::sync::Arc;

fn main() -> Result<()> {
    println!("=== VM Dependency Injection Demo ===\n");

    // 1. Basic DI setup
    basic_di_demo()?;

    // 2. Custom effect handlers
    custom_effects_demo()?;

    // 3. Development vs production configurations
    config_demo()?;

    Ok(())
}

fn basic_di_demo() -> Result<()> {
    println!("1. Basic DI Setup");
    println!("----------------");

    // Create DI container with VM services
    let mut builder = ContainerBuilder::new();
    builder.register_vm_services();

    let container = Arc::new(builder.build());

    // Create VM provider
    let provider = ContainerVMProvider::new(container);

    // Create simple bytecode
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    // Add constant and load it
    let const_idx = chunk.add_constant(Value::Integer(42));
    chunk.add_instruction(Instruction::with_arg(Opcode::PushConst, const_idx));
    chunk.add_instruction(Instruction::new(Opcode::Return));

    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;

    // Create VM through DI
    let vm_builder = provider.create_vm_builder()?;
    let mut vm = vm_builder.with_bytecode(bytecode).build()?;

    let result = vm.run()?;
    println!("Result: {:?}", result);
    println!();

    Ok(())
}

fn custom_effects_demo() -> Result<()> {
    println!("2. Custom Effects Demo");
    println!("--------------------");

    // Create custom effect handler
    struct LoggingHandler;

    impl EffectHandler for LoggingHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::IO
        }

        fn handle_sync(
            &self,
            operation: &str,
            args: &[fluentai_core::value::Value],
        ) -> EffectResult {
            println!("  [EFFECT] {} called with {:?}", operation, args);
            Ok(fluentai_core::value::Value::Nil)
        }
    }

    // Create DI container with custom effect handler
    let mut builder = ContainerBuilder::new();
    builder.register_vm_services();

    // Register effect handler provider
    let effect_provider = EffectHandlerProvider::new();
    effect_provider.register_factory(EffectType::IO, || {
        Arc::new(LoggingHandler) as Arc<dyn EffectHandler>
    });
    builder.register_instance(Arc::new(effect_provider));

    let container = Arc::new(builder.build());
    let provider = ContainerVMProvider::new(container);

    // Create bytecode that uses effects
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("effects".to_string()));

    // Add constants
    let msg_idx = chunk.add_constant(Value::String("Hello, DI!".to_string()));
    let _op_idx = chunk.add_constant(Value::String("print".to_string()));

    // For now, just return the message without the effect
    // (Effect opcode implementation may require different stack arrangement)
    chunk.add_instruction(Instruction::with_arg(Opcode::PushConst, msg_idx));
    chunk.add_instruction(Instruction::new(Opcode::Return));

    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;

    let vm_builder = provider.create_vm_builder()?;
    let mut vm = vm_builder.with_bytecode(bytecode).build()?;

    let result = vm.run()?;
    println!("Result: {:?}", result);
    println!();

    Ok(())
}

fn config_demo() -> Result<()> {
    println!("3. Configuration Demo");
    println!("-------------------");

    // Development configuration
    println!("Development VM:");
    {
        let mut builder = ContainerBuilder::new();
        builder.register_vm_services_with_config(DevelopmentVMConfig::default());

        let container = Arc::new(builder.build());
        let provider = ContainerVMProvider::new(container);

        let bytecode = create_test_bytecode();
        let vm_builder = provider.create_vm_builder()?;
        let mut vm = vm_builder.with_bytecode(bytecode).build()?;

        println!("  Running with trace enabled...");
        let result = vm.run()?;
        println!("  Result: {:?}", result);
    }

    println!("\nProduction VM:");
    {
        let mut builder = ContainerBuilder::new();
        builder.register_vm_services_with_config(ProductionVMConfig::default());

        let container = Arc::new(builder.build());
        let provider = ContainerVMProvider::new(container);

        let bytecode = create_test_bytecode();
        let vm_builder = provider.create_vm_builder()?;
        let mut vm = vm_builder.with_bytecode(bytecode).build()?;

        println!("  Running without trace...");
        let result = vm.run()?;
        println!("  Result: {:?}", result);
    }

    println!();

    Ok(())
}

fn create_test_bytecode() -> Bytecode {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("arithmetic".to_string()));

    // Simple arithmetic: 10 + 32
    let const1 = chunk.add_constant(Value::Integer(10));
    let const2 = chunk.add_constant(Value::Integer(32));

    chunk.add_instruction(Instruction::with_arg(Opcode::PushConst, const1));
    chunk.add_instruction(Instruction::with_arg(Opcode::PushConst, const2));
    chunk.add_instruction(Instruction::new(Opcode::Add));
    chunk.add_instruction(Instruction::new(Opcode::Return));

    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;

    bytecode
}
