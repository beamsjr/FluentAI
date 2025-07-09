//! Example demonstrating effect handler integration with DI container

#[cfg(feature = "di")]
use fluentai_di::prelude::*;
#[cfg(feature = "di")]
use fluentai_effects::{
    provider::di::register_effect_services, EffectContext, EffectHandler, EffectHandlerProvider,
};
use std::sync::Arc;

#[cfg(feature = "di")]
fn main() -> anyhow::Result<()> {
    println!("=== Effect Handlers with DI Container ===\n");

    // Create DI container with effect services
    let mut builder = ContainerBuilder::new();

    // Register effect services
    register_effect_services(&mut builder)?;

    let container = builder.build();

    // Resolve effect provider
    let provider = container.resolve::<Arc<EffectHandlerProvider>>()?;
    println!("✓ Resolved EffectHandlerProvider from DI container");

    // Create effect context from provider
    let context = provider.create_context()?;
    println!("✓ Created EffectContext from provider");

    // Test that default handlers are available
    use fluentai_core::value::Value;
    use fluentai_effects::EffectType;

    let result = context.perform_sync(
        EffectType::IO,
        "print",
        &[Value::String("Hello from DI!".to_string())],
    )?;
    println!("✓ IO handler works: {:?}", result);

    // Create application service with context
    let app_service = Arc::new(ApplicationService::new(context.clone()));
    println!("✓ Created ApplicationService that depends on EffectContext");

    app_service.run()?;

    // Demonstrate hierarchical providers
    println!("\n=== Hierarchical Providers ===");

    // Create child provider
    let child_provider = Arc::new(EffectHandlerProvider::create_child(provider.clone()));

    // Register custom handler in child provider
    let custom_io = Arc::new(CustomIOHandler::new());
    child_provider.register_singleton(custom_io);

    // Create new context from child provider
    let child_context = child_provider.create_context()?;

    // Create service with child context
    let child_app = Arc::new(ApplicationService::new(child_context));
    println!("✓ Created ApplicationService with hierarchical provider");

    child_app.run()?;

    // Demonstrate module-based registration
    println!("\n=== Module-based Registration ===");

    let mut module_builder = ContainerBuilder::new();

    // Create effect module
    let effect_module = EffectModule::new();
    effect_module.register(&mut module_builder)?;

    // Create application module
    let app_module = ApplicationModule::new();
    app_module.register(&mut module_builder)?;

    let module_container = module_builder.build();

    // Get provider and create context
    let module_provider = module_container.resolve::<Arc<EffectHandlerProvider>>()?;
    let module_context = module_provider.create_context()?;
    let module_app = Arc::new(ApplicationService::new(module_context));
    println!("✓ Created services from modular registration");

    module_app.run()?;

    Ok(())
}

#[cfg(not(feature = "di"))]
fn main() {
    println!("This example requires the 'di' feature to be enabled.");
    println!("Run with: cargo run --example di_integration --features di");
}

/// Application service that depends on effect context
#[cfg(feature = "di")]
struct ApplicationService {
    effect_context: EffectContext,
}

#[cfg(feature = "di")]
impl ApplicationService {
    fn new(effect_context: EffectContext) -> Self {
        Self { effect_context }
    }

    fn run(&self) -> anyhow::Result<()> {
        use fluentai_core::value::Value;
        use fluentai_effects::EffectType;

        println!("  Running application service...");

        // Use various effects
        self.effect_context.perform_sync(
            EffectType::IO,
            "print",
            &[Value::String("  - Application is running".to_string())],
        )?;

        // Get current time
        let time = self
            .effect_context
            .perform_sync(EffectType::Time, "now", &[])?;
        println!("  - Current time: {:?}", time);

        // Generate random number
        let random = self
            .effect_context
            .perform_sync(EffectType::Random, "float", &[])?;
        println!("  - Random number: {:?}", random);

        Ok(())
    }
}

/// Custom IO handler for scoped container
#[cfg(feature = "di")]
struct CustomIOHandler {
    prefix: String,
}

#[cfg(feature = "di")]
impl CustomIOHandler {
    fn new() -> Self {
        Self {
            prefix: "[SCOPED]".to_string(),
        }
    }
}

#[cfg(feature = "di")]
#[async_trait::async_trait]
impl EffectHandler for CustomIOHandler {
    fn effect_type(&self) -> fluentai_effects::EffectType {
        fluentai_effects::EffectType::IO
    }

    fn handle_sync(
        &self,
        operation: &str,
        args: &[fluentai_core::value::Value],
    ) -> fluentai_effects::EffectResult {
        use fluentai_core::value::Value;

        match operation {
            "print" => {
                if let Some(Value::String(s)) = args.get(0) {
                    println!("{} {}", self.prefix, s);
                }
                Ok(Value::Nil)
            }
            _ => {
                // Delegate to default handler
                let default = fluentai_effects::IOHandler::new();
                default.handle_sync(operation, args)
            }
        }
    }
}

/// Effect module for DI registration
#[cfg(feature = "di")]
struct EffectModule;

#[cfg(feature = "di")]
impl EffectModule {
    fn new() -> Self {
        Self
    }

    fn register(&self, builder: &mut ContainerBuilder) -> anyhow::Result<()> {
        use fluentai_effects::EffectHandlerBuilder;

        // Register custom effect provider
        builder.register_singleton(|| {
            let provider = EffectHandlerBuilder::new()
                .with_defaults()
                .with_handler(Arc::new(CustomIOHandler::new()))
                .build();
            Arc::new(provider)
        });

        Ok(())
    }
}

/// Application module for DI registration
#[cfg(feature = "di")]
struct ApplicationModule;

#[cfg(feature = "di")]
impl ApplicationModule {
    fn new() -> Self {
        Self
    }

    fn register(&self, _builder: &mut ContainerBuilder) -> anyhow::Result<()> {
        // No services to register here since we create ApplicationService manually
        Ok(())
    }
}
