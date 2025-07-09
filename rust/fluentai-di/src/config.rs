//! Configuration-based dependency injection

use crate::builder::ContainerBuilder;
use crate::error::{DiError, DiResult};
use crate::service::ServiceLifetime;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Service configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServiceConfig {
    /// Service type name
    pub service_type: String,
    /// Implementation type name
    pub implementation_type: String,
    /// Service lifetime
    pub lifetime: ServiceLifetimeConfig,
    /// Constructor parameters
    #[serde(default)]
    pub parameters: HashMap<String, serde_json::Value>,
}

/// Service lifetime configuration
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ServiceLifetimeConfig {
    Transient,
    Scoped,
    Singleton,
}

impl From<ServiceLifetimeConfig> for ServiceLifetime {
    fn from(config: ServiceLifetimeConfig) -> Self {
        match config {
            ServiceLifetimeConfig::Transient => ServiceLifetime::Transient,
            ServiceLifetimeConfig::Scoped => ServiceLifetime::Scoped,
            ServiceLifetimeConfig::Singleton => ServiceLifetime::Singleton,
        }
    }
}

/// Container configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContainerConfig {
    /// Service configurations
    pub services: Vec<ServiceConfig>,
    /// Module configurations
    #[serde(default)]
    pub modules: Vec<String>,
}

impl ContainerConfig {
    /// Load configuration from TOML string
    pub fn from_toml(toml_str: &str) -> DiResult<Self> {
        toml::from_str(toml_str)
            .map_err(|e| DiError::ConfigError(format!("Failed to parse TOML: {}", e)))
    }

    /// Load configuration from JSON string
    pub fn from_json(json_str: &str) -> DiResult<Self> {
        serde_json::from_str(json_str)
            .map_err(|e| DiError::ConfigError(format!("Failed to parse JSON: {}", e)))
    }

    /// Apply configuration to a container builder
    pub fn apply_to_builder(&self, builder: &mut ContainerBuilder) -> DiResult<()> {
        use crate::registry::{RegistryContainerBuilderExt, ServiceRegistry};

        let registry = ServiceRegistry::global();

        // Register interface mappings
        for service in &self.services {
            if service.service_type != service.implementation_type {
                registry.register_interface(&service.service_type, &service.implementation_type)?;
            }
        }

        // Register services
        for service in &self.services {
            builder.register_from_registry(&service.service_type, service.lifetime.into())?;
        }

        Ok(())
    }
}

/// Configuration builder with validation
pub struct ConfigBuilder {
    services: Vec<ServiceConfig>,
    modules: Vec<String>,
}

impl ConfigBuilder {
    /// Create a new configuration builder
    pub fn new() -> Self {
        Self {
            services: Vec::new(),
            modules: Vec::new(),
        }
    }

    /// Add a service configuration
    pub fn add_service(&mut self, config: ServiceConfig) -> &mut Self {
        self.services.push(config);
        self
    }

    /// Add a module
    pub fn add_module(&mut self, module_name: String) -> &mut Self {
        self.modules.push(module_name);
        self
    }

    /// Build the configuration
    pub fn build(self) -> ContainerConfig {
        ContainerConfig {
            services: self.services,
            modules: self.modules,
        }
    }
}

impl Default for ConfigBuilder {
    fn default() -> Self {
        Self::new()
    }
}

// Example configuration file format:
// ```toml
// [[services]]
// service_type = "ILogger"
// implementation_type = "ConsoleLogger"
// lifetime = "singleton"
//
// [[services]]
// service_type = "IDatabase"
// implementation_type = "PostgresDatabase"
// lifetime = "scoped"
//
// [services.parameters]
// connection_string = "postgres://localhost/mydb"
//
// modules = ["core", "web", "data"]
// ```
