//! Main UI compiler implementation

use crate::error::{CompilerError, Result};
use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_parser::parse;
use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use indexmap::IndexMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OutputFormat {
    VanillaJS,
    React,
    Vue,
    WebComponent,
    Preact,
}

impl Default for OutputFormat {
    fn default() -> Self {
        OutputFormat::VanillaJS
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompilerOptions {
    pub output_format: OutputFormat,
    pub minify: bool,
    pub source_maps: bool,
    pub module_format: String, // esm, commonjs, umd
    pub runtime_import: String,
    pub optimize_bundle_size: bool,
    pub tree_shake: bool,
    pub target_browsers: Vec<String>,
    pub enable_ui_optimization: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            output_format: OutputFormat::default(),
            minify: false,
            source_maps: true,
            module_format: "esm".to_string(),
            runtime_import: "@fluentai/runtime".to_string(),
            optimize_bundle_size: true,
            tree_shake: true,
            target_browsers: vec!["defaults".to_string()],
            enable_ui_optimization: true,
        }
    }
}

pub struct UICompiler {
    options: CompilerOptions,
    imports: HashSet<String>,
    helpers: HashSet<String>,
    component_registry: IndexMap<String, ComponentDefinition>,
}

#[derive(Debug, Clone)]
struct ComponentDefinition {
    name: String,
    props: Vec<String>,
    template: NodeId,
    styles: Option<String>,
    reactive_state: Vec<String>,
}

impl UICompiler {
    /// Create a new UI compiler
    pub fn new(options: CompilerOptions) -> Result<Self> {
        Ok(Self {
            options,
            imports: HashSet::new(),
            helpers: HashSet::new(),
            component_registry: IndexMap::new(),
        })
    }
    
    /// Compile FluentAi UI code to JavaScript
    pub fn compile(&mut self, source: &str) -> Result<String> {
        // Parse the source
        let graph = parse(source)
            .map_err(|e| CompilerError::ParseError(e.to_string()))?;
        
        // Optimize if enabled
        let optimized_graph = if self.options.enable_ui_optimization {
            self.optimize_ui_graph(graph)?
        } else {
            graph
        };
        
        // Extract components and compile
        self.extract_components(&optimized_graph)?;
        let js_code = self.compile_graph(&optimized_graph)?;
        
        // Generate imports
        let imports = self.generate_imports();
        
        // Generate helper functions
        let helpers = self.generate_helpers();
        
        // Generate component definitions
        let components = self.generate_components()?;
        
        // Combine everything
        let mut output = String::new();
        output.push_str(&imports);
        output.push_str("\n\n");
        output.push_str(&helpers);
        output.push_str("\n\n");
        output.push_str(&components);
        output.push_str("\n\n");
        output.push_str(&js_code);
        
        // Post-process based on output format
        self.post_process(output)
    }
    
    /// Optimize the UI graph for better performance
    fn optimize_ui_graph(&self, graph: Graph) -> Result<Graph> {
        // Use the FluentAi optimizer
        let mut optimizer = fluentai_optimizer::GraphOptimizer::new();
        optimizer.optimize(&graph)
            .map_err(|e| CompilerError::OptimizationError(e.to_string()))
    }
    
    /// Extract UI components from the graph
    fn extract_components(&mut self, graph: &Graph) -> Result<()> {
        // Look for UI component definitions in the graph
        for (node_id, node) in &graph.nodes {
            match node {
                Node::Application { function, args } => {
                    // Check if this is a component definition
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        if name == "ui:component" || name == "defcomponent" {
                            self.process_component_definition(*node_id, args, graph)?;
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }
    
    /// Process a component definition
    fn process_component_definition(
        &mut self,
        _node_id: NodeId,
        args: &[NodeId],
        graph: &Graph,
    ) -> Result<()> {
        if args.len() < 2 {
            return Err(CompilerError::ParseError(
                "Component definition requires name and template".to_string()
            ));
        }
        
        // Extract component name
        let name = match graph.get_node(args[0]) {
            Some(Node::Literal(lit)) => lit.to_string(),
            Some(Node::Variable { name }) => name.clone(),
            _ => return Err(CompilerError::ParseError(
                "Component name must be a string literal or variable".to_string()
            )),
        };
        
        // Extract template
        let template = args[1];
        
        // Create component definition
        let component = ComponentDefinition {
            name: name.clone(),
            props: Vec::new(), // Extract from template analysis
            template,
            styles: None,
            reactive_state: Vec::new(),
        };
        
        self.component_registry.insert(name, component);
        Ok(())
    }
    
    /// Compile the AST graph to JavaScript
    fn compile_graph(&mut self, graph: &Graph) -> Result<String> {
        // Delegate to codegen module based on output format
        match self.options.output_format {
            OutputFormat::VanillaJS => {
                crate::codegen::vanilla_js::compile_graph(graph, &mut self.imports, &mut self.helpers)
            }
            OutputFormat::React => {
                crate::codegen::react::compile_graph(graph, &mut self.imports, &mut self.helpers)
            }
            OutputFormat::Vue => {
                crate::codegen::vue::compile_graph(graph, &mut self.imports, &mut self.helpers)
            }
            OutputFormat::WebComponent => {
                crate::codegen::web_component::compile_graph(graph, &mut self.imports, &mut self.helpers)
            }
            OutputFormat::Preact => {
                // Preact uses React codegen with different imports
                self.imports.insert("preact".to_string());
                crate::codegen::react::compile_graph(graph, &mut self.imports, &mut self.helpers)
            }
        }
    }
    
    /// Generate import statements
    fn generate_imports(&self) -> String {
        let mut imports = Vec::new();
        
        // Add runtime import
        imports.push(format!(
            "import {{ FluentAiRuntime }} from '{}';",
            self.options.runtime_import
        ));
        
        // Add framework-specific imports
        match self.options.output_format {
            OutputFormat::React => {
                imports.push("import React from 'react';".to_string());
            }
            OutputFormat::Vue => {
                imports.push("import { createApp, reactive, computed, watch } from 'vue';".to_string());
            }
            OutputFormat::Preact => {
                imports.push("import { h, Component } from 'preact';".to_string());
            }
            _ => {}
        }
        
        // Add other imports
        for import in &self.imports {
            imports.push(format!("import '{}';", import));
        }
        
        imports.join("\n")
    }
    
    /// Generate helper functions
    fn generate_helpers(&self) -> String {
        let mut helpers = Vec::new();
        
        // Add common helpers
        if self.helpers.contains("createElement") {
            helpers.push(include_str!("../runtime/helpers/createElement.js").to_string());
        }
        
        if self.helpers.contains("reactive") {
            helpers.push(include_str!("../runtime/helpers/reactive.js").to_string());
        }
        
        helpers.join("\n\n")
    }
    
    /// Generate component definitions
    fn generate_components(&self) -> Result<String> {
        let mut components = Vec::new();
        
        for (name, def) in &self.component_registry {
            let component_code = match self.options.output_format {
                OutputFormat::React => self.generate_react_component(name, def)?,
                OutputFormat::Vue => self.generate_vue_component(name, def)?,
                OutputFormat::WebComponent => self.generate_web_component(name, def)?,
                _ => self.generate_vanilla_component(name, def)?,
            };
            components.push(component_code);
        }
        
        Ok(components.join("\n\n"))
    }
    
    /// Generate a React component
    fn generate_react_component(&self, name: &str, _def: &ComponentDefinition) -> Result<String> {
        Ok(format!(
            "export function {}(props) {{\n  // Component implementation\n  return null;\n}}",
            name
        ))
    }
    
    /// Generate a Vue component
    fn generate_vue_component(&self, name: &str, _def: &ComponentDefinition) -> Result<String> {
        Ok(format!(
            "export const {} = {{\n  name: '{}',\n  template: '',\n  // Component implementation\n}};",
            name, name
        ))
    }
    
    /// Generate a Web Component
    fn generate_web_component(&self, name: &str, _def: &ComponentDefinition) -> Result<String> {
        Ok(format!(
            "class {} extends HTMLElement {{\n  // Component implementation\n}}\ncustomElements.define('{}', {});",
            name, 
            name.to_lowercase().replace("_", "-"),
            name
        ))
    }
    
    /// Generate a vanilla JS component
    fn generate_vanilla_component(&self, name: &str, _def: &ComponentDefinition) -> Result<String> {
        Ok(format!(
            "export function {}(props) {{\n  // Component implementation\n  return document.createElement('div');\n}}",
            name
        ))
    }
    
    /// Post-process the output
    fn post_process(&self, output: String) -> Result<String> {
        // Add source maps if requested
        let mut final_output = output;
        if self.options.source_maps {
            final_output.push_str("\n//# sourceMappingURL=output.js.map");
        }
        
        Ok(final_output)
    }
}