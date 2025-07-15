//! WebAssembly bindings for FluentAI VM integration
//! 
//! This module provides WASM bindings to compile and execute FluentAI code (.flc files)
//! in the browser, with full integration to the Continuum UI renderer.

use wasm_bindgen::prelude::*;
use web_sys::HtmlCanvasElement;
use crate::RendererBridge;
use std::sync::{Arc, Mutex};
use std::cell::RefCell;
use std::rc::Rc;
use rustc_hash::FxHashMap;

// Re-export types from the VM
use fluentai_vm::{VM, VMError, Value, Bytecode, Compiler, CompilerOptions};
// use fluentai_effects::{EffectRuntime, EffectContext}; // TODO: Make effects WASM-compatible
use fluentai_modules::{ModuleLoader, ModuleConfig};

/// WASM-exposed FluentAI VM instance
#[wasm_bindgen]
pub struct FluentAIVM {
    vm: Rc<RefCell<VM>>,
    renderer_bridge: RendererBridge,
    canvas: HtmlCanvasElement,
}

#[wasm_bindgen]
impl FluentAIVM {
    /// Create a new FluentAI VM instance with a canvas for rendering
    #[wasm_bindgen(constructor)]
    pub fn new(canvas: HtmlCanvasElement) -> Result<FluentAIVM, JsValue> {
        // Initialize logging
        console_error_panic_hook::set_once();
        console_log::init_with_level(log::Level::Info).ok();
        
        // Create renderer bridge
        let renderer_bridge = RendererBridge::new();
        
        // Create a dummy bytecode for initial VM (will be replaced when code is compiled)
        let dummy_bytecode = Bytecode::new();
        
        Ok(FluentAIVM {
            vm: Rc::new(RefCell::new(VM::new(dummy_bytecode))),
            renderer_bridge,
            canvas,
        })
    }
    
    /// Compile FluentAI source code
    #[wasm_bindgen]
    pub fn compile(&mut self, source: &str) -> Result<(), JsValue> {
        // First parse the source
        let ast = fluentai_parser::parse_flc(source)
            .map_err(|e| {
                // Try to extract more detailed error information
                let error_str = e.to_string();
                // Look for position information in the error
                if error_str.contains("at position") {
                    // Extract line number from position
                    let lines: Vec<&str> = source.lines().collect();
                    let mut pos = 0;
                    let mut line_num = 1;
                    let mut col_num = 1;
                    
                    // Try to find the position mentioned in the error
                    if let Some(pos_start) = error_str.find("at position ") {
                        if let Some(pos_str) = error_str[pos_start + 12..].split(' ').next() {
                            if let Ok(error_pos) = pos_str.parse::<usize>() {
                                // Calculate line and column
                                for (idx, line) in lines.iter().enumerate() {
                                    if pos + line.len() + 1 > error_pos {
                                        line_num = idx + 1;
                                        col_num = error_pos - pos + 1;
                                        break;
                                    }
                                    pos += line.len() + 1; // +1 for newline
                                }
                            }
                        }
                    }
                    
                    JsValue::from_str(&format!("Parse error at line {}, column {}: {}", line_num, col_num, error_str))
                } else {
                    JsValue::from_str(&format!("Parse error: {}", error_str))
                }
            })?;
        
        let options = CompilerOptions::default();
        let mut compiler = Compiler::with_options(options);
        
        match compiler.compile(&ast) {
            Ok(bytecode) => {
                // Create a new VM with the compiled bytecode
                let mut vm = VM::new(bytecode);
                
                // Set up effect runtime with renderer bridge
                // TODO: Re-enable when effects are WASM-compatible
                // let effect_runtime = self.create_effect_runtime();
                // vm.set_effect_runtime(Arc::new(effect_runtime));
                
                // Enable features
                vm.enable_trace();
                vm.with_sandbox_security();
                
                // Set up module loader
                let module_loader = ModuleLoader::new(ModuleConfig::default());
                vm.set_module_loader(module_loader);
                
                // Replace the VM
                *self.vm.borrow_mut() = vm;
                
                Ok(())
            }
            Err(e) => {
                Err(JsValue::from_str(&format!("Compilation error: {}", e)))
            }
        }
    }
    
    /// Execute the compiled FluentAI code
    #[wasm_bindgen]
    pub fn execute(&mut self) -> Result<JsValue, JsValue> {
        // Run the VM and get the result
        let result = {
            let mut vm = self.vm.borrow_mut();
            vm.run()
        };
        
        match result {
            Ok(value) => {
                // Convert VM value to JS value
                Ok(self.vm_value_to_js(&value))
            }
            Err(e) => {
                Err(JsValue::from_str(&format!("Runtime error: {}", e)))
            }
        }
    }
    
    /// Compile and execute FluentAI source code in one step
    #[wasm_bindgen]
    pub fn run(&mut self, source: &str) -> Result<JsValue, JsValue> {
        self.compile(source)?;
        self.execute()
    }
    
    /// Call a global function by name
    #[wasm_bindgen]
    pub fn call_function(&mut self, name: &str, args: &JsValue) -> Result<JsValue, JsValue> {
        // Convert JS args to VM values first (outside of VM borrow)
        let args_array: Vec<JsValue> = if args.is_array() {
            let array = js_sys::Array::from(args);
            array.iter().collect()
        } else if args.is_undefined() || args.is_null() {
            vec![]
        } else {
            vec![args.clone()]
        };
        
        let mut vm_args = Vec::new();
        for arg in &args_array {
            vm_args.push(self.js_value_to_vm(&arg)?);
        }
        
        // Execute the function call
        let result = {
            let mut vm = self.vm.borrow_mut();
            
            // Get the function from globals
            let func = vm.get_global(name)
                .ok_or_else(|| JsValue::from_str(&format!("Function '{}' not found", name)))?
                .clone();
            
            // Push function onto stack
            vm.push(func).map_err(|e| JsValue::from_str(&format!("Stack error: {}", e)))?;
            
            // Push arguments
            for vm_value in vm_args {
                vm.push(vm_value).map_err(|e| JsValue::from_str(&format!("Stack error: {}", e)))?;
            }
            
            // Call the function
            let arg_count = args_array.len();
            vm.call_value(arg_count).map_err(|e| JsValue::from_str(&format!("Call error: {}", e)))?;
            
            // Get the result
            vm.pop().map_err(|e| JsValue::from_str(&format!("Stack error: {}", e)))
        }?;
        
        Ok(self.vm_value_to_js(&result))
    }
    
    /// Set a global variable
    #[wasm_bindgen]
    pub fn set_global(&mut self, name: &str, value: &JsValue) -> Result<(), JsValue> {
        let vm_value = self.js_value_to_vm(value)?;
        let mut vm = self.vm.borrow_mut();
        vm.set_global(name.to_string(), vm_value);
        Ok(())
    }
    
    /// Get a global variable
    #[wasm_bindgen]
    pub fn get_global(&self, name: &str) -> Result<JsValue, JsValue> {
        let value = {
            let vm = self.vm.borrow();
            vm.get_global(name).cloned()
        };
        
        match value {
            Some(value) => Ok(self.vm_value_to_js(&value)),
            None => Err(JsValue::from_str(&format!("Global '{}' not found", name)))
        }
    }
    
    /// Get all global variables as a JS object
    #[wasm_bindgen]
    pub fn get_all_globals(&self) -> Result<JsValue, JsValue> {
        let globals = {
            let vm = self.vm.borrow();
            vm.get_globals().clone()
        };
        
        let obj = js_sys::Object::new();
        for (name, value) in globals {
            js_sys::Reflect::set(&obj, &JsValue::from_str(&name), &self.vm_value_to_js(&value))
                .map_err(|_| JsValue::from_str("Failed to set property"))?;
        }
        
        Ok(obj.into())
    }
    
    /// Render the current scene to the canvas
    #[wasm_bindgen]
    pub fn render(&self) -> Result<(), JsValue> {
        // Get the 2D context for now (until WebGL/WebGPU is fully integrated)
        let context = self.canvas
            .get_context("2d")?
            .unwrap()
            .dyn_into::<web_sys::CanvasRenderingContext2d>()?;
        
        // Clear canvas
        context.clear_rect(0.0, 0.0, self.canvas.width() as f64, self.canvas.height() as f64);
        
        // Get scene from bridge and render
        let scene = self.renderer_bridge.scene();
        let scene = scene.lock().unwrap();
        
        // Basic 2D rendering (temporary until 3D renderer is integrated)
        for renderable in scene.get_renderables() {
            use crate::primitives::Renderable;
            match renderable {
                Renderable::Rect { transform, size, color, radius } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    context.fill_rect(
                        transform.position.x as f64,
                        transform.position.y as f64,
                        size.width as f64,
                        size.height as f64,
                    );
                }
                Renderable::Text { transform, content, size, color, .. } => {
                    context.set_fill_style(&JsValue::from_str(&format!(
                        "rgba({}, {}, {}, {})",
                        (color.r * 255.0) as u8,
                        (color.g * 255.0) as u8,
                        (color.b * 255.0) as u8,
                        color.a
                    )));
                    
                    context.set_font(&format!("{}px sans-serif", size));
                    context.fill_text(
                        content,
                        transform.position.x as f64,
                        transform.position.y as f64,
                    )?;
                }
                _ => {}
            }
        }
        
        Ok(())
    }
    
    // Helper methods
    
    /* TODO: Re-enable when effects are WASM-compatible
    /// Create effect runtime with Continuum UI integration
    fn create_effect_runtime(&self) -> EffectRuntime {
        let mut runtime = EffectRuntime::default();
        
        // Register IO effect handlers
        runtime.register_handler("IO", "print", {
            let console_log = web_sys::console::log_1;
            Box::new(move |args| {
                if let Some(msg) = args.get(0).and_then(|v| v.as_string()) {
                    console_log(&JsValue::from_str(&msg));
                }
                Ok(Value::Nil)
            })
        });
        
        runtime.register_handler("IO", "println", {
            let console_log = web_sys::console::log_1;
            Box::new(move |args| {
                if let Some(msg) = args.get(0).and_then(|v| v.as_string()) {
                    console_log(&JsValue::from_str(&msg));
                }
                Ok(Value::Nil)
            })
        });
        
        // Register Continuum UI effect handlers
        let bridge = self.renderer_bridge.clone();
        runtime.register_handler("Continuum", "render", {
            let bridge = bridge.clone();
            Box::new(move |args| {
                // Convert args to JSON values
                let json_args: Vec<serde_json::Value> = args.iter()
                    .map(|v| core_value_to_json(v))
                    .collect();
                
                match bridge.handle_dom_effect("render", json_args) {
                    Ok(result) => Ok(json_to_core_value(&result)),
                    Err(e) => Err(format!("Render error: {}", e))
                }
            })
        });
        
        runtime.register_handler("Continuum", "clear", {
            let bridge = bridge.clone();
            Box::new(move |_args| {
                match bridge.handle_dom_effect("clear", vec![]) {
                    Ok(result) => Ok(json_to_core_value(&result)),
                    Err(e) => Err(format!("Clear error: {}", e))
                }
            })
        });
        
        // Register WebGL renderer effect handlers
        runtime.register_handler("WebGL", "createRenderer", {
            Box::new(move |_args| {
                // Return a placeholder renderer ID for now
                Ok(Value::String("webgl-renderer-1".to_string()))
            })
        });
        
        runtime
    }
    */
    
    /// Convert VM value to JS value
    fn vm_value_to_js(&self, value: &Value) -> JsValue {
        match value {
            Value::Nil => JsValue::NULL,
            Value::Boolean(b) => JsValue::from_bool(*b),
            Value::Integer(i) => JsValue::from_f64(*i as f64),
            Value::Float(f) => JsValue::from_f64(*f),
            Value::String(s) => JsValue::from_str(s),
            Value::Symbol(s) => JsValue::from_str(&format!(":{}", s)),
            Value::List(items) => {
                let array = js_sys::Array::new();
                for item in items {
                    array.push(&self.vm_value_to_js(item));
                }
                array.into()
            }
            Value::Vector(items) => {
                let array = js_sys::Array::new();
                for item in items {
                    array.push(&self.vm_value_to_js(item));
                }
                array.into()
            }
            Value::Map(map) => {
                let obj = js_sys::Object::new();
                for (key, val) in map {
                    js_sys::Reflect::set(&obj, &JsValue::from_str(key), &self.vm_value_to_js(val)).ok();
                }
                obj.into()
            }
            Value::Tagged { tag, values } => {
                let obj = js_sys::Object::new();
                js_sys::Reflect::set(&obj, &JsValue::from_str("_tag"), &JsValue::from_str(tag)).ok();
                let array = js_sys::Array::new();
                for val in values {
                    array.push(&self.vm_value_to_js(val));
                }
                js_sys::Reflect::set(&obj, &JsValue::from_str("_values"), &array).ok();
                obj.into()
            }
            Value::Module { name, exports } => {
                let obj = js_sys::Object::new();
                js_sys::Reflect::set(&obj, &JsValue::from_str("_module"), &JsValue::from_str(name)).ok();
                let exports_obj = js_sys::Object::new();
                for (key, val) in exports {
                    js_sys::Reflect::set(&exports_obj, &JsValue::from_str(key), &self.vm_value_to_js(val)).ok();
                }
                js_sys::Reflect::set(&obj, &JsValue::from_str("_exports"), &exports_obj).ok();
                obj.into()
            }
            _ => JsValue::from_str(&format!("<{}>", value.type_name()))
        }
    }
    
    /// Convert JS value to VM value
    fn js_value_to_vm(&self, value: &JsValue) -> Result<Value, JsValue> {
        if value.is_null() || value.is_undefined() {
            Ok(Value::Nil)
        } else if let Some(b) = value.as_bool() {
            Ok(Value::Boolean(b))
        } else if let Some(n) = value.as_f64() {
            if n.fract() == 0.0 && n >= i64::MIN as f64 && n <= i64::MAX as f64 {
                Ok(Value::Integer(n as i64))
            } else {
                Ok(Value::Float(n))
            }
        } else if let Some(s) = value.as_string() {
            Ok(Value::String(s))
        } else if js_sys::Array::is_array(value) {
            let array = js_sys::Array::from(value);
            let mut items = Vec::new();
            for item in array.iter() {
                items.push(self.js_value_to_vm(&item)?);
            }
            Ok(Value::List(items))
        } else if value.is_object() {
            let obj = js_sys::Object::from(value.clone());
            let entries = js_sys::Object::entries(&obj);
            let mut map = FxHashMap::default();
            
            for entry in entries.iter() {
                let entry_array = js_sys::Array::from(&entry);
                if entry_array.length() >= 2 {
                    let key = entry_array.get(0).as_string()
                        .ok_or_else(|| JsValue::from_str("Object key must be string"))?;
                    let val = self.js_value_to_vm(&entry_array.get(1))?;
                    map.insert(key, val);
                }
            }
            Ok(Value::Map(map))
        } else {
            Err(JsValue::from_str(&format!("Cannot convert JS value to VM value")))
        }
    }
}

/// Helper function to get value type name
fn value_type_name(value: &Value) -> &'static str {
    value.type_name()
}

/// Convert Value to JSON
fn core_value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Nil => serde_json::Value::Null,
        Value::Boolean(b) => serde_json::Value::Bool(*b),
        Value::Integer(i) => serde_json::Value::Number(serde_json::Number::from(*i)),
        Value::Float(f) => serde_json::Value::Number(
            serde_json::Number::from_f64(*f).unwrap_or(serde_json::Number::from(0))
        ),
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::List(items) => {
            let json_items: Vec<serde_json::Value> = items.iter()
                .map(core_value_to_json)
                .collect();
            serde_json::Value::Array(json_items)
        }
        Value::Map(map) => {
            let mut json_map = serde_json::Map::new();
            for (k, v) in map {
                json_map.insert(k.clone(), core_value_to_json(v));
            }
            serde_json::Value::Object(json_map)
        }
        _ => serde_json::Value::String(format!("<{:?}>", value))
    }
}

/// Convert JSON to Value
fn json_to_core_value(json: &serde_json::Value) -> Value {
    match json {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(b) => Value::Boolean(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Integer(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Float(0.0)
            }
        }
        serde_json::Value::String(s) => Value::String(s.clone()),
        serde_json::Value::Array(items) => {
            let core_items: Vec<Value> = items.iter()
                .map(json_to_core_value)
                .collect();
            Value::List(core_items)
        }
        serde_json::Value::Object(map) => {
            let mut core_map = FxHashMap::default();
            for (k, v) in map {
                core_map.insert(k.clone(), json_to_core_value(v));
            }
            Value::Map(core_map)
        }
    }
}

/// Create a new FluentAI VM instance
#[wasm_bindgen]
pub fn create_fluentai_vm(canvas: HtmlCanvasElement) -> Result<FluentAIVM, JsValue> {
    FluentAIVM::new(canvas)
}

/// Simple function to test compilation
#[wasm_bindgen]
pub fn test_compile(source: &str) -> Result<String, JsValue> {
    // First parse the source
    let ast = fluentai_parser::parse_flc(source)
        .map_err(|e| {
            let error_str = e.to_string();
            // Look for position information in the error
            if error_str.contains("at position") {
                // Extract line number from position
                let lines: Vec<&str> = source.lines().collect();
                let mut pos = 0;
                let mut line_num = 1;
                let mut col_num = 1;
                
                // Try to find the position mentioned in the error
                if let Some(pos_start) = error_str.find("at position ") {
                    if let Some(pos_str) = error_str[pos_start + 12..].split(' ').next() {
                        if let Ok(error_pos) = pos_str.parse::<usize>() {
                            // Calculate line and column
                            for (idx, line) in lines.iter().enumerate() {
                                if pos + line.len() + 1 > error_pos {
                                    line_num = idx + 1;
                                    col_num = error_pos - pos + 1;
                                    break;
                                }
                                pos += line.len() + 1; // +1 for newline
                            }
                        }
                    }
                }
                
                JsValue::from_str(&format!("Parse error at line {}, column {}: {}", line_num, col_num, error_str))
            } else {
                JsValue::from_str(&format!("Parse error: {}", error_str))
            }
        })?;
    
    let options = CompilerOptions::default();
    let mut compiler = Compiler::with_options(options);
    
    match compiler.compile(&ast) {
        Ok(bytecode) => {
            Ok(format!("Compilation successful! {} chunks generated", bytecode.chunks.len()))
        }
        Err(e) => {
            Err(JsValue::from_str(&format!("Compilation error: {}", e)))
        }
    }
}