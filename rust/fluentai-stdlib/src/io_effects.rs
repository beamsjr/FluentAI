//! Effect-aware I/O implementation
//! 
//! This module provides I/O operations that integrate with the effect system
//! for proper sandboxing and permission control.

use crate::value::Value;
use anyhow::{anyhow, Result};
use std::sync::Arc;
use std::cell::RefCell;

/// I/O effect context for stdlib functions
pub struct IOEffectContext {
    /// Whether I/O operations are allowed
    pub io_allowed: bool,
    /// Optional whitelist of allowed file paths
    pub allowed_paths: Option<Vec<String>>,
    /// Optional read-only mode
    pub read_only: bool,
    /// Custom I/O handler
    pub io_handler: Option<Arc<dyn IOHandler>>,
}

impl Default for IOEffectContext {
    fn default() -> Self {
        Self {
            io_allowed: true, // Allow I/O by default
            allowed_paths: None,
            read_only: false,
            io_handler: None,
        }
    }
}

/// Trait for custom I/O handling
pub trait IOHandler: Send + Sync {
    /// Handle file read operation
    fn file_read(&self, path: &str) -> Result<String>;
    
    /// Handle file write operation
    fn file_write(&self, path: &str, content: &str) -> Result<()>;
    
    /// Handle file append operation
    fn file_append(&self, path: &str, content: &str) -> Result<()>;
    
    /// Handle file delete operation
    fn file_delete(&self, path: &str) -> Result<()>;
    
    /// Check if file exists
    fn file_exists(&self, path: &str) -> Result<bool>;
    
    /// List directory contents
    fn dir_list(&self, path: &str) -> Result<Vec<String>>;
    
    /// Create directory
    fn dir_create(&self, path: &str) -> Result<()>;
    
    /// Get current directory
    fn current_directory(&self) -> Result<String>;
    
    /// Read line from stdin
    fn read_line(&self) -> Result<String>;
    
    /// Print to stdout
    fn print(&self, content: &str) -> Result<()>;
    
    /// Print line to stdout
    fn print_line(&self, content: &str) -> Result<()>;
}

/// Thread-local I/O effect context
thread_local! {
    static IO_CONTEXT: RefCell<IOEffectContext> = RefCell::new(IOEffectContext::default());
}

/// Set the thread-local I/O effect context
pub fn set_io_context(context: IOEffectContext) {
    IO_CONTEXT.with(|ctx| {
        *ctx.borrow_mut() = context;
    });
}

/// Get a reference to the thread-local I/O effect context
pub fn with_io_context<F, R>(f: F) -> R
where
    F: FnOnce(&IOEffectContext) -> R,
{
    IO_CONTEXT.with(|ctx| {
        f(&*ctx.borrow())
    })
}

/// Check if a path is allowed
fn check_path_allowed(path: &str) -> Result<()> {
    with_io_context(|ctx| {
        if !ctx.io_allowed {
            return Err(anyhow!("I/O operations are not allowed"));
        }
        
        if let Some(allowed) = &ctx.allowed_paths {
            // Try to canonicalize the path, but if it doesn't exist, 
            // still check the path prefix
            let path_to_check = if let Ok(canonical) = std::path::Path::new(path).canonicalize() {
                canonical.to_string_lossy().to_string()
            } else {
                // Path doesn't exist yet, so just check the prefix
                path.to_string()
            };
                
            let allowed_match = allowed.iter().any(|allowed_path| {
                path_to_check.starts_with(allowed_path)
            });
            
            if !allowed_match {
                return Err(anyhow!("Path '{}' is not in allowed paths", path));
            }
        }
        
        Ok(())
    })
}

/// Check if write operations are allowed
fn check_write_allowed() -> Result<()> {
    with_io_context(|ctx| {
        if ctx.read_only {
            return Err(anyhow!("Write operations are not allowed in read-only mode"));
        }
        Ok(())
    })
}

// Re-implement I/O functions with effect awareness

pub fn file_read_with_effects(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("file-read: expected 1 argument (path)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-read: expected string path")),
    };
    
    check_path_allowed(path)?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            let content = handler.file_read(path)?;
            Ok(Value::String(content))
        } else {
            // Default implementation
            let content = std::fs::read_to_string(path)
                .map_err(|e| anyhow!("file-read: {}", e))?;
            Ok(Value::String(content))
        }
    })
}

pub fn file_write_with_effects(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(anyhow!("file-write: expected 2 arguments (path, content)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-write: expected string path")),
    };
    
    let content = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-write: expected string content")),
    };
    
    check_path_allowed(path)?;
    check_write_allowed()?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            handler.file_write(path, content)?;
        } else {
            // Default implementation
            std::fs::write(path, content)
                .map_err(|e| anyhow!("file-write: {}", e))?;
        }
        Ok(Value::Nil)
    })
}

pub fn print_line_with_effects(args: &[Value]) -> Result<Value> {
    with_io_context(|ctx| {
        if !ctx.io_allowed {
            return Err(anyhow!("I/O operations are not allowed"));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
            
        if let Some(handler) = &ctx.io_handler {
            handler.print_line(&output)?;
        } else {
            // Default implementation
            println!("{}", output);
        }
        
        Ok(Value::Nil)
    })
}

pub fn file_append_with_effects(args: &[Value]) -> Result<Value> {
    if args.len() < 2 {
        return Err(anyhow!("file-append: expected 2 arguments (path, content)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-append: expected string path")),
    };
    
    let content = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-append: expected string content")),
    };
    
    check_path_allowed(path)?;
    check_write_allowed()?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            handler.file_append(path, content)?;
        } else {
            // Default implementation
            use std::fs::OpenOptions;
            use std::io::Write;
            let mut file = OpenOptions::new()
                .append(true)
                .create(true)
                .open(path)
                .map_err(|e| anyhow!("file-append: {}", e))?;
            file.write_all(content.as_bytes())
                .map_err(|e| anyhow!("file-append: {}", e))?;
        }
        Ok(Value::Nil)
    })
}

pub fn file_delete_with_effects(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("file-delete: expected 1 argument (path)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-delete: expected string path")),
    };
    
    check_path_allowed(path)?;
    check_write_allowed()?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            handler.file_delete(path)?;
        } else {
            // Default implementation
            std::fs::remove_file(path)
                .map_err(|e| anyhow!("file-delete: {}", e))?;
        }
        Ok(Value::Nil)
    })
}

pub fn file_exists_with_effects(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("file-exists?: expected 1 argument (path)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("file-exists?: expected string path")),
    };
    
    check_path_allowed(path)?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            let exists = handler.file_exists(path)?;
            Ok(Value::Bool(exists))
        } else {
            // Default implementation
            Ok(Value::Bool(std::path::Path::new(path).exists()))
        }
    })
}

pub fn dir_list_with_effects(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("dir-list: expected 1 argument (path)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("dir-list: expected string path")),
    };
    
    check_path_allowed(path)?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            let entries = handler.dir_list(path)?;
            Ok(Value::List(entries.into_iter().map(Value::String).collect()))
        } else {
            // Default implementation
            let entries = std::fs::read_dir(path)
                .map_err(|e| anyhow!("dir-list: {}", e))?;
            let mut names = Vec::new();
            for entry in entries {
                let entry = entry.map_err(|e| anyhow!("dir-list: {}", e))?;
                names.push(Value::String(entry.file_name().to_string_lossy().to_string()));
            }
            Ok(Value::List(names))
        }
    })
}

pub fn dir_create_with_effects(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("dir-create: expected 1 argument (path)"));
    }
    
    let path = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("dir-create: expected string path")),
    };
    
    check_path_allowed(path)?;
    check_write_allowed()?;
    
    with_io_context(|ctx| {
        if let Some(handler) = &ctx.io_handler {
            handler.dir_create(path)?;
        } else {
            // Default implementation
            std::fs::create_dir_all(path)
                .map_err(|e| anyhow!("dir-create: {}", e))?;
        }
        Ok(Value::Nil)
    })
}

pub fn current_directory_with_effects(_args: &[Value]) -> Result<Value> {
    with_io_context(|ctx| {
        if !ctx.io_allowed {
            return Err(anyhow!("I/O operations are not allowed"));
        }
        
        if let Some(handler) = &ctx.io_handler {
            let dir = handler.current_directory()?;
            Ok(Value::String(dir))
        } else {
            // Default implementation
            let dir = std::env::current_dir()
                .map_err(|e| anyhow!("current-directory: {}", e))?;
            Ok(Value::String(dir.to_string_lossy().to_string()))
        }
    })
}

pub fn read_line_with_effects(_args: &[Value]) -> Result<Value> {
    with_io_context(|ctx| {
        if !ctx.io_allowed {
            return Err(anyhow!("I/O operations are not allowed"));
        }
        
        if let Some(handler) = &ctx.io_handler {
            let line = handler.read_line()?;
            Ok(Value::String(line))
        } else {
            // Default implementation
            use std::io::{self, BufRead};
            let stdin = io::stdin();
            let mut line = String::new();
            stdin.lock().read_line(&mut line)
                .map_err(|e| anyhow!("read-line: {}", e))?;
            // Remove trailing newline
            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::String(line))
        }
    })
}

pub fn print_with_effects(args: &[Value]) -> Result<Value> {
    with_io_context(|ctx| {
        if !ctx.io_allowed {
            return Err(anyhow!("I/O operations are not allowed"));
        }
        
        let output = args.iter()
            .map(|v| format!("{}", v))
            .collect::<Vec<_>>()
            .join(" ");
            
        if let Some(handler) = &ctx.io_handler {
            handler.print(&output)?;
        } else {
            // Default implementation
            print!("{}", output);
            use std::io::{self, Write};
            io::stdout().flush().map_err(|e| anyhow!("print: {}", e))?;
        }
        
        Ok(Value::Nil)
    })
}

// Example sandbox handler that logs all I/O operations
pub struct LoggingIOHandler {
    log: Arc<std::sync::Mutex<Vec<String>>>,
}

impl LoggingIOHandler {
    pub fn new() -> Self {
        Self {
            log: Arc::new(std::sync::Mutex::new(Vec::new())),
        }
    }
    
    pub fn get_log(&self) -> Vec<String> {
        self.log.lock().unwrap().clone()
    }
    
    fn log_operation(&self, op: &str) {
        self.log.lock().unwrap().push(op.to_string());
    }
}

impl IOHandler for LoggingIOHandler {
    fn file_read(&self, path: &str) -> Result<String> {
        self.log_operation(&format!("READ: {}", path));
        Err(anyhow!("File operations not allowed in sandbox"))
    }
    
    fn file_write(&self, path: &str, _content: &str) -> Result<()> {
        self.log_operation(&format!("WRITE: {}", path));
        Err(anyhow!("File operations not allowed in sandbox"))
    }
    
    fn file_append(&self, path: &str, _content: &str) -> Result<()> {
        self.log_operation(&format!("APPEND: {}", path));
        Err(anyhow!("File operations not allowed in sandbox"))
    }
    
    fn file_delete(&self, path: &str) -> Result<()> {
        self.log_operation(&format!("DELETE: {}", path));
        Err(anyhow!("File operations not allowed in sandbox"))
    }
    
    fn file_exists(&self, path: &str) -> Result<bool> {
        self.log_operation(&format!("EXISTS: {}", path));
        Ok(false)
    }
    
    fn dir_list(&self, path: &str) -> Result<Vec<String>> {
        self.log_operation(&format!("LIST: {}", path));
        Ok(vec![])
    }
    
    fn dir_create(&self, path: &str) -> Result<()> {
        self.log_operation(&format!("MKDIR: {}", path));
        Err(anyhow!("Directory operations not allowed in sandbox"))
    }
    
    fn current_directory(&self) -> Result<String> {
        self.log_operation("PWD");
        Ok("/sandbox".to_string())
    }
    
    fn read_line(&self) -> Result<String> {
        self.log_operation("READLINE");
        Err(anyhow!("Input not allowed in sandbox"))
    }
    
    fn print(&self, content: &str) -> Result<()> {
        self.log_operation(&format!("PRINT: {}", content));
        Ok(())
    }
    
    fn print_line(&self, content: &str) -> Result<()> {
        self.log_operation(&format!("PRINTLN: {}", content));
        Ok(())
    }
}