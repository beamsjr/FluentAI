/// Hot reload server for live development
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use web_time::SystemTime;
use std::collections::HashMap;
use notify::{Watcher, RecursiveMode, Event};
use std::sync::mpsc::{channel, Receiver};

/// Hot reload server for watching file changes
pub struct HotReloadServer {
    enabled: bool,
    watch_paths: Vec<PathBuf>,
    file_hashes: Arc<Mutex<HashMap<PathBuf, u64>>>,
    watcher: Option<notify::RecommendedWatcher>,
    receiver: Option<Receiver<notify::Result<Event>>>,
    reload_callbacks: Vec<Box<dyn Fn(&ReloadEvent) + Send + Sync>>,
}

#[derive(Debug, Clone)]
pub struct ReloadEvent {
    pub path: PathBuf,
    pub event_type: ReloadEventType,
    pub timestamp: SystemTime,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ReloadEventType {
    Modified,
    Created,
    Deleted,
    StyleChanged,
    ComponentChanged,
    AssetChanged,
}

impl HotReloadServer {
    pub fn new() -> Self {
        Self {
            enabled: false,
            watch_paths: vec![
                PathBuf::from("src"),
                PathBuf::from("assets"),
                PathBuf::from("styles"),
            ],
            file_hashes: Arc::new(Mutex::new(HashMap::new())),
            watcher: None,
            receiver: None,
            reload_callbacks: Vec::new(),
        }
    }
    
    /// Start the hot reload server
    pub fn start(&mut self) {
        if self.enabled {
            return;
        }
        
        let (tx, rx) = channel();
        
        let mut watcher = notify::recommended_watcher(move |res| {
            let _ = tx.send(res);
        }).expect("Failed to create file watcher");
        
        // Watch all configured paths
        for path in &self.watch_paths {
            if path.exists() {
                watcher.watch(path, RecursiveMode::Recursive)
                    .expect("Failed to watch path");
                println!("👁️  Watching: {}", path.display());
            }
        }
        
        self.watcher = Some(watcher);
        self.receiver = Some(rx);
        self.enabled = true;
        
        // Start WebSocket server in separate thread
        self.start_websocket_server();
    }
    
    /// Stop the hot reload server
    pub fn stop(&mut self) {
        self.enabled = false;
        self.watcher = None;
        self.receiver = None;
    }
    
    /// Add a path to watch
    pub fn add_watch_path(&mut self, path: PathBuf) {
        if !self.watch_paths.contains(&path) {
            self.watch_paths.push(path.clone());
            
            if let Some(ref mut watcher) = self.watcher {
                if path.exists() {
                    let _ = watcher.watch(&path, RecursiveMode::Recursive);
                }
            }
        }
    }
    
    /// Check for file changes
    pub fn check_for_changes(&mut self) -> Option<ReloadEvent> {
        if !self.enabled {
            return None;
        }
        
        if let Some(ref receiver) = self.receiver {
            // Non-blocking receive
            if let Ok(Ok(event)) = receiver.try_recv() {
                return self.process_event(event);
            }
        }
        
        None
    }
    
    /// Process file system event
    fn process_event(&mut self, event: Event) -> Option<ReloadEvent> {
        match event.kind {
            notify::EventKind::Modify(_) => {
                for path in event.paths {
                    if self.should_reload(&path) {
                        return Some(ReloadEvent {
                            path: path.clone(),
                            event_type: self.get_event_type(&path),
                            timestamp: SystemTime::now(),
                        });
                    }
                }
            }
            notify::EventKind::Create(_) => {
                for path in event.paths {
                    return Some(ReloadEvent {
                        path,
                        event_type: ReloadEventType::Created,
                        timestamp: SystemTime::now(),
                    });
                }
            }
            notify::EventKind::Remove(_) => {
                for path in event.paths {
                    return Some(ReloadEvent {
                        path,
                        event_type: ReloadEventType::Deleted,
                        timestamp: SystemTime::now(),
                    });
                }
            }
            _ => {}
        }
        
        None
    }
    
    /// Check if file should trigger reload
    fn should_reload(&self, path: &Path) -> bool {
        // Check file extension
        if let Some(ext) = path.extension() {
            match ext.to_str() {
                Some("flc") | Some("rs") | Some("css") | Some("json") => true,
                Some("png") | Some("jpg") | Some("svg") => true,
                _ => false,
            }
        } else {
            false
        }
    }
    
    /// Get event type based on file
    fn get_event_type(&self, path: &Path) -> ReloadEventType {
        if let Some(ext) = path.extension() {
            match ext.to_str() {
                Some("css") => ReloadEventType::StyleChanged,
                Some("flc") | Some("rs") => ReloadEventType::ComponentChanged,
                Some("png") | Some("jpg") | Some("svg") => ReloadEventType::AssetChanged,
                _ => ReloadEventType::Modified,
            }
        } else {
            ReloadEventType::Modified
        }
    }
    
    /// Force a reload
    pub fn force_reload(&mut self) {
        let event = ReloadEvent {
            path: PathBuf::from("*"),
            event_type: ReloadEventType::Modified,
            timestamp: SystemTime::now(),
        };
        
        self.notify_callbacks(&event);
    }
    
    /// Add reload callback
    pub fn on_reload<F>(&mut self, callback: F)
    where
        F: Fn(&ReloadEvent) + Send + Sync + 'static
    {
        self.reload_callbacks.push(Box::new(callback));
    }
    
    /// Notify all callbacks
    fn notify_callbacks(&self, event: &ReloadEvent) {
        for callback in &self.reload_callbacks {
            callback(event);
        }
    }
    
    /// Start WebSocket server for browser communication
    fn start_websocket_server(&self) {
        use std::thread;
        
        thread::spawn(|| {
            // Simple HTTP server for hot reload
            let listener = std::net::TcpListener::bind("127.0.0.1:3030")
                .expect("Failed to bind hot reload server");
            
            println!("🔥 Hot reload server running on http://localhost:3030");
            
            for stream in listener.incoming() {
                if let Ok(mut stream) = stream {
                    // Handle WebSocket upgrade
                    let response = "HTTP/1.1 200 OK\r\n\
                                   Content-Type: text/html\r\n\
                                   \r\n\
                                   <script>\
                                   const ws = new WebSocket('ws://localhost:3031');\
                                   ws.onmessage = (e) => {\
                                       if (e.data === 'reload') {\
                                           window.location.reload();\
                                       }\
                                   };\
                                   </script>";
                    
                    let _ = stream.write(response.as_bytes());
                }
            }
        });
        
        // WebSocket server for reload notifications
        thread::spawn(|| {
            // TODO: Implement actual WebSocket server
            // For now, this is a placeholder
        });
    }
}

use std::io::Write;

/// Hot reload client for FluentAI runtime
pub struct HotReloadClient {
    server_url: String,
    connected: bool,
}

impl HotReloadClient {
    pub fn new(server_url: &str) -> Self {
        Self {
            server_url: server_url.to_string(),
            connected: false,
        }
    }
    
    /// Connect to hot reload server
    pub fn connect(&mut self) -> Result<(), String> {
        // TODO: Implement WebSocket client connection
        self.connected = true;
        Ok(())
    }
    
    /// Poll for reload events
    pub fn poll(&mut self) -> Option<ReloadEvent> {
        if !self.connected {
            return None;
        }
        
        // TODO: Check for messages from server
        None
    }
}

/// Module hot swapping support
pub struct ModuleHotSwap {
    modules: HashMap<String, ModuleInfo>,
}

#[derive(Clone)]
struct ModuleInfo {
    path: PathBuf,
    last_modified: SystemTime,
    exports: Vec<String>,
}

impl ModuleHotSwap {
    pub fn new() -> Self {
        Self {
            modules: HashMap::new(),
        }
    }
    
    /// Register a module for hot swapping
    pub fn register_module(&mut self, name: &str, path: PathBuf) {
        let last_modified = std::fs::metadata(&path)
            .and_then(|m| m.modified())
            .unwrap_or(SystemTime::now());
        
        self.modules.insert(name.to_string(), ModuleInfo {
            path,
            last_modified,
            exports: Vec::new(),
        });
    }
    
    /// Check if module needs reloading
    pub fn needs_reload(&self, name: &str) -> bool {
        if let Some(info) = self.modules.get(name) {
            if let Ok(metadata) = std::fs::metadata(&info.path) {
                if let Ok(modified) = metadata.modified() {
                    return modified > info.last_modified;
                }
            }
        }
        false
    }
    
    /// Reload a module
    pub fn reload_module(&mut self, name: &str) -> Result<(), String> {
        if let Some(info) = self.modules.get_mut(name) {
            // TODO: Implement actual module reloading
            // This would involve:
            // 1. Saving component state
            // 2. Unloading old module
            // 3. Loading new module
            // 4. Restoring component state
            
            info.last_modified = SystemTime::now();
            Ok(())
        } else {
            Err(format!("Module '{}' not found", name))
        }
    }
}

/// State preservation for hot reload
pub struct StatePreserver {
    preserved_state: HashMap<String, serde_json::Value>,
}

impl StatePreserver {
    pub fn new() -> Self {
        Self {
            preserved_state: HashMap::new(),
        }
    }
    
    /// Preserve component state
    pub fn preserve<T: serde::Serialize>(
        &mut self,
        key: &str,
        state: &T,
    ) -> Result<(), serde_json::Error> {
        let value = serde_json::to_value(state)?;
        self.preserved_state.insert(key.to_string(), value);
        Ok(())
    }
    
    /// Restore component state
    pub fn restore<T: serde::de::DeserializeOwned>(
        &self,
        key: &str,
    ) -> Option<T> {
        self.preserved_state.get(key)
            .and_then(|value| serde_json::from_value(value.clone()).ok())
    }
    
    /// Clear preserved state
    pub fn clear(&mut self) {
        self.preserved_state.clear();
    }
}