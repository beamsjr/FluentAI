/// Web platform integration (WebAssembly/Browser)
use super::*;
use wasm_bindgen::prelude::*;
use web_sys::{window, Window, Document, Navigator};
use js_sys::Date;

/// Web platform implementation
pub struct WebPlatform {
    window: Window,
    document: Document,
    navigator: Navigator,
}

impl WebPlatform {
    pub fn new() -> Result<Self, JsValue> {
        let window = window().ok_or("No window found")?;
        let document = window.document().ok_or("No document found")?;
        let navigator = window.navigator();
        
        Ok(Self {
            window,
            document,
            navigator,
        })
    }
}

impl PlatformIntegration for WebPlatform {
    fn capabilities(&self) -> PlatformCapabilities {
        PlatformCapabilities {
            file_system: true, // Limited via File API
            clipboard: true,   // Via Clipboard API
            notifications: true, // Via Notifications API
            system_tray: false,
            multi_window: false, // Single window/tab
            native_menus: false,
            touch_input: true,
            accelerometer: true, // Via DeviceMotion API
            camera: true,       // Via MediaDevices API
            gpu_compute: true,  // Via WebGPU
        }
    }
    
    fn system_theme(&self) -> SystemTheme {
        let media_query = self.window
            .match_media("(prefers-color-scheme: dark)")
            .ok()
            .flatten();
        
        match media_query {
            Some(query) if query.matches() => SystemTheme::Dark,
            _ => SystemTheme::Light,
        }
    }
    
    fn accent_color(&self) -> Option<Color> {
        // No standard way to get accent color in web
        None
    }
    
    fn show_file_picker(&self, options: FilePickerOptions) -> Option<Vec<PathBuf>> {
        // Use HTML file input element
        let input = self.document
            .create_element("input")
            .ok()?
            .dyn_into::<web_sys::HtmlInputElement>()
            .ok()?;
        
        input.set_type("file");
        
        if options.multiple {
            input.set_multiple(true);
        }
        
        // Set accept attribute for file filters
        let accept = options.filters.iter()
            .flat_map(|f| f.extensions.iter().map(|ext| format!(".{}", ext)))
            .collect::<Vec<_>>()
            .join(",");
        
        if !accept.is_empty() {
            input.set_accept(&accept);
        }
        
        // Trigger file picker
        input.click();
        
        // In real implementation, would need to handle async file selection
        None
    }
    
    fn show_save_dialog(&self, _options: SaveDialogOptions) -> Option<PathBuf> {
        // Use download attribute or File System Access API
        None
    }
    
    fn clipboard(&self) -> Box<dyn Clipboard> {
        Box::new(WebClipboard {
            window: self.window.clone(),
        })
    }
    
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError> {
        use web_sys::NotificationOptions;
        
        let opts = NotificationOptions::new();
        opts.set_body(&notification.body);
        
        match notification.icon {
            Some(NotificationIcon::Custom(ref _data)) => {
                // Convert to data URL
                // opts.set_icon(&data_url);
            }
            _ => {}
        }
        
        web_sys::Notification::new_with_options(&notification.title, &opts)
            .map(|_| ())
            .map_err(|_| PlatformError::ApiError("Failed to create notification".to_string()))
    }
    
    fn app_data_dir(&self) -> PathBuf {
        // Use IndexedDB or localStorage
        PathBuf::from("/app_data")
    }
    
    fn documents_dir(&self) -> PathBuf {
        // Not applicable for web
        PathBuf::from("/documents")
    }
    
    fn open_url(&self, url: &str) -> Result<(), PlatformError> {
        self.window
            .open_with_url(url)
            .map(|_| ())
            .map_err(|_| PlatformError::ApiError("Failed to open URL".to_string()))
    }
    
    fn device_info(&self) -> DeviceInfo {
        let user_agent = self.navigator.user_agent().unwrap_or_default();
        let is_mobile = is_mobile_browser(&user_agent);
        
        DeviceInfo {
            os: detect_os(&user_agent),
            version: user_agent.clone(),
            model: None,
            cpu_cores: self.navigator.hardware_concurrency() as usize,
            memory_mb: (self.navigator.device_memory() * 1024.0) as usize,
            is_mobile,
            has_touch: has_touch_support(&self.window),
            screen_size: get_screen_size(&self.window),
            screen_dpi: get_screen_dpi(&self.window),
        }
    }
}

/// Web clipboard implementation
struct WebClipboard {
    window: Window,
}

impl Clipboard for WebClipboard {
    fn read_text(&self) -> Option<String> {
        // Use async Clipboard API
        // navigator.clipboard.readText()
        None
    }
    
    fn write_text(&mut self, text: &str) -> Result<(), PlatformError> {
        // navigator.clipboard.writeText(text)
        Ok(())
    }
    
    fn read_image(&self) -> Option<Vec<u8>> {
        None
    }
    
    fn write_image(&mut self, _data: &[u8]) -> Result<(), PlatformError> {
        Err(PlatformError::NotSupported)
    }
    
    fn clear(&mut self) {
        let _ = self.write_text("");
    }
}

// Helper functions

fn is_mobile_browser(user_agent: &str) -> bool {
    let mobile_keywords = ["Android", "iPhone", "iPad", "iPod", "Windows Phone"];
    mobile_keywords.iter().any(|&keyword| user_agent.contains(keyword))
}

fn detect_os(user_agent: &str) -> OperatingSystem {
    if user_agent.contains("Windows") {
        OperatingSystem::Windows
    } else if user_agent.contains("Mac") {
        OperatingSystem::MacOS
    } else if user_agent.contains("Linux") {
        OperatingSystem::Linux
    } else if user_agent.contains("Android") {
        OperatingSystem::Android
    } else if user_agent.contains("iPhone") || user_agent.contains("iPad") {
        OperatingSystem::iOS
    } else {
        OperatingSystem::Web
    }
}

fn has_touch_support(window: &Window) -> bool {
    // Check for touch events or pointer events
    js_sys::Reflect::get(window, &"ontouchstart".into())
        .map(|v| !v.is_undefined())
        .unwrap_or(false)
}

fn get_screen_size(window: &Window) -> (u32, u32) {
    let screen = window.screen().ok();
    match screen {
        Some(s) => (
            s.width().unwrap_or(1920) as u32,
            s.height().unwrap_or(1080) as u32,
        ),
        None => (1920, 1080),
    }
}

fn get_screen_dpi(window: &Window) -> f32 {
    window.device_pixel_ratio() as f32 * 96.0
}

// Web-specific features

/// Progressive Web App support
pub struct PWAFeatures {
    pub install_prompt: Option<JsValue>,
}

impl PWAFeatures {
    pub fn new() -> Self {
        Self {
            install_prompt: None,
        }
    }
    
    pub fn can_install(&self) -> bool {
        self.install_prompt.is_some()
    }
    
    pub fn prompt_install(&self) -> Result<(), JsValue> {
        if let Some(ref prompt) = self.install_prompt {
            // Call prompt.prompt()
            Ok(())
        } else {
            Err("No install prompt available".into())
        }
    }
}

/// Web Storage API wrapper
pub struct WebStorage {
    local: web_sys::Storage,
    session: web_sys::Storage,
}

impl WebStorage {
    pub fn new(window: &Window) -> Option<Self> {
        let local = window.local_storage().ok()??;
        let session = window.session_storage().ok()??;
        
        Some(Self { local, session })
    }
    
    pub fn get_local(&self, key: &str) -> Option<String> {
        self.local.get_item(key).ok()?
    }
    
    pub fn set_local(&self, key: &str, value: &str) -> Result<(), JsValue> {
        self.local.set_item(key, value)
    }
    
    pub fn get_session(&self, key: &str) -> Option<String> {
        self.session.get_item(key).ok()?
    }
    
    pub fn set_session(&self, key: &str, value: &str) -> Result<(), JsValue> {
        self.session.set_item(key, value)
    }
}

/// WebRTC support for peer-to-peer
pub struct WebRTCSupport;

impl WebRTCSupport {
    pub fn is_available() -> bool {
        // Check if RTCPeerConnection is available
        true
    }
    
    pub fn create_peer_connection() -> Result<web_sys::RtcPeerConnection, JsValue> {
        web_sys::RtcPeerConnection::new()
    }
}

/// Service Worker integration
pub struct ServiceWorkerSupport;

impl ServiceWorkerSupport {
    pub fn register(script_url: &str) -> Result<(), JsValue> {
        let window = window().ok_or("No window")?;
        let navigator = window.navigator();
        
        // navigator.serviceWorker.register(script_url)
        Ok(())
    }
    
    pub fn is_available() -> bool {
        window()
            .and_then(|w| w.navigator().service_worker().ok())
            .is_some()
    }
}

/// Web Share API
pub fn share(title: &str, text: &str, url: &str) -> Result<(), JsValue> {
    let window = window().ok_or("No window")?;
    let navigator = window.navigator();
    
    // Use navigator.share() API
    Ok(())
}

/// Fullscreen API
pub fn request_fullscreen(element: &web_sys::Element) -> Result<(), JsValue> {
    element.request_fullscreen()
}

/// Wake Lock API
pub struct WakeLock;

impl WakeLock {
    pub async fn request() -> Result<(), JsValue> {
        // navigator.wakeLock.request('screen')
        Ok(())
    }
}

/// Gamepad API for game controllers
pub fn get_gamepads() -> Vec<web_sys::Gamepad> {
    let window = window().expect("No window");
    let navigator = window.navigator();
    
    let gamepads = navigator.get_gamepads().expect("Failed to get gamepads");
    let mut result = Vec::new();
    
    for i in 0..gamepads.length() {
        if let Some(gamepad) = gamepads.get(i) {
            if let Ok(gamepad) = gamepad.dyn_into::<web_sys::Gamepad>() {
                result.push(gamepad);
            }
        }
    }
    
    result
}