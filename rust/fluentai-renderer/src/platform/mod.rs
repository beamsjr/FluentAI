/// Platform-specific integration for FluentAI renderer
use crate::primitives::Color;
use std::path::PathBuf;

#[cfg(not(target_arch = "wasm32"))]
pub mod desktop;
#[cfg(target_arch = "wasm32")]
pub mod web;
#[cfg(not(target_arch = "wasm32"))]
pub mod mobile;

/// Platform capabilities and features
#[derive(Debug, Clone, PartialEq)]
pub struct PlatformCapabilities {
    pub file_system: bool,
    pub clipboard: bool,
    pub notifications: bool,
    pub system_tray: bool,
    pub multi_window: bool,
    pub native_menus: bool,
    pub touch_input: bool,
    pub accelerometer: bool,
    pub camera: bool,
    pub gpu_compute: bool,
}

/// Platform-specific events
#[derive(Debug, Clone)]
pub enum PlatformEvent {
    /// Window events
    WindowResized { width: u32, height: u32 },
    WindowMoved { x: i32, y: i32 },
    WindowFocused(bool),
    WindowClosed,
    
    /// File system events
    FileDropped(Vec<PathBuf>),
    FileHovered(Vec<PathBuf>),
    FileHoverCancelled,
    
    /// System events
    ThemeChanged(SystemTheme),
    MemoryWarning,
    PowerStateChanged(PowerState),
    
    /// Mobile-specific events
    OrientationChanged(DeviceOrientation),
    AppBackground,
    AppForeground,
    
    /// Custom platform event
    Custom(String, serde_json::Value),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum SystemTheme {
    Light,
    Dark,
    HighContrast,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PowerState {
    OnBattery { percentage: u8 },
    Charging { percentage: u8 },
    PluggedIn,
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DeviceOrientation {
    Portrait,
    PortraitUpsideDown,
    LandscapeLeft,
    LandscapeRight,
}

/// Platform integration trait
pub trait PlatformIntegration: Send + Sync {
    /// Get platform capabilities
    fn capabilities(&self) -> PlatformCapabilities;
    
    /// Get current system theme
    fn system_theme(&self) -> SystemTheme;
    
    /// Get system accent color
    fn accent_color(&self) -> Option<Color>;
    
    /// Show native file picker
    fn show_file_picker(&self, options: FilePickerOptions) -> Option<Vec<PathBuf>>;
    
    /// Show native save dialog
    fn show_save_dialog(&self, options: SaveDialogOptions) -> Option<PathBuf>;
    
    /// Access clipboard
    fn clipboard(&self) -> Box<dyn Clipboard>;
    
    /// Show system notification
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError>;
    
    /// Get storage directory for app data
    fn app_data_dir(&self) -> PathBuf;
    
    /// Get user documents directory
    fn documents_dir(&self) -> PathBuf;
    
    /// Open URL in default browser
    fn open_url(&self, url: &str) -> Result<(), PlatformError>;
    
    /// Get device info
    fn device_info(&self) -> DeviceInfo;
}

/// File picker options
#[derive(Debug, Clone)]
pub struct FilePickerOptions {
    pub title: String,
    pub filters: Vec<FileFilter>,
    pub multiple: bool,
    pub directory: bool,
    pub starting_directory: Option<PathBuf>,
}

#[derive(Debug, Clone)]
pub struct FileFilter {
    pub name: String,
    pub extensions: Vec<String>,
}

/// Save dialog options
#[derive(Debug, Clone)]
pub struct SaveDialogOptions {
    pub title: String,
    pub default_name: String,
    pub filters: Vec<FileFilter>,
    pub starting_directory: Option<PathBuf>,
}

/// Clipboard trait
pub trait Clipboard: Send + Sync {
    fn read_text(&self) -> Option<String>;
    fn write_text(&mut self, text: &str) -> Result<(), PlatformError>;
    fn read_image(&self) -> Option<Vec<u8>>;
    fn write_image(&mut self, data: &[u8]) -> Result<(), PlatformError>;
    fn clear(&mut self);
}

/// System notification
#[derive(Debug, Clone)]
pub struct Notification {
    pub title: String,
    pub body: String,
    pub icon: Option<NotificationIcon>,
    pub sound: Option<NotificationSound>,
    pub actions: Vec<NotificationAction>,
}

#[derive(Debug, Clone)]
pub enum NotificationIcon {
    Default,
    Info,
    Warning,
    Error,
    Custom(Vec<u8>),
}

#[derive(Debug, Clone)]
pub enum NotificationSound {
    Default,
    None,
    Custom(String),
}

#[derive(Debug, Clone)]
pub struct NotificationAction {
    pub id: String,
    pub label: String,
}

/// Device information
#[derive(Debug, Clone)]
pub struct DeviceInfo {
    pub os: OperatingSystem,
    pub version: String,
    pub model: Option<String>,
    pub cpu_cores: usize,
    pub memory_mb: usize,
    pub is_mobile: bool,
    pub has_touch: bool,
    pub screen_size: (u32, u32),
    pub screen_dpi: f32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperatingSystem {
    Windows,
    MacOS,
    Linux,
    iOS,
    Android,
    Web,
    Unknown,
}

/// Platform errors
#[derive(Debug, thiserror::Error)]
pub enum PlatformError {
    #[error("Feature not supported on this platform")]
    NotSupported,
    
    #[error("Permission denied: {0}")]
    PermissionDenied(String),
    
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),
    
    #[error("Platform API error: {0}")]
    ApiError(String),
}

/// Window options for desktop platforms
#[derive(Debug, Clone)]
pub struct WindowOptions {
    pub title: String,
    pub size: (u32, u32),
    pub position: Option<(i32, i32)>,
    pub resizable: bool,
    pub fullscreen: bool,
    pub transparent: bool,
    pub always_on_top: bool,
    pub decorations: bool,
    pub icon: Option<Vec<u8>>,
}

impl Default for WindowOptions {
    fn default() -> Self {
        Self {
            title: "FluentAI App".to_string(),
            size: (800, 600),
            position: None,
            resizable: true,
            fullscreen: false,
            transparent: false,
            always_on_top: false,
            decorations: true,
            icon: None,
        }
    }
}

/// Menu builder for native menus
pub struct MenuBuilder {
    items: Vec<MenuItem>,
}

#[derive(Debug, Clone)]
pub enum MenuItem {
    Action {
        label: String,
        id: String,
        accelerator: Option<String>,
        enabled: bool,
    },
    Separator,
    Submenu {
        label: String,
        items: Vec<MenuItem>,
    },
}

impl MenuBuilder {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }
    
    pub fn add_item(mut self, label: &str, id: &str) -> Self {
        self.items.push(MenuItem::Action {
            label: label.to_string(),
            id: id.to_string(),
            accelerator: None,
            enabled: true,
        });
        self
    }
    
    pub fn add_item_with_accelerator(mut self, label: &str, id: &str, accelerator: &str) -> Self {
        self.items.push(MenuItem::Action {
            label: label.to_string(),
            id: id.to_string(),
            accelerator: Some(accelerator.to_string()),
            enabled: true,
        });
        self
    }
    
    pub fn add_separator(mut self) -> Self {
        self.items.push(MenuItem::Separator);
        self
    }
    
    pub fn add_submenu(mut self, label: &str, submenu: MenuBuilder) -> Self {
        self.items.push(MenuItem::Submenu {
            label: label.to_string(),
            items: submenu.items,
        });
        self
    }
    
    pub fn build(self) -> Vec<MenuItem> {
        self.items
    }
}

/// System tray support
pub trait SystemTray: Send + Sync {
    fn set_icon(&mut self, icon: &[u8]) -> Result<(), PlatformError>;
    fn set_tooltip(&mut self, tooltip: &str);
    fn set_menu(&mut self, menu: Vec<MenuItem>);
    fn show(&mut self);
    fn hide(&mut self);
}

/// Touch input support
#[derive(Debug, Clone, Copy)]
pub struct Touch {
    pub id: u64,
    pub position: (f32, f32),
    pub force: Option<f32>,
}

#[derive(Debug, Clone)]
pub enum TouchEvent {
    Start(Touch),
    Move(Touch),
    End(Touch),
    Cancel(Touch),
}

/// Accelerometer data
#[derive(Debug, Clone, Copy)]
pub struct AccelerometerData {
    pub x: f32,
    pub y: f32,
    pub z: f32,
    pub timestamp: f64,
}

/// Camera access
pub trait Camera: Send + Sync {
    fn list_cameras(&self) -> Vec<CameraInfo>;
    fn open(&mut self, camera_id: &str) -> Result<(), PlatformError>;
    fn close(&mut self);
    fn capture_frame(&mut self) -> Option<CameraFrame>;
    fn start_recording(&mut self, output_path: &PathBuf) -> Result<(), PlatformError>;
    fn stop_recording(&mut self) -> Result<(), PlatformError>;
}

#[derive(Debug, Clone)]
pub struct CameraInfo {
    pub id: String,
    pub name: String,
    pub position: CameraPosition,
    pub resolutions: Vec<(u32, u32)>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum CameraPosition {
    Front,
    Back,
    External,
    Unknown,
}

#[derive(Debug, Clone)]
pub struct CameraFrame {
    pub data: Vec<u8>,
    pub width: u32,
    pub height: u32,
    pub format: PixelFormat,
    pub timestamp: f64,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum PixelFormat {
    Rgb,
    Rgba,
    Bgr,
    Bgra,
    Yuv420,
}