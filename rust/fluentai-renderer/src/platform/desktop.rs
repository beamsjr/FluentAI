/// Desktop platform integration (Windows, macOS, Linux)
use super::*;
use std::path::PathBuf;

/// Desktop platform implementation
pub struct DesktopPlatform {
    #[cfg(target_os = "windows")]
    clipboard: WindowsClipboard,
    #[cfg(target_os = "macos")]
    clipboard: MacOSClipboard,
    #[cfg(target_os = "linux")]
    clipboard: LinuxClipboard,
}

impl DesktopPlatform {
    pub fn new() -> Self {
        Self {
            #[cfg(target_os = "windows")]
            clipboard: WindowsClipboard::new(),
            #[cfg(target_os = "macos")]
            clipboard: MacOSClipboard::new(),
            #[cfg(target_os = "linux")]
            clipboard: LinuxClipboard::new(),
        }
    }
}

impl PlatformIntegration for DesktopPlatform {
    fn capabilities(&self) -> PlatformCapabilities {
        PlatformCapabilities {
            file_system: true,
            clipboard: true,
            notifications: true,
            system_tray: true,
            multi_window: true,
            native_menus: true,
            touch_input: cfg!(target_os = "windows"), // Windows tablets
            accelerometer: false,
            camera: true,
            gpu_compute: true,
        }
    }
    
    fn system_theme(&self) -> SystemTheme {
        #[cfg(target_os = "windows")]
        return get_windows_theme();
        
        #[cfg(target_os = "macos")]
        return get_macos_theme();
        
        #[cfg(target_os = "linux")]
        return get_linux_theme();
    }
    
    fn accent_color(&self) -> Option<Color> {
        #[cfg(target_os = "windows")]
        return get_windows_accent_color();
        
        #[cfg(target_os = "macos")]
        return get_macos_accent_color();
        
        #[cfg(target_os = "linux")]
        return None; // Varies by desktop environment
    }
    
    fn show_file_picker(&self, options: FilePickerOptions) -> Option<Vec<PathBuf>> {
        use rfd::FileDialog;
        
        let mut dialog = FileDialog::new()
            .set_title(&options.title);
        
        if let Some(dir) = options.starting_directory {
            dialog = dialog.set_directory(dir);
        }
        
        for filter in &options.filters {
            dialog = dialog.add_filter(&filter.name, &filter.extensions);
        }
        
        if options.directory {
            dialog.pick_folder().map(|p| vec![p])
        } else if options.multiple {
            dialog.pick_files()
        } else {
            dialog.pick_file().map(|p| vec![p])
        }
    }
    
    fn show_save_dialog(&self, options: SaveDialogOptions) -> Option<PathBuf> {
        use rfd::FileDialog;
        
        let mut dialog = FileDialog::new()
            .set_title(&options.title)
            .set_file_name(&options.default_name);
        
        if let Some(dir) = options.starting_directory {
            dialog = dialog.set_directory(dir);
        }
        
        for filter in &options.filters {
            dialog = dialog.add_filter(&filter.name, &filter.extensions);
        }
        
        dialog.save_file()
    }
    
    fn clipboard(&self) -> Box<dyn Clipboard> {
        #[cfg(target_os = "windows")]
        return Box::new(self.clipboard.clone());
        
        #[cfg(target_os = "macos")]
        return Box::new(self.clipboard.clone());
        
        #[cfg(target_os = "linux")]
        return Box::new(self.clipboard.clone());
    }
    
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError> {
        use notify_rust::Notification as NativeNotification;
        
        let mut native = NativeNotification::new();
        native.summary(&notification.title)
            .body(&notification.body);
        
        // Set icon based on notification type
        match &notification.icon {
            Some(NotificationIcon::Info) => {
                native.icon("dialog-information");
            }
            Some(NotificationIcon::Warning) => {
                native.icon("dialog-warning");
            }
            Some(NotificationIcon::Error) => {
                native.icon("dialog-error");
            }
            _ => {}
        }
        
        native.show()
            .map(|_| ())
            .map_err(|e| PlatformError::ApiError(e.to_string()))
    }
    
    fn app_data_dir(&self) -> PathBuf {
        dirs::data_local_dir()
            .unwrap_or_else(|| PathBuf::from("."))
            .join("fluentai")
    }
    
    fn documents_dir(&self) -> PathBuf {
        dirs::document_dir()
            .unwrap_or_else(|| dirs::home_dir().unwrap_or_else(|| PathBuf::from(".")))
    }
    
    fn open_url(&self, url: &str) -> Result<(), PlatformError> {
        open::that(url)
            .map_err(|e| PlatformError::ApiError(e.to_string()))
    }
    
    fn device_info(&self) -> DeviceInfo {
        let os = if cfg!(target_os = "windows") {
            OperatingSystem::Windows
        } else if cfg!(target_os = "macos") {
            OperatingSystem::MacOS
        } else if cfg!(target_os = "linux") {
            OperatingSystem::Linux
        } else {
            OperatingSystem::Unknown
        };
        
        DeviceInfo {
            os,
            version: std::env::consts::OS.to_string(),
            model: None,
            cpu_cores: num_cpus::get(),
            memory_mb: get_system_memory_mb(),
            is_mobile: false,
            has_touch: has_touch_support(),
            screen_size: get_primary_monitor_size(),
            screen_dpi: get_primary_monitor_dpi(),
        }
    }
}

// Platform-specific clipboard implementations

#[cfg(target_os = "windows")]
#[derive(Clone)]
struct WindowsClipboard;

#[cfg(target_os = "windows")]
impl WindowsClipboard {
    fn new() -> Self {
        Self
    }
}

#[cfg(target_os = "windows")]
impl Clipboard for WindowsClipboard {
    fn read_text(&self) -> Option<String> {
        clipboard_win::get_clipboard_string().ok()
    }
    
    fn write_text(&mut self, text: &str) -> Result<(), PlatformError> {
        clipboard_win::set_clipboard_string(text)
            .map_err(|e| PlatformError::ApiError(e.to_string()))
    }
    
    fn read_image(&self) -> Option<Vec<u8>> {
        // TODO: Implement image clipboard support
        None
    }
    
    fn write_image(&mut self, _data: &[u8]) -> Result<(), PlatformError> {
        Err(PlatformError::NotSupported)
    }
    
    fn clear(&mut self) {
        let _ = clipboard_win::empty_clipboard();
    }
}

#[cfg(target_os = "macos")]
#[derive(Clone)]
struct MacOSClipboard;

#[cfg(target_os = "macos")]
impl MacOSClipboard {
    fn new() -> Self {
        Self
    }
}

#[cfg(target_os = "macos")]
impl Clipboard for MacOSClipboard {
    fn read_text(&self) -> Option<String> {
        // Use cocoa or objc crate for macOS clipboard
        todo!("macOS clipboard implementation")
    }
    
    fn write_text(&mut self, _text: &str) -> Result<(), PlatformError> {
        todo!("macOS clipboard implementation")
    }
    
    fn read_image(&self) -> Option<Vec<u8>> {
        None
    }
    
    fn write_image(&mut self, _data: &[u8]) -> Result<(), PlatformError> {
        Err(PlatformError::NotSupported)
    }
    
    fn clear(&mut self) {
        // Clear clipboard
    }
}

#[cfg(target_os = "linux")]
#[derive(Clone)]
struct LinuxClipboard;

#[cfg(target_os = "linux")]
impl LinuxClipboard {
    fn new() -> Self {
        Self
    }
}

#[cfg(target_os = "linux")]
impl Clipboard for LinuxClipboard {
    fn read_text(&self) -> Option<String> {
        // Use x11-clipboard or similar
        todo!("Linux clipboard implementation")
    }
    
    fn write_text(&mut self, _text: &str) -> Result<(), PlatformError> {
        todo!("Linux clipboard implementation")
    }
    
    fn read_image(&self) -> Option<Vec<u8>> {
        None
    }
    
    fn write_image(&mut self, _data: &[u8]) -> Result<(), PlatformError> {
        Err(PlatformError::NotSupported)
    }
    
    fn clear(&mut self) {
        // Clear clipboard
    }
}

// Helper functions

#[cfg(target_os = "windows")]
fn get_windows_theme() -> SystemTheme {
    // Read from registry: HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Themes\Personalize
    // AppsUseLightTheme DWORD value
    SystemTheme::Light // Default
}

#[cfg(target_os = "macos")]
fn get_macos_theme() -> SystemTheme {
    // Use NSApplication appearance
    SystemTheme::Light // Default
}

#[cfg(target_os = "linux")]
fn get_linux_theme() -> SystemTheme {
    // Check GTK theme or Qt theme
    SystemTheme::Light // Default
}

#[cfg(target_os = "windows")]
fn get_windows_accent_color() -> Option<Color> {
    // Read from registry
    None
}

#[cfg(target_os = "macos")]
fn get_macos_accent_color() -> Option<Color> {
    // Use NSColor.controlAccentColor
    None
}

fn get_system_memory_mb() -> usize {
    // Use sysinfo crate or platform APIs
    8192 // Default 8GB
}

fn has_touch_support() -> bool {
    #[cfg(target_os = "windows")]
    {
        // Check for touch support via Windows API
        false
    }
    
    #[cfg(not(target_os = "windows"))]
    false
}

fn get_primary_monitor_size() -> (u32, u32) {
    // Use winit or platform APIs
    (1920, 1080) // Default Full HD
}

fn get_primary_monitor_dpi() -> f32 {
    // Use winit or platform APIs
    96.0 // Default DPI
}

// Window creation helper
pub fn create_window(event_loop: &winit::event_loop::EventLoopWindowTarget<()>, options: WindowOptions) -> Result<winit::window::Window, PlatformError> {
    use winit::{
        window::{WindowBuilder, Icon},
        dpi::LogicalSize,
    };
    let mut builder = WindowBuilder::new()
        .with_title(&options.title)
        .with_inner_size(LogicalSize::new(options.size.0, options.size.1))
        .with_resizable(options.resizable)
        .with_fullscreen(if options.fullscreen {
            Some(winit::window::Fullscreen::Borderless(None))
        } else {
            None
        })
        .with_transparent(options.transparent)
        .with_decorations(options.decorations);
    
    if let Some((x, y)) = options.position {
        builder = builder.with_position(winit::dpi::LogicalPosition::new(x, y));
    }
    
    if let Some(icon_data) = options.icon {
        // Load icon from data
        // builder = builder.with_window_icon(Some(icon));
    }
    
    let window = builder.build(event_loop)
        .map_err(|e| PlatformError::ApiError(e.to_string()))?;
    
    // Set always on top after creation
    if options.always_on_top {
        window.set_window_level(winit::window::WindowLevel::AlwaysOnTop);
    }
    
    Ok(window)
}

// Additional desktop-specific features

/// Desktop app lifecycle
pub trait DesktopApp {
    fn on_before_quit(&mut self) -> bool { true }
    fn on_quit(&mut self) {}
    fn on_open_files(&mut self, _files: Vec<PathBuf>) {}
    fn on_reopen(&mut self) {}
}

/// Global menu bar support (macOS primarily)
pub fn set_application_menu(menu: Vec<MenuItem>) {
    #[cfg(target_os = "macos")]
    {
        // Set macOS application menu
    }
}

/// Dock/taskbar integration
pub trait DockIntegration {
    fn set_badge(&mut self, text: Option<&str>);
    fn set_progress(&mut self, progress: Option<f32>);
    fn bounce(&mut self, critical: bool);
}