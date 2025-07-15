// Platform integration demonstration

use fluentai_renderer::platform::*;
use fluentai_renderer::primitives::Color;
use std::path::PathBuf;

fn main() {
    println!("FluentAI Platform Integration Demo");
    println!("==================================\n");
    
    // Create platform instance based on current OS
    let platform = create_platform();
    
    // Demo 1: Platform capabilities
    demo_capabilities(&platform);
    
    // Demo 2: System information
    demo_system_info(&platform);
    
    // Demo 3: File operations
    demo_file_operations(&platform);
    
    // Demo 4: Clipboard operations
    demo_clipboard(&platform);
    
    // Demo 5: Platform-specific features
    demo_platform_specific();
}

fn create_platform() -> Box<dyn PlatformIntegration> {
    #[cfg(not(target_arch = "wasm32"))]
    {
        Box::new(desktop::DesktopPlatform::new())
    }
    
    #[cfg(target_arch = "wasm32")]
    {
        Box::new(web::WebPlatform::new().expect("Failed to create web platform"))
    }
}

fn demo_capabilities(platform: &dyn PlatformIntegration) {
    println!("Demo 1: Platform Capabilities");
    println!("-----------------------------");
    
    let caps = platform.capabilities();
    
    println!("Platform capabilities:");
    println!("  File system: {}", caps.file_system);
    println!("  Clipboard: {}", caps.clipboard);
    println!("  Notifications: {}", caps.notifications);
    println!("  System tray: {}", caps.system_tray);
    println!("  Multi-window: {}", caps.multi_window);
    println!("  Native menus: {}", caps.native_menus);
    println!("  Touch input: {}", caps.touch_input);
    println!("  Accelerometer: {}", caps.accelerometer);
    println!("  Camera: {}", caps.camera);
    println!("  GPU compute: {}", caps.gpu_compute);
    
    println!();
}

fn demo_system_info(platform: &dyn PlatformIntegration) {
    println!("Demo 2: System Information");
    println!("--------------------------");
    
    // Theme and colors
    let theme = platform.system_theme();
    println!("System theme: {:?}", theme);
    
    if let Some(color) = platform.accent_color() {
        println!("Accent color: {:?}", color);
    } else {
        println!("Accent color: Not available");
    }
    
    // Device info
    let info = platform.device_info();
    println!("\nDevice information:");
    println!("  OS: {:?} {}", info.os, info.version);
    if let Some(model) = info.model {
        println!("  Model: {}", model);
    }
    println!("  CPU cores: {}", info.cpu_cores);
    println!("  Memory: {} MB", info.memory_mb);
    println!("  Mobile: {}", info.is_mobile);
    println!("  Touch support: {}", info.has_touch);
    println!("  Screen: {}x{} @ {} DPI", info.screen_size.0, info.screen_size.1, info.screen_dpi);
    
    // Directories
    println!("\nApp directories:");
    println!("  App data: {:?}", platform.app_data_dir());
    println!("  Documents: {:?}", platform.documents_dir());
    
    println!();
}

fn demo_file_operations(platform: &dyn PlatformIntegration) {
    println!("Demo 3: File Operations");
    println!("-----------------------");
    
    // Note: These operations would show actual dialogs in a real app
    println!("File picker example:");
    let file_options = FilePickerOptions {
        title: "Select Image Files".to_string(),
        filters: vec![
            FileFilter {
                name: "Images".to_string(),
                extensions: vec!["png".to_string(), "jpg".to_string(), "jpeg".to_string()],
            },
            FileFilter {
                name: "All Files".to_string(),
                extensions: vec!["*".to_string()],
            },
        ],
        multiple: true,
        directory: false,
        starting_directory: None,
    };
    
    println!("  Options: {:?}", file_options);
    println!("  (Would show file picker dialog)");
    
    println!("\nSave dialog example:");
    let save_options = SaveDialogOptions {
        title: "Save Project".to_string(),
        default_name: "project.flai".to_string(),
        filters: vec![
            FileFilter {
                name: "FluentAI Project".to_string(),
                extensions: vec!["flai".to_string()],
            },
        ],
        starting_directory: None,
    };
    
    println!("  Options: {:?}", save_options);
    println!("  (Would show save dialog)");
    
    println!();
}

fn demo_clipboard(platform: &dyn PlatformIntegration) {
    println!("Demo 4: Clipboard Operations");
    println!("----------------------------");
    
    let mut clipboard = platform.clipboard();
    
    // Write text
    let test_text = "Hello from FluentAI!";
    match clipboard.write_text(test_text) {
        Ok(_) => println!("Written to clipboard: '{}'", test_text),
        Err(e) => println!("Failed to write to clipboard: {}", e),
    }
    
    // Read text
    if let Some(text) = clipboard.read_text() {
        println!("Read from clipboard: '{}'", text);
    } else {
        println!("No text in clipboard");
    }
    
    println!();
}

fn demo_platform_specific() {
    println!("Demo 5: Platform-Specific Features");
    println!("----------------------------------");
    
    #[cfg(target_os = "windows")]
    demo_windows_features();
    
    #[cfg(target_os = "macos")]
    demo_macos_features();
    
    #[cfg(target_os = "linux")]
    demo_linux_features();
    
    #[cfg(target_arch = "wasm32")]
    demo_web_features();
    
    #[cfg(any(target_os = "ios", target_os = "android"))]
    demo_mobile_features();
    
    println!();
}

#[cfg(target_os = "windows")]
fn demo_windows_features() {
    println!("Windows-specific features:");
    println!("  - Jump lists");
    println!("  - Taskbar progress");
    println!("  - Windows notifications");
    println!("  - Registry access");
    
    // Example: Creating a window with Windows-specific options
    let window_options = WindowOptions {
        title: "FluentAI on Windows".to_string(),
        size: (1024, 768),
        position: Some((100, 100)),
        resizable: true,
        fullscreen: false,
        transparent: false,
        always_on_top: false,
        decorations: true,
        icon: None,
    };
    
    println!("\nWindow options: {:?}", window_options);
}

#[cfg(target_os = "macos")]
fn demo_macos_features() {
    println!("macOS-specific features:");
    println!("  - Touch Bar support");
    println!("  - Dock integration");
    println!("  - Global menu bar");
    println!("  - Force Touch");
    
    // Example: Menu bar
    let menu = MenuBuilder::new()
        .add_item("New", "new")
        .add_item_with_accelerator("Open", "open", "Cmd+O")
        .add_separator()
        .add_submenu("Edit", MenuBuilder::new()
            .add_item_with_accelerator("Cut", "cut", "Cmd+X")
            .add_item_with_accelerator("Copy", "copy", "Cmd+C")
            .add_item_with_accelerator("Paste", "paste", "Cmd+V")
        )
        .build();
    
    println!("\nSample menu structure created");
}

#[cfg(target_os = "linux")]
fn demo_linux_features() {
    println!("Linux-specific features:");
    println!("  - Desktop environment integration");
    println!("  - System tray");
    println!("  - DBus communication");
    println!("  - X11/Wayland support");
}

#[cfg(target_arch = "wasm32")]
fn demo_web_features() {
    println!("Web-specific features:");
    println!("  - Progressive Web App");
    println!("  - Service Workers");
    println!("  - WebRTC");
    println!("  - Web Storage");
    println!("  - Gamepad API");
    println!("  - Share API");
    
    use fluentai_renderer::platform::web::*;
    
    println!("\nWeb platform capabilities:");
    println!("  Service Worker available: {}", ServiceWorkerSupport::is_available());
    println!("  WebRTC available: {}", WebRTCSupport::is_available());
}

#[cfg(any(target_os = "ios", target_os = "android"))]
fn demo_mobile_features() {
    println!("Mobile-specific features:");
    
    use fluentai_renderer::platform::mobile::*;
    
    let platform = MobilePlatform::new();
    
    // Mobile permissions
    println!("\nMobile permissions:");
    let permissions = [
        MobilePermission::Camera,
        MobilePermission::Location,
        MobilePermission::Notifications,
    ];
    
    for perm in &permissions {
        println!("  {:?}: (would request)", perm);
    }
    
    // Vibration patterns
    println!("\nVibration patterns:");
    println!("  Success pattern");
    println!("  Warning pattern");
    println!("  Custom pattern: [100ms, 50ms, 100ms]");
    
    // Device sensors
    println!("\nDevice sensors:");
    println!("  Accelerometer");
    println!("  Gyroscope");
    println!("  Magnetometer");
    println!("  Proximity sensor");
}

// Cross-platform UI example
fn create_cross_platform_ui() {
    println!("\nCross-Platform UI Example:");
    println!("-------------------------");
    
    println!("This UI would adapt to each platform:");
    println!("  - Native menus on desktop");
    println!("  - Touch gestures on mobile");
    println!("  - PWA features on web");
    println!("  - Platform-specific theming");
    
    // Example of adaptive UI code
    println!("\nAdaptive UI code example:");
    println!(r#"
ui! {
    Stack {
        // Platform-specific navigation
        #[cfg(desktop)]
        MenuBar { ... }
        
        #[cfg(mobile)]
        BottomNavigation { ... }
        
        #[cfg(web)]
        Hamburger { ... }
        
        // Content adapts to platform
        ScrollView {
            padding: if is_mobile { 16 } else { 24 },
            
            Content {
                // Touch-optimized on mobile
                button_size: if has_touch { 48 } else { 36 },
            }
        }
    }
}
"#);
}