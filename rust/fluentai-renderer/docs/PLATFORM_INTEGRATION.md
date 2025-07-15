# FluentAI Platform Integration

This document describes the platform integration features in FluentAI renderer, providing a unified API for cross-platform development while leveraging platform-specific capabilities.

## Overview

The platform integration layer provides:
- Unified API across desktop, web, and mobile platforms
- Access to platform-specific features
- Automatic adaptation to platform capabilities
- Native look and feel on each platform

## Supported Platforms

### Desktop
- **Windows**: Full Win32 API integration
- **macOS**: Cocoa/AppKit integration
- **Linux**: GTK/Qt integration

### Mobile
- **iOS**: UIKit integration
- **Android**: Android SDK integration

### Web
- **Browser**: Web API integration
- **PWA**: Progressive Web App features

## Core API

### Platform Detection

```rust
use fluentai_renderer::platform::*;

// Get current platform
let platform = create_platform();

// Check capabilities
let caps = platform.capabilities();
if caps.multi_window {
    // Desktop features
}

// Get device info
let info = platform.device_info();
match info.os {
    OperatingSystem::Windows => // Windows-specific
    OperatingSystem::MacOS => // macOS-specific
    OperatingSystem::iOS => // iOS-specific
    _ => // Other platforms
}
```

### File Operations

```rust
// File picker
let options = FilePickerOptions {
    title: "Select Files".to_string(),
    filters: vec![
        FileFilter {
            name: "Images".to_string(),
            extensions: vec!["png".to_string(), "jpg".to_string()],
        }
    ],
    multiple: true,
    directory: false,
    starting_directory: None,
};

if let Some(files) = platform.show_file_picker(options) {
    for file in files {
        println!("Selected: {:?}", file);
    }
}

// Save dialog
let save_options = SaveDialogOptions {
    title: "Save As".to_string(),
    default_name: "document.txt".to_string(),
    filters: vec![/* ... */],
    starting_directory: None,
};

if let Some(path) = platform.show_save_dialog(save_options) {
    // Save to path
}
```

### Clipboard

```rust
let mut clipboard = platform.clipboard();

// Text
clipboard.write_text("Hello, World!")?;
let text = clipboard.read_text();

// Images
clipboard.write_image(&image_data)?;
let image = clipboard.read_image();
```

### Notifications

```rust
let notification = Notification {
    title: "Task Complete".to_string(),
    body: "Your export has finished.".to_string(),
    icon: Some(NotificationIcon::Success),
    sound: Some(NotificationSound::Default),
    actions: vec![
        NotificationAction {
            id: "open".to_string(),
            label: "Open File".to_string(),
        }
    ],
};

platform.show_notification(notification)?;
```

### System Integration

```rust
// Theme detection
let theme = platform.system_theme();
let accent = platform.accent_color();

// Open URLs
platform.open_url("https://example.com")?;

// App directories
let app_data = platform.app_data_dir();
let documents = platform.documents_dir();
```

## Platform-Specific Features

### Desktop Features

```rust
#[cfg(not(target_arch = "wasm32"))]
{
    use fluentai_renderer::platform::desktop::*;
    
    // Window creation
    let window = create_window(WindowOptions {
        title: "My App".to_string(),
        size: (1024, 768),
        position: Some((100, 100)),
        resizable: true,
        fullscreen: false,
        transparent: false,
        always_on_top: false,
        decorations: true,
        icon: Some(icon_data),
    })?;
    
    // Native menus
    let menu = MenuBuilder::new()
        .add_item("New", "new")
        .add_item_with_accelerator("Open", "open", "Ctrl+O")
        .add_separator()
        .add_submenu("Edit", MenuBuilder::new()
            .add_item("Cut", "cut")
            .add_item("Copy", "copy")
            .add_item("Paste", "paste")
        )
        .build();
    
    set_application_menu(menu);
}
```

### Mobile Features

```rust
#[cfg(any(target_os = "ios", target_os = "android"))]
{
    use fluentai_renderer::platform::mobile::*;
    
    let platform = MobilePlatform::new();
    
    // Permissions
    platform.request_permission(MobilePermission::Camera)?;
    
    // Vibration
    platform.vibrate(VibrationPattern::Success)?;
    
    // Screen control
    platform.set_keep_screen_on(true)?;
    platform.set_screen_brightness(0.8)?;
    
    // Battery info
    if let Some(level) = platform.battery_level() {
        println!("Battery: {}%", (level * 100.0) as u8);
    }
}
```

### Web Features

```rust
#[cfg(target_arch = "wasm32")]
{
    use fluentai_renderer::platform::web::*;
    
    // PWA support
    let pwa = PWAFeatures::new();
    if pwa.can_install() {
        pwa.prompt_install()?;
    }
    
    // Web Storage
    let storage = WebStorage::new(&window)?;
    storage.set_local("key", "value")?;
    let value = storage.get_local("key");
    
    // Service Worker
    if ServiceWorkerSupport::is_available() {
        ServiceWorkerSupport::register("/sw.js")?;
    }
    
    // Share API
    share("Check this out!", "Amazing content", "https://example.com")?;
    
    // Fullscreen
    request_fullscreen(&element)?;
    
    // Wake Lock
    WakeLock::request().await?;
}
```

## FluentAI Integration

### Platform Effect

```fluentai
// Define platform effects
effect Platform {
    show_file_picker(options: FilePickerOptions) -> Option<List<Path>>,
    show_save_dialog(options: SaveDialogOptions) -> Option<Path>,
    clipboard_read() -> Option<string>,
    clipboard_write(text: string),
    show_notification(notification: Notification),
    open_url(url: string),
}

// Use in application
private function open_file() {
    let options = FilePickerOptions {
        title: "Open Document",
        filters: [FileFilter("Documents", ["txt", "md"])],
        multiple: false,
    };
    
    if let Some(files) = perform Platform.show_file_picker(options) {
        load_document(files[0]);
    }
}
```

### Adaptive UI

```fluentai
// Platform-aware component
@UI.component
function AdaptiveButton(label: string, on_click: () -> ()) -> UI.Element {
    let platform = perform Platform.info();
    
    UI.Button(label) {
        on_click: on_click,
        // Adapt size for touch
        size: if platform.has_touch { .large } else { .medium },
        // Platform-specific styling
        style: match platform.os {
            case .iOS => UI.Style.ios_button(),
            case .Android => UI.Style.material_button(),
            case .Windows => UI.Style.fluent_button(),
            _ => UI.Style.default_button(),
        },
    }
}
```

### Platform Events

```fluentai
// Subscribe to platform events
private function setup_platform_handlers() {
    // File drops
    Platform.on_files_dropped((files) => {
        for file in files {
            open_file(file);
        }
    });
    
    // Theme changes
    Platform.on_theme_changed((theme) => {
        update_app_theme(theme);
    });
    
    // Power state
    Platform.on_power_state_changed((state) => {
        match state {
            case .OnBattery(level) if level < 20 => {
                enable_power_saving_mode();
            },
            _ => disable_power_saving_mode(),
        }
    });
    
    // Mobile lifecycle
    Platform.on_app_background(() => {
        save_state();
        pause_animations();
    });
    
    Platform.on_app_foreground(() => {
        restore_state();
        resume_animations();
    });
}
```

## Best Practices

### 1. Feature Detection

Always check capabilities before using platform features:

```fluentai
if Platform.capabilities().notifications {
    show_notification("Task complete");
} else {
    show_in_app_message("Task complete");
}
```

### 2. Graceful Degradation

Provide fallbacks for unsupported features:

```fluentai
private function save_file(data: bytes) {
    if Platform.capabilities().file_system {
        // Use native file dialog
        save_with_dialog(data);
    } else {
        // Use download API for web
        trigger_download(data);
    }
}
```

### 3. Platform Optimization

Optimize for each platform's strengths:

```fluentai
private function setup_rendering() {
    match Platform.type() {
        case .Desktop => {
            // Use all CPU cores
            set_thread_pool_size(Platform.cpu_cores());
            enable_gpu_acceleration();
        },
        case .Mobile => {
            // Optimize for battery
            set_render_quality(0.8);
            enable_touch_gestures();
        },
        case .Web => {
            // Optimize for bandwidth
            enable_progressive_loading();
            use_webgl_if_available();
        },
    }
}
```

### 4. Responsive Design

Design UIs that adapt to different form factors:

```fluentai
@UI.component
function ResponsiveLayout(content: UI.Element) -> UI.Element {
    let screen = Platform.screen_size();
    let is_mobile = screen.width < 768;
    
    UI.Stack {
        direction: if is_mobile { .vertical } else { .horizontal },
        padding: if is_mobile { 16 } else { 24 },
        spacing: if is_mobile { 12 } else { 20 },
        
        children: [
            if !is_mobile {
                Sidebar()
            },
            
            UI.Stack {
                flex: 1,
                children: [content],
            },
            
            if is_mobile {
                BottomNavigation()
            },
        ],
    }
}
```

## Security Considerations

### Permissions

Always request permissions appropriately:

```fluentai
private async function use_camera() {
    // Request permission first
    let granted = perform Platform.request_permission(
        Permission.Camera,
        reason: "To take profile photos"
    ).await();
    
    if granted {
        open_camera();
    } else {
        show_alternative_options();
    }
}
```

### Sandboxing

Respect platform sandboxing:

```fluentai
// Use appropriate directories
let app_data = Platform.app_data_dir(); // App-specific data
let documents = Platform.documents_dir(); // User documents
let temp = Platform.temp_dir(); // Temporary files

// Don't access arbitrary paths
// ❌ let path = "/etc/passwd";
// ✅ let path = app_data.join("config.json");
```

### Data Protection

Handle sensitive data appropriately:

```fluentai
// Use platform secure storage for sensitive data
Platform.secure_storage_write("api_key", encrypted_key);

// Clear clipboard after sensitive operations
Platform.clipboard_write(password);
Timer.delay(30.seconds).then(() => {
    Platform.clipboard_clear();
});
```

## Testing

### Platform Mocking

```rust
#[cfg(test)]
mod tests {
    use super::*;
    
    struct MockPlatform {
        theme: SystemTheme,
        files: Vec<PathBuf>,
    }
    
    impl PlatformIntegration for MockPlatform {
        fn system_theme(&self) -> SystemTheme {
            self.theme
        }
        
        fn show_file_picker(&self, _: FilePickerOptions) -> Option<Vec<PathBuf>> {
            Some(self.files.clone())
        }
        
        // ... other methods
    }
}
```

### Cross-Platform Testing

Test on all target platforms:

1. **Desktop**: Windows 10/11, macOS 11+, Ubuntu 20.04+
2. **Mobile**: iOS 14+, Android 8+
3. **Web**: Chrome, Firefox, Safari, Edge

## Future Enhancements

- **AR/VR Support**: Platform APIs for immersive experiences
- **Stylus/Pen Input**: Pressure-sensitive input on supported devices
- **Biometric Authentication**: Face ID, Touch ID, fingerprint
- **NFC/Bluetooth**: Near-field communication and BLE
- **Platform AI APIs**: Core ML, ML Kit, Web ML