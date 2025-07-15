/// Mobile platform integration (iOS, Android)
use super::*;
use std::sync::{Arc, Mutex};

/// Mobile platform implementation
pub struct MobilePlatform {
    #[cfg(target_os = "ios")]
    inner: IOSPlatform,
    #[cfg(target_os = "android")]
    inner: AndroidPlatform,
    orientation: Arc<Mutex<DeviceOrientation>>,
}

impl MobilePlatform {
    pub fn new() -> Self {
        Self {
            #[cfg(target_os = "ios")]
            inner: IOSPlatform::new(),
            #[cfg(target_os = "android")]
            inner: AndroidPlatform::new(),
            orientation: Arc::new(Mutex::new(DeviceOrientation::Portrait)),
        }
    }
    
    /// Request specific permissions
    pub fn request_permission(&self, permission: MobilePermission) -> Result<bool, PlatformError> {
        #[cfg(target_os = "ios")]
        return self.inner.request_permission(permission);
        
        #[cfg(target_os = "android")]
        return self.inner.request_permission(permission);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
    
    /// Get current battery level
    pub fn battery_level(&self) -> Option<f32> {
        #[cfg(target_os = "ios")]
        return self.inner.battery_level();
        
        #[cfg(target_os = "android")]
        return self.inner.battery_level();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        None
    }
    
    /// Vibrate device
    pub fn vibrate(&self, pattern: VibrationPattern) -> Result<(), PlatformError> {
        #[cfg(target_os = "ios")]
        return self.inner.vibrate(pattern);
        
        #[cfg(target_os = "android")]
        return self.inner.vibrate(pattern);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
    
    /// Set screen brightness (0.0 to 1.0)
    pub fn set_screen_brightness(&self, brightness: f32) -> Result<(), PlatformError> {
        let brightness = brightness.clamp(0.0, 1.0);
        
        #[cfg(target_os = "ios")]
        return self.inner.set_screen_brightness(brightness);
        
        #[cfg(target_os = "android")]
        return self.inner.set_screen_brightness(brightness);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
    
    /// Keep screen on
    pub fn set_keep_screen_on(&self, keep_on: bool) -> Result<(), PlatformError> {
        #[cfg(target_os = "ios")]
        return self.inner.set_keep_screen_on(keep_on);
        
        #[cfg(target_os = "android")]
        return self.inner.set_keep_screen_on(keep_on);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
}

impl PlatformIntegration for MobilePlatform {
    fn capabilities(&self) -> PlatformCapabilities {
        PlatformCapabilities {
            file_system: true,
            clipboard: true,
            notifications: true,
            system_tray: false,
            multi_window: false,
            native_menus: false,
            touch_input: true,
            accelerometer: true,
            camera: true,
            gpu_compute: true,
        }
    }
    
    fn system_theme(&self) -> SystemTheme {
        #[cfg(target_os = "ios")]
        return self.inner.system_theme();
        
        #[cfg(target_os = "android")]
        return self.inner.system_theme();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        SystemTheme::Light
    }
    
    fn accent_color(&self) -> Option<Color> {
        #[cfg(target_os = "ios")]
        return self.inner.accent_color();
        
        #[cfg(target_os = "android")]
        return self.inner.accent_color();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        None
    }
    
    fn show_file_picker(&self, options: FilePickerOptions) -> Option<Vec<PathBuf>> {
        #[cfg(any(target_os = "ios", target_os = "android"))]
        {
            // Use platform-specific document picker
            None
        }
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        None
    }
    
    fn show_save_dialog(&self, _options: SaveDialogOptions) -> Option<PathBuf> {
        // Mobile platforms typically use share sheets instead
        None
    }
    
    fn clipboard(&self) -> Box<dyn Clipboard> {
        #[cfg(target_os = "ios")]
        return Box::new(IOSClipboard::new());
        
        #[cfg(target_os = "android")]
        return Box::new(AndroidClipboard::new());
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Box::new(DummyClipboard)
    }
    
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError> {
        #[cfg(target_os = "ios")]
        return self.inner.show_notification(notification);
        
        #[cfg(target_os = "android")]
        return self.inner.show_notification(notification);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
    
    fn app_data_dir(&self) -> PathBuf {
        #[cfg(target_os = "ios")]
        return self.inner.app_data_dir();
        
        #[cfg(target_os = "android")]
        return self.inner.app_data_dir();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        PathBuf::from(".")
    }
    
    fn documents_dir(&self) -> PathBuf {
        #[cfg(target_os = "ios")]
        return self.inner.documents_dir();
        
        #[cfg(target_os = "android")]
        return self.inner.documents_dir();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        PathBuf::from(".")
    }
    
    fn open_url(&self, url: &str) -> Result<(), PlatformError> {
        #[cfg(target_os = "ios")]
        return self.inner.open_url(url);
        
        #[cfg(target_os = "android")]
        return self.inner.open_url(url);
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        Err(PlatformError::NotSupported)
    }
    
    fn device_info(&self) -> DeviceInfo {
        #[cfg(target_os = "ios")]
        return self.inner.device_info();
        
        #[cfg(target_os = "android")]
        return self.inner.device_info();
        
        #[cfg(not(any(target_os = "ios", target_os = "android")))]
        DeviceInfo {
            os: OperatingSystem::Unknown,
            version: String::new(),
            model: None,
            cpu_cores: 1,
            memory_mb: 1024,
            is_mobile: true,
            has_touch: true,
            screen_size: (375, 812),
            screen_dpi: 326.0,
        }
    }
}

/// Mobile-specific permissions
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MobilePermission {
    Camera,
    Microphone,
    Location,
    Contacts,
    Calendar,
    Photos,
    Notifications,
    Motion,
    Bluetooth,
}

/// Vibration patterns
#[derive(Debug, Clone)]
pub enum VibrationPattern {
    /// Single vibration in milliseconds
    Once(u32),
    /// Pattern of vibration and pause durations
    Pattern(Vec<u32>),
    /// Predefined patterns
    Success,
    Warning,
    Error,
    Selection,
}

// iOS-specific implementation
#[cfg(target_os = "ios")]
struct IOSPlatform;

#[cfg(target_os = "ios")]
impl IOSPlatform {
    fn new() -> Self {
        Self
    }
    
    fn request_permission(&self, permission: MobilePermission) -> Result<bool, PlatformError> {
        // Use iOS APIs to request permission
        Ok(false)
    }
    
    fn battery_level(&self) -> Option<f32> {
        // UIDevice.current.batteryLevel
        None
    }
    
    fn vibrate(&self, pattern: VibrationPattern) -> Result<(), PlatformError> {
        // Use UIImpactFeedbackGenerator or AudioServicesPlaySystemSound
        Ok(())
    }
    
    fn set_screen_brightness(&self, brightness: f32) -> Result<(), PlatformError> {
        // UIScreen.main.brightness = brightness
        Ok(())
    }
    
    fn set_keep_screen_on(&self, keep_on: bool) -> Result<(), PlatformError> {
        // UIApplication.shared.isIdleTimerDisabled = keep_on
        Ok(())
    }
    
    fn system_theme(&self) -> SystemTheme {
        // Check UITraitCollection.current.userInterfaceStyle
        SystemTheme::Light
    }
    
    fn accent_color(&self) -> Option<Color> {
        // UIColor.tintColor
        Some(Color::new(0.0, 0.478, 1.0, 1.0)) // iOS blue
    }
    
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError> {
        // Use UNUserNotificationCenter
        Ok(())
    }
    
    fn app_data_dir(&self) -> PathBuf {
        // NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)
        PathBuf::from("/var/mobile/Containers/Data/Application/...")
    }
    
    fn documents_dir(&self) -> PathBuf {
        self.app_data_dir().join("Documents")
    }
    
    fn open_url(&self, url: &str) -> Result<(), PlatformError> {
        // UIApplication.shared.open(URL(string: url))
        Ok(())
    }
    
    fn device_info(&self) -> DeviceInfo {
        DeviceInfo {
            os: OperatingSystem::iOS,
            version: "15.0".to_string(), // UIDevice.current.systemVersion
            model: Some("iPhone".to_string()), // UIDevice.current.model
            cpu_cores: 6,
            memory_mb: 4096,
            is_mobile: true,
            has_touch: true,
            screen_size: (390, 844), // UIScreen.main.bounds
            screen_dpi: 460.0, // UIScreen.main.scale * 160
        }
    }
}

#[cfg(target_os = "ios")]
struct IOSClipboard;

#[cfg(target_os = "ios")]
impl IOSClipboard {
    fn new() -> Self {
        Self
    }
}

#[cfg(target_os = "ios")]
impl Clipboard for IOSClipboard {
    fn read_text(&self) -> Option<String> {
        // UIPasteboard.general.string
        None
    }
    
    fn write_text(&mut self, text: &str) -> Result<(), PlatformError> {
        // UIPasteboard.general.string = text
        Ok(())
    }
    
    fn read_image(&self) -> Option<Vec<u8>> {
        // UIPasteboard.general.image
        None
    }
    
    fn write_image(&mut self, _data: &[u8]) -> Result<(), PlatformError> {
        // UIPasteboard.general.image = UIImage(data: data)
        Ok(())
    }
    
    fn clear(&mut self) {
        // UIPasteboard.general.items = []
    }
}

// Android-specific implementation
#[cfg(target_os = "android")]
struct AndroidPlatform;

#[cfg(target_os = "android")]
impl AndroidPlatform {
    fn new() -> Self {
        Self
    }
    
    fn request_permission(&self, permission: MobilePermission) -> Result<bool, PlatformError> {
        // Use Android permission APIs
        Ok(false)
    }
    
    fn battery_level(&self) -> Option<f32> {
        // BatteryManager
        None
    }
    
    fn vibrate(&self, pattern: VibrationPattern) -> Result<(), PlatformError> {
        // Vibrator service
        Ok(())
    }
    
    fn set_screen_brightness(&self, brightness: f32) -> Result<(), PlatformError> {
        // WindowManager.LayoutParams.screenBrightness
        Ok(())
    }
    
    fn set_keep_screen_on(&self, keep_on: bool) -> Result<(), PlatformError> {
        // Window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON)
        Ok(())
    }
    
    fn system_theme(&self) -> SystemTheme {
        // Check Configuration.uiMode & Configuration.UI_MODE_NIGHT_MASK
        SystemTheme::Light
    }
    
    fn accent_color(&self) -> Option<Color> {
        // Material You dynamic color
        None
    }
    
    fn show_notification(&self, notification: Notification) -> Result<(), PlatformError> {
        // NotificationManager
        Ok(())
    }
    
    fn app_data_dir(&self) -> PathBuf {
        // Context.getFilesDir()
        PathBuf::from("/data/data/com.fluentai.app/files")
    }
    
    fn documents_dir(&self) -> PathBuf {
        // Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOCUMENTS)
        PathBuf::from("/storage/emulated/0/Documents")
    }
    
    fn open_url(&self, url: &str) -> Result<(), PlatformError> {
        // Intent(Intent.ACTION_VIEW, Uri.parse(url))
        Ok(())
    }
    
    fn device_info(&self) -> DeviceInfo {
        DeviceInfo {
            os: OperatingSystem::Android,
            version: "12".to_string(), // Build.VERSION.RELEASE
            model: Some("Pixel".to_string()), // Build.MODEL
            cpu_cores: 8,
            memory_mb: 8192,
            is_mobile: true,
            has_touch: true,
            screen_size: (1080, 2400),
            screen_dpi: 420.0,
        }
    }
}

#[cfg(target_os = "android")]
struct AndroidClipboard;

#[cfg(target_os = "android")]
impl AndroidClipboard {
    fn new() -> Self {
        Self
    }
}

#[cfg(target_os = "android")]
impl Clipboard for AndroidClipboard {
    fn read_text(&self) -> Option<String> {
        // ClipboardManager
        None
    }
    
    fn write_text(&mut self, text: &str) -> Result<(), PlatformError> {
        // ClipboardManager.setPrimaryClip()
        Ok(())
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

// Dummy clipboard for unsupported platforms
#[cfg(not(any(target_os = "ios", target_os = "android")))]
struct DummyClipboard;

#[cfg(not(any(target_os = "ios", target_os = "android")))]
impl Clipboard for DummyClipboard {
    fn read_text(&self) -> Option<String> { None }
    fn write_text(&mut self, _: &str) -> Result<(), PlatformError> { Err(PlatformError::NotSupported) }
    fn read_image(&self) -> Option<Vec<u8>> { None }
    fn write_image(&mut self, _: &[u8]) -> Result<(), PlatformError> { Err(PlatformError::NotSupported) }
    fn clear(&mut self) {}
}

// Mobile-specific features

/// Biometric authentication
pub trait BiometricAuth {
    fn is_available(&self) -> bool;
    fn authenticate(&self, reason: &str) -> Result<bool, PlatformError>;
}

/// GPS/Location services
pub trait LocationServices {
    fn request_location_permission(&self) -> Result<bool, PlatformError>;
    fn get_current_location(&self) -> Result<Location, PlatformError>;
    fn start_location_updates<F>(&self, callback: F) where F: Fn(Location) + Send + 'static;
    fn stop_location_updates(&self);
}

#[derive(Debug, Clone)]
pub struct Location {
    pub latitude: f64,
    pub longitude: f64,
    pub altitude: Option<f64>,
    pub accuracy: f32,
    pub timestamp: f64,
}

/// Push notifications
pub trait PushNotifications {
    fn register_for_push(&self) -> Result<String, PlatformError>;
    fn unregister_for_push(&self) -> Result<(), PlatformError>;
    fn set_badge_count(&self, count: u32);
}

/// In-app purchases
pub trait InAppPurchases {
    fn get_products(&self, product_ids: Vec<String>) -> Result<Vec<Product>, PlatformError>;
    fn purchase(&self, product_id: &str) -> Result<Receipt, PlatformError>;
    fn restore_purchases(&self) -> Result<Vec<Receipt>, PlatformError>;
}

#[derive(Debug, Clone)]
pub struct Product {
    pub id: String,
    pub title: String,
    pub description: String,
    pub price: String,
    pub currency: String,
}

#[derive(Debug, Clone)]
pub struct Receipt {
    pub product_id: String,
    pub transaction_id: String,
    pub purchase_date: f64,
}