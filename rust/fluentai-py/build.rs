fn main() {
    pyo3_build_config::add_extension_module_link_args();

    // On macOS, we need to link against the Python framework
    #[cfg(target_os = "macos")]
    {
        // Add the framework search path
        println!("cargo:rustc-link-search=framework=/opt/homebrew/opt/python@3.13/Frameworks");
    }
}
