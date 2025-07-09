use fluentai_ui_compiler::{CompilerOptions, OutputFormat, UICompiler};

#[test]
fn test_simple_ui_element() {
    let ui_code = r#"(ui:element "div" {} (ui:text "Hello World"))"#;
    
    let mut options = CompilerOptions::default();
    options.output_format = OutputFormat::VanillaJS;
    
    let mut compiler = UICompiler::new(options).expect("Failed to create compiler");
    let result = compiler.compile(ui_code);
    
    match result {
        Ok(js_code) => {
            println!("Generated JS:\n{}", js_code);
            assert!(js_code.contains("createElement"));
            assert!(js_code.contains("createTextNode"));
        }
        Err(e) => panic!("Compilation failed: {:?}", e),
    }
}

#[test]
fn test_ui_with_binding() {
    let ui_code = r#"
        (let ((count 0))
          (ui:element "div" {}
            (ui:text count)))
    "#;
    
    let mut options = CompilerOptions::default();
    options.output_format = OutputFormat::VanillaJS;
    
    let mut compiler = UICompiler::new(options).expect("Failed to create compiler");
    let result = compiler.compile(ui_code);
    
    match result {
        Ok(js_code) => {
            println!("Generated JS with binding:\n{}", js_code);
            assert!(js_code.contains("const count = 0"));
        }
        Err(e) => panic!("Compilation failed: {:?}", e),
    }
}

#[test]
fn test_ui_event_handler() {
    let ui_code = r#"
        (ui:element "button" 
          (ui:on "click" (lambda () (print "clicked")))
          (ui:text "Click me"))
    "#;
    
    let mut options = CompilerOptions::default();
    options.output_format = OutputFormat::VanillaJS;
    
    let mut compiler = UICompiler::new(options).expect("Failed to create compiler");
    let result = compiler.compile(ui_code);
    
    match result {
        Ok(js_code) => {
            println!("Generated JS with event:\n{}", js_code);
            assert!(js_code.contains("onClick"));
        }
        Err(e) => panic!("Compilation failed: {:?}", e),
    }
}