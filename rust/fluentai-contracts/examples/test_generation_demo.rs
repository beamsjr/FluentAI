//! Example demonstrating automatic test case generation from symbolic execution

use fluentai_parser::parse;
use fluentai_contracts::{
    SymbolicExecutor, 
    test_generation::{TestGenerator, format_test_cases, TestLanguage}
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example: Safe division function
    println!("=== Test Generation for Safe Division Function ===");
    {
        let program = r#"
            (define (safe-divide a b)
              (if (= b 0)
                  0
                  (/ a b)))
        "#;
        
        let graph = parse(program)?;
        
        // Execute symbolically
        let executor = SymbolicExecutor::new();
        let states = executor.execute_function_by_name(&graph, "safe-divide")?;
        
        println!("Found {} execution paths", states.len());
        
        // Generate test cases
        let generator = TestGenerator::new();
        let test_cases = generator.generate_tests(&states, &["a".to_string(), "b".to_string()])?;
        
        println!("Generated {} test cases", test_cases.len());
        
        // Format as FluentAi tests
        println!("\nFluentAi test cases:");
        println!("{}", format_test_cases(&test_cases, "safe-divide", TestLanguage::FluentAi));
        
        // Format as Rust tests
        println!("\nRust test cases:");
        println!("{}", format_test_cases(&test_cases, "safe_divide", TestLanguage::Rust));
    }
    
    // Example: Triangle classification
    println!("\n\n=== Test Generation for Triangle Classification ===");
    {
        let program = r#"
            (define (triangle-type a b c)
              (if (or (<= a 0) (<= b 0) (<= c 0))
                  'invalid
                  (if (or (>= a (+ b c)) (>= b (+ a c)) (>= c (+ a b)))
                      'invalid
                      (if (and (= a b) (= b c))
                          'equilateral
                          (if (or (= a b) (= b c) (= a c))
                              'isosceles
                              'scalene)))))
        "#;
        
        let graph = parse(program)?;
        
        // Execute symbolically with limited depth to avoid explosion
        let executor = SymbolicExecutor::with_limits(50, 100);
        let states = executor.execute_function_by_name(&graph, "triangle-type")?;
        
        println!("Found {} execution paths", states.len());
        
        // Generate test cases
        let generator = TestGenerator::new();
        let test_cases = generator.generate_tests(
            &states, 
            &["a".to_string(), "b".to_string(), "c".to_string()]
        )?;
        
        println!("Generated {} test cases", test_cases.len());
        
        // Show first few test cases
        for (i, test) in test_cases.iter().take(5).enumerate() {
            println!("\nTest case {}: {}", i + 1, test.description);
            println!("  Inputs: a={:?}, b={:?}, c={:?}", 
                test.inputs.get("a"), 
                test.inputs.get("b"), 
                test.inputs.get("c")
            );
        }
    }
    
    Ok(())
}