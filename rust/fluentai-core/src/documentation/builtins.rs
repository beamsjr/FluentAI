//! Compile-time enforcement for built-in function documentation

use crate::documentation::Documentation;

/// Macro to define a built-in function with enforced documentation
#[macro_export]
macro_rules! define_builtin {
    (
        name: $name:expr,
        signature: $signature:expr,
        description: $description:expr,
        examples: [$($example:expr),* $(,)?],
        module: $module:expr
    ) => {
        {
            // Compile-time validation through const assertions
            const _: () = {
                // Ensure name is not empty
                assert!($name.len() > 0, "Built-in name cannot be empty");
                // Ensure signature is not empty
                assert!($signature.len() > 0, "Built-in signature cannot be empty");
                // Ensure description is not empty
                assert!($description.len() > 0, "Built-in description cannot be empty");
                // Ensure at least one example is provided
                const EXAMPLES: &[&str] = &[$($example),*];
                assert!(EXAMPLES.len() > 0, "Built-in must have at least one example");
            };

            $crate::documentation::BuiltinDoc::new($name, $signature, $description, &[$($example),*], $module)
        }
    };
}

/// Registry of all built-in functions with compile-time validation
pub struct BuiltinRegistry;

impl BuiltinRegistry {
    /// Get all built-in functions with validated documentation
    pub fn all_builtins() -> Vec<crate::documentation::BuiltinDoc> {
        vec![
            // Core list operations
            define_builtin!(
                name: "cons",
                signature: "<list>.cons(<item>)",
                description: "Constructs a new list by prepending an item to an existing list.",
                examples: ["[2, 3].cons(1)", "[].cons(\"a\")", "xs.cons(x)"],
                module: "core"
            ),
            define_builtin!(
                name: "head",
                signature: "<list>.head()",
                description: "Returns the first element of a list. Error if list is empty.",
                examples: ["[1, 2, 3].head()", "[\"hello\"].head()", "xs.head()"],
                module: "core"
            ),
            define_builtin!(
                name: "tail",
                signature: "<list>.tail()",
                description: "Returns the list without its first element. Error if list is empty.",
                examples: ["[1, 2, 3].tail()", "[\"a\", \"b\"].tail()", "xs.tail()"],
                module: "core"
            ),
            // Basic arithmetic operations
            define_builtin!(
                name: "+",
                signature: "<num1> + <num2>",
                description: "Adds two or more numbers together.",
                examples: ["1 + 2", "1 + 2 + 3 + 4", "5.5 + 2.5"],
                module: "core"
            ),
            define_builtin!(
                name: "-",
                signature: "<num1> - <num2> | -<num>",
                description: "Subtracts numbers. With one argument, negates it.",
                examples: ["5 - 3", "10 - 3 - 2", "-5"],
                module: "core"
            ),
            define_builtin!(
                name: "*",
                signature: "<num1> * <num2>",
                description: "Multiplies two or more numbers together.",
                examples: ["2 * 3", "2 * 3 * 4", "5.5 * 2"],
                module: "core"
            ),
            define_builtin!(
                name: "/",
                signature: "<num1> / <num2>",
                description: "Divides numbers. Integer division returns a float.",
                examples: ["10 / 2", "20 / 4 / 2", "5 / 2"],
                module: "core"
            ),
            // String operations
            define_builtin!(
                name: "string-length",
                signature: "<string>.length()",
                description: "Returns the length of a string.",
                examples: ["\"hello\".length()", "\"\".length()"],
                module: "strings"
            ),
            define_builtin!(
                name: "string-append",
                signature: "<string1>.append(<string2>)",
                description: "Concatenates multiple strings.",
                examples: ["\"Hello, \".append(\"World!\")", "a.append(b).append(c)"],
                module: "strings"
            ),
            // I/O operations (via effects)
            define_builtin!(
                name: "print",
                signature: "print(<value>)",
                description: "Prints values to standard output without a newline.",
                examples: ["print(\"Hello, World!\")", "print(x)"],
                module: "io"
            ),
            define_builtin!(
                name: "println",
                signature: "println(<value>)",
                description: "Prints values to standard output with a newline.",
                examples: ["println(\"Hello, World!\")", "println(x)"],
                module: "io"
            ),
            // Additional arithmetic
            define_builtin!(
                name: "%",
                signature: "<num1> % <num2>",
                description: "Returns the remainder of dividing the first number by the second.",
                examples: ["10 % 3", "7 % 2"],
                module: "core"
            ),
            // Additional comparison operators are defined in register_operators() to avoid duplicates

            // Boolean operations are defined in register_operators() to avoid duplicates

            // List operations
            define_builtin!(
                name: "list",
                signature: "[<item1>, <item2>, ...]",
                description: "Creates a list from the given arguments.",
                examples: ["[1, 2, 3]", "[\"a\", \"b\", \"c\"]", "[]"],
                module: "core"
            ),
            define_builtin!(
                name: "length",
                signature: "<list>.length()",
                description: "Returns the length of a list.",
                examples: ["[1, 2, 3].length()", "[].length()", "xs.length()"],
                module: "core"
            ),
            define_builtin!(
                name: "is_empty",
                signature: "<list>.is_empty()",
                description: "Tests if a list is empty.",
                examples: ["[].is_empty()", "[1, 2].is_empty()", "xs.is_empty()"],
                module: "core"
            ),
            // String operations aliases
            define_builtin!(
                name: "to_uppercase",
                signature: "<string>.to_uppercase()",
                description: "Converts a string to uppercase.",
                examples: ["\"hello\".to_uppercase()", "\"World\".to_uppercase()"],
                module: "strings"
            ),
            define_builtin!(
                name: "to_lowercase",
                signature: "<string>.to_lowercase()",
                description: "Converts a string to lowercase.",
                examples: ["\"HELLO\".to_lowercase()", "\"World\".to_lowercase()"],
                module: "strings"
            ),
        ]
    }

    /// Validate that all built-ins have proper documentation
    pub fn validate_all() -> Result<(), &'static str> {
        // This will be checked at compile time by the macro
        Ok(())
    }
}

/// Get documentation for a specific built-in by name
pub fn get_builtin_doc(name: &str) -> Option<Documentation> {
    BuiltinRegistry::all_builtins()
        .into_iter()
        .find(|b| b.name == name)
        .map(|b| b.to_documentation())
}
