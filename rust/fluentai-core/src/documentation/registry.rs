//! Documentation registry for FluentAi

use crate::documentation::impls::*;
use crate::documentation::traits::{
    Associativity, BuiltinDoc, Documentation, DocumentedNode, KeywordDoc, OperatorDoc,
};
use rustc_hash::FxHashMap;

/// Registry that holds all documentation for the language
pub struct DocumentationRegistry {
    constructs: FxHashMap<String, Documentation>,
    operators: Vec<OperatorDoc>,
    keywords: Vec<KeywordDoc>,
    builtins: Vec<BuiltinDoc>,
}

impl DocumentationRegistry {
    /// Creates a new registry and registers all language constructs
    pub fn new() -> Self {
        let mut registry = Self {
            constructs: FxHashMap::default(),
            operators: Vec::new(),
            keywords: Vec::new(),
            builtins: Vec::new(),
        };

        registry.register_all();
        registry
    }

    /// Registers all language constructs
    fn register_all(&mut self) {
        // Register literals
        self.register::<IntegerDoc>();
        self.register::<FloatDoc>();
        self.register::<StringDoc>();
        self.register::<BooleanDoc>();
        self.register::<NilDoc>();

        // Register variables
        self.register::<VariableDoc>();
        self.register::<QualifiedVariableDoc>();

        // Register functions
        self.register::<LambdaDoc>();
        self.register::<ApplicationDoc>();
        self.register::<LetDoc>();
        self.register::<LetrecDoc>();

        // Register control flow
        self.register::<IfDoc>();
        self.register::<MatchDoc>();
        self.register::<DoDoc>();

        // Register effects
        self.register::<EffectDoc>();

        // Register data structures
        self.register::<ListDoc>();
        self.register::<ListLiteralDoc>();
        self.register::<MapDoc>();
        self.register::<TaggedDoc>();

        // Register modules
        self.register::<ModuleDoc>();
        self.register::<ImportDoc>();
        self.register::<ExportDoc>();

        // Register async
        self.register::<AsyncDoc>();
        self.register::<AwaitDoc>();
        self.register::<SpawnDoc>();
        self.register::<ChannelDoc>();
        self.register::<SendDoc>();
        self.register::<ReceiveDoc>();

        // Register operators
        self.register_operators();

        // Register keywords
        self.register_keywords();

        // Register built-in functions
        self.register_builtins();
    }

    /// Registers a single construct
    fn register<T: DocumentedNode>(&mut self) {
        let docs = T::get_docs();
        self.constructs.insert(docs.name.clone(), docs);
    }

    /// Registers all operators
    fn register_operators(&mut self) {
        self.operators.extend(vec![
            // Arithmetic operators
            OperatorDoc::new(
                "+",
                "Addition",
                60,
                Associativity::Left,
                "Adds two or more numbers together.",
                &["1 + 2", "1 + 2 + 3 + 4", "5.5 + 2.5"],
            ),
            OperatorDoc::new(
                "-",
                "Subtraction",
                60,
                Associativity::Left,
                "Subtracts numbers. With one argument, negates it.",
                &["5 - 3", "10 - 3 - 2", "-5"],
            ),
            OperatorDoc::new(
                "*",
                "Multiplication",
                70,
                Associativity::Left,
                "Multiplies two or more numbers together.",
                &["2 * 3", "2 * 3 * 4", "5.5 * 2"],
            ),
            OperatorDoc::new(
                "/",
                "Division",
                70,
                Associativity::Left,
                "Divides numbers. Integer division returns a float.",
                &["10 / 2", "20 / 4 / 2", "5 / 2"],
            ),
            // Comparison operators
            OperatorDoc::new(
                "==",
                "Equality",
                40,
                Associativity::None,
                "Tests if two values are equal.",
                &["5 == 5", "\"hello\" == \"hello\"", "x == y"],
            ),
            OperatorDoc::new(
                "!=",
                "Inequality",
                40,
                Associativity::None,
                "Tests if two values are not equal.",
                &["5 != 3", "\"hello\" != \"world\"", "x != y"],
            ),
            OperatorDoc::new(
                "<",
                "Less Than",
                50,
                Associativity::None,
                "Tests if the first value is less than the second.",
                &["3 < 5", "x < 10", "\"a\" < \"b\""],
            ),
            OperatorDoc::new(
                ">",
                "Greater Than",
                50,
                Associativity::None,
                "Tests if the first value is greater than the second.",
                &["5 > 3", "x > 0", "\"z\" > \"a\""],
            ),
            OperatorDoc::new(
                "<=",
                "Less Than or Equal",
                50,
                Associativity::None,
                "Tests if the first value is less than or equal to the second.",
                &["3 <= 5", "5 <= 5", "x <= limit"],
            ),
            OperatorDoc::new(
                ">=",
                "Greater Than or Equal",
                50,
                Associativity::None,
                "Tests if the first value is greater than or equal to the second.",
                &["5 >= 3", "5 >= 5", "score >= threshold"],
            ),
            // Logical operators
            OperatorDoc::new(
                "and",
                "Logical AND",
                30,
                Associativity::Left,
                "Returns true if all arguments are true, false otherwise.",
                &["true && true", "(x > 0) && (x < 10)", "a && b && c"],
            ),
            OperatorDoc::new(
                "or",
                "Logical OR",
                20,
                Associativity::Left,
                "Returns true if any argument is true, false otherwise.",
                &["true || false", "(x < 0) || (x > 10)", "a || b || c"],
            ),
            OperatorDoc::new(
                "not",
                "Logical NOT",
                80,
                Associativity::Right,
                "Returns the logical negation of its argument.",
                &["!true", "!false", "!(x == 0)"],
            ),
        ]);
    }

    /// Registers all keywords
    fn register_keywords(&mut self) {
        self.keywords.extend(vec![
            KeywordDoc::new("lambda", 
                "Creates an anonymous function.",
                "(<params>) => <body> | <param> => <body>",
                &["x => x + 1", "(x, y) => x * y"]),

            KeywordDoc::new("let",
                "Creates local variable bindings.",
                "let <var> = <expr>; <body>",
                &["let x = 5; x + 1", "let x = 1; let y = 2; x + y"]),

            KeywordDoc::new("letrec",
                "Creates recursive local bindings.",
                "let <var> = <expr>; <body> (recursive)",
                &["let fact = (n) => { if (n == 0) { 1 } else { n * fact(n - 1) } }; fact(5)"]),

            KeywordDoc::new("if",
                "Conditional expression.",
                "if (<condition>) { <then> } else { <else> }",
                &["if (x > 0) { \"positive\" } else { \"non-positive\" }"]),

            KeywordDoc::new("match",
                "Pattern matching expression.",
                "<expr>.match().case(<pattern>, <body>)....get()",
                &["x.match().case(0, \"zero\").case(1, \"one\").case(_, \"other\").get()"]),

            KeywordDoc::new("do",
                "Evaluates expressions in sequence, returns the last value.",
                "{ <expr1>; <expr2>; ... }",
                &["{ print(\"Starting\"); compute(); print(\"Done\") }"]),

            KeywordDoc::new("effect",
                "Performs an effectful operation. Can also use shorthand <type>:<operation> syntax.",
                "perform <type>.<operation>(<args>...)",
                &["perform IO.print(\"Hello\")", "io.print(\"Hello\")"]),

            KeywordDoc::new("module",
                "Defines a module.",
                "mod <name> { export { <items>... }; <body> }",
                &["mod math { export { add, subtract }; ... }"]),

            KeywordDoc::new("import",
                "Imports from a module.",
                "use <module-path>::{<items>...} | use <module-path>::*",
                &["use std::math::{sin, cos};"]),

            KeywordDoc::new("export",
                "Exports from a module.",
                "export { <name>, ... };",
                &["export { helper, utility };"]),

            KeywordDoc::new("async",
                "Creates an asynchronous computation.",
                "async { <body> }",
                &["async { http.get(url) }"]),

            KeywordDoc::new("await",
                "Waits for an async computation.",
                "<async-expr>.await()",
                &["future.await()"]),

            KeywordDoc::new("spawn",
                "Spawns a concurrent task.",
                "spawn { <expr> }",
                &["spawn { process_data() }"]),

            KeywordDoc::new("chan",
                "Creates a channel.",
                "channel() | channel(<capacity>)",
                &["let ch = channel();"]),

            KeywordDoc::new("send!",
                "Sends to a channel.",
                "<channel>.send(<value>)",
                &["ch.send(42)"]),

            KeywordDoc::new("recv!",
                "Receives from a channel.",
                "<channel>.receive()",
                &["ch.receive()"]),
        ]);
    }

    /// Registers all built-in functions
    fn register_builtins(&mut self) {
        // Use the compile-time validated built-ins from the builtins module
        self.builtins = crate::documentation::builtins::BuiltinRegistry::all_builtins();
    }

    /// Get documentation for a specific construct by name
    pub fn get(&self, name: &str) -> Option<&Documentation> {
        self.constructs.get(name)
    }

    /// Search documentation by query string
    pub fn search(&self, query: &str) -> Vec<Documentation> {
        let query_lower = query.to_lowercase();
        let mut results = Vec::new();

        // Search constructs
        for doc in self.constructs.values() {
            if doc.name.to_lowercase().contains(&query_lower)
                || doc.description.to_lowercase().contains(&query_lower)
                || doc.syntax.to_lowercase().contains(&query_lower)
            {
                results.push(doc.clone());
            }
        }

        // Search operators
        for op in &self.operators {
            if op.name.to_lowercase().contains(&query_lower)
                || op.symbol.contains(query)
                || op.description.to_lowercase().contains(&query_lower)
            {
                let doc = op.to_documentation();
                if !results.iter().any(|d| d.name == doc.name) {
                    results.push(doc);
                }
            }
        }

        // Search keywords
        for kw in &self.keywords {
            if kw.keyword.to_lowercase().contains(&query_lower)
                || kw.description.to_lowercase().contains(&query_lower)
            {
                let doc = kw.to_documentation();
                if !results.iter().any(|d| d.name == doc.name) {
                    results.push(doc);
                }
            }
        }

        // Search builtins
        for builtin in &self.builtins {
            if builtin.name.to_lowercase().contains(&query_lower)
                || builtin.description.to_lowercase().contains(&query_lower)
                || builtin.signature.to_lowercase().contains(&query_lower)
            {
                let doc = builtin.to_documentation();
                if !results.iter().any(|d| d.name == doc.name) {
                    results.push(doc);
                }
            }
        }

        results
    }

    /// List all available constructs
    pub fn list_all(&self) -> Vec<Documentation> {
        let mut all = Vec::new();

        // Add all constructs
        all.extend(self.constructs.values().cloned());

        // Add all operators as documentation
        for op in &self.operators {
            all.push(op.to_documentation());
        }

        // Add all keywords as documentation
        for kw in &self.keywords {
            all.push(kw.to_documentation());
        }

        // Add all builtins as documentation
        for builtin in &self.builtins {
            all.push(builtin.to_documentation());
        }

        all
    }

    /// Get all operators
    pub fn get_operators(&self) -> &[OperatorDoc] {
        &self.operators
    }

    /// Get all keywords
    pub fn get_keywords(&self) -> &[KeywordDoc] {
        &self.keywords
    }

    /// Get all built-in functions
    pub fn get_builtins(&self) -> &[BuiltinDoc] {
        &self.builtins
    }

    /// List all user-facing constructs (filters out internal documentation)
    pub fn list_user_facing(&self) -> Vec<Documentation> {
        self.list_all()
            .into_iter()
            .filter(|doc| doc.visibility == crate::documentation::DocumentationVisibility::Public)
            .collect()
    }

    /// Search only user-facing documentation
    pub fn search_user_facing(&self, query: &str) -> Vec<Documentation> {
        self.search(query)
            .into_iter()
            .filter(|doc| doc.visibility == crate::documentation::DocumentationVisibility::Public)
            .collect()
    }
}

impl Default for DocumentationRegistry {
    fn default() -> Self {
        Self::new()
    }
}
