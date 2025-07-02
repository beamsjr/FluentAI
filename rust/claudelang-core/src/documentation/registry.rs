//! Documentation registry for ClaudeLang

use crate::documentation::traits::{Documentation, DocumentedNode, OperatorDoc, KeywordDoc, BuiltinDoc, Associativity};
use crate::documentation::impls::*;
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
            OperatorDoc::new("+", "Addition", 60, Associativity::Left, 
                "Adds two or more numbers together.", 
                &["(+ 1 2)", "(+ 1 2 3 4)", "(+ 5.5 2.5)"]),
            
            OperatorDoc::new("-", "Subtraction", 60, Associativity::Left,
                "Subtracts numbers. With one argument, negates it.",
                &["(- 5 3)", "(- 10 3 2)", "(- 5)"]),
            
            OperatorDoc::new("*", "Multiplication", 70, Associativity::Left,
                "Multiplies two or more numbers together.",
                &["(* 2 3)", "(* 2 3 4)", "(* 5.5 2)"]),
            
            OperatorDoc::new("/", "Division", 70, Associativity::Left,
                "Divides numbers. Integer division returns a float.",
                &["(/ 10 2)", "(/ 20 4 2)", "(/ 5 2)"]),
            
            // Comparison operators
            OperatorDoc::new("=", "Equality", 40, Associativity::None,
                "Tests if two values are equal.",
                &["(= 5 5)", "(= \"hello\" \"hello\")", "(= x y)"]),
            
            OperatorDoc::new("!=", "Inequality", 40, Associativity::None,
                "Tests if two values are not equal.",
                &["(!= 5 3)", "(!= \"hello\" \"world\")", "(!= x y)"]),
            
            OperatorDoc::new("<", "Less Than", 50, Associativity::None,
                "Tests if the first value is less than the second.",
                &["(< 3 5)", "(< x 10)", "(< \"a\" \"b\")"]),
            
            OperatorDoc::new(">", "Greater Than", 50, Associativity::None,
                "Tests if the first value is greater than the second.",
                &["(> 5 3)", "(> x 0)", "(> \"z\" \"a\")"]),
            
            OperatorDoc::new("<=", "Less Than or Equal", 50, Associativity::None,
                "Tests if the first value is less than or equal to the second.",
                &["(<= 3 5)", "(<= 5 5)", "(<= x limit)"]),
            
            OperatorDoc::new(">=", "Greater Than or Equal", 50, Associativity::None,
                "Tests if the first value is greater than or equal to the second.",
                &["(>= 5 3)", "(>= 5 5)", "(>= score threshold)"]),
            
            // Logical operators
            OperatorDoc::new("and", "Logical AND", 30, Associativity::Left,
                "Returns true if all arguments are true, false otherwise.",
                &["(and true true)", "(and (> x 0) (< x 10))", "(and a b c)"]),
            
            OperatorDoc::new("or", "Logical OR", 20, Associativity::Left,
                "Returns true if any argument is true, false otherwise.",
                &["(or true false)", "(or (< x 0) (> x 10))", "(or a b c)"]),
            
            OperatorDoc::new("not", "Logical NOT", 80, Associativity::Right,
                "Returns the logical negation of its argument.",
                &["(not true)", "(not false)", "(not (= x 0))"]),
        ]);
    }
    
    /// Registers all keywords
    fn register_keywords(&mut self) {
        self.keywords.extend(vec![
            KeywordDoc::new("lambda", 
                "Creates an anonymous function.",
                "(lambda (<params>) <body>)",
                &["(lambda (x) (+ x 1))", "(lambda (x y) (* x y))"]),
            
            KeywordDoc::new("let",
                "Creates local variable bindings.",
                "(let ((<var> <expr>) ...) <body>)",
                &["(let ((x 5)) (+ x 1))", "(let ((x 1) (y 2)) (+ x y))"]),
            
            KeywordDoc::new("letrec",
                "Creates recursive local bindings.",
                "(letrec ((<var> <expr>) ...) <body>)",
                &["(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))"]),
            
            KeywordDoc::new("if",
                "Conditional expression.",
                "(if <condition> <then> <else>)",
                &["(if (> x 0) \"positive\" \"non-positive\")"]),
            
            KeywordDoc::new("match",
                "Pattern matching expression.",
                "(match <expr> (<pattern> <body>) ...)",
                &["(match x (0 \"zero\") (1 \"one\") (_ \"other\"))"]),
            
            KeywordDoc::new("do",
                "Evaluates expressions in sequence, returns the last value.",
                "(do <expr1> <expr2> ...)",
                &["(do (print \"Starting\") (compute) (print \"Done\"))"]),
            
            KeywordDoc::new("effect",
                "Performs an effectful operation. Can also use shorthand <type>:<operation> syntax.",
                "(effect <type> <operation> <args>...) | (<type>:<operation> <args>...)",
                &["(effect IO print \"Hello\")", "(io:print \"Hello\")"]),
            
            KeywordDoc::new("module",
                "Defines a module.",
                "(module <name> <exports> <body>)",
                &["(module math [add subtract] ...)"]),
            
            KeywordDoc::new("import",
                "Imports from a module.",
                "(import <module-path> [<items>...] | *)",
                &["(import \"std/math\" [sin cos])"]),
            
            KeywordDoc::new("export",
                "Exports from a module.",
                "(export [<name> ...])",
                &["(export [helper utility])"]),
            
            KeywordDoc::new("async",
                "Creates an asynchronous computation.",
                "(async <body>)",
                &["(async (http-get url))"]),
            
            KeywordDoc::new("await",
                "Waits for an async computation.",
                "(await <async-expr>)",
                &["(await future)"]),
            
            KeywordDoc::new("spawn",
                "Spawns a concurrent task.",
                "(spawn <expr>)",
                &["(spawn (process-data))"]),
            
            KeywordDoc::new("chan",
                "Creates a channel.",
                "(chan)",
                &["(let ((ch (chan))) ...)"]),
            
            KeywordDoc::new("send!",
                "Sends to a channel.",
                "(send! <channel> <value>)",
                &["(send! ch 42)"]),
            
            KeywordDoc::new("recv!",
                "Receives from a channel.",
                "(recv! <channel>)",
                &["(recv! ch)"]),
        ]);
    }
    
    /// Registers all built-in functions
    fn register_builtins(&mut self) {
        self.builtins.extend(vec![
            // Core list operations
            BuiltinDoc::new("cons", 
                "(cons <item> <list>)",
                "Constructs a new list by prepending an item to an existing list.",
                &["(cons 1 [2 3])", "(cons \"a\" [])", "(cons x xs)"],
                "core"),
            
            BuiltinDoc::new("car",
                "(car <list>)",
                "Returns the first element of a list. Error if list is empty.",
                &["(car [1 2 3])", "(car [\"hello\"])", "(car xs)"],
                "core"),
            
            BuiltinDoc::new("cdr",
                "(cdr <list>)",
                "Returns the list without its first element. Error if list is empty.",
                &["(cdr [1 2 3])", "(cdr [\"a\" \"b\"])", "(cdr xs)"],
                "core"),
            
            BuiltinDoc::new("list?",
                "(list? <value>)",
                "Tests if a value is a list.",
                &["(list? [1 2 3])", "(list? \"hello\")", "(list? [])"],
                "core"),
            
            BuiltinDoc::new("null?",
                "(null? <list>)",
                "Tests if a list is empty.",
                &["(null? [])", "(null? [1 2])", "(null? xs)"],
                "core"),
            
            // Map operations
            BuiltinDoc::new("make-map",
                "(make-map)",
                "Creates a new empty map.",
                &["(make-map)", "(let ((m (make-map))) m)"],
                "core"),
            
            BuiltinDoc::new("map-get",
                "(map-get <map> <key>)",
                "Gets a value from a map by key. Returns nil if key not found.",
                &["(map-get m \"name\")", "(map-get config :debug)"],
                "core"),
            
            BuiltinDoc::new("map-set",
                "(map-set <map> <key> <value>)",
                "Sets a key-value pair in a map. Returns the updated map.",
                &["(map-set m \"name\" \"Alice\")", "(map-set m :count 42)"],
                "core"),
            
            BuiltinDoc::new("map?",
                "(map? <value>)",
                "Tests if a value is a map.",
                &["(map? {\"a\" 1})", "(map? [1 2 3])", "(map? m)"],
                "core"),
            
            // Tagged values
            BuiltinDoc::new("make-tagged",
                "(make-tagged <tag> <value> ...)",
                "Creates a tagged value with the given tag and values.",
                &["(make-tagged \"Point\" 3 4)", "(make-tagged \"Some\" x)"],
                "core"),
            
            BuiltinDoc::new("tagged?",
                "(tagged? <value>)",
                "Tests if a value is a tagged value.",
                &["(tagged? (make-tagged \"Point\" 1 2))", "(tagged? 42)"],
                "core"),
            
            BuiltinDoc::new("tagged-tag",
                "(tagged-tag <tagged>)",
                "Gets the tag of a tagged value.",
                &["(tagged-tag (make-tagged \"Point\" 1 2))"],
                "core"),
            
            BuiltinDoc::new("tagged-values",
                "(tagged-values <tagged>)",
                "Gets the values of a tagged value as a list.",
                &["(tagged-values (make-tagged \"Point\" 3 4))"],
                "core"),
            
            // String operations
            BuiltinDoc::new("string-length",
                "(string-length <string>)",
                "Returns the length of a string.",
                &["(string-length \"hello\")", "(string-length \"\")"],
                "strings"),
            
            BuiltinDoc::new("string-concat",
                "(string-concat <string1> <string2> ...)",
                "Concatenates multiple strings.",
                &["(string-concat \"Hello, \" \"World!\")", "(string-concat a b c)"],
                "strings"),
            
            BuiltinDoc::new("substring",
                "(substring <string> <start> <end>)",
                "Extracts a substring from start to end indices.",
                &["(substring \"hello\" 1 4)", "(substring s 0 5)"],
                "strings"),
            
            // IO operations
            BuiltinDoc::new("print",
                "(print <value> ...)",
                "Prints values to standard output with a newline.",
                &["(print \"Hello, World!\")", "(print x y z)"],
                "io"),
            
            BuiltinDoc::new("read-line",
                "(read-line)",
                "Reads a line from standard input.",
                &["(let ((name (read-line))) (print \"Hello, \" name))"],
                "io"),
            
            // Higher-order functions
            BuiltinDoc::new("map",
                "(map <function> <list>)",
                "Applies a function to each element of a list, returning a new list.",
                &["(map (lambda (x) (* x 2)) [1 2 3])", "(map string-length [\"a\" \"bb\" \"ccc\"])"],
                "functional"),
            
            BuiltinDoc::new("filter",
                "(filter <predicate> <list>)",
                "Returns a new list containing only elements that satisfy the predicate.",
                &["(filter (lambda (x) (> x 0)) [-1 2 -3 4])", "(filter even? [1 2 3 4])"],
                "functional"),
            
            BuiltinDoc::new("fold",
                "(fold <function> <initial> <list>)",
                "Reduces a list to a single value using a binary function.",
                &["(fold + 0 [1 2 3 4])", "(fold (lambda (acc x) (cons x acc)) [] [1 2 3])"],
                "functional"),
        ]);
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
            if doc.name.to_lowercase().contains(&query_lower) ||
               doc.description.to_lowercase().contains(&query_lower) ||
               doc.syntax.to_lowercase().contains(&query_lower) {
                results.push(doc.clone());
            }
        }
        
        // Search operators
        for op in &self.operators {
            if op.name.to_lowercase().contains(&query_lower) ||
               op.symbol.contains(query) ||
               op.description.to_lowercase().contains(&query_lower) {
                let doc = op.to_documentation();
                if !results.iter().any(|d| d.name == doc.name) {
                    results.push(doc);
                }
            }
        }
        
        // Search keywords
        for kw in &self.keywords {
            if kw.keyword.to_lowercase().contains(&query_lower) ||
               kw.description.to_lowercase().contains(&query_lower) {
                let doc = kw.to_documentation();
                if !results.iter().any(|d| d.name == doc.name) {
                    results.push(doc);
                }
            }
        }
        
        // Search builtins
        for builtin in &self.builtins {
            if builtin.name.to_lowercase().contains(&query_lower) ||
               builtin.description.to_lowercase().contains(&query_lower) ||
               builtin.signature.to_lowercase().contains(&query_lower) {
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
}

impl Default for DocumentationRegistry {
    fn default() -> Self {
        Self::new()
    }
}