//! Documentation implementations for all AST nodes

use crate::documentation::traits::{DocumentationCategory, DocumentedNode};

// Marker types for documentation
pub struct LiteralDoc;
pub struct VariableDoc;
pub struct LambdaDoc;
pub struct LetDoc;
pub struct LetrecDoc;
pub struct IfDoc;
pub struct ApplicationDoc;
pub struct EffectDoc;
pub struct ListDoc;
pub struct MatchDoc;
pub struct ModuleDoc;
pub struct ImportDoc;
pub struct ExportDoc;
pub struct QualifiedVariableDoc;
pub struct AsyncDoc;
pub struct AwaitDoc;
pub struct SpawnDoc;
pub struct ChannelDoc;
pub struct SendDoc;
pub struct ReceiveDoc;

// Additional constructs
pub struct DoDoc;
pub struct MapDoc;
pub struct TaggedDoc;
pub struct ListLiteralDoc;

// Literal types
pub struct IntegerDoc;
pub struct FloatDoc;
pub struct StringDoc;
pub struct BooleanDoc;
pub struct NilDoc;

impl DocumentedNode for IntegerDoc {
    fn name() -> &'static str {
        "Integer"
    }

    fn syntax() -> &'static str {
        "<integer>"
    }

    fn description() -> &'static str {
        "Integer literals represent whole numbers. FluentAi supports 64-bit signed integers."
    }

    fn examples() -> &'static [&'static str] {
        &["42", "-17", "0", "1000000"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

impl DocumentedNode for FloatDoc {
    fn name() -> &'static str {
        "Float"
    }

    fn syntax() -> &'static str {
        "<float>"
    }

    fn description() -> &'static str {
        "Floating-point literals represent decimal numbers. FluentAi uses 64-bit double precision."
    }

    fn examples() -> &'static [&'static str] {
        &["3.14", "-2.5", "0.0", "1.23e-4"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

impl DocumentedNode for StringDoc {
    fn name() -> &'static str {
        "String"
    }

    fn syntax() -> &'static str {
        "\"<text>\""
    }

    fn description() -> &'static str {
        "String literals represent text data. Strings are enclosed in double quotes and support escape sequences."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "\"Hello, World!\"",
            "\"Line 1\\nLine 2\"",
            "\"Tab\\there\"",
            "\"Quote: \\\"example\\\"\"",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

impl DocumentedNode for BooleanDoc {
    fn name() -> &'static str {
        "Boolean"
    }

    fn syntax() -> &'static str {
        "true | false"
    }

    fn description() -> &'static str {
        "Boolean literals represent truth values. Only 'true' and 'false' are valid boolean values."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "true",
            "false",
            "(if true \"yes\" \"no\")",
            "(and true false)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

impl DocumentedNode for NilDoc {
    fn name() -> &'static str {
        "Nil"
    }

    fn syntax() -> &'static str {
        "nil"
    }

    fn description() -> &'static str {
        "Nil represents the absence of a value. It is the only value of its type."
    }

    fn examples() -> &'static [&'static str] {
        &["nil", "(if false \"yes\" nil)", "(let ((x nil)) x)"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Literal
    }
}

impl DocumentedNode for VariableDoc {
    fn name() -> &'static str {
        "Variable"
    }

    fn syntax() -> &'static str {
        "<identifier>"
    }

    fn description() -> &'static str {
        "Variables reference values bound in the current scope. Variable names must start with a letter or underscore."
    }

    fn examples() -> &'static [&'static str] {
        &["x", "count", "my_variable", "_temp"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Variable
    }

    fn see_also() -> &'static [&'static str] {
        &["Let", "Lambda", "QualifiedVariable"]
    }
}

impl DocumentedNode for LambdaDoc {
    fn name() -> &'static str {
        "Lambda"
    }

    fn syntax() -> &'static str {
        "(lambda (<params>) <body>)"
    }

    fn description() -> &'static str {
        "Lambda creates anonymous functions. Parameters are bound in the function body scope."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(lambda (x) (+ x 1))",
            "(lambda (x y) (* x y))",
            "(lambda () \"Hello\")",
            "((lambda (x) (* x x)) 5)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Function
    }

    fn see_also() -> &'static [&'static str] {
        &["Application", "Let"]
    }
}

impl DocumentedNode for LetDoc {
    fn name() -> &'static str {
        "Let"
    }

    fn syntax() -> &'static str {
        "(let ((<var> <expr>) ...) <body>)"
    }

    fn description() -> &'static str {
        "Let creates local variable bindings. Bindings are evaluated sequentially and are available in the body."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(let ((x 5)) (+ x 1))",
            "(let ((x 10) (y 20)) (+ x y))",
            "(let ((x 5) (y (* x 2))) y)",
            "(let () \"no bindings\")",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Function
    }

    fn see_also() -> &'static [&'static str] {
        &["Letrec", "Lambda"]
    }
}

impl DocumentedNode for LetrecDoc {
    fn name() -> &'static str {
        "Letrec"
    }

    fn syntax() -> &'static str {
        "(letrec ((<var> <expr>) ...) <body>)"
    }

    fn description() -> &'static str {
        "Letrec creates recursive local bindings. All bindings are in scope for all binding expressions, enabling mutual recursion."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))",
            "(letrec ((even? (lambda (n) (if (= n 0) true (odd? (- n 1))))) \
                      (odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))) \
              (even? 4))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Function
    }

    fn see_also() -> &'static [&'static str] {
        &["Let", "Lambda"]
    }
}

impl DocumentedNode for IfDoc {
    fn name() -> &'static str {
        "If"
    }

    fn syntax() -> &'static str {
        "(if <condition> <then> <else>)"
    }

    fn description() -> &'static str {
        "Conditional expression. Evaluates condition, then evaluates and returns either the then or else branch."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(if true \"yes\" \"no\")",
            "(if (> x 0) \"positive\" \"non-positive\")",
            "(if (= x 0) 0 (/ 1 x))",
            "(if condition (do-something) nil)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::ControlFlow
    }

    fn see_also() -> &'static [&'static str] {
        &["Match", "Boolean"]
    }
}

impl DocumentedNode for ApplicationDoc {
    fn name() -> &'static str {
        "Application"
    }

    fn syntax() -> &'static str {
        "(<function> <arg1> <arg2> ...)"
    }

    fn description() -> &'static str {
        "Function application. Applies a function to zero or more arguments."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(+ 1 2)",
            "(print \"Hello\")",
            "(map square [1 2 3])",
            "((lambda (x) (* x 2)) 5)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Function
    }

    fn see_also() -> &'static [&'static str] {
        &["Lambda"]
    }
}

impl DocumentedNode for EffectDoc {
    fn name() -> &'static str {
        "Effect"
    }

    fn syntax() -> &'static str {
        "(effect <type> <operation> <args>...) | (<type>:<operation> <args>...)"
    }

    fn description() -> &'static str {
        "Performs an effectful operation. Effects are tracked by the type system and handled by effect handlers. Can be called using explicit effect syntax or shorthand notation with colon."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(effect IO print \"Hello\")",
            "(io:print \"Hello\")",
            "(effect State get)",
            "(state:get)",
            "(effect State set 42)",
            "(state:set 42)",
            "(effect Error raise \"Something went wrong\")",
            "(error:raise \"Something went wrong\")",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Effect
    }

    fn see_also() -> &'static [&'static str] {
        &["Async", "IO", "print", "read-line"]
    }
}

impl DocumentedNode for ListDoc {
    fn name() -> &'static str {
        "List"
    }

    fn syntax() -> &'static str {
        "[<expr1> <expr2> ...]"
    }

    fn description() -> &'static str {
        "List literal. Creates a list containing the evaluated expressions."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "[1 2 3]",
            "[\"a\" \"b\" \"c\"]",
            "[]",
            "[x (+ y 1) (* z 2)]",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }

    fn see_also() -> &'static [&'static str] {
        &["cons", "car", "cdr"]
    }
}

impl DocumentedNode for MatchDoc {
    fn name() -> &'static str {
        "Match"
    }

    fn syntax() -> &'static str {
        "(match <expr> (<pattern> <body>) ...)"
    }

    fn description() -> &'static str {
        "Pattern matching expression. Matches the expression against patterns and evaluates the corresponding body."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(match x (0 \"zero\") (1 \"one\") (_ \"other\"))",
            "(match lst ([] \"empty\") ([x] \"single\") ([x y ...] \"multiple\"))",
            "(match val (true \"yes\") (false \"no\"))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::PatternMatching
    }

    fn see_also() -> &'static [&'static str] {
        &["If"]
    }
}

impl DocumentedNode for ModuleDoc {
    fn name() -> &'static str {
        "Module"
    }

    fn syntax() -> &'static str {
        "(module <name> (export <names>...) <body>)"
    }

    fn description() -> &'static str {
        "Defines a module with a name, list of exports, and body. Modules provide namespace isolation and encapsulation. Only exported symbols are accessible from outside the module."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(module math (export add multiply pi) (let ((pi 3.14159) (add +) (multiply *)) nil))",
            "(module utils (export helper) (let ((helper (lambda (x) (* x 2))) (internal (lambda () \"private\"))) nil))",
            "(module counter (export make-counter) (let ((make-counter (lambda () (let ((count 0)) (lambda () (set! count (+ count 1))))))) nil))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Module
    }

    fn see_also() -> &'static [&'static str] {
        &["Import", "Export", "QualifiedVariable"]
    }
}

impl DocumentedNode for ImportDoc {
    fn name() -> &'static str {
        "Import"
    }

    fn syntax() -> &'static str {
        "(import \"<module-path>\" (<items>...) | *)"
    }

    fn description() -> &'static str {
        "Imports items from a module. Can import specific items with a list or all exports with *. Imported items become available in the current scope. Supports aliasing with 'as'."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(import \"math\" (sin cos))",
            "(import \"utils\" *)",
            "(import \"./local-module\" (helper))",
            "(import \"math\" (sin as sine cos as cosine))",
            "(import \"collections\" (map filter reduce))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Module
    }

    fn see_also() -> &'static [&'static str] {
        &["Module", "Export", "QualifiedVariable"]
    }
}

impl DocumentedNode for ExportDoc {
    fn name() -> &'static str {
        "Export"
    }

    fn syntax() -> &'static str {
        "(export <name1> <name2> ... | <name> as <alias> ...)"
    }

    fn description() -> &'static str {
        "Exports items from the current module. Can optionally rename exports with aliases."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(export add subtract multiply)",
            "(export internal-helper as helper)",
            "(export x y z)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Module
    }

    fn see_also() -> &'static [&'static str] {
        &["Module", "Import"]
    }
}

impl DocumentedNode for QualifiedVariableDoc {
    fn name() -> &'static str {
        "QualifiedVariable"
    }

    fn syntax() -> &'static str {
        "<module>.<variable>"
    }

    fn description() -> &'static str {
        "References a variable from a specific module namespace."
    }

    fn examples() -> &'static [&'static str] {
        &["math.pi", "std.print", "utils.helper"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Variable
    }

    fn see_also() -> &'static [&'static str] {
        &["Variable", "Import", "Module"]
    }
}

impl DocumentedNode for AsyncDoc {
    fn name() -> &'static str {
        "Async"
    }

    fn syntax() -> &'static str {
        "(async <body>)"
    }

    fn description() -> &'static str {
        "Creates an asynchronous computation that can be awaited."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(async (http-get \"https://api.example.com\"))",
            "(async (do (sleep 1000) \"done\"))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Await", "Spawn"]
    }
}

impl DocumentedNode for AwaitDoc {
    fn name() -> &'static str {
        "Await"
    }

    fn syntax() -> &'static str {
        "(await <async-expr>)"
    }

    fn description() -> &'static str {
        "Waits for an asynchronous computation to complete and returns its result."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(await (async (+ 1 2)))",
            "(let ((future (async (compute)))) (await future))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Async", "Spawn"]
    }
}

impl DocumentedNode for SpawnDoc {
    fn name() -> &'static str {
        "Spawn"
    }

    fn syntax() -> &'static str {
        "(spawn <expr>)"
    }

    fn description() -> &'static str {
        "Spawns a new concurrent task. Returns immediately without waiting for completion."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(spawn (process-data data))",
            "(spawn (do (print \"Task started\") (long-computation)))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Async", "Channel"]
    }
}

impl DocumentedNode for ChannelDoc {
    fn name() -> &'static str {
        "Channel"
    }

    fn syntax() -> &'static str {
        "(chan)"
    }

    fn description() -> &'static str {
        "Creates a new communication channel for sending values between concurrent tasks."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(let ((ch (chan))) ...)",
            "(let ((ch (chan))) (spawn (send! ch 42)) (recv! ch))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Send", "Receive", "Spawn"]
    }
}

impl DocumentedNode for SendDoc {
    fn name() -> &'static str {
        "Send"
    }

    fn syntax() -> &'static str {
        "(send! <channel> <value>)"
    }

    fn description() -> &'static str {
        "Sends a value through a channel. May block if the channel is full."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(send! ch 42)",
            "(send! ch \"message\")",
            "(send! ch [1 2 3])",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Channel", "Receive"]
    }
}

impl DocumentedNode for ReceiveDoc {
    fn name() -> &'static str {
        "Receive"
    }

    fn syntax() -> &'static str {
        "(recv! <channel>)"
    }

    fn description() -> &'static str {
        "Receives a value from a channel. Blocks until a value is available."
    }

    fn examples() -> &'static [&'static str] {
        &["(recv! ch)", "(let ((msg (recv! ch))) (print msg))"]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::Async
    }

    fn see_also() -> &'static [&'static str] {
        &["Channel", "Send"]
    }
}

impl DocumentedNode for DoDoc {
    fn name() -> &'static str {
        "Do"
    }

    fn syntax() -> &'static str {
        "(do <expr1> <expr2> ...)"
    }

    fn description() -> &'static str {
        "Evaluates expressions in sequence from left to right and returns the value of the last expression. Useful for performing side effects in order."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(do (print \"Starting\") (+ 1 2))",
            "(do (print \"Step 1\") (print \"Step 2\") (print \"Step 3\") \"Done\")",
            "(let ((x 0)) (do (set! x 5) (print x) x))",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::ControlFlow
    }

    fn see_also() -> &'static [&'static str] {
        &["Let", "Effect"]
    }
}

impl DocumentedNode for MapDoc {
    fn name() -> &'static str {
        "Map"
    }

    fn syntax() -> &'static str {
        "{<key> <value> ...}"
    }

    fn description() -> &'static str {
        "Map (dictionary/hash table) literal. Creates a key-value data structure. Keys are typically strings or symbols. Maps provide O(1) average-case lookup."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "{\"name\" \"Alice\" \"age\" 30}",
            "{:x 10 :y 20}",
            "(let ((m {\"a\" 1 \"b\" 2})) (map-get m \"a\"))",
            "{}",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }

    fn see_also() -> &'static [&'static str] {
        &["map-get", "map-set", "map-keys", "map-values"]
    }
}

impl DocumentedNode for TaggedDoc {
    fn name() -> &'static str {
        "Tagged"
    }

    fn syntax() -> &'static str {
        "(make-tagged <tag> <value1> <value2> ...)"
    }

    fn description() -> &'static str {
        "Tagged values are user-defined data types with a tag (constructor name) and associated values. Used with pattern matching to create algebraic data types."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "(make-tagged \"Point\" 3 4)",
            "(make-tagged \"Some\" 42)",
            "(make-tagged \"None\")",
            "(match value (\"Some\" x) x (\"None\") 0)",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }

    fn see_also() -> &'static [&'static str] {
        &["Match", "tagged?", "tagged-tag", "tagged-values"]
    }
}

impl DocumentedNode for ListLiteralDoc {
    fn name() -> &'static str {
        "ListLiteral"
    }

    fn syntax() -> &'static str {
        "[<expr1> <expr2> ...]"
    }

    fn description() -> &'static str {
        "List literal syntax using square brackets. Equivalent to nested cons calls but more convenient. Creates a linked list data structure."
    }

    fn examples() -> &'static [&'static str] {
        &[
            "[1 2 3]",
            "[\"a\" \"b\" \"c\"]",
            "[]",
            "[x (+ y 1) (* z 2)]",
            "[[1 2] [3 4] [5 6]]",
        ]
    }

    fn category() -> DocumentationCategory {
        DocumentationCategory::DataStructure
    }

    fn see_also() -> &'static [&'static str] {
        &["List", "cons", "car", "cdr", "list-ref"]
    }
}
