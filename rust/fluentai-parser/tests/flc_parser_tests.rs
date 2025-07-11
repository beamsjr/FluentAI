//! Comprehensive test suite for FLC parser

use fluentai_parser::parse;

#[test]
fn test_parse_arithmetic() {
    let cases = vec![
        ("1 + 2", "addition"),
        ("10 - 5", "subtraction"),
        ("3 * 4", "multiplication"),
        ("20 / 4", "division"),
        ("17 % 5", "modulo"),
        ("1 + 2 * 3", "precedence multiply"),
        ("(1 + 2) * 3", "parentheses"),
        ("1 + 2 + 3", "left associative"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_comparison() {
    let cases = vec![
        ("x == y", "equality"),
        ("x != y", "inequality"),
        ("x < y", "less than"),
        ("x > y", "greater than"),
        ("x <= y", "less or equal"),
        ("x >= y", "greater or equal"),
        ("x > 5 && y < 10", "logical and"),
        ("x == 0 || y == 0", "logical or"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_functions() {
    let cases = vec![
        ("private function add(x, y) { x + y }", "simple function"),
        ("public function greet(name) { f\"Hello, {name}!\" }", "public function"),
        ("private function factorial(n) { if (n <= 1) { 1 } else { n * factorial(n - 1) } }", "recursive function"),
        ("private function identity(x) { x }", "identity function"),
        ("private function no_params() { 42 }", "no parameters"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_lambdas() {
    let cases = vec![
        ("x => x * 2", "simple lambda"),
        ("(x, y) => x + y", "multi-param lambda"),
        ("() => 42", "no-param lambda"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_let_bindings() {
    let cases = vec![
        ("let x = 5; x", "simple let"),
        ("let x = 5; let y = 10; x + y", "multiple lets"),
        ("let {x, y} = point; x + y", "destructuring let"),
        ("let {name, age} = user; name", "destructuring with multiple fields"),
        ("let {} = empty; 42", "empty destructuring"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_method_chains() {
    let cases = vec![
        ("list.map(f)", "simple method"),
        ("list.map(f).filter(pred)", "chain"),
        ("users.filter(u => u.age > 18)", "method with lambda"),
        ("data.process().validate().save()", "long chain"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_pattern_matching() {
    let cases = vec![
        ("x.match().case(0, \"zero\").get()", "simple match"),
        ("x.match().case(Ok(v), v).case(Err(e), 0).get()", "result match"),
        ("x.match().case(_, \"default\").get()", "wildcard match"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_async_await() {
    let cases = vec![
        // Async is used with function definitions, not blocks
        ("private async function fetch_data() { http.get(\"/api\") }", "async function"),
        ("fetch(url).await()", "await expression"),
        ("data.fetch().await().process()", "await in chain"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_blocks() {
    let cases = vec![
        ("{ 42 }", "simple block"),
        ("{ let x = 5; x }", "block with let"),
        ("{ let x = 5; let y = 10; x + y }", "block with multiple lets"),
        ("{ print(\"hello\"); 42 }", "block with side effect"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_spawn() {
    let cases = vec![
        ("spawn { fetch_data() }", "spawn with braces"),
        ("spawn(fetch_data())", "spawn with parentheses"),
        ("spawn { () => 42 }", "spawn with lambda"),
        ("spawn(() => compute())", "spawn lambda with parens"),
        ("spawn { fetch_data() }.await()", "spawn with await"),
        ("let task = spawn { process() }; task", "spawn in let binding"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test that spawn creates the right AST structure
    use fluentai_core::ast::Node;
    
    let result = parse("spawn { compute() }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have a Spawn node
    let has_spawn_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Spawn { .. })
    });
    assert!(has_spawn_node, "Should create Spawn node");
}

#[test]
fn test_parse_collections() {
    let cases = vec![
        ("[1, 2, 3]", "list literal"),
        ("{\"key\": \"value\"}", "map literal"),
        ("{}", "empty map"),
        ("{\"name\": \"Alice\", \"age\": 30}", "map with multiple entries"),
        ("{\"nested\": {\"key\": \"value\"}}", "nested map"),
        ("#{1, 2, 3}", "set literal"),
        ("#{}", "empty set"),
        ("#{\"a\", \"b\", \"c\"}", "string set"),
        ("[1, 2, [3, 4]]", "nested list"),
        ("[1, ...other, 3]", "list with spread"),
        ("[...first, ...second]", "list with multiple spreads"),
        ("#{1, ...other_set, 3}", "set with spread"),
        ("[...items]", "list with only spread"),
        ("#{...items}", "set with only spread"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_error_handling() {
    let cases = vec![
        ("try { risky() } catch (e) { process_error(e) }", "try-catch"),
        ("try { risky() } finally { cleanup() }", "try-finally"),
        ("try { risky() } catch (e) { process_error(e) } finally { cleanup() }", "try-catch-finally"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_modules() {
    let cases = vec![
        ("use std::io;", "simple import"),
        ("use math::{sin, cos, tan};", "multi import"),
        ("use http::client as http_client;", "aliased import"),
        ("use std::{io as stdio, fs};", "mixed aliased and non-aliased imports"),
        ("use math::{sin as sine, cos as cosine};", "multiple aliased imports"),
        ("use collections::*;", "wildcard import"),
        ("use std::io::*;", "nested wildcard import"),
        ("mod math { private function add(x, y) { x + y } }", "module definition"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_pipes() {
    let cases = vec![
        ("5 |> double", "simple pipe"),
        ("5 |> double |> add(10)", "pipe chain"),
        ("[1, 2, 3] |> map(double) |> sum", "collection pipe"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_string_interpolation() {
    let cases = vec![
        (r#"f"Hello, {name}!""#, "simple interpolation"),
        (r#"f"Result: {x + y}""#, "expression interpolation"),
        (r#"f"User: {user.name} (ID: {user.id})""#, "multiple interpolations"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_error_cases() {
    let error_cases = vec![
        ("private function", "incomplete function"),
        ("let x =", "incomplete let"),
        ("if (x)", "incomplete if"),
        ("{", "unclosed brace"),
        ("1 +", "incomplete expression"),
        ("private 123", "invalid definition"),
    ];
    
    for (input, desc) in error_cases {
        let result = parse(input);
        assert!(result.is_err(), "Should fail to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_complex_example() {
    let input = r#"
private function process_users(users) {
    users
        .filter(user => user.age >= 18)
        .map(user => user.name)
        .sort()
}

private async function main() {
    let users = fetch_users().await();
    let processed = process_users(users);
    save_results(processed).await()
}
"#;
    
    let result = parse(input);
    assert!(result.is_ok(), "Failed to parse complex example: {:?}", result);
}

#[test]
fn test_parse_printable_construct() {
    let cases = vec![
        ("$(42)", "simple printable"),
        ("$(x + y)", "expression printable"),
        ("$(users.count())", "method call printable"),
        ("let x = 5; $(x)", "printable after statement"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_actor_with_handlers() {
    let input = r#"
private actor Counter {
    count: int = 0;
    
    private handle Inc(amount: int) {
        self.count = self.count + amount;
    }
    
    private handle Get() -> int {
        self.count
    }
}
"#;
    
    let result = parse(input);
    assert!(result.is_ok(), "Failed to parse actor with handlers: {:?}", result);
}

#[test]
fn test_parse_effect_system() {
    let cases = vec![
        ("private effect Database { private function query(sql: string) -> Result; }", "effect definition"),
        ("private function get_user(id: int).with(Database) { db.query(f\"SELECT * FROM users WHERE id={id}\") }", "function with effect"),
        ("public function fetch_data().with(Network) { http.get(\"/api\") }", "public function with network effect"),
        ("private function rand_int().with(Random) { random.next_int() }", "function with random effect"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test that effect nodes are created
    use fluentai_core::ast::Node;
    
    let result = parse("private function do_io().with(Database) { db.read() }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we created an Effect node
    let has_effect_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Effect { .. })
    });
    assert!(has_effect_node, "Should create Effect node for function with .with()");
}

#[test]
fn test_parse_channels() {
    // Also test that AST nodes are correctly created
    use fluentai_core::ast::Node;
    
    let result = parse("channel()");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we created a Channel node
    let has_channel_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Channel { .. })
    });
    assert!(has_channel_node, "Should create Channel node");
    
    // Test send operation
    let result = parse("ch.send(42)");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    let has_send_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Send { .. })
    });
    assert!(has_send_node, "Should create Send node");
    
    // Test receive operation
    let result = parse("ch.receive()");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    let has_receive_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Receive { .. })
    });
    assert!(has_receive_node, "Should create Receive node");
}

#[test]
fn test_parse_channels_syntax_variations() {
    let cases = vec![
        ("let ch = channel(); ch", "channel creation"),
        ("let ch2 = Channel.new(); ch2", "Channel.new() syntax"),
        ("let buffered = channel(10); buffered", "buffered channel"),
        ("let buffered2 = Channel.new(100); buffered2", "buffered Channel.new()"),
        ("ch.send(42)", "channel send"),
        ("ch.send(\"hello\")", "channel send string"),
        ("let value = ch.receive(); value", "channel receive"),
        ("ch.receive()", "channel receive expression"),
        ("channel()", "channel expression"),
        ("Channel.new()", "Channel.new() expression"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_advanced_patterns() {
    // Test Or patterns
    let or_pattern_cases = vec![
        ("value.match().case(1 | 2 | 3, \"low\").get()", "simple or pattern"),
        ("value.match().case(\"red\" | \"green\" | \"blue\", \"primary\").get()", "string or pattern"),
        ("value.match().case(true | false, \"boolean\").get()", "boolean or pattern"),
    ];
    
    for (input, desc) in or_pattern_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test Guard patterns
    let guard_pattern_cases = vec![
        ("value.match().case(n when n > 0, \"positive\").get()", "simple guard"),
        ("value.match().case(x when x % 2 == 0, \"even\").get()", "modulo guard"),
        ("value.match().case(s when s.length() > 5, \"long\").get()", "method call guard"),
    ];
    
    for (input, desc) in guard_pattern_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test Range patterns
    let range_pattern_cases = vec![
        ("value.match().case(1..10, \"single digit\").get()", "exclusive range"),
        ("value.match().case(1..=10, \"inclusive single digit\").get()", "inclusive range"),
        ("value.match().case(0..100, \"percentage\").get()", "large range"),
    ];
    
    for (input, desc) in range_pattern_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test As patterns
    let as_pattern_cases = vec![
        ("value.match().case(Some(x) as whole, process(whole)).get()", "as pattern with constructor"),
        ("value.match().case(42 as n, n * 2).get()", "as pattern with literal"),
        ("value.match().case(Point(x, y) as p, transform(p)).get()", "as pattern with multiple args"),
    ];
    
    for (input, desc) in as_pattern_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test combined patterns
    let combined_cases = vec![
        ("value.match().case(1 | 2 | 3 when x > 0, \"low positive\").get()", "or with guard"),
        ("value.match().case(Some(x) as s when x > 0, process(s)).get()", "as with guard"),
        ("value.match().case(1..10 as n, n * 2).get()", "range with as"),
    ];
    
    for (input, desc) in combined_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_pattern_ast_structure() {
    use fluentai_core::ast::{Node, Pattern};
    
    // Test that Or patterns create the right AST structure
    let result = parse("match x { 1 | 2 | 3 => \"low\", _ => \"other\" }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have a Match node with Or pattern
    let has_or_pattern = graph.nodes.values().any(|node| {
        if let Node::Match { branches, .. } = node {
            branches.iter().any(|(pattern, _)| matches!(pattern, Pattern::Or(_)))
        } else {
            false
        }
    });
    assert!(has_or_pattern, "Should create Or pattern in match expression");
    
    // Test that Range patterns create the right AST structure
    let result = parse("match x { 1..10 => \"single\", _ => \"other\" }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    let has_range_pattern = graph.nodes.values().any(|node| {
        if let Node::Match { branches, .. } = node {
            branches.iter().any(|(pattern, _)| matches!(pattern, Pattern::Range(_)))
        } else {
            false
        }
    });
    assert!(has_range_pattern, "Should create Range pattern in match expression");
}

#[test]
fn test_parse_optional_chaining() {
    let cases = vec![
        ("user.?name", "optional property access"),
        ("user.?get_name()", "optional method call"),
        ("user.?address.?city", "chained optional access"),
        ("users.?find(id).?name", "mixed optional and regular chaining"),
        ("config.?database.?get_connection()", "optional method at end"),
        ("api.?fetch(url, options)", "optional method with arguments"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test that optional chaining creates the right AST structure
    use fluentai_core::ast::Node;
    
    let result = parse("user.?name");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have an Application node with optional_chain_ prefix
    let has_optional_chain = graph.nodes.values().any(|node| {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name.starts_with("optional_chain_")
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_optional_chain, "Should create optional chain application");
}

#[test]
fn test_parse_spread_operator() {
    use fluentai_core::ast::Node;
    
    // Test list with spread
    let result = parse("[1, ...other, 3]");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have a list_with_spread function call
    let has_list_spread = graph.nodes.values().any(|node| {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "list_with_spread"
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_list_spread, "Should create list_with_spread application");
    
    // Check that spread elements are marked with __spread__
    let has_spread_marker = graph.nodes.values().any(|node| {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "__spread__"
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_spread_marker, "Should mark spread elements with __spread__");
    
    // Test regular list without spread
    let result = parse("[1, 2, 3]");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Should use Node::List, not list_with_spread
    let has_list_node = graph.nodes.values().any(|node| matches!(node, Node::List(_)));
    assert!(has_list_node, "Regular list should use Node::List");
}

#[test]
fn test_parse_trait_implementation() {
    let input = r#"
User as Serializable {
    private function to_json(self) {
        f"{\"id\": {self.id}, \"name\": \"{self.name}\"}"
    }
}
"#;
    
    let result = parse(input);
    assert!(result.is_ok(), "Failed to parse trait implementation: {:?}", result);
}

#[test]
fn test_parse_actor_handlers() {
    // Test a simple actor with state and handlers
    let input = r#"
private actor Counter {
    count: int = 0;
    
    private handle Inc(amount: int) {
        self.count = self.count + amount;
    }
    
    private handle Get() -> int {
        self.count
    }
}
"#;
    
    let result = parse(input);
    assert!(result.is_ok(), "Failed to parse actor handlers: {:?}", result);
}

#[test]
fn test_parse_assignment_statements() {
    let cases = vec![
        ("x = 5;", "simple assignment"),
        ("x = y + z;", "assignment with expression"),
        ("let x = 5; x + 1", "let with expression body (not assignment)"),
        ("{ x = 5; y = x + 1; }", "multiple assignments in block"),
        ("private function inc_x() { x = x + 1; }", "assignment in function"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
}

#[test]
fn test_assignment_vs_expression() {
    // Test that expressions without assignment work correctly
    let expr_cases = vec![
        ("x", "simple variable"),
        ("x + y", "arithmetic expression"),
        ("f(x)", "function call"),
        ("{ x; y; z }", "expression block"),
    ];
    
    for (input, desc) in expr_cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse expression {}: {:?}", desc, result);
    }
}

#[test]
fn test_parse_contracts() {
    let cases = vec![
        ("@requires(n >= 0)\nprivate function factorial(n: int) -> int { if (n <= 1) { 1 } else { n * factorial(n - 1) } }", "simple precondition"),
        ("@ensures(result >= 1)\nprivate function factorial(n: int) -> int { if (n <= 1) { 1 } else { n * factorial(n - 1) } }", "simple postcondition"),
        ("@requires(n >= 0)\n@ensures(result >= 1)\nprivate function factorial(n: int) -> int { if (n <= 1) { 1 } else { n * factorial(n - 1) } }", "pre and post conditions"),
        ("@complexity(\"O(n)\")\nprivate function sum(n: int) -> int { if (n <= 0) { 0 } else { n + sum(n - 1) } }", "complexity annotation"),
        ("@pure(true)\nprivate function add(x: int, y: int) -> int { x + y }", "pure function"),
        ("@contract(factorial)\n@requires(n >= 0)\n@ensures(result >= 1)\n@complexity(\"O(n)\")\n@pure(false)\nprivate function factorial(n: int) -> int { if (n <= 1) { 1 } else { n * factorial(n - 1) } }", "full contract"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test that Contract nodes are created
    use fluentai_core::ast::Node;
    
    let result = parse("@requires(x > 0)\nprivate function sqrt(x: float) -> float { x.sqrt() }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we created a Contract node
    let has_contract_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Contract { .. })
    });
    assert!(has_contract_node, "Should create Contract node for function with annotations");
}

#[test]
fn test_parse_map_literals() {
    // Test that map literals are parsed correctly
    use fluentai_core::ast::Node;
    
    let result = parse("{\"key\": \"value\"}");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we created an Application node calling make_map
    let has_make_map = graph.nodes.values().any(|node| {
        if let Node::Application { function, args } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "make_map" && args.len() == 2
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_make_map, "Map literal should be converted to make_map call");
    
    // Test empty map
    let result = parse("{}");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    let has_empty_make_map = graph.nodes.values().any(|node| {
        if let Node::Application { function, args } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "make_map" && args.is_empty()
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_empty_make_map, "Empty map should be converted to make_map() call");
}

#[test]
fn test_parse_set_literals() {
    // Test that set literals are parsed correctly
    use fluentai_core::ast::Node;
    
    let result = parse("#{1, 2, 3}");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we created an Application node calling make_set
    let has_make_set = graph.nodes.values().any(|node| {
        if let Node::Application { function, args } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "make_set" && args.len() == 3
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_make_set, "Set literal should be converted to make_set call");
    
    // Test empty set
    let result = parse("#{}");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    let has_empty_make_set = graph.nodes.values().any(|node| {
        if let Node::Application { function, args } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                name == "make_set" && args.is_empty()
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_empty_make_set, "Empty set should be converted to make_set() call");
}

#[test]
fn test_parse_destructuring() {
    // Test that destructuring creates proper bindings
    use fluentai_core::ast::Node;
    
    let result = parse("let {x, y} = point; x + y");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have Let node with bindings
    let has_let_with_bindings = graph.nodes.values().any(|node| {
        if let Node::Let { bindings, .. } = node {
            // Should have 3 bindings: temp var for point, x, and y
            bindings.len() >= 3
        } else {
            false
        }
    });
    assert!(has_let_with_bindings, "Destructuring should create Let node with multiple bindings");
}

#[test]
fn test_parse_import_aliasing() {
    // Test that import aliasing creates proper AST nodes
    use fluentai_core::ast::Node;
    
    let result = parse("use http::client as http_client;");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have Import node with alias
    let has_aliased_import = graph.nodes.values().any(|node| {
        if let Node::Import { import_list, .. } = node {
            import_list.iter().any(|item| item.alias.is_some())
        } else {
            false
        }
    });
    assert!(has_aliased_import, "Import aliasing should create Import node with aliased items");
}

#[test]
fn test_parse_object_literals() {
    let cases = vec![
        ("User { name: \"Alice\", age: 30 }", "struct construction"),
        ("Point { x: 10, y: 20 }", "struct with numeric fields"),
        ("Config { }", "empty struct construction"),
        ("Person { name: \"Bob\", address: Address { street: \"Main\", number: 123 } }", "nested struct construction"),
        ("let user = User { name: \"Alice\", age: 30 }; user", "struct construction in let binding"),
    ];
    
    for (input, desc) in cases {
        let result = parse(input);
        assert!(result.is_ok(), "Failed to parse {}: {:?}", desc, result);
    }
    
    // Test that struct construction creates Application nodes
    use fluentai_core::ast::Node;
    
    let result = parse("Point { x: 10, y: 20 }");
    assert!(result.is_ok());
    let graph = result.unwrap();
    
    // Check that we have Application node (struct construction is desugared to function call)
    let has_application = graph.nodes.values().any(|node| {
        matches!(node, Node::Application { .. })
    });
    assert!(has_application, "Struct construction should create Application node");
}