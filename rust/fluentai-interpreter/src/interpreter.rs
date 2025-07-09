//! Core interpreter implementation

use std::cell::RefCell;
use std::rc::Rc;
use std::time::{Duration, Instant};

use rustc_hash::FxHashMap;

use fluentai_core::{
    ast::{Graph, Literal, Node, NodeId},
    value::Value as CoreValue,
};
// TODO: Use proper stdlib registry when exported
// use fluentai_stdlib::registry::STDLIB_REGISTRY;
use fluentai_effects::EffectContext;
// TODO: Use contract verifier when available
// use fluentai_contracts::{ContractVerifier, ContractViolation};
use fluentai_types::TypeChecker;

use crate::{
    async_runtime::AsyncRuntime,
    debug::{DebugAction, DebugEvent, DebugMode, Debugger},
    environment::Environment,
    error::{InterpreterError, InterpreterResult},
    provenance::ExecutionTrace,
    value::{Closure, Value, ValueData},
};

/// Execution mode for the interpreter
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    /// Direct tree-walking interpretation
    TreeWalking,
    /// Compile to bytecode and run on VM
    Bytecode,
    /// JIT compile to native code
    #[cfg(feature = "jit")]
    JIT,
}

/// Interpreter options
#[derive(Debug, Clone)]
pub struct InterpreterOptions {
    /// Execution mode
    pub mode: ExecutionMode,
    /// Enable contract checking
    pub enable_contracts: bool,
    /// Enable type checking
    pub enable_types: bool,
    /// Maximum recursion depth
    pub max_recursion_depth: usize,
    /// Execution timeout
    pub timeout: Option<Duration>,
    /// Debug mode configuration
    pub debug_mode: DebugMode,
    /// Enable provenance tracking
    pub track_provenance: bool,
    /// Enable tail call optimization
    pub tail_call_optimization: bool,
}

impl Default for InterpreterOptions {
    fn default() -> Self {
        Self {
            mode: ExecutionMode::TreeWalking,
            enable_contracts: true,
            enable_types: true,
            max_recursion_depth: 1000,
            timeout: None,
            debug_mode: DebugMode::default(),
            track_provenance: true,
            tail_call_optimization: true,
        }
    }
}

/// Tree-walking interpreter for FluentAi
pub struct Interpreter {
    /// Global environment
    global_env: Environment,
    /// Interpreter options
    options: InterpreterOptions,
    /// Effect context
    _effect_context: EffectContext,
    /// Async runtime
    async_runtime: Option<AsyncRuntime>,
    /// Contract verifier
    // contract_verifier: Option<ContractVerifier>,
    /// Type checker
    _type_checker: Option<TypeChecker>,
    /// Debugger
    debugger: Option<Debugger>,
    /// Execution trace
    execution_trace: ExecutionTrace,
    /// Current recursion depth
    recursion_depth: RefCell<usize>,
    /// Start time for timeout checking
    start_time: Instant,
    /// Cache for evaluated nodes
    node_cache: RefCell<FxHashMap<NodeId, Value>>,
}

impl Interpreter {
    /// Create a new interpreter
    pub fn new(options: InterpreterOptions) -> Self {
        let mut interpreter = Self {
            global_env: Environment::new(),
            options: options.clone(),
            _effect_context: EffectContext::default(),
            async_runtime: AsyncRuntime::new().ok(),
            // contract_verifier: if options.enable_contracts {
            //     Some(ContractVerifier::new())
            // } else {
            //     None
            // },
            _type_checker: if options.enable_types {
                Some(TypeChecker::new())
            } else {
                None
            },
            debugger: if options.debug_mode.enabled {
                Some(Debugger::new(
                    options.debug_mode.clone(),
                    Box::new(crate::debug::InteractiveDebugHandler::new()),
                ))
            } else {
                None
            },
            execution_trace: ExecutionTrace::new(100),
            recursion_depth: RefCell::new(0),
            start_time: Instant::now(),
            node_cache: RefCell::new(FxHashMap::default()),
        };

        // Initialize standard library
        interpreter.init_stdlib();

        interpreter
    }

    /// Initialize standard library functions
    fn init_stdlib(&mut self) {
        // TODO: Register all stdlib functions when registry is available
        // for (name, func) in STDLIB_REGISTRY.functions() {
        //     let value = Value::new(ValueData::BuiltinFunction {
        //         name: name.clone(),
        //         arity: func.arity(),
        //         variadic: func.is_variadic(),
        //     });
        //     self.global_env.bind(name, value).ok();
        // }

        // Register basic built-in functions for now
        let builtins = vec![
            ("+", 2, true),
            ("-", 2, true),
            ("*", 2, true),
            ("/", 2, false),
            (">", 2, false),
            ("<", 2, false),
            ("=", 2, false),
            ("list", 0, true),
            ("cons", 2, false),
            ("car", 1, false),
            ("cdr", 1, false),
        ];

        for (name, arity, variadic) in builtins {
            let value = Value::new(ValueData::BuiltinFunction {
                name: name.to_string(),
                arity,
                variadic,
            });
            self.global_env.bind(name.to_string(), value).ok();
        }

        // Register contract predicates if contracts are enabled
        if self.options.enable_contracts {
            self.register_contract_predicates();
        }
    }

    /// Register contract predicate functions
    fn register_contract_predicates(&mut self) {
        let predicates = vec![
            ("number?", 1, false),
            ("int?", 1, false),
            ("float?", 1, false),
            ("string?", 1, false),
            ("list?", 1, false),
            ("nil?", 1, false),
            ("empty?", 1, false),
            ("sorted?", 1, false),
        ];

        for (name, arity, variadic) in predicates {
            let value = Value::new(ValueData::BuiltinFunction {
                name: name.to_string(),
                arity,
                variadic,
            });
            self.global_env.bind(name.to_string(), value).ok();
        }
    }

    /// Interpret a graph
    pub fn interpret(&mut self, graph: &Graph) -> InterpreterResult<Value> {
        self.start_time = Instant::now();
        self.node_cache.borrow_mut().clear();

        if let Some(root_id) = graph.root_id {
            self.eval_node(root_id, graph, &self.global_env.clone())
        } else {
            Ok(Value::new(ValueData::Nil))
        }
    }

    /// Evaluate a single node
    pub fn eval_node(
        &mut self,
        node_id: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        // Check recursion depth
        {
            let mut depth = self.recursion_depth.borrow_mut();
            if *depth >= self.options.max_recursion_depth {
                return Err(InterpreterError::StackOverflow);
            }
            *depth += 1;
        }

        // Check timeout
        if let Some(timeout) = self.options.timeout {
            if self.start_time.elapsed() > timeout {
                return Err(InterpreterError::Timeout);
            }
        }

        // Check cache
        if let Some(cached) = self.node_cache.borrow().get(&node_id) {
            let mut depth = self.recursion_depth.borrow_mut();
            *depth = depth.saturating_sub(1);
            return Ok(cached.clone());
        }

        // Get node
        let node = graph
            .get_node(node_id)
            .ok_or(InterpreterError::NodeNotFound(node_id))?;

        // Debug event
        if let Some(debugger) = &mut self.debugger {
            let event = DebugEvent::NodeEnter {
                node_id,
                node_type: format!("{:?}", node),
                location: None, // TODO: Get from source map
            };

            match debugger.record_event(event) {
                DebugAction::Continue => {}
                DebugAction::Pause => {
                    // TODO: Handle pause
                }
                DebugAction::Abort => {
                    return Err(InterpreterError::Interrupted);
                }
                _ => {}
            }
        }

        // Evaluate based on node type
        let result = match node {
            Node::Literal(lit) => self.eval_literal(lit),
            Node::Variable { name } => self.eval_variable(name, env),
            Node::Application { function, args } => {
                self.eval_application(*function, args, graph, env)
            }
            Node::Lambda { params, body } => self.eval_lambda(params, *body, env),
            Node::Let { bindings, body } => self.eval_let(bindings, *body, graph, env),
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => self.eval_if(*condition, *then_branch, *else_branch, graph, env),
            Node::List(elements) => self.eval_list(elements, graph, env),
            // TODO: Handle map nodes when available
            // Node::Map(pairs) => self.eval_map(pairs, graph, env),
            Node::Async { body } => self.eval_async(*body, graph, env),
            Node::Await { expr } => self.eval_await(*expr, graph, env),
            Node::Spawn { expr } => self.eval_spawn(*expr, graph, env),
            Node::Channel => self.eval_channel(),
            Node::Send { channel, value } => self.eval_send(*channel, *value, graph, env),
            Node::Receive { channel } => self.eval_receive(*channel, graph, env),
            _ => Err(InterpreterError::InvalidOperation(format!(
                "Cannot evaluate node type: {:?}",
                node
            ))),
        };

        // Add provenance if enabled
        let mut result = result?;
        if self.options.track_provenance {
            result = result.with_provenance(node_id);
        }

        // Cache result
        self.node_cache.borrow_mut().insert(node_id, result.clone());

        // Debug event
        if let Some(debugger) = &mut self.debugger {
            let event = DebugEvent::NodeExit {
                node_id,
                value: result.clone(),
            };
            debugger.record_event(event);
        }

        // Restore recursion depth
        {
            let mut depth = self.recursion_depth.borrow_mut();
            *depth = depth.saturating_sub(1);
        }

        Ok(result)
    }

    /// Evaluate a literal
    fn eval_literal(&self, lit: &Literal) -> InterpreterResult<Value> {
        Ok(Value::from_literal(lit))
    }

    /// Evaluate a variable
    fn eval_variable(&self, name: &str, env: &Environment) -> InterpreterResult<Value> {
        env.lookup(name)
            .ok_or_else(|| InterpreterError::NameError(format!("Undefined variable: {}", name)))
    }

    /// Evaluate a function application
    fn eval_application(
        &mut self,
        function_id: NodeId,
        args: &[NodeId],
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let func_val = self.eval_node(function_id, graph, env)?;

        match &func_val.data {
            ValueData::BuiltinFunction {
                name,
                arity,
                variadic,
            } => self.eval_builtin(name, *arity, *variadic, args, graph, env),
            ValueData::Closure(closure) => {
                self.eval_closure_call(closure.clone(), args, graph, env)
            }
            _ => Err(InterpreterError::TypeError(format!(
                "Cannot call non-function value: {}",
                func_val
            ))),
        }
    }

    /// Evaluate a builtin function call
    fn eval_builtin(
        &mut self,
        name: &str,
        arity: usize,
        variadic: bool,
        args: &[NodeId],
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        // Check arity
        if !variadic && args.len() != arity {
            return Err(InterpreterError::ArityError {
                expected: arity,
                actual: args.len(),
            });
        }

        // Evaluate arguments
        let mut arg_values = Vec::new();
        for arg_id in args {
            arg_values.push(self.eval_node(*arg_id, graph, env)?);
        }

        // Call builtin
        self.call_builtin(name, arg_values)
    }

    /// Call a builtin function
    fn call_builtin(&mut self, name: &str, args: Vec<Value>) -> InterpreterResult<Value> {
        // TODO: Use stdlib registry when available
        // For now, implement basic builtins directly
        match name {
            "+" => {
                let mut sum = 0i64;
                for arg in args {
                    sum += arg.to_integer().ok_or_else(|| {
                        InterpreterError::TypeError("Expected integer".to_string())
                    })?;
                }
                Ok(Value::new(ValueData::Integer(sum)))
            }
            "-" => {
                if args.is_empty() {
                    return Err(InterpreterError::ArityError {
                        expected: 1,
                        actual: 0,
                    });
                }
                let mut result = args[0]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                for arg in &args[1..] {
                    result -= arg.to_integer().ok_or_else(|| {
                        InterpreterError::TypeError("Expected integer".to_string())
                    })?;
                }
                Ok(Value::new(ValueData::Integer(result)))
            }
            "*" => {
                let mut product = 1i64;
                for arg in args {
                    product *= arg.to_integer().ok_or_else(|| {
                        InterpreterError::TypeError("Expected integer".to_string())
                    })?;
                }
                Ok(Value::new(ValueData::Integer(product)))
            }
            "/" => {
                if args.len() != 2 {
                    return Err(InterpreterError::ArityError {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                let a = args[0]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                let b = args[1]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                if b == 0 {
                    return Err(InterpreterError::DivisionByZero);
                }
                Ok(Value::new(ValueData::Integer(a / b)))
            }
            ">" => {
                if args.len() != 2 {
                    return Err(InterpreterError::ArityError {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                let a = args[0]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                let b = args[1]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                Ok(Value::new(ValueData::Boolean(a > b)))
            }
            "<" => {
                if args.len() != 2 {
                    return Err(InterpreterError::ArityError {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                let a = args[0]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                let b = args[1]
                    .to_integer()
                    .ok_or_else(|| InterpreterError::TypeError("Expected integer".to_string()))?;
                Ok(Value::new(ValueData::Boolean(a < b)))
            }
            "=" => {
                if args.len() != 2 {
                    return Err(InterpreterError::ArityError {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                Ok(Value::new(ValueData::Boolean(args[0] == args[1])))
            }
            "list" => Ok(Value::new(ValueData::List(args))),
            "cons" => {
                if args.len() != 2 {
                    return Err(InterpreterError::ArityError {
                        expected: 2,
                        actual: args.len(),
                    });
                }
                match &args[1].data {
                    ValueData::List(tail) => {
                        let mut new_list = vec![args[0].clone()];
                        new_list.extend(tail.clone());
                        Ok(Value::new(ValueData::List(new_list)))
                    }
                    _ => Err(InterpreterError::TypeError(
                        "Second argument to cons must be a list".to_string(),
                    )),
                }
            }
            "car" => {
                if args.len() != 1 {
                    return Err(InterpreterError::ArityError {
                        expected: 1,
                        actual: args.len(),
                    });
                }
                match &args[0].data {
                    ValueData::List(items) => {
                        if items.is_empty() {
                            Err(InterpreterError::InvalidOperation(
                                "car of empty list".to_string(),
                            ))
                        } else {
                            Ok(items[0].clone())
                        }
                    }
                    _ => Err(InterpreterError::TypeError(
                        "Argument to car must be a list".to_string(),
                    )),
                }
            }
            "cdr" => {
                if args.len() != 1 {
                    return Err(InterpreterError::ArityError {
                        expected: 1,
                        actual: args.len(),
                    });
                }
                match &args[0].data {
                    ValueData::List(items) => {
                        if items.is_empty() {
                            Err(InterpreterError::InvalidOperation(
                                "cdr of empty list".to_string(),
                            ))
                        } else {
                            Ok(Value::new(ValueData::List(items[1..].to_vec())))
                        }
                    }
                    _ => Err(InterpreterError::TypeError(
                        "Argument to cdr must be a list".to_string(),
                    )),
                }
            }
            _ => Err(InterpreterError::NameError(format!(
                "Unknown builtin function: {}",
                name
            ))),
        }
    }

    /// Convert interpreter value to core value
    #[allow(dead_code)]
    fn value_to_core(&self, value: &Value) -> CoreValue {
        match &value.data {
            ValueData::Nil => CoreValue::Nil,
            ValueData::Boolean(b) => CoreValue::Boolean(*b),
            ValueData::Integer(i) => CoreValue::Integer(*i),
            ValueData::Float(f) => CoreValue::Float(*f),
            ValueData::String(s) => CoreValue::String(s.clone()),
            ValueData::List(items) => {
                let core_items: Vec<CoreValue> =
                    items.iter().map(|v| self.value_to_core(v)).collect();
                CoreValue::List(core_items)
            }
            _ => CoreValue::Nil, // TODO: Handle other types
        }
    }

    /// Convert core value to interpreter value
    #[allow(dead_code)]
    fn core_to_value(&self, value: &CoreValue) -> Value {
        let data = match value {
            CoreValue::Nil => ValueData::Nil,
            CoreValue::Boolean(b) => ValueData::Boolean(*b),
            CoreValue::Integer(i) => ValueData::Integer(*i),
            CoreValue::Float(f) => ValueData::Float(*f),
            CoreValue::String(s) => ValueData::String(s.clone()),
            CoreValue::List(items) => {
                let values: Vec<Value> = items.iter().map(|v| self.core_to_value(v)).collect();
                ValueData::List(values)
            }
            _ => ValueData::Nil, // TODO: Handle other types
        };
        Value::new(data)
    }

    /// Evaluate a closure call
    fn eval_closure_call(
        &mut self,
        closure: Rc<Closure>,
        args: &[NodeId],
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        // Check arity
        if args.len() != closure.params.len() {
            return Err(InterpreterError::ArityError {
                expected: closure.params.len(),
                actual: args.len(),
            });
        }

        // Create new environment for the function
        let func_env = closure.env.extend();

        // Bind parameters
        for (param, arg_id) in closure.params.iter().zip(args) {
            let arg_val = self.eval_node(*arg_id, graph, env)?;
            func_env.bind(param.clone(), arg_val)?;
        }

        // Evaluate body
        self.eval_node(closure.body, graph, &func_env)
    }

    /// Evaluate a lambda expression
    fn eval_lambda(
        &self,
        params: &[String],
        body: NodeId,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let closure = Closure {
            params: params.to_vec(),
            body,
            env: env.clone(),
            is_async: false,
        };

        Ok(Value::new(ValueData::Closure(Rc::new(closure))))
    }

    /// Evaluate a let expression
    fn eval_let(
        &mut self,
        bindings: &[(String, NodeId)],
        body: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let let_env = env.extend();

        for (name, val_id) in bindings {
            let val = self.eval_node(*val_id, graph, &let_env)?;
            let_env.bind(name.clone(), val)?;
        }

        self.eval_node(body, graph, &let_env)
    }

    /// Evaluate an if expression
    fn eval_if(
        &mut self,
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let cond_val = self.eval_node(condition, graph, env)?;

        if cond_val.is_truthy() {
            self.eval_node(then_branch, graph, env)
        } else {
            self.eval_node(else_branch, graph, env)
        }
    }

    /// Evaluate a list expression
    fn eval_list(
        &mut self,
        elements: &[NodeId],
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let mut values = Vec::new();
        for elem_id in elements {
            values.push(self.eval_node(*elem_id, graph, env)?);
        }
        Ok(Value::new(ValueData::List(values)))
    }

    // TODO: Implement eval_map when Map node type is available
    // /// Evaluate a map expression
    // fn eval_map(
    //     &mut self,
    //     pairs: &[(NodeId, NodeId)],
    //     graph: &Graph,
    //     env: &Environment,
    // ) -> InterpreterResult<Value> {
    //     let mut map = HashMap::new();
    //     for (key_id, val_id) in pairs {
    //         let key_val = self.eval_node(*key_id, graph, env)?;
    //         let key = match &key_val.data {
    //             ValueData::String(s) => s.clone(),
    //             _ => key_val.to_string(),
    //         };
    //         let val = self.eval_node(*val_id, graph, env)?;
    //         map.insert(key, val);
    //     }
    //     Ok(Value::new(ValueData::Map(map)))
    // }

    /// Get the current environment (for debugging)
    pub fn environment(&self) -> &Environment {
        &self.global_env
    }

    /// Get execution trace
    pub fn trace(&self) -> &ExecutionTrace {
        &self.execution_trace
    }

    /// Evaluate async block
    fn eval_async(
        &mut self,
        body: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        // For now, just evaluate the body synchronously
        // TODO: Implement proper async evaluation
        self.eval_node(body, graph, env)
    }

    /// Evaluate await expression
    fn eval_await(
        &mut self,
        expr: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        // Evaluate the expression
        let future_val = self.eval_node(expr, graph, env)?;

        // For now, just return the value
        // TODO: Implement proper await handling for async values
        Ok(future_val)
    }

    /// Evaluate spawn expression
    fn eval_spawn(
        &mut self,
        expr: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        if let Some(_runtime) = &self.async_runtime {
            // Clone necessary data for the spawned task
            let _graph_clone = graph.clone();
            let _env_clone = env.clone();
            let _expr_clone = expr;

            // Create a simple task handle value
            // TODO: Implement proper async handle value type
            Ok(Value::new(ValueData::String(format!("task:{}", expr))))
        } else {
            Err(InterpreterError::RuntimeError(
                "Async runtime not available".to_string(),
            ))
        }
    }

    /// Create a new channel
    fn eval_channel(&self) -> InterpreterResult<Value> {
        if let Some(_runtime) = &self.async_runtime {
            // TODO: Implement proper channel value type
            Ok(Value::new(ValueData::String("channel".to_string())))
        } else {
            Err(InterpreterError::RuntimeError(
                "Async runtime not available".to_string(),
            ))
        }
    }

    /// Send a value to a channel
    fn eval_send(
        &mut self,
        channel: NodeId,
        value: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let _channel_val = self.eval_node(channel, graph, env)?;
        let value_val = self.eval_node(value, graph, env)?;

        // TODO: Implement proper channel send
        Ok(value_val)
    }

    /// Receive a value from a channel
    fn eval_receive(
        &mut self,
        channel: NodeId,
        graph: &Graph,
        env: &Environment,
    ) -> InterpreterResult<Value> {
        let _channel_val = self.eval_node(channel, graph, env)?;

        // TODO: Implement proper channel receive
        Ok(Value::new(ValueData::Nil))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse;

    #[test]
    fn test_eval_literal() {
        let mut interp = Interpreter::new(InterpreterOptions::default());
        let graph = parse("42").unwrap();
        let result = interp.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(42));
    }

    #[test]
    fn test_eval_arithmetic() {
        let mut interp = Interpreter::new(InterpreterOptions::default());
        let graph = parse("(+ 1 2)").unwrap();
        let result = interp.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(3));
    }

    #[test]
    fn test_eval_let() {
        let mut interp = Interpreter::new(InterpreterOptions::default());
        let graph = parse("(let ((x 10)) (+ x 5))").unwrap();
        let result = interp.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(15));
    }

    #[test]
    fn test_eval_if() {
        let mut interp = Interpreter::new(InterpreterOptions::default());
        let graph = parse("(if (> 5 3) 1 2)").unwrap();
        let result = interp.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(1));
    }

    #[test]
    fn test_eval_lambda() {
        let mut interp = Interpreter::new(InterpreterOptions::default());
        let graph = parse("((lambda (x) (* x 2)) 5)").unwrap();
        let result = interp.interpret(&graph).unwrap();
        assert_eq!(result.to_integer(), Some(10));
    }
}
