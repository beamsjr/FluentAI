//! Python bindings for FluentAi Rust implementation

use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_parser::parse as rust_parse;
use fluentai_vm::{compiler::Compiler, vm::VM, Value};
use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use std::collections::HashMap;

/// Python wrapper for AST Graph
#[pyclass(name = "RustGraph")]
#[derive(Clone)]
struct PyGraph {
    inner: Graph,
}

#[pymethods]
impl PyGraph {
    #[new]
    fn new() -> Self {
        Self {
            inner: Graph::new(),
        }
    }

    #[getter]
    fn root_id(&self) -> Option<String> {
        self.inner.root_id.map(|id| id.to_string())
    }

    #[getter]
    fn nodes(&self) -> HashMap<String, PyNode> {
        self.inner
            .nodes
            .iter()
            .map(|(id, node)| (id.to_string(), PyNode::from_node(node.clone())))
            .collect()
    }
}

/// Python wrapper for AST Node
#[pyclass(name = "RustNode")]
#[derive(Clone)]
struct PyNode {
    node_type: String,
    data: HashMap<String, PyObject>,
}

impl PyNode {
    fn from_node(node: Node) -> Self {
        Python::with_gil(|py| {
            let mut data = HashMap::new();
            let node_type = match &node {
                Node::Literal(lit) => {
                    data.insert(
                        "literal_type".to_string(),
                        match lit {
                            Literal::Integer(_) => "int",
                            Literal::Float(_) => "float",
                            Literal::String(_) => "string",
                            Literal::Boolean(_) => "bool",
                            Literal::Nil => "nil",
                        }
                        .to_object(py),
                    );

                    data.insert(
                        "value".to_string(),
                        match lit {
                            Literal::Integer(n) => n.to_object(py),
                            Literal::Float(f) => f.to_object(py),
                            Literal::String(s) => s.to_object(py),
                            Literal::Boolean(b) => b.to_object(py),
                            Literal::Nil => py.None(),
                        },
                    );
                    "Literal"
                }
                Node::Variable { name } => {
                    data.insert("name".to_string(), name.to_object(py));
                    "Variable"
                }
                Node::Lambda { params, body } => {
                    data.insert("parameter_names".to_string(), params.to_object(py));
                    data.insert("body_id".to_string(), body.to_string().to_object(py));
                    "Lambda"
                }
                Node::Let { bindings, body } => {
                    let py_bindings: Vec<HashMap<String, String>> = bindings
                        .iter()
                        .map(|(name, id)| {
                            let mut binding = HashMap::new();
                            binding.insert("name".to_string(), name.clone());
                            binding.insert("value_id".to_string(), id.to_string());
                            binding
                        })
                        .collect();
                    data.insert("bindings".to_string(), py_bindings.to_object(py));
                    data.insert("body_id".to_string(), body.to_string().to_object(py));
                    "Let"
                }
                Node::If {
                    condition,
                    then_branch,
                    else_branch,
                } => {
                    data.insert(
                        "condition_id".to_string(),
                        condition.to_string().to_object(py),
                    );
                    data.insert("then_id".to_string(), then_branch.to_string().to_object(py));
                    data.insert("else_id".to_string(), else_branch.to_string().to_object(py));
                    "If"
                }
                Node::Application { function, args } => {
                    data.insert(
                        "function_id".to_string(),
                        function.to_string().to_object(py),
                    );
                    data.insert(
                        "argument_ids".to_string(),
                        args.iter()
                            .map(|id| id.to_string())
                            .collect::<Vec<_>>()
                            .to_object(py),
                    );
                    "Application"
                }
                Node::Effect {
                    effect_type,
                    operation,
                    args,
                } => {
                    data.insert(
                        "effect_type".to_string(),
                        effect_type.to_string().to_object(py),
                    );
                    data.insert("operation".to_string(), operation.to_object(py));
                    data.insert(
                        "argument_ids".to_string(),
                        args.iter()
                            .map(|id| id.to_string())
                            .collect::<Vec<_>>()
                            .to_object(py),
                    );
                    "Effect"
                }
                Node::List(elements) => {
                    data.insert(
                        "elements".to_string(),
                        elements
                            .iter()
                            .map(|id| id.to_string())
                            .collect::<Vec<_>>()
                            .to_object(py),
                    );
                    "List"
                }
                Node::Match { expr, branches } => {
                    data.insert("expr_id".to_string(), expr.to_string().to_object(py));
                    // Simplified pattern representation for now
                    data.insert("branches".to_string(), branches.len().to_object(py));
                    "Match"
                }
                Node::Async { body } => {
                    data.insert("body_id".to_string(), body.to_string().to_object(py));
                    "Async"
                }
                Node::Await { expr } => {
                    data.insert("expr_id".to_string(), expr.to_string().to_object(py));
                    "Await"
                }
                Node::Spawn { expr } => {
                    data.insert("expr_id".to_string(), expr.to_string().to_object(py));
                    "Spawn"
                }
                Node::Channel { .. } => "Channel",
                Node::Send { channel, value } => {
                    data.insert("channel_id".to_string(), channel.to_string().to_object(py));
                    data.insert("value_id".to_string(), value.to_string().to_object(py));
                    "Send"
                }
                Node::Receive { channel } => {
                    data.insert("channel_id".to_string(), channel.to_string().to_object(py));
                    "Receive"
                }
                Node::TrySend { channel, value } => {
                    data.insert("channel_id".to_string(), channel.to_string().to_object(py));
                    data.insert("value_id".to_string(), value.to_string().to_object(py));
                    "TrySend"
                }
                Node::TryReceive { channel } => {
                    data.insert("channel_id".to_string(), channel.to_string().to_object(py));
                    "TryReceive"
                }
                Node::Select { branches, default } => {
                    let py_branches: Vec<(String, String)> = branches
                        .iter()
                        .map(|(op, handler)| (op.to_string(), handler.to_string()))
                        .collect();
                    data.insert("branches".to_string(), py_branches.to_object(py));
                    if let Some(def) = default {
                        data.insert("default".to_string(), def.to_string().to_object(py));
                    }
                    "Select"
                }
                Node::Letrec { bindings, body } => {
                    let py_bindings: Vec<HashMap<String, String>> = bindings
                        .iter()
                        .map(|(name, id)| {
                            let mut binding = HashMap::new();
                            binding.insert("name".to_string(), name.clone());
                            binding.insert("value_id".to_string(), id.to_string());
                            binding
                        })
                        .collect();
                    data.insert("bindings".to_string(), py_bindings.to_object(py));
                    data.insert("body_id".to_string(), body.to_string().to_object(py));
                    "Letrec"
                }
                Node::Module {
                    name,
                    exports,
                    body,
                } => {
                    data.insert("name".to_string(), name.to_object(py));
                    data.insert("exports".to_string(), exports.to_object(py));
                    data.insert("body_id".to_string(), body.to_string().to_object(py));
                    "Module"
                }
                Node::Import {
                    module_path,
                    import_list,
                    import_all,
                } => {
                    data.insert("module_path".to_string(), module_path.to_object(py));
                    data.insert("import_list".to_string(), import_list.len().to_object(py));
                    data.insert("import_all".to_string(), import_all.to_object(py));
                    "Import"
                }
                Node::Export { export_list } => {
                    data.insert("export_list".to_string(), export_list.len().to_object(py));
                    "Export"
                }
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    data.insert("module".to_string(), module_name.to_object(py));
                    data.insert("name".to_string(), variable_name.to_object(py));
                    "QualifiedVariable"
                }
                Node::Contract {
                    function_name,
                    preconditions,
                    postconditions,
                    invariants,
                    complexity,
                    pure,
                } => {
                    data.insert("function_name".to_string(), function_name.to_object(py));
                    data.insert(
                        "preconditions".to_string(),
                        preconditions.len().to_object(py),
                    );
                    data.insert(
                        "postconditions".to_string(),
                        postconditions.len().to_object(py),
                    );
                    data.insert("invariants".to_string(), invariants.len().to_object(py));
                    data.insert("complexity".to_string(), complexity.to_object(py));
                    data.insert("pure".to_string(), pure.to_object(py));
                    "Contract"
                }
                Node::Handler { handlers, body: _ } => {
                    data.insert("handler_count".to_string(), handlers.len().to_object(py));
                    let handler_types: Vec<String> = handlers
                        .iter()
                        .map(|(effect_type, _, _)| format!("{:?}", effect_type))
                        .collect();
                    data.insert("effect_types".to_string(), handler_types.to_object(py));
                    "Handler"
                }
                Node::Define { name, value } => {
                    data.insert("name".to_string(), name.to_object(py));
                    data.insert("value_id".to_string(), value.to_string().to_object(py));
                    "Define"
                }
                Node::Begin { exprs } => {
                    data.insert(
                        "expression_ids".to_string(),
                        exprs
                            .iter()
                            .map(|id| id.to_string())
                            .collect::<Vec<_>>()
                            .to_object(py),
                    );
                    "Begin"
                }
                Node::Select { branches, default } => {
                    data.insert("branch_count".to_string(), branches.len().to_object(py));
                    data.insert("has_default".to_string(), default.is_some().to_object(py));
                    "Select"
                }
                Node::Actor { initial_state, handler } => {
                    data.insert("initial_state".to_string(), initial_state.to_string().to_object(py));
                    data.insert("handler".to_string(), handler.to_string().to_object(py));
                    "Actor"
                }
                Node::ActorSend { actor, message } => {
                    data.insert("actor".to_string(), actor.to_string().to_object(py));
                    data.insert("message".to_string(), message.to_string().to_object(py));
                    "ActorSend"
                }
                Node::ActorReceive { patterns, timeout } => {
                    data.insert("pattern_count".to_string(), patterns.len().to_object(py));
                    data.insert("has_timeout".to_string(), timeout.is_some().to_object(py));
                    "ActorReceive"
                }
                Node::Become { new_state } => {
                    data.insert("new_state".to_string(), new_state.to_string().to_object(py));
                    "Become"
                }
                Node::Try { body, catch_branches, finally } => {
                    data.insert("body".to_string(), body.to_string().to_object(py));
                    data.insert("catch_count".to_string(), catch_branches.len().to_object(py));
                    data.insert("has_finally".to_string(), finally.is_some().to_object(py));
                    "Try"
                }
                Node::Throw { error } => {
                    data.insert("error".to_string(), error.to_string().to_object(py));
                    "Throw"
                }
                Node::Promise { body } => {
                    data.insert("body".to_string(), body.to_string().to_object(py));
                    "Promise"
                }
                Node::PromiseAll { promises } => {
                    data.insert("promise_count".to_string(), promises.len().to_object(py));
                    "PromiseAll"
                }
                Node::PromiseRace { promises } => {
                    data.insert("promise_count".to_string(), promises.len().to_object(py));
                    "PromiseRace"
                }
                Node::Timeout { duration, promise, default } => {
                    data.insert("duration".to_string(), duration.to_string().to_object(py));
                    data.insert("promise".to_string(), promise.to_string().to_object(py));
                    data.insert("has_default".to_string(), default.is_some().to_object(py));
                    "Timeout"
                }
            };

            Self {
                node_type: node_type.to_string(),
                data,
            }
        })
    }
}

#[pymethods]
impl PyNode {
    #[getter]
    fn node_type(&self) -> &str {
        &self.node_type
    }

    fn get(&self, key: &str) -> Option<PyObject> {
        self.data.get(key).cloned()
    }

    fn __repr__(&self) -> String {
        format!(
            "RustNode(type={}, data={:?})",
            self.node_type,
            self.data.keys().collect::<Vec<_>>()
        )
    }
}

/// Parse FluentAi source code using Rust parser
#[pyfunction]
fn parse(source: &str) -> PyResult<PyGraph> {
    match rust_parse(source) {
        Ok(graph) => Ok(PyGraph { inner: graph }),
        Err(e) => Err(PyValueError::new_err(format!("Parse error: {}", e))),
    }
}

/// Evaluate FluentAi code and return the result
#[pyfunction]
fn evaluate(source: &str) -> PyResult<PyObject> {
    Python::with_gil(|py| {
        // Parse the source
        let graph =
            rust_parse(source).map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;

        // Compile to bytecode
        let compiler = Compiler::new();
        let bytecode = compiler
            .compile(&graph)
            .map_err(|e| PyValueError::new_err(format!("Compilation error: {}", e)))?;

        // Create VM and execute
        let mut vm = VM::new(bytecode);
        match vm.run() {
            Ok(value) => value_to_python(py, &value),
            Err(e) => Err(PyValueError::new_err(format!("Runtime error: {}", e))),
        }
    })
}

/// Compile FluentAi code to bytecode
#[pyfunction]
fn compile(source: &str) -> PyResult<Vec<u8>> {
    // Parse the source
    let graph =
        rust_parse(source).map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;

    // Compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler
        .compile(&graph)
        .map_err(|e| PyValueError::new_err(format!("Compilation error: {}", e)))?;

    // Serialize bytecode (simplified - just return opcode bytes)
    let mut bytes = Vec::new();
    for chunk in &bytecode.chunks {
        for instruction in &chunk.instructions {
            bytes.push(opcode_to_u8(&instruction.opcode));
            bytes.extend_from_slice(&instruction.arg.to_le_bytes());
        }
    }

    Ok(bytes)
}

/// Convert Rust Value to Python object
fn value_to_python(py: Python, value: &Value) -> PyResult<PyObject> {
    match value {
        Value::Integer(n) => Ok(n.to_object(py)),
        Value::Float(f) => Ok(f.to_object(py)),
        Value::String(s) => Ok(s.to_object(py)),
        Value::Boolean(b) => Ok(b.to_object(py)),
        Value::Nil => Ok(py.None()),
        Value::List(elements) => {
            let py_list = PyList::empty(py);
            for elem in elements {
                py_list.append(value_to_python(py, elem)?)?;
            }
            Ok(py_list.to_object(py))
        }
        Value::Function { .. } => Ok("<function>".to_object(py)),
        Value::Map(map) => {
            let py_dict = PyDict::new(py);
            for (k, v) in map {
                py_dict.set_item(k, value_to_python(py, v)?)?;
            }
            Ok(py_dict.to_object(py))
        }
        Value::Promise(id) => Ok(format!("<promise:{}>", id).to_object(py)),
        Value::Future { chunk_id, .. } => Ok(format!("<future:{}>", chunk_id).to_object(py)),
        Value::Channel(id) => Ok(format!("<channel:{}>", id).to_object(py)),
        Value::Cell(id) => Ok(format!("<cell:{}>", id).to_object(py)),
        Value::Tagged { tag, values } => {
            let py_dict = PyDict::new(py);
            py_dict.set_item("tag", tag)?;
            let py_values = PyList::empty(py);
            for val in values {
                py_values.append(value_to_python(py, val)?)?;
            }
            py_dict.set_item("values", py_values)?;
            Ok(py_dict.to_object(py))
        }
        Value::Module { name, exports } => {
            let py_dict = PyDict::new(py);
            py_dict.set_item("name", name)?;
            let py_exports = PyDict::new(py);
            for (key, val) in exports {
                py_exports.set_item(key, value_to_python(py, val)?)?;
            }
            py_dict.set_item("exports", py_exports)?;
            Ok(py_dict.to_object(py))
        }
        Value::GcHandle(_) => Ok("<gc-handle>".to_object(py)),
        Value::Symbol(s) => Ok(format!(":{}", s).to_object(py)),
        Value::Procedure(_) => Ok("<procedure>".to_object(py)),
        Value::Vector(elements) => {
            let py_list = PyList::empty(py);
            for elem in elements {
                py_list.append(value_to_python(py, elem)?)?;
            }
            Ok(py_list.to_object(py))
        }
        Value::NativeFunction { name, .. } => {
            Ok(format!("<native-function:{}>", name).to_object(py))
        }
        Value::Actor(id) => Ok(format!("<actor:{}>", id).to_object(py)),
        Value::Error { kind, message, .. } => {
            Ok(format!("<error:{}:{}>", kind, message).to_object(py))
        }
    }
}

/// Convert opcode to u8 representation
fn opcode_to_u8(opcode: &fluentai_vm::bytecode::Opcode) -> u8 {
    use fluentai_vm::bytecode::Opcode::*;
    match opcode {
        Push => 0,
        Pop => 1,
        Dup => 2,
        Swap => 3,
        Add => 4,
        Sub => 5,
        Mul => 6,
        Div => 7,
        Mod => 8,
        Neg => 9,
        AddInt => 10,
        SubInt => 11,
        MulInt => 12,
        DivInt => 13,
        Eq => 14,
        Ne => 15,
        Lt => 16,
        Le => 17,
        Gt => 18,
        Ge => 19,
        LtInt => 20,
        LeInt => 21,
        GtInt => 22,
        GeInt => 23,
        And => 24,
        Or => 25,
        Not => 26,
        Jump => 27,
        JumpIf => 28,
        JumpIfNot => 29,
        Call => 30,
        Return => 31,
        Load => 32,
        Store => 33,
        LoadGlobal => 34,
        StoreGlobal => 35,
        LoadLocal0 => 36,
        LoadLocal1 => 37,
        LoadLocal2 => 38,
        LoadLocal3 => 39,
        StoreLocal0 => 40,
        StoreLocal1 => 41,
        StoreLocal2 => 42,
        StoreLocal3 => 43,
        MakeFunc => 44,
        MakeEnv => 45,
        PopEnv => 46,
        MakeList => 47,
        ListHead => 48,
        ListTail => 49,
        ListCons => 50,
        ListLen => 51,
        ListEmpty => 52,
        StrLen => 53,
        StrConcat => 54,
        StrUpper => 55,
        StrLower => 56,
        PushInt0 => 57,
        PushInt1 => 58,
        PushInt2 => 59,
        PushIntSmall => 60,
        PushTrue => 61,
        PushFalse => 62,
        PushNil => 63,
        PushConst => 64,
        Effect => 65,
        EffectAsync => 66,
        Await => 67,
        Spawn => 68,
        Channel => 69,
        ChannelWithCapacity => 70,
        Send => 71,
        Receive => 72,
        TrySend => 73,
        TryReceive => 74,
        Halt => 75,
        Nop => 76,
        MakeClosure => 77,
        LoadCaptured => 78,
        PopN => 79,
        MakeCell => 80,
        CellGet => 81,
        CellSet => 82,
        MakeTagged => 83,
        GetTag => 84,
        GetTaggedField => 85,
        IsTagged => 86,
        LoadModule => 87,
        ImportBinding => 88,
        LoadQualified => 89,
        BeginModule => 90,
        EndModule => 91,
        ExportBinding => 92,
        AddFloat => 93,
        SubFloat => 94,
        MulFloat => 95,
        DivFloat => 96,
        MakeHandler => 97,
        InstallHandler => 98,
        UninstallHandler => 99,
        ImportAll => 100,
        GcAlloc => 101,
        GcDeref => 102,
        GcSet => 103,
        GcCollect => 104,
        TailCall => 105,
        TailReturn => 106,
        LoopStart => 107,
        LoopEnd => 108,
        UpdateLocal => 109,
        Select => 110,
        CreateActor => 111,
        ActorSend => 112,
        ActorReceive => 113,
        Become => 114,
        Try => 115,
        Catch => 116,
        Finally => 117,
        Throw => 118,
        PushHandler => 119,
        PopHandler => 120,
        PromiseNew => 121,
        PromiseAll => 122,
        PromiseRace => 123,
        WithTimeout => 124,
    }
}

/// Benchmark function to measure parser performance
#[pyfunction]
fn benchmark_parser(source: &str, iterations: usize) -> PyResult<f64> {
    use std::time::Instant;

    let start = Instant::now();
    for _ in 0..iterations {
        let _ =
            rust_parse(source).map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;
    }
    let elapsed = start.elapsed();

    Ok(elapsed.as_secs_f64() / iterations as f64)
}

/// Python module definition
#[pymodule]
fn fluentai_rust(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyGraph>()?;
    m.add_class::<PyNode>()?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add_function(wrap_pyfunction!(evaluate, m)?)?;
    m.add_function(wrap_pyfunction!(compile, m)?)?;
    m.add_function(wrap_pyfunction!(benchmark_parser, m)?)?;
    Ok(())
}
