//! Python bindings for ClaudeLang Rust implementation

use pyo3::prelude::*;
use pyo3::exceptions::PyValueError;
use pyo3::types::PyList;
use claudelang_parser::parse as rust_parse;
use claudelang_core::ast::{Graph, Node, Literal};
use claudelang_vm::{compiler::Compiler, vm::VM, bytecode::Value};
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
        self.inner.nodes
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
                    data.insert("literal_type".to_string(), 
                        match lit {
                            Literal::Integer(_) => "int",
                            Literal::Float(_) => "float",
                            Literal::String(_) => "string",
                            Literal::Boolean(_) => "bool",
                            Literal::Nil => "nil",
                        }.to_object(py));
                    
                    data.insert("value".to_string(), 
                        match lit {
                            Literal::Integer(n) => n.to_object(py),
                            Literal::Float(f) => f.to_object(py),
                            Literal::String(s) => s.to_object(py),
                            Literal::Boolean(b) => b.to_object(py),
                            Literal::Nil => py.None(),
                        });
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
                Node::If { condition, then_branch, else_branch } => {
                    data.insert("condition_id".to_string(), condition.to_string().to_object(py));
                    data.insert("then_id".to_string(), then_branch.to_string().to_object(py));
                    data.insert("else_id".to_string(), else_branch.to_string().to_object(py));
                    "If"
                }
                Node::Application { function, args } => {
                    data.insert("function_id".to_string(), function.to_string().to_object(py));
                    data.insert("argument_ids".to_string(), 
                        args.iter().map(|id| id.to_string()).collect::<Vec<_>>().to_object(py));
                    "Application"
                }
                Node::Effect { effect_type, operation, args } => {
                    data.insert("effect_type".to_string(), effect_type.to_string().to_object(py));
                    data.insert("operation".to_string(), operation.to_object(py));
                    data.insert("argument_ids".to_string(), 
                        args.iter().map(|id| id.to_string()).collect::<Vec<_>>().to_object(py));
                    "Effect"
                }
                Node::List(elements) => {
                    data.insert("elements".to_string(), 
                        elements.iter().map(|id| id.to_string()).collect::<Vec<_>>().to_object(py));
                    "List"
                }
                Node::Match { expr, branches } => {
                    data.insert("expr_id".to_string(), expr.to_string().to_object(py));
                    // Simplified pattern representation for now
                    data.insert("branches".to_string(), branches.len().to_object(py));
                    "Match"
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
        format!("RustNode(type={}, data={:?})", self.node_type, self.data.keys().collect::<Vec<_>>())
    }
}

/// Parse ClaudeLang source code using Rust parser
#[pyfunction]
fn parse(source: &str) -> PyResult<PyGraph> {
    match rust_parse(source) {
        Ok(graph) => Ok(PyGraph { inner: graph }),
        Err(e) => Err(PyValueError::new_err(format!("Parse error: {}", e))),
    }
}

/// Evaluate ClaudeLang code and return the result
#[pyfunction]
fn evaluate(source: &str) -> PyResult<PyObject> {
    Python::with_gil(|py| {
        // Parse the source
        let graph = rust_parse(source)
            .map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;
        
        // Compile to bytecode
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph)
            .map_err(|e| PyValueError::new_err(format!("Compilation error: {}", e)))?;
        
        // Create VM and execute
        let mut vm = VM::new(bytecode);
        match vm.run() {
            Ok(value) => value_to_python(py, &value),
            Err(e) => Err(PyValueError::new_err(format!("Runtime error: {}", e))),
        }
    })
}

/// Compile ClaudeLang code to bytecode
#[pyfunction]
fn compile(source: &str) -> PyResult<Vec<u8>> {
    // Parse the source
    let graph = rust_parse(source)
        .map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;
    
    // Compile to bytecode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)
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
        Value::Int(n) => Ok(n.to_object(py)),
        Value::Float(f) => Ok(f.to_object(py)),
        Value::String(s) => Ok(s.to_object(py)),
        Value::Bool(b) => Ok(b.to_object(py)),
        Value::Nil => Ok(py.None()),
        Value::List(elements) => {
            let py_list = PyList::empty(py);
            for elem in elements {
                py_list.append(value_to_python(py, elem)?)?;
            }
            Ok(py_list.to_object(py))
        }
        Value::Function { .. } => Ok("<function>".to_object(py)),
    }
}

/// Convert opcode to u8 representation
fn opcode_to_u8(opcode: &claudelang_vm::bytecode::Opcode) -> u8 {
    use claudelang_vm::bytecode::Opcode::*;
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
        Effect => 64,
        Halt => 65,
        Nop => 66,
    }
}

/// Benchmark function to measure parser performance
#[pyfunction]
fn benchmark_parser(source: &str, iterations: usize) -> PyResult<f64> {
    use std::time::Instant;
    
    let start = Instant::now();
    for _ in 0..iterations {
        let _ = rust_parse(source).map_err(|e| PyValueError::new_err(format!("Parse error: {}", e)))?;
    }
    let elapsed = start.elapsed();
    
    Ok(elapsed.as_secs_f64() / iterations as f64)
}

/// Python module definition
#[pymodule]
fn claudelang_rust(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<PyGraph>()?;
    m.add_class::<PyNode>()?;
    m.add_function(wrap_pyfunction!(parse, m)?)?;
    m.add_function(wrap_pyfunction!(evaluate, m)?)?;
    m.add_function(wrap_pyfunction!(compile, m)?)?;
    m.add_function(wrap_pyfunction!(benchmark_parser, m)?)?;
    Ok(())
}