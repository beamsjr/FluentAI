#[cfg(test)]
mod tests {
    use crate::traits::*;
    use crate::ast::{Graph, Node, NodeId, Literal};
    use std::sync::{Arc, Mutex};
    use anyhow::Result;
    
    // ===== Test Implementations =====
    
    struct TestModuleLoader {
        modules: Arc<Mutex<std::collections::HashMap<String, Arc<Graph>>>>,
        paths: Vec<String>,
    }
    
    impl TestModuleLoader {
        fn new() -> Self {
            Self {
                modules: Arc::new(Mutex::new(std::collections::HashMap::new())),
                paths: vec!["/test/path1".to_string(), "/test/path2".to_string()],
            }
        }
        
        fn add_module(&self, name: &str, graph: Graph) {
            self.modules.lock().unwrap().insert(name.to_string(), Arc::new(graph));
        }
    }
    
    impl ModuleLoader for TestModuleLoader {
        fn load_module(&self, name: &str) -> Result<Arc<Graph>> {
            self.modules
                .lock()
                .unwrap()
                .get(name)
                .cloned()
                .ok_or_else(|| anyhow::anyhow!("Module not found: {}", name))
        }
        
        fn module_exists(&self, name: &str) -> bool {
            self.modules.lock().unwrap().contains_key(name)
        }
        
        fn search_paths(&self) -> &[String] {
            &self.paths
        }
    }
    
    struct TestStdlibProvider {
        functions: std::collections::HashMap<String, (usize, Option<usize>, bool)>,
    }
    
    impl TestStdlibProvider {
        fn new() -> Self {
            let mut functions = std::collections::HashMap::new();
            // name -> (min_arity, max_arity, is_pure)
            functions.insert("+".to_string(), (2, Some(2), true));
            functions.insert("print".to_string(), (1, None, false));
            functions.insert("map".to_string(), (2, Some(2), true));
            
            Self { functions }
        }
    }
    
    impl StdlibProvider for TestStdlibProvider {
        fn has_function(&self, name: &str) -> bool {
            self.functions.contains_key(name)
        }
        
        fn get_arity(&self, name: &str) -> Option<(usize, Option<usize>)> {
            self.functions.get(name).map(|(min, max, _)| (*min, *max))
        }
        
        fn is_pure(&self, name: &str) -> bool {
            self.functions.get(name).map(|(_, _, pure)| *pure).unwrap_or(false)
        }
    }
    
    struct TestEffectHandler {
        name: String,
        operations: Vec<String>,
    }
    
    impl TestEffectHandler {
        fn new(name: &str, operations: Vec<&str>) -> Self {
            Self {
                name: name.to_string(),
                operations: operations.iter().map(|s| s.to_string()).collect(),
            }
        }
    }
    
    impl EffectHandler for TestEffectHandler {
        fn effect_name(&self) -> &str {
            &self.name
        }
        
        fn can_handle(&self, operation: &str) -> bool {
            self.operations.contains(&operation.to_string())
        }
        
        fn supported_operations(&self) -> Vec<String> {
            self.operations.clone()
        }
    }
    
    struct TestEffectContextProvider {
        handlers: Arc<Mutex<std::collections::HashMap<String, Arc<dyn EffectHandler>>>>,
    }
    
    impl TestEffectContextProvider {
        fn new() -> Self {
            Self {
                handlers: Arc::new(Mutex::new(std::collections::HashMap::new())),
            }
        }
    }
    
    impl EffectContextProvider for TestEffectContextProvider {
        fn register_handler(&self, handler: Box<dyn EffectHandler>) -> Result<()> {
            let name = handler.effect_name().to_string();
            self.handlers.lock().unwrap().insert(name, handler.into());
            Ok(())
        }
        
        fn get_handler(&self, effect_name: &str) -> Option<Arc<dyn EffectHandler>> {
            self.handlers.lock().unwrap().get(effect_name).cloned()
        }
        
        fn has_effect(&self, effect_name: &str) -> bool {
            self.handlers.lock().unwrap().contains_key(effect_name)
        }
    }
    
    struct TestOptimizationPass {
        name: String,
        min_level: OptimizationLevel,
    }
    
    impl TestOptimizationPass {
        fn new(name: &str, min_level: OptimizationLevel) -> Self {
            Self {
                name: name.to_string(),
                min_level,
            }
        }
    }
    
    impl OptimizationPass for TestOptimizationPass {
        fn name(&self) -> &str {
            &self.name
        }
        
        fn optimize(&self, graph: &Graph) -> Result<Graph> {
            // Simple optimization: clone the graph (real implementation would transform it)
            Ok(graph.clone())
        }
        
        fn should_run_at_level(&self, level: OptimizationLevel) -> bool {
            level >= self.min_level
        }
    }
    
    struct TestTypeChecker;
    
    impl TypeChecker for TestTypeChecker {
        fn check(&self, _graph: &Graph) -> Result<()> {
            // Simple implementation: always succeeds
            Ok(())
        }
        
        fn get_type(&self, _graph: &Graph, _node_id: NodeId) -> Result<String> {
            // Simple implementation: everything is "Any"
            Ok("Any".to_string())
        }
    }
    
    struct TestCodeGenerator {
        name: String,
        extension: String,
    }
    
    impl TestCodeGenerator {
        fn new(name: &str, extension: &str) -> Self {
            Self {
                name: name.to_string(),
                extension: extension.to_string(),
            }
        }
    }
    
    impl CodeGenerator for TestCodeGenerator {
        fn name(&self) -> &str {
            &self.name
        }
        
        fn generate(&self, _graph: &Graph) -> Result<Vec<u8>> {
            // Simple implementation: return dummy bytecode
            Ok(vec![0xDE, 0xAD, 0xBE, 0xEF])
        }
        
        fn file_extension(&self) -> &str {
            &self.extension
        }
    }
    
    struct TestDiagnosticReporter {
        errors: Arc<Mutex<Vec<(String, Option<NodeId>)>>>,
        warnings: Arc<Mutex<Vec<(String, Option<NodeId>)>>>,
        infos: Arc<Mutex<Vec<(String, Option<NodeId>)>>>,
    }
    
    impl TestDiagnosticReporter {
        fn new() -> Self {
            Self {
                errors: Arc::new(Mutex::new(Vec::new())),
                warnings: Arc::new(Mutex::new(Vec::new())),
                infos: Arc::new(Mutex::new(Vec::new())),
            }
        }
    }
    
    impl DiagnosticReporter for TestDiagnosticReporter {
        fn report_error(&self, message: &str, location: Option<NodeId>) {
            self.errors.lock().unwrap().push((message.to_string(), location));
        }
        
        fn report_warning(&self, message: &str, location: Option<NodeId>) {
            self.warnings.lock().unwrap().push((message.to_string(), location));
        }
        
        fn report_info(&self, message: &str, location: Option<NodeId>) {
            self.infos.lock().unwrap().push((message.to_string(), location));
        }
        
        fn has_errors(&self) -> bool {
            !self.errors.lock().unwrap().is_empty()
        }
        
        fn error_count(&self) -> usize {
            self.errors.lock().unwrap().len()
        }
    }
    
    // ===== ModuleLoader Tests =====
    
    #[test]
    fn test_module_loader() {
        let loader = TestModuleLoader::new();
        
        // Test empty loader
        assert!(!loader.module_exists("test"));
        assert!(loader.load_module("test").is_err());
        
        // Add a module
        let mut graph = Graph::new();
        graph.add_node(Node::Literal(Literal::Integer(42)));
        loader.add_module("test", graph);
        
        // Test module exists
        assert!(loader.module_exists("test"));
        assert!(!loader.module_exists("other"));
        
        // Test loading
        let loaded = loader.load_module("test").unwrap();
        assert_eq!(loaded.nodes.len(), 1);
        
        // Test search paths
        let paths = loader.search_paths();
        assert_eq!(paths.len(), 2);
        assert_eq!(paths[0], "/test/path1");
    }
    
    // ===== StdlibProvider Tests =====
    
    #[test]
    fn test_stdlib_provider() {
        let provider = TestStdlibProvider::new();
        
        // Test known functions
        assert!(provider.has_function("+"));
        assert!(provider.has_function("print"));
        assert!(provider.has_function("map"));
        assert!(!provider.has_function("unknown"));
        
        // Test arity
        assert_eq!(provider.get_arity("+"), Some((2, Some(2))));
        assert_eq!(provider.get_arity("print"), Some((1, None)));
        assert_eq!(provider.get_arity("unknown"), None);
        
        // Test purity
        assert!(provider.is_pure("+"));
        assert!(provider.is_pure("map"));
        assert!(!provider.is_pure("print"));
        assert!(!provider.is_pure("unknown"));
    }
    
    // ===== EffectHandler Tests =====
    
    #[test]
    fn test_effect_handler() {
        let handler = TestEffectHandler::new("IO", vec!["print", "read", "write"]);
        
        assert_eq!(handler.effect_name(), "IO");
        
        assert!(handler.can_handle("print"));
        assert!(handler.can_handle("read"));
        assert!(!handler.can_handle("delete"));
        
        let ops = handler.supported_operations();
        assert_eq!(ops.len(), 3);
        assert!(ops.contains(&"print".to_string()));
    }
    
    // ===== EffectContextProvider Tests =====
    
    #[test]
    fn test_effect_context_provider() {
        let provider = TestEffectContextProvider::new();
        
        // Initially empty
        assert!(!provider.has_effect("IO"));
        assert!(provider.get_handler("IO").is_none());
        
        // Register handler
        let handler = Box::new(TestEffectHandler::new("IO", vec!["print"]));
        provider.register_handler(handler).unwrap();
        
        // Check registration
        assert!(provider.has_effect("IO"));
        assert!(!provider.has_effect("State"));
        
        // Get handler
        let retrieved = provider.get_handler("IO").unwrap();
        assert_eq!(retrieved.effect_name(), "IO");
        assert!(retrieved.can_handle("print"));
    }
    
    // ===== OptimizationLevel Tests =====
    
    #[test]
    fn test_optimization_levels() {
        assert!(OptimizationLevel::None < OptimizationLevel::Basic);
        assert!(OptimizationLevel::Basic < OptimizationLevel::Standard);
        assert!(OptimizationLevel::Standard < OptimizationLevel::Aggressive);
        
        assert_eq!(OptimizationLevel::None as u8, 0);
        assert_eq!(OptimizationLevel::Basic as u8, 1);
        assert_eq!(OptimizationLevel::Standard as u8, 2);
        assert_eq!(OptimizationLevel::Aggressive as u8, 3);
    }
    
    // ===== OptimizationPass Tests =====
    
    #[test]
    fn test_optimization_pass() {
        let pass1 = TestOptimizationPass::new("ConstantFolding", OptimizationLevel::Basic);
        let pass2 = TestOptimizationPass::new("LoopUnrolling", OptimizationLevel::Aggressive);
        
        assert_eq!(pass1.name(), "ConstantFolding");
        assert_eq!(pass2.name(), "LoopUnrolling");
        
        // Test level checking
        assert!(!pass1.should_run_at_level(OptimizationLevel::None));
        assert!(pass1.should_run_at_level(OptimizationLevel::Basic));
        assert!(pass1.should_run_at_level(OptimizationLevel::Standard));
        assert!(pass1.should_run_at_level(OptimizationLevel::Aggressive));
        
        assert!(!pass2.should_run_at_level(OptimizationLevel::None));
        assert!(!pass2.should_run_at_level(OptimizationLevel::Basic));
        assert!(!pass2.should_run_at_level(OptimizationLevel::Standard));
        assert!(pass2.should_run_at_level(OptimizationLevel::Aggressive));
        
        // Test optimization
        let graph = Graph::new();
        let optimized = pass1.optimize(&graph).unwrap();
        assert_eq!(optimized.nodes.len(), graph.nodes.len());
    }
    
    // ===== TypeChecker Tests =====
    
    #[test]
    fn test_type_checker() {
        let checker = TestTypeChecker;
        let mut graph = Graph::new();
        let node_id = graph.add_node(Node::Literal(Literal::Integer(42)));
        
        // Type checking should succeed
        assert!(checker.check(&graph).is_ok());
        
        // Get type
        let node_type = checker.get_type(&graph, node_id).unwrap();
        assert_eq!(node_type, "Any");
    }
    
    // ===== CodeGenerator Tests =====
    
    #[test]
    fn test_code_generator() {
        let generator = TestCodeGenerator::new("LLVM", "ll");
        
        assert_eq!(generator.name(), "LLVM");
        assert_eq!(generator.file_extension(), "ll");
        
        let graph = Graph::new();
        let code = generator.generate(&graph).unwrap();
        assert_eq!(code, vec![0xDE, 0xAD, 0xBE, 0xEF]);
    }
    
    // ===== DiagnosticReporter Tests =====
    
    #[test]
    fn test_diagnostic_reporter() {
        let reporter = TestDiagnosticReporter::new();
        
        // Initially no errors
        assert!(!reporter.has_errors());
        assert_eq!(reporter.error_count(), 0);
        
        // Report various diagnostics
        let node_id = NodeId::new(1).unwrap();
        reporter.report_error("Type mismatch", Some(node_id));
        reporter.report_warning("Unused variable", None);
        reporter.report_info("Optimization applied", Some(node_id));
        
        // Check errors
        assert!(reporter.has_errors());
        assert_eq!(reporter.error_count(), 1);
        
        // Report another error
        reporter.report_error("Undefined variable", None);
        assert_eq!(reporter.error_count(), 2);
    }
    
    // ===== Integration Tests =====
    
    #[test]
    fn test_trait_objects() {
        // Test that implementations can be used as trait objects
        let loader: Box<dyn ModuleLoader> = Box::new(TestModuleLoader::new());
        assert_eq!(loader.search_paths().len(), 2);
        
        let provider: Box<dyn StdlibProvider> = Box::new(TestStdlibProvider::new());
        assert!(provider.has_function("+"));
        
        let handler: Box<dyn EffectHandler> = Box::new(TestEffectHandler::new("Test", vec![]));
        assert_eq!(handler.effect_name(), "Test");
        
        let context: Box<dyn EffectContextProvider> = Box::new(TestEffectContextProvider::new());
        assert!(!context.has_effect("Test"));
        
        let pass: Box<dyn OptimizationPass> = Box::new(TestOptimizationPass::new("Test", OptimizationLevel::None));
        assert_eq!(pass.name(), "Test");
        
        let checker: Box<dyn TypeChecker> = Box::new(TestTypeChecker);
        assert!(checker.check(&Graph::new()).is_ok());
        
        let generator: Box<dyn CodeGenerator> = Box::new(TestCodeGenerator::new("Test", "test"));
        assert_eq!(generator.name(), "Test");
        
        let reporter: Box<dyn DiagnosticReporter> = Box::new(TestDiagnosticReporter::new());
        assert!(!reporter.has_errors());
    }
    
    #[test]
    fn test_send_sync_bounds() {
        // This test verifies that all traits have Send + Sync bounds
        fn assert_send_sync<T: Send + Sync>() {}
        
        assert_send_sync::<Box<dyn ModuleLoader>>();
        assert_send_sync::<Box<dyn StdlibProvider>>();
        assert_send_sync::<Box<dyn EffectHandler>>();
        assert_send_sync::<Box<dyn EffectContextProvider>>();
        assert_send_sync::<Box<dyn OptimizationPass>>();
        assert_send_sync::<Box<dyn TypeChecker>>();
        assert_send_sync::<Box<dyn CodeGenerator>>();
        assert_send_sync::<Box<dyn DiagnosticReporter>>();
    }
}