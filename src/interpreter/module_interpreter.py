"""
ClaudeLang Module-aware Interpreter

This extends the base interpreter with module system support.
"""

from typing import Dict, Any, Optional
import os
from ..core.ast import *
from ..parser.sexpr_parser import parse
from ..modules.module_system import ModuleLoader, ModuleResolver
from .interpreter import Interpreter, Value, Environment


class ModuleInterpreter(Interpreter):
    """Interpreter with module system support"""
    
    def __init__(self, module_paths: List[str] = None):
        super().__init__()
        self.module_loader = ModuleLoader()
        self.module_resolver = ModuleResolver()
        self.module_paths = module_paths or ["modules", "."]
        self.current_module: Optional[str] = None
        self.module_envs: Dict[str, Environment] = {}
    
    def evaluate(self, graph: Graph, env: Optional[Environment] = None) -> Any:
        """Evaluate a graph with module support"""
        if env is None:
            env = self._create_global_env()
        
        # Check if this is a module definition
        root = graph.nodes[graph.root_id]
        if root.type == NodeType.MODULE:
            return self._evaluate_module(root, graph, env)
        
        # Process imports first
        for node_id, node in graph.nodes.items():
            if node.type == NodeType.IMPORT:
                self._evaluate_import(node, graph, env)
        
        # Regular evaluation
        return super().evaluate(graph, env)
    
    def _evaluate_module(self, node: ASTNode, graph: Graph, env: Environment) -> Any:
        """Evaluate a module definition"""
        name = node.attributes["name"]
        exports = node.attributes.get("exports", [])
        body_id = node.children[0]
        
        # Create module environment
        module_env = env.extend()
        self.current_module = name
        self.module_envs[name] = module_env
        
        # Evaluate module body
        result = self._eval_node(body_id, graph, module_env)
        
        # Extract exports
        export_map = {}
        if isinstance(result, Value) and isinstance(result.data, tuple):
            # Exports are returned as a tuple
            for i, export_name in enumerate(exports):
                if i < len(result.data):
                    export_map[export_name] = result.data[i]
        
        self.current_module = None
        return export_map
    
    def _evaluate_import(self, node: ASTNode, graph: Graph, env: Environment) -> None:
        """Evaluate an import statement"""
        module_path = node.attributes["module"]
        imports = node.attributes.get("imports", [])
        
        # Load module if not already loaded
        from ..modules.module_system import ModuleEnvironment
        module_env = ModuleEnvironment(search_paths=self.module_paths)
        module_info = self.module_loader.load_module(module_path, module_env)
        
        # Create module environment if needed
        if module_info.name not in self.module_envs:
            module_env = env.extend()
            self.module_envs[module_info.name] = module_env
            
            # Evaluate the module
            module_result = super().evaluate(module_info.graph, module_env)
            
            # Store exports in module environment
            if isinstance(module_result, dict):
                for name, value in module_result.items():
                    module_env.bind(name, Value(value))
        
        # Import specified symbols
        module_env = self.module_envs[module_info.name]
        if imports == ["*"]:
            # Import all exports
            for export_name in module_info.exports:
                if export_name in module_env.bindings:
                    env.bind(export_name, module_env.bindings[export_name])
        else:
            # Import specific symbols
            for import_name in imports:
                if import_name in module_env.bindings:
                    env.bind(import_name, module_env.bindings[import_name])
    
    def _eval_node(self, node_id: str, graph: Graph, env: Environment) -> Any:
        """Extended node evaluation with module support"""
        node = graph.nodes[node_id]
        
        if node.type == NodeType.IMPORT:
            self._evaluate_import(node, graph, env)
            return Value(None)
        elif node.type == NodeType.EXPORT:
            # Exports are handled at module level
            return Value(None)
        elif node.type == NodeType.QUALIFIED_VAR:
            module_name = node.attributes["module"]
            var_name = node.attributes["name"]
            
            if module_name in self.module_envs:
                module_env = self.module_envs[module_name]
                value = module_env.lookup(var_name)
                if value:
                    return value
            
            raise RuntimeError(f"Unbound qualified variable: {module_name}:{var_name}")
        else:
            return super()._eval_node(node_id, graph, env)


def run_file(filename: str, module_paths: List[str] = None):
    """Run a ClaudeLang file with module support"""
    with open(filename, 'r') as f:
        source = f.read()
    
    # Set up module paths
    if module_paths is None:
        file_dir = os.path.dirname(os.path.abspath(filename))
        module_paths = [
            os.path.join(file_dir, "modules"),
            file_dir,
            "modules",
            "."
        ]
    
    # Parse and evaluate
    graph = parse(source)
    interpreter = ModuleInterpreter(module_paths)
    return interpreter.evaluate(graph)


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        result = run_file(sys.argv[1])
        if result is not None:
            print(result)