"""
ClaudeLang Module System

This module implements the module system for ClaudeLang, including:
- Module loading and caching
- Import/export resolution
- Module isolation and namespacing
- Dependency management
"""

import os
import json
from pathlib import Path
from typing import Dict, List, Optional, Set, Any, Tuple
from dataclasses import dataclass, field

from ..core.ast import Graph, ASTNode, NodeType, Module, Import, Export, Variable, QualifiedVariable
from ..parser.sexpr_parser import parse
from ..core.cache import _ast_cache
from ..interpreter.interpreter import Interpreter


@dataclass
class ModuleInfo:
    """Information about a loaded module"""
    path: str
    name: str
    graph: Graph
    exports: Dict[str, str]  # exported_name -> node_id
    dependencies: List[str]  # Module paths this module depends on
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ModuleEnvironment:
    """Environment for module execution"""
    modules: Dict[str, ModuleInfo] = field(default_factory=dict)  # path -> ModuleInfo
    bindings: Dict[str, Any] = field(default_factory=dict)  # Qualified name -> value
    search_paths: List[str] = field(default_factory=list)
    
    def __post_init__(self):
        # Add default search paths
        if not self.search_paths:
            self.search_paths = [
                ".",  # Current directory
                "./modules",  # Local modules directory
                str(Path(__file__).parent.parent / "stdlib" / "modules"),  # Stdlib modules
            ]


class ModuleLoader:
    """Handles loading and caching of modules"""
    
    def __init__(self):
        self.module_cache: Dict[str, ModuleInfo] = {}
        self.loading: Set[str] = set()  # Prevent circular imports
    
    def load_module(self, module_path: str, env: ModuleEnvironment) -> ModuleInfo:
        """Load a module from file or cache"""
        # Resolve module path
        resolved_path = self._resolve_module_path(module_path, env.search_paths)
        if not resolved_path:
            raise ImportError(f"Module not found: {module_path}")
        
        # Check cache
        if resolved_path in self.module_cache:
            return self.module_cache[resolved_path]
        
        # Check for circular imports
        if resolved_path in self.loading:
            raise ImportError(f"Circular import detected: {resolved_path}")
        
        # Mark as loading
        self.loading.add(resolved_path)
        
        try:
            # Load and parse module
            module_info = self._parse_module(resolved_path)
            
            # Process imports
            for node in module_info.graph.nodes.values():
                if isinstance(node, Import):
                    imported = self.load_module(node.module_path, env)
                    module_info.dependencies.append(imported.path)
            
            # Cache module
            self.module_cache[resolved_path] = module_info
            env.modules[resolved_path] = module_info
            
            return module_info
            
        finally:
            self.loading.remove(resolved_path)
    
    def _resolve_module_path(self, module_path: str, search_paths: List[str]) -> Optional[str]:
        """Resolve module path to absolute path"""
        # Direct path
        if os.path.isabs(module_path):
            if os.path.exists(module_path):
                return os.path.abspath(module_path)
            return None
        
        # Try with .cl extension
        if not module_path.endswith('.cl'):
            module_path_with_ext = module_path + '.cl'
        else:
            module_path_with_ext = module_path
        
        # Search in paths
        for search_path in search_paths:
            full_path = os.path.join(search_path, module_path_with_ext)
            if os.path.exists(full_path):
                return os.path.abspath(full_path)
            
            # Try as directory with module.cl
            dir_path = os.path.join(search_path, module_path, "module.cl")
            if os.path.exists(dir_path):
                return os.path.abspath(dir_path)
        
        return None
    
    def _parse_module(self, path: str) -> ModuleInfo:
        """Parse a module file"""
        with open(path, 'r') as f:
            source = f.read()
        
        # Parse the module
        graph = parse(source)
        
        # Extract module name from path
        module_name = Path(path).stem
        
        # Find exports
        exports = {}
        for node_id, node in graph.nodes.items():
            if isinstance(node, Export):
                for export in node.export_list:
                    export_name = export.get("as", export["name"])
                    exports[export_name] = export["name"]
            elif isinstance(node, Module):
                module_name = node.name or module_name
                exports.update({name: name for name in node.exports})
        
        return ModuleInfo(
            path=path,
            name=module_name,
            graph=graph,
            exports=exports,
            dependencies=[]
        )


class ModuleResolver:
    """Resolves imports and exports in modules"""
    
    def resolve_imports(self, module_info: ModuleInfo, env: ModuleEnvironment) -> Dict[str, Any]:
        """Resolve all imports for a module"""
        imports = {}
        
        for node in module_info.graph.nodes.values():
            if isinstance(node, Import):
                imported_module = env.modules.get(node.module_path)
                if not imported_module:
                    raise ImportError(f"Module not loaded: {node.module_path}")
                
                if node.import_all:
                    # Import all exports
                    for export_name, local_name in imported_module.exports.items():
                        qualified_name = f"{imported_module.name}.{export_name}"
                        imports[export_name] = qualified_name
                else:
                    # Import specific items
                    for item in node.import_list:
                        name = item["name"]
                        as_name = item.get("as", name)
                        
                        if name not in imported_module.exports:
                            raise ImportError(f"'{name}' not exported from {node.module_path}")
                        
                        qualified_name = f"{imported_module.name}.{name}"
                        imports[as_name] = qualified_name
        
        return imports
    
    def resolve_qualified_variables(self, graph: Graph, imports: Dict[str, str]) -> Graph:
        """Replace imported variables with qualified variables"""
        # Create a new graph with resolved variables
        new_graph = Graph(metadata=graph.metadata)
        
        for node_id, node in graph.nodes.items():
            if isinstance(node, Variable) and node.name in imports:
                # Replace with qualified variable
                qualified_name = imports[node.name]
                module_name, var_name = qualified_name.split('.', 1)
                
                qual_var = QualifiedVariable(
                    module_name=module_name,
                    variable_name=var_name
                )
                new_graph.add_node(qual_var)
                new_graph.nodes[node_id] = qual_var
            else:
                new_graph.nodes[node_id] = node
        
        new_graph.root_id = graph.root_id
        return new_graph


class ModuleInterpreter:
    """Interpreter with module support"""
    
    def __init__(self):
        self.loader = ModuleLoader()
        self.resolver = ModuleResolver()
        self.base_interpreter = Interpreter()
        self.module_values: Dict[str, Dict[str, Any]] = {}  # module_name -> {var_name -> value}
    
    def load_and_execute_module(self, module_path: str, env: Optional[ModuleEnvironment] = None) -> Any:
        """Load and execute a module"""
        if env is None:
            env = ModuleEnvironment()
        
        # Load module
        module_info = self.loader.load_module(module_path, env)
        
        # Execute dependencies first
        for dep_path in module_info.dependencies:
            if dep_path not in self.module_values:
                self.load_and_execute_module(dep_path, env)
        
        # Resolve imports
        imports = self.resolver.resolve_imports(module_info, env)
        
        # Resolve qualified variables in graph
        resolved_graph = self.resolver.resolve_qualified_variables(module_info.graph, imports)
        
        # Execute module
        result = self.execute_module(module_info, resolved_graph, env)
        
        return result
    
    def execute_module(self, module_info: ModuleInfo, graph: Graph, env: ModuleEnvironment) -> Any:
        """Execute a module and store its exports"""
        # Create module-specific interpreter environment
        module_env = self.base_interpreter.global_env.extend()
        
        # Add imported values to environment
        for node_id, node in graph.nodes.items():
            if isinstance(node, QualifiedVariable):
                module_name = node.module_name
                var_name = node.variable_name
                
                if module_name in self.module_values:
                    if var_name in self.module_values[module_name]:
                        value = self.module_values[module_name][var_name]
                        # Store as a literal in the graph
                        from ..core.ast import Literal, Value
                        lit = Literal(value=value)
                        graph.nodes[node_id] = lit
        
        # Execute the module
        result = self.base_interpreter.interpret(graph, module_env)
        
        # Store exported values
        module_values = {}
        for export_name, local_name in module_info.exports.items():
            value = module_env.lookup(local_name)
            if value:
                module_values[export_name] = value.data
        
        self.module_values[module_info.name] = module_values
        
        return result


# Module-aware functions for the standard library
def register_module_functions():
    """Register module-related functions"""
    from ..core.primitives import PRIMITIVES
    from ..core.ast import Function, EffectType, TypeAnnotation
    
    PRIMITIVES.register(
        "require",
        Function(
            name="require",
            arity=1,
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Module")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda path: _require_module(path)
    )
    
    PRIMITIVES.register(
        "module-exports",
        Function(
            name="module-exports",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Module"),
                    TypeAnnotation("List", [TypeAnnotation("String")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda module: _get_module_exports(module)
    )


def _require_module(path: str):
    """Runtime module loading"""
    interpreter = ModuleInterpreter()
    env = ModuleEnvironment()
    
    try:
        result = interpreter.load_and_execute_module(path, env)
        module_info = env.modules.get(path)
        if module_info:
            return {
                "type": "module",
                "name": module_info.name,
                "exports": module_info.exports,
                "values": interpreter.module_values.get(module_info.name, {})
            }
    except Exception as e:
        return {"error": f"Failed to load module: {str(e)}"}


def _get_module_exports(module):
    """Get list of exports from a module"""
    if isinstance(module, dict) and module.get("type") == "module":
        return list(module.get("exports", {}).keys())
    return []


# Register module functions when imported
register_module_functions()