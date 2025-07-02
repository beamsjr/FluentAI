"""
UI Compiler for ClaudeLang

This module compiles ClaudeLang UI code to JavaScript that can run in browsers.
It supports various output formats including standalone JS, React components,
and Web Components.
"""

import json
from typing import Dict, Any, List, Optional, Set, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto

from ..core.ast import *
from ..core.graph import Graph
from ..parser.sexpr_parser import parse

# Ensure UI node types are imported
from ..core.ast import UIComponent, UIControlFlow


class OutputFormat(Enum):
    """Supported output formats for UI compilation"""
    VANILLA_JS = auto()
    REACT = auto()
    VUE = auto()
    WEB_COMPONENT = auto()
    PREACT = auto()


@dataclass
class CompilerOptions:
    """Options for UI compilation"""
    output_format: OutputFormat = OutputFormat.VANILLA_JS
    minify: bool = False
    source_maps: bool = True
    module_format: str = "esm"  # esm, commonjs, umd
    runtime_import: str = "@claudelang/runtime"
    optimize_bundle_size: bool = True
    tree_shake: bool = True
    target_browsers: List[str] = field(default_factory=lambda: ["defaults"])


class UICompiler:
    """Compiles ClaudeLang UI code to JavaScript"""
    
    def __init__(self, options: Optional[CompilerOptions] = None):
        self.options = options or CompilerOptions()
        self.imports: Set[str] = set()
        self.helpers: Set[str] = set()
        self.component_registry: Dict[str, str] = {}
    
    def compile(self, source: str) -> str:
        """Compile ClaudeLang UI code to JavaScript"""
        # Parse the source
        graph = parse(source)
        
        # Extract components and compile
        js_code = self._compile_graph(graph)
        
        # Add runtime imports
        imports = self._generate_imports()
        
        # Add helper functions
        helpers = self._generate_helpers()
        
        # Add component definitions
        components = self._generate_components()
        
        # Combine everything
        output = f"{imports}\n\n{helpers}\n\n{components}\n\n{js_code}"
        
        # Post-process based on output format
        return self._post_process(output)
    
    def _compile_graph(self, graph: Graph) -> str:
        """Compile AST graph to JavaScript"""
        # First, process all component definitions
        for node_id, node in graph.nodes.items():
            if isinstance(node, UIComponent):
                self._compile_node(node_id, graph)
        
        # Then compile the main code starting from root
        return self._compile_node(graph.root_id, graph)
    
    def _compile_node(self, node_id: str, graph: Graph) -> str:
        """Compile an AST node to JavaScript"""
        node = graph.nodes.get(node_id)
        if not node:
            return "undefined"
        
        if isinstance(node, Literal):
            return self._compile_literal(node)
        elif isinstance(node, Variable):
            return self._compile_variable(node)
        elif isinstance(node, Function):
            return self._compile_function(node)
        elif isinstance(node, Lambda):
            return self._compile_lambda(node, graph)
        elif isinstance(node, Application):
            return self._compile_application(node, graph)
        elif isinstance(node, Let):
            return self._compile_let(node, graph)
        elif isinstance(node, If):
            return self._compile_if(node, graph)
        elif isinstance(node, Effect):
            return self._compile_effect(node, graph)
        elif isinstance(node, Sequence):
            return self._compile_sequence(node, graph)
        elif isinstance(node, UIComponent):
            return self._compile_ui_component(node, graph)
        elif isinstance(node, UIControlFlow):
            return self._compile_ui_control_flow(node, graph)
        else:
            return f"/* Unsupported node type: {type(node).__name__} */"
    
    def _compile_literal(self, node: Literal) -> str:
        """Compile literal to JavaScript"""
        if node.literal_type == 'string':
            return json.dumps(node.value)
        elif node.literal_type == 'int' or node.literal_type == 'float':
            return str(node.value)
        elif node.literal_type == 'bool':
            return 'true' if node.value else 'false'
        elif node.literal_type == 'nil':
            return 'null'
        elif node.literal_type == 'list':
            # For list literals, we just return an empty array
            # The parser creates list literals using cons operations
            return "[]"
        else:
            return json.dumps(node.value)
    
    def _compile_variable(self, node: Variable) -> str:
        """Compile variable reference"""
        # Convert Lisp-style names to JavaScript
        js_name = node.name.replace('-', '_').replace('!', '').replace('?', '_p')
        return js_name
    
    def _compile_function(self, node: Function) -> str:
        """Compile built-in function reference"""
        # Return a reference to the primitive
        self.imports.add("ClaudeLang")
        return f"ClaudeLang.primitives['{node.name}']"
    
    def _compile_lambda(self, node: Lambda, graph: Graph) -> str:
        """Compile lambda to JavaScript function"""
        params = [self._compile_variable(Variable(name=p)) for p in node.parameter_names]
        body = self._compile_node(node.body_id, graph)
        
        if self.options.output_format == OutputFormat.VANILLA_JS:
            return f"({', '.join(params)}) => {body}"
        else:
            return f"function({', '.join(params)}) {{ return {body}; }}"
    
    def _compile_application(self, node: Application, graph: Graph) -> str:
        """Compile function application"""
        func_node = graph.nodes.get(node.function_id)
        args = [self._compile_node(arg_id, graph) for arg_id in node.argument_ids]
        
        # Handle primitives and special forms
        if isinstance(func_node, Variable):
            func_name = func_node.name
            
            # Check if it's a primitive
            if func_name in ['+', '-', '*', '/', '=', '<', '>', '<=', '>=', 
                           'not', 'and', 'or', 'cons', 'car', 'cdr', 'empty?',
                           'length', 'map', 'filter', 'reduce', 'concat', 
                           'to-string', 'get', 'set']:
                self.imports.add("ClaudeLang")
                return f"ClaudeLang.primitives['{func_name}']({', '.join(args)})"
            
            # UI primitives
            elif func_name.startswith("dom:") or func_name.startswith("ui:"):
                self.imports.add("ClaudeLang")
                return f"ClaudeLang.primitives['{func_name}']({', '.join(args)})"
            
            # Reactive operations via effects
            elif func_name.startswith("reactive:"):
                return self._compile_reactive_call(func_name, args)
        
        # Special handling for Function nodes (built-in functions)
        elif isinstance(func_node, Function):
            func_name = func_node.name
            
            # Optimize cons operations that build lists
            if func_name == "cons" and len(args) == 2 and args[1] == "[]":
                return f"[{args[0]}]"
            
            self.imports.add("ClaudeLang")
            return f"ClaudeLang.primitives['{func_name}']({', '.join(args)})"
        
        # Regular function call
        func = self._compile_node(node.function_id, graph)
        return f"{func}({', '.join(args)})"
    
    def _compile_let(self, node: Let, graph: Graph) -> str:
        """Compile let binding"""
        bindings = []
        for binding in node.bindings:
            name = self._compile_variable(Variable(name=binding["name"]))
            value = self._compile_node(binding["value_id"], graph)
            bindings.append(f"const {name} = {value};")
        
        body = self._compile_node(node.body_id, graph)
        
        if self.options.output_format == OutputFormat.VANILLA_JS:
            return f"(() => {{ {' '.join(bindings)} return {body}; }})()"
        else:
            return f"(function() {{ {' '.join(bindings)} return {body}; }})()"
    
    def _compile_if(self, node: If, graph: Graph) -> str:
        """Compile if expression"""
        condition = self._compile_node(node.condition_id, graph)
        then_expr = self._compile_node(node.then_id, graph)
        else_expr = self._compile_node(node.else_id, graph)
        
        return f"({condition} ? {then_expr} : {else_expr})"
    
    def _compile_effect(self, node: Effect, graph: Graph) -> str:
        """Compile effect to JavaScript"""
        args = [self._compile_node(arg_id, graph) for arg_id in node.argument_ids]
        self.imports.add("ClaudeLang")
        
        # All effects go through the runtime
        return f"ClaudeLang.performEffect('{node.effect_type.name}', '{node.operation}', [{', '.join(args)}])"
    
    def _compile_sequence(self, node: Sequence, graph: Graph) -> str:
        """Compile sequence of expressions"""
        steps = [self._compile_node(step_id, graph) for step_id in node.step_ids]
        
        if len(steps) == 0:
            return "undefined"
        elif len(steps) == 1:
            return steps[0]
        else:
            # Use comma operator for sequences
            return f"({', '.join(steps)})"
    
    def _compile_dom_h(self, args: List[str]) -> str:
        """Compile dom:h to appropriate output format"""
        tag, props, children = args[0], args[1], args[2] if len(args) > 2 else "[]"
        
        if self.options.output_format == OutputFormat.REACT:
            self.imports.add("React")
            return f"React.createElement({tag}, {props}, ...{children})"
        elif self.options.output_format == OutputFormat.VUE:
            self.imports.add("h")
            return f"h({tag}, {props}, {children})"
        else:
            self.imports.add("h")
            self.helpers.add("h")
            return f"h({tag}, {props}, {children})"
    
    def _compile_ui_if(self, args: List[str]) -> str:
        """Compile ui:if conditional rendering"""
        condition, then_vnode, else_vnode = args
        return f"({condition} ? {then_vnode} : {else_vnode})"
    
    def _compile_ui_for(self, args: List[str]) -> str:
        """Compile ui:for list rendering"""
        items, render_fn = args
        return f"{items}.map({render_fn})"
    
    def _compile_reactive_call(self, func_name: str, args: List[str]) -> str:
        """Compile reactive state calls"""
        operation = func_name.split(':')[1]
        
        if operation == "ref":
            self.imports.add("ref")
            return f"ref({args[0]})"
        elif operation == "get":
            return f"{args[0]}.value"
        elif operation == "set":
            return f"({args[0]}.value = {args[1]})"
        elif operation == "computed":
            self.imports.add("computed")
            return f"computed({args[0]})"
        elif operation == "watch":
            self.imports.add("watch")
            return f"watch({', '.join(args)})"
        else:
            return f"/* Unknown reactive operation: {operation} */"
    
    def _compile_dom_effect(self, node: Effect, graph: Graph) -> str:
        """Compile DOM effect"""
        args = [self._compile_node(arg_id, graph) for arg_id in node.argument_ids]
        
        if node.operation == "render":
            self.imports.add("render")
            return f"render({args[0]}, {args[1] if len(args) > 1 else '\"#app\"'})"
        else:
            return f"/* Unsupported DOM effect: {node.operation} */"
    
    def _compile_state_effect(self, node: Effect, graph: Graph) -> str:
        """Compile state effect"""
        # State effects are handled by reactive compilation
        return self._compile_reactive_call(f"reactive:{node.operation}", 
                                         [self._compile_node(arg_id, graph) 
                                          for arg_id in node.argument_ids])
    
    def _generate_imports(self) -> str:
        """Generate import statements"""
        if self.options.output_format == OutputFormat.REACT:
            imports = ["import React from 'react';"]
            if "render" in self.imports:
                imports.append("import { render } from 'react-dom';")
            return '\n'.join(imports)
        elif self.options.output_format == OutputFormat.VUE:
            return "import { h, ref, computed, watch } from 'vue';"
        else:
            # For vanilla JS, we'll include the runtime inline or as a script tag
            # For now, assume the runtime is loaded globally
            return "// Requires ClaudeLang runtime to be loaded"
    
    def _generate_helpers(self) -> str:
        """Generate helper functions"""
        helpers = []
        
        if "h" in self.helpers and self.options.output_format == OutputFormat.VANILLA_JS:
            helpers.append("""
function h(tag, props, children) {
  return { tag, props: props || {}, children: children || [] };
}
""")
        
        return '\n'.join(helpers)
    
    def _generate_components(self) -> str:
        """Generate component definitions"""
        if not self.component_registry:
            return ""
        
        components = ["// Component definitions"]
        for name, code in self.component_registry.items():
            components.append(code)
        
        return '\n\n'.join(components)
    
    def _post_process(self, code: str) -> str:
        """Post-process the generated code"""
        # Add module wrapper if needed
        if self.options.module_format == "commonjs":
            code = f"module.exports = {{\n{code}\n}};"
        elif self.options.module_format == "umd":
            code = f"""
(function (root, factory) {{
    if (typeof define === 'function' && define.amd) {{
        define([], factory);
    }} else if (typeof module === 'object' && module.exports) {{
        module.exports = factory();
    }} else {{
        root.ClaudeLangUI = factory();
    }}
}}(typeof self !== 'undefined' ? self : this, function () {{
    {code}
}}));
"""
        
        # TODO: Add minification if requested
        # TODO: Generate source maps if requested
        
        return code
    
    def _compile_ui_component(self, node: UIComponent, graph: Graph) -> str:
        """Compile UI component definition"""
        # Generate a component factory function
        render_fn = self._compile_node(node.render_function_id, graph)
        
        # Build prop validation object
        prop_defs = {}
        for prop_name, prop_def in node.props.items():
            prop_defs[prop_name] = {
                "type": prop_def.prop_type,
                "required": prop_def.required,
                "default": prop_def.default_value
            }
        
        # Generate component code
        component_code = f"""
ClaudeLang.defineComponent('{node.name}', {{
    props: {json.dumps(prop_defs)},
    render: {render_fn}
}})"""
        
        # Add to component registry
        self.component_registry[node.name] = component_code
        
        # Return component reference
        return f"ClaudeLang.components['{node.name}']"
    
    def _compile_ui_control_flow(self, node: UIControlFlow, graph: Graph) -> str:
        """Compile UI control flow (if/for/when)"""
        if node.flow_type == "if":
            condition = self._compile_node(node.condition_id, graph)
            then_expr = self._compile_node(node.then_id, graph)
            else_expr = self._compile_node(node.else_id, graph) if node.else_id else "null"
            return f"({condition} ? {then_expr} : {else_expr})"
        
        elif node.flow_type == "for":
            items = self._compile_node(node.items_id, graph)
            render_fn = self._compile_node(node.render_func_id, graph)
            return f"{items}.map({render_fn})"
        
        elif node.flow_type == "when":
            condition = self._compile_node(node.condition_id, graph)
            then_expr = self._compile_node(node.then_id, graph)
            return f"({condition} ? {then_expr} : null)"
        
        else:
            return f"/* Unknown UI control flow: {node.flow_type} */"


def compile_ui_file(input_file: str, output_file: str, 
                   options: Optional[CompilerOptions] = None) -> None:
    """Compile a ClaudeLang UI file to JavaScript"""
    with open(input_file, 'r') as f:
        source = f.read()
    
    compiler = UICompiler(options)
    js_code = compiler.compile(source)
    
    with open(output_file, 'w') as f:
        f.write(js_code)