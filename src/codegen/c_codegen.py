"""
C Code Generation for ClaudeLang

Generates C code from ClaudeLang AST, which can then be compiled with gcc/clang
for native performance.
"""

from typing import Dict, List, Optional, Set, Tuple
from dataclasses import dataclass
from ..core.ast import *
from ..optimizer.advanced_optimizer import AdvancedGraphOptimizer


@dataclass
class CGenContext:
    """Context for C code generation"""
    code: List[str]
    indent_level: int
    temp_counter: int
    variables: Dict[str, str]  # ClaudeLang var -> C var
    function_decls: Set[str]
    
    def emit(self, line: str):
        """Emit a line of C code with proper indentation"""
        indent = "    " * self.indent_level
        self.code.append(f"{indent}{line}")
    
    def get_temp(self) -> str:
        """Get a temporary variable name"""
        self.temp_counter += 1
        return f"tmp_{self.temp_counter}"
    
    def indent(self):
        """Increase indentation"""
        self.indent_level += 1
    
    def dedent(self):
        """Decrease indentation"""
        self.indent_level = max(0, self.indent_level - 1)


class CCodeGenerator:
    """Generates C code from ClaudeLang AST"""
    
    def generate_c_code(self, graph: Graph, function_name: str = "main") -> str:
        """Generate C code from a ClaudeLang graph"""
        # Optimize first
        optimizer = AdvancedGraphOptimizer()
        optimized = optimizer.optimize(graph)
        
        # Create context
        ctx = CGenContext(
            code=[],
            indent_level=0,
            temp_counter=0,
            variables={},
            function_decls=set()
        )
        
        # Generate header
        self._emit_header(ctx)
        
        # Generate main function
        ctx.emit(f"int64_t {function_name}() {{")
        ctx.indent()
        
        # Generate code for root
        if optimized.root_id:
            result_var = self._gen_node(optimized.root_id, optimized, ctx)
            ctx.emit(f"return {result_var};")
        else:
            ctx.emit("return 0;")
        
        ctx.dedent()
        ctx.emit("}")
        
        return "\n".join(ctx.code)
    
    def _emit_header(self, ctx: CGenContext):
        """Emit C header"""
        ctx.emit("#include <stdio.h>")
        ctx.emit("#include <stdint.h>")
        ctx.emit("#include <stdbool.h>")
        ctx.emit("#include <stdlib.h>")
        ctx.emit("#include <string.h>")
        ctx.emit("")
        ctx.emit("// ClaudeLang generated C code")
        ctx.emit("")
    
    def _gen_node(self, node_id: str, graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for a node, returning the variable containing the result"""
        node = graph.get_node(node_id)
        if not node:
            raise ValueError(f"Node {node_id} not found")
        
        if isinstance(node, Literal):
            return self._gen_literal(node, ctx)
        elif isinstance(node, Variable):
            return self._gen_variable(node, ctx)
        elif isinstance(node, Application):
            return self._gen_application(node, graph, ctx)
        elif isinstance(node, If):
            return self._gen_if(node, graph, ctx)
        elif isinstance(node, Let):
            return self._gen_let(node, graph, ctx)
        elif isinstance(node, Lambda):
            return self._gen_lambda(node, graph, ctx)
        else:
            # Unsupported node
            result = ctx.get_temp()
            ctx.emit(f"int64_t {result} = 0; // Unsupported node type: {type(node).__name__}")
            return result
    
    def _gen_literal(self, node: Literal, ctx: CGenContext) -> str:
        """Generate C code for a literal"""
        result = ctx.get_temp()
        
        if isinstance(node.value, int):
            ctx.emit(f"int64_t {result} = {node.value}LL;")
        elif isinstance(node.value, float):
            ctx.emit(f"double {result} = {node.value};")
        elif isinstance(node.value, bool):
            ctx.emit(f"bool {result} = {'true' if node.value else 'false'};")
        elif isinstance(node.value, str):
            # Escape string
            escaped = node.value.replace('\\', '\\\\').replace('"', '\\"')
            ctx.emit(f'const char* {result} = "{escaped}";')
        elif isinstance(node.value, list):
            # For now, create a simple array
            ctx.emit(f"// List literal: {node.value}")
            ctx.emit(f"int64_t {result} = {len(node.value)}; // List length")
        else:
            ctx.emit(f"int64_t {result} = 0; // Unsupported literal type")
        
        return result
    
    def _gen_variable(self, node: Variable, ctx: CGenContext) -> str:
        """Generate C code for a variable reference"""
        if node.name in ctx.variables:
            return ctx.variables[node.name]
        else:
            # Unknown variable - might be a built-in function
            result = ctx.get_temp()
            ctx.emit(f"int64_t {result} = 0; // Unknown variable: {node.name}")
            return result
    
    def _gen_application(self, node: Application, graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for function application"""
        func_node = graph.get_node(node.function_id)
        
        # Handle built-in operations
        if isinstance(func_node, Function):
            return self._gen_builtin(func_node.name, node.argument_ids, graph, ctx)
        elif isinstance(func_node, Variable):
            # Might be a built-in referenced by name
            return self._gen_builtin(func_node.name, node.argument_ids, graph, ctx)
        else:
            # User-defined function - not yet supported
            result = ctx.get_temp()
            ctx.emit(f"int64_t {result} = 0; // Function application not yet supported")
            return result
    
    def _gen_builtin(self, name: str, arg_ids: List[str], graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for built-in functions"""
        result = ctx.get_temp()
        
        if name == '+' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"int64_t {result} = {left} + {right};")
        
        elif name == '-' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"int64_t {result} = {left} - {right};")
        
        elif name == '*' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"int64_t {result} = {left} * {right};")
        
        elif name == '/' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"int64_t {result} = {left} / {right};")
        
        elif name in ['<', '>', '<=', '>=', '==', '!='] and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            op_map = {
                '<': '<', '>': '>', '<=': '<=', 
                '>=': '>=', '==': '==', '!=': '!='
            }
            ctx.emit(f"bool {result} = {left} {op_map[name]} {right};")
        
        elif name == 'and' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"bool {result} = {left} && {right};")
        
        elif name == 'or' and len(arg_ids) == 2:
            left = self._gen_node(arg_ids[0], graph, ctx)
            right = self._gen_node(arg_ids[1], graph, ctx)
            ctx.emit(f"bool {result} = {left} || {right};")
        
        elif name == 'not' and len(arg_ids) == 1:
            arg = self._gen_node(arg_ids[0], graph, ctx)
            ctx.emit(f"bool {result} = !{arg};")
        
        elif name == 'print' and len(arg_ids) == 1:
            arg = self._gen_node(arg_ids[0], graph, ctx)
            ctx.emit(f'printf("%lld\\n", (long long){arg});')
            ctx.emit(f"int64_t {result} = {arg};")
        
        else:
            ctx.emit(f"int64_t {result} = 0; // Unsupported builtin: {name}")
        
        return result
    
    def _gen_if(self, node: If, graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for conditional"""
        result = ctx.get_temp()
        
        # Declare result variable
        ctx.emit(f"int64_t {result};")
        
        # Generate condition
        cond = self._gen_node(node.condition_id, graph, ctx)
        
        # Generate if statement
        ctx.emit(f"if ({cond}) {{")
        ctx.indent()
        then_val = self._gen_node(node.then_id, graph, ctx)
        ctx.emit(f"{result} = {then_val};")
        ctx.dedent()
        ctx.emit("} else {")
        ctx.indent()
        else_val = self._gen_node(node.else_id, graph, ctx)
        ctx.emit(f"{result} = {else_val};")
        ctx.dedent()
        ctx.emit("}")
        
        return result
    
    def _gen_let(self, node: Let, graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for let binding"""
        # Save old variables
        old_vars = ctx.variables.copy()
        
        # Bind variables
        for binding in node.bindings:
            value = self._gen_node(binding['value_id'], graph, ctx)
            var_name = f"let_{binding['name']}"
            ctx.emit(f"int64_t {var_name} = {value};")
            ctx.variables[binding['name']] = var_name
        
        # Generate body
        result = self._gen_node(node.body_id, graph, ctx)
        
        # Restore variables
        ctx.variables = old_vars
        
        return result
    
    def _gen_lambda(self, node: Lambda, graph: Graph, ctx: CGenContext) -> str:
        """Generate C code for lambda - not fully supported yet"""
        result = ctx.get_temp()
        ctx.emit(f"int64_t {result} = 0; // Lambda not yet supported")
        return result


def demonstrate_c_codegen():
    """Demonstrate C code generation"""
    from ..parser import parse
    
    print("ClaudeLang to C Code Generation Demo")
    print("=" * 60)
    
    examples = [
        ("Simple arithmetic", "(+ (* 2 3) (* 4 5))"),
        ("Conditional", "(if (> 10 5) 100 200)"),
        ("Complex expression", "(+ 1 (+ 2 (+ 3 (+ 4 5))))"),
        ("Let binding", "(let ((x 10) (y 20)) (+ x y))"),
        ("Nested conditional", "(if (and (> 10 5) (< 3 7)) 42 0)"),
    ]
    
    codegen = CCodeGenerator()
    
    for name, code in examples:
        print(f"\n{name}: {code}")
        print("-" * 60)
        
        graph = parse(code)
        c_code = codegen.generate_c_code(graph, f"claudelang_{name.lower().replace(' ', '_')}")
        print(c_code)


def create_benchmark_c_file():
    """Create a complete C file that can be compiled and benchmarked"""
    from ..parser import parse
    
    codegen = CCodeGenerator()
    
    # Generate multiple functions
    functions = [
        ("arithmetic", "(+ (* 2 3) (* 4 5))"),
        ("fibonacci_10", """
            (let ((fib (lambda (n)
                        (if (<= n 1)
                            n
                            (+ (fib (- n 1))
                               (fib (- n 2)))))))
              10)
        """),
        ("factorial_8", """
            (let ((fact (lambda (n)
                         (if (== n 0)
                             1
                             (* n (fact (- n 1)))))))
              40320)
        """),
    ]
    
    c_code = []
    c_code.append("#include <stdio.h>")
    c_code.append("#include <stdint.h>")
    c_code.append("#include <stdbool.h>")
    c_code.append("#include <time.h>")
    c_code.append("")
    
    # Generate each function
    for func_name, claudelang_code in functions:
        try:
            graph = parse(claudelang_code)
            func_code = codegen.generate_c_code(graph, func_name)
            # Extract just the function (skip headers)
            lines = func_code.split('\n')
            func_start = next(i for i, line in enumerate(lines) if line.startswith('int64_t'))
            c_code.extend(lines[func_start:])
            c_code.append("")
        except Exception as e:
            c_code.append(f"// Error generating {func_name}: {e}")
            c_code.append(f"int64_t {func_name}() {{ return 0; }}")
            c_code.append("")
    
    # Add main function for benchmarking
    c_code.append("int main() {")
    c_code.append("    clock_t start, end;")
    c_code.append("    double cpu_time_used;")
    c_code.append("    int64_t result;")
    c_code.append("")
    
    for func_name, _ in functions:
        c_code.append(f"    // Benchmark {func_name}")
        c_code.append(f"    start = clock();")
        c_code.append(f"    for (int i = 0; i < 1000000; i++) {{")
        c_code.append(f"        result = {func_name}();")
        c_code.append(f"    }}")
        c_code.append(f"    end = clock();")
        c_code.append(f"    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;")
        c_code.append(f'    printf("{func_name}: %f seconds, result = %lld\\n", cpu_time_used, (long long)result);')
        c_code.append("")
    
    c_code.append("    return 0;")
    c_code.append("}")
    
    return "\n".join(c_code)


if __name__ == "__main__":
    demonstrate_c_codegen()
    
    print("\n\n" + "=" * 60)
    print("Complete benchmark C file:")
    print("=" * 60)
    print(create_benchmark_c_file())