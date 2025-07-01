"""
Simple tests for ClaudeLang Module System components
"""

import unittest
import os
import tempfile
import shutil
from pathlib import Path
from src.parser.sexpr_parser import parse
from src.core.ast import Module, Import, Export, Graph


class TestModuleParsingComponents(unittest.TestCase):
    """Test parsing of module-related AST nodes"""
    
    def test_parse_module_form(self):
        """Test parsing module form"""
        source = """
        (module math
          (export add subtract)
          (let ((add (lambda (x y) (+ x y)))
                (subtract (lambda (x y) (- x y))))
            'math))
        """
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Module)
        self.assertEqual(root.name, "math")
        self.assertEqual(set(root.exports), {"add", "subtract"})
    
    def test_parse_import_form(self):
        """Test parsing import form"""
        source = '(import "math")'
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Import)
        self.assertEqual(root.module_path, "math")
    
    def test_parse_export_form(self):
        """Test parsing export form"""
        source = "(export foo bar baz)"
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Export)
        self.assertEqual(len(root.export_list), 3)
        self.assertEqual(root.export_list[0]["name"], "foo")
        self.assertEqual(root.export_list[1]["name"], "bar")
        self.assertEqual(root.export_list[2]["name"], "baz")
    
    def test_module_with_body(self):
        """Test module with body expression"""
        source = """
        (module simple
          (export value)
          (let ((value 42))
            value))
        """
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Module)
        self.assertIsNotNone(root.body_id)
        
        # Check body is a let expression
        body = graph.get_node(root.body_id)
        self.assertEqual(body.node_type.name, "LET")
    
    def test_import_with_items(self):
        """Test import with specific items"""
        source = '(import "math" (sin cos tan))'
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Import)
        self.assertEqual(len(root.import_list), 3)
        self.assertEqual(root.import_list[0]["name"], "sin")
        self.assertEqual(root.import_list[1]["name"], "cos")
        self.assertEqual(root.import_list[2]["name"], "tan")
    
    def test_import_all(self):
        """Test import all syntax"""
        source = '(import "utils" *)'
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Import)
        self.assertTrue(root.import_all)
    
    def test_empty_module(self):
        """Test empty module"""
        source = "(module empty 'empty)"  # Module needs a body
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Module)
        self.assertEqual(root.name, "empty")
        self.assertEqual(len(root.exports), 0)
        self.assertIsNotNone(root.body_id)
    
    def test_module_no_exports(self):
        """Test module with no exports"""
        source = """
        (module internal
          (let ((secret 'hidden))
            secret))
        """
        
        graph = parse(source)
        root = graph.get_node(graph.root_id)
        
        self.assertIsInstance(root, Module)
        self.assertEqual(len(root.exports), 0)


class TestModuleFileOperations(unittest.TestCase):
    """Test module file operations"""
    
    def setUp(self):
        """Set up test directory"""
        self.test_dir = tempfile.mkdtemp()
    
    def tearDown(self):
        """Clean up test directory"""
        shutil.rmtree(self.test_dir)
    
    def test_module_path_resolution(self):
        """Test basic module path resolution"""
        # Create test structure
        module_paths = [
            "math.cl",
            "utils/string.cl",
            "data/structures/tree.cl",
            "mypackage/module.cl"
        ]
        
        for path in module_paths:
            full_path = os.path.join(self.test_dir, path)
            os.makedirs(os.path.dirname(full_path), exist_ok=True)
            with open(full_path, 'w') as f:
                f.write(f"(module {Path(path).stem})")
        
        # Test path existence
        for path in module_paths:
            full_path = os.path.join(self.test_dir, path)
            self.assertTrue(os.path.exists(full_path))
    
    def test_search_path_priority(self):
        """Test module search path priority"""
        # Create same module in different locations
        search_paths = [
            os.path.join(self.test_dir, "local"),
            os.path.join(self.test_dir, "system"),
            os.path.join(self.test_dir, "builtin")
        ]
        
        for i, path in enumerate(search_paths):
            os.makedirs(path, exist_ok=True)
            module_file = os.path.join(path, "test.cl")
            with open(module_file, 'w') as f:
                f.write(f'(module test (export value) (let ((value {i})) value))')
        
        # First in search path should win
        found = None
        for search_path in search_paths:
            test_file = os.path.join(search_path, "test.cl")
            if os.path.exists(test_file):
                found = test_file
                break
        
        self.assertIsNotNone(found)
        self.assertIn("local", found)
    
    def test_module_extensions(self):
        """Test module file extensions"""
        # Test that .cl is the standard extension
        module_file = os.path.join(self.test_dir, "test.cl")
        with open(module_file, 'w') as f:
            f.write("(module test)")
        
        # Should find with extension
        self.assertTrue(os.path.exists(module_file))
        
        # Should be able to reference without extension
        base_name = os.path.join(self.test_dir, "test")
        self.assertEqual(base_name + ".cl", module_file)


class TestModuleDependencyGraph(unittest.TestCase):
    """Test module dependency tracking"""
    
    def test_simple_dependency(self):
        """Test simple dependency tracking"""
        # Mock dependency graph
        dependencies = {
            "app": ["utils", "math"],
            "utils": ["string"],
            "math": [],
            "string": []
        }
        
        # Test direct dependencies
        self.assertIn("utils", dependencies["app"])
        self.assertIn("math", dependencies["app"])
        
        # Test transitive dependencies
        all_deps = set()
        
        def collect_deps(module):
            for dep in dependencies.get(module, []):
                all_deps.add(dep)
                collect_deps(dep)
        
        collect_deps("app")
        self.assertEqual(all_deps, {"utils", "math", "string"})
    
    def test_circular_dependency_detection(self):
        """Test circular dependency detection"""
        # Mock circular dependency
        dependencies = {
            "a": ["b"],
            "b": ["c"],
            "c": ["a"]  # Creates cycle
        }
        
        # Simple cycle detection
        def has_cycle(deps, start, visited=None, path=None):
            if visited is None:
                visited = set()
            if path is None:
                path = set()
            
            visited.add(start)
            path.add(start)
            
            for neighbor in deps.get(start, []):
                if neighbor in path:
                    return True
                if neighbor not in visited:
                    if has_cycle(deps, neighbor, visited, path):
                        return True
            
            path.remove(start)
            return False
        
        self.assertTrue(has_cycle(dependencies, "a"))
    
    def test_dependency_order(self):
        """Test topological ordering of dependencies"""
        # Mock dependencies
        dependencies = {
            "app": ["ui", "logic"],
            "ui": ["widgets"],
            "logic": ["data"],
            "widgets": [],
            "data": []
        }
        
        # Simple topological sort
        def topological_sort(deps):
            visited = set()
            stack = []
            
            def visit(node):
                if node in visited:
                    return
                visited.add(node)
                for dep in deps.get(node, []):
                    visit(dep)
                stack.append(node)
            
            for node in deps:
                visit(node)
            
            return stack
        
        order = topological_sort(dependencies)
        
        # Dependencies should come before dependents
        self.assertTrue(order.index("widgets") < order.index("ui"))
        self.assertTrue(order.index("data") < order.index("logic"))
        self.assertTrue(order.index("ui") < order.index("app"))
        self.assertTrue(order.index("logic") < order.index("app"))


class TestModuleNamespacing(unittest.TestCase):
    """Test module namespacing"""
    
    def test_qualified_names(self):
        """Test qualified name construction"""
        module_name = "math"
        export_name = "sin"
        
        qualified = f"{module_name}.{export_name}"
        self.assertEqual(qualified, "math.sin")
        
        # Test parsing qualified names
        parts = qualified.split(".")
        self.assertEqual(len(parts), 2)
        self.assertEqual(parts[0], module_name)
        self.assertEqual(parts[1], export_name)
    
    def test_nested_module_names(self):
        """Test nested module naming"""
        # Test path-based module names
        module_path = "data/structures/tree"
        
        # Different naming conventions
        dot_notation = module_path.replace("/", ".")
        self.assertEqual(dot_notation, "data.structures.tree")
        
        # Reverse conversion
        path_notation = dot_notation.replace(".", "/")
        self.assertEqual(path_notation, module_path)
    
    def test_name_conflicts(self):
        """Test handling of name conflicts"""
        # Mock multiple modules exporting same name
        exports = {
            "math": ["max", "min", "abs"],
            "utils": ["max", "min", "first"],
            "data": ["first", "last", "max"]
        }
        
        # Qualified names should disambiguate
        qualified_names = set()
        for module, names in exports.items():
            for name in names:
                qualified = f"{module}.{name}"
                qualified_names.add(qualified)
        
        # All qualified names should be unique
        self.assertEqual(len(qualified_names), 9)  # 3 + 3 + 3
        
        # Common names
        common_names = {"max", "min", "first"}
        for name in common_names:
            modules_with_name = [m for m, names in exports.items() if name in names]
            self.assertGreater(len(modules_with_name), 1)


class TestModuleAST(unittest.TestCase):
    """Test module-related AST structures"""
    
    def test_module_ast_creation(self):
        """Test creating Module AST nodes"""
        module = Module(
            name="test",
            exports=["foo", "bar"],
            body_id="body-123"
        )
        
        self.assertEqual(module.name, "test")
        self.assertEqual(set(module.exports), {"foo", "bar"})
        self.assertEqual(module.body_id, "body-123")
        self.assertEqual(module.node_type.name, "MODULE")
    
    def test_import_ast_creation(self):
        """Test creating Import AST nodes"""
        import_node = Import(
            module_path="math",
            import_list=[{"name": "sin"}, {"name": "cos"}],
            import_all=False
        )
        
        self.assertEqual(import_node.module_path, "math")
        self.assertEqual(len(import_node.import_list), 2)
        self.assertFalse(import_node.import_all)
        self.assertEqual(import_node.node_type.name, "IMPORT")
    
    def test_export_ast_creation(self):
        """Test creating Export AST nodes"""
        export_node = Export(
            export_list=[
                {"name": "internal1", "as": "public1"},
                {"name": "internal2"}
            ]
        )
        
        self.assertEqual(len(export_node.export_list), 2)
        self.assertEqual(export_node.export_list[0]["as"], "public1")
        self.assertNotIn("as", export_node.export_list[1])
        self.assertEqual(export_node.node_type.name, "EXPORT")
    
    def test_ast_in_graph(self):
        """Test module AST nodes in graph"""
        graph = Graph()
        
        # Create module node
        module = Module(name="test", exports=["value"])
        module_id = graph.add_node(module)
        
        # Create import node
        import_node = Import(module_path="base")
        import_id = graph.add_node(import_node)
        
        # Create export node
        export_node = Export(export_list=[{"name": "value"}])
        export_id = graph.add_node(export_node)
        
        # Verify all nodes are in graph
        self.assertIn(module_id, graph.nodes)
        self.assertIn(import_id, graph.nodes)
        self.assertIn(export_id, graph.nodes)
        
        # Verify correct types
        self.assertIsInstance(graph.get_node(module_id), Module)
        self.assertIsInstance(graph.get_node(import_id), Import)
        self.assertIsInstance(graph.get_node(export_id), Export)


if __name__ == '__main__':
    unittest.main()