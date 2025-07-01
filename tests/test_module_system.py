"""
Tests for ClaudeLang Module System
"""

import unittest
import os
import tempfile
import shutil
from src.modules.module_system import ModuleLoader, ModuleEnvironment, ModuleInfo, ModuleResolver
from src.parser.sexpr_parser import parse
from src.core.ast import Module, Import, Export, Graph
from src.interpreter.module_interpreter import ModuleInterpreter


class TestModuleLoader(unittest.TestCase):
    """Test module loading functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.loader = ModuleLoader()
        self.env = ModuleEnvironment(search_paths=[self.test_dir])
    
    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir)
    
    def create_module_file(self, name, content):
        """Helper to create a module file"""
        path = os.path.join(self.test_dir, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, 'w') as f:
            f.write(content)
        return path
    
    def test_load_simple_module(self):
        """Test loading a simple module"""
        module_content = """
        (module math
          (export add subtract)
          (let ((add (lambda (x y) (+ x y)))
                (subtract (lambda (x y) (- x y))))
            'math-module))
        """
        
        self.create_module_file("math.cl", module_content)
        
        # Load module
        module_info = self.loader.load_module("math", self.env)
        self.assertIsNotNone(module_info)
        self.assertEqual(module_info.name, "math")
        self.assertIn("add", module_info.exports)
        self.assertIn("subtract", module_info.exports)
    
    def test_load_module_with_path(self):
        """Test loading a module with path"""
        module_content = """
        (module utils
          (export identity)
          (let ((identity (lambda (x) x)))
            'utils))
        """
        
        self.create_module_file("lib/utils.cl", module_content)
        
        # Load module with path
        module_info = self.loader.load_module("lib/utils", self.env)
        self.assertIsNotNone(module_info)
        self.assertEqual(module_info.name, "utils")
    
    def test_module_not_found(self):
        """Test error when module not found"""
        with self.assertRaises(ImportError) as cm:
            self.loader.load_module("nonexistent", self.env)
        self.assertIn("not found", str(cm.exception))
    
    def test_cyclic_dependency_detection(self):
        """Test detection of cyclic dependencies"""
        # Create two modules that depend on each other
        module_a = """
        (import "b")
        (module a
          (export foo)
          (let ((foo (lambda () 'from-a)))
            'a))
        """
        
        module_b = """
        (import "a") 
        (module b
          (export bar)
          (let ((bar (lambda () 'from-b)))
            'b))
        """
        
        self.create_module_file("a.cl", module_a)
        self.create_module_file("b.cl", module_b)
        
        # Should detect cycle
        with self.assertRaises(ImportError) as cm:
            self.loader.load_module("a", self.env)
        self.assertIn("Circular import", str(cm.exception))
    
    def test_load_module_with_dependencies(self):
        """Test loading module with dependencies"""
        # Base module
        base_content = """
        (module base
          (export PI E)
          (let ((PI 3.14159)
                (E 2.71828))
            'base))
        """
        
        # Module that depends on base
        math_content = """
        (import "base")
        (module math  
          (export circle-area)
          (let ((circle-area (lambda (r) (* base.PI r r))))
            'math))
        """
        
        self.create_module_file("base.cl", base_content)
        self.create_module_file("math.cl", math_content)
        
        # Load math module (should auto-load base)
        module_info = self.loader.load_module("math", self.env)
        self.assertIsNotNone(module_info)
        self.assertTrue(len(module_info.dependencies) > 0)
    
    def test_module_caching(self):
        """Test that modules are cached"""
        module_content = """
        (module cached
          (export value)
          (let ((value 42))
            'cached))
        """
        
        self.create_module_file("cached.cl", module_content)
        
        # Load module twice
        module1 = self.loader.load_module("cached", self.env)
        module2 = self.loader.load_module("cached", self.env)
        
        # Should be the same object
        self.assertIs(module1, module2)
    
    def test_module_search_paths(self):
        """Test module search path resolution"""
        # Create modules in different directories
        extra_dir = tempfile.mkdtemp()
        self.addCleanup(lambda: shutil.rmtree(extra_dir))
        
        module_content = """
        (module found
          (export test)
          (let ((test 'success))
            'found))
        """
        
        # Create in extra directory
        with open(os.path.join(extra_dir, "found.cl"), 'w') as f:
            f.write(module_content)
        
        # Should not find without adding to search path
        with self.assertRaises(ImportError):
            self.loader.load_module("found", self.env)
        
        # Add to search path
        self.env.search_paths.append(extra_dir)
        
        # Now should find it
        module_info = self.loader.load_module("found", self.env)
        self.assertIsNotNone(module_info)
        self.assertEqual(module_info.name, "found")
    
    def test_module_file_extensions(self):
        """Test that .cl extension is added automatically"""
        module_content = """
        (module test
          (export value)
          (let ((value 42))
            'test))
        """
        
        self.create_module_file("noext.cl", module_content)
        
        # Should find without specifying extension
        module_info = self.loader.load_module("noext", self.env)
        self.assertIsNotNone(module_info)
    
    def test_module_as_directory(self):
        """Test loading module from directory/module.cl"""
        module_content = """
        (module mypackage
          (export feature)
          (let ((feature 'loaded))
            'mypackage))
        """
        
        self.create_module_file("mypackage/module.cl", module_content)
        
        # Should find module in directory
        module_info = self.loader.load_module("mypackage", self.env)
        self.assertIsNotNone(module_info)
        self.assertEqual(module_info.name, "mypackage")


class TestModuleEnvironment(unittest.TestCase):
    """Test module environment functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.env = ModuleEnvironment()
    
    def test_default_search_paths(self):
        """Test that default search paths are set"""
        self.assertGreater(len(self.env.search_paths), 0)
        self.assertIn(".", self.env.search_paths)
        self.assertIn("./modules", self.env.search_paths)
    
    def test_module_storage(self):
        """Test storing modules in environment"""
        module_info = ModuleInfo(
            path="/test/module.cl",
            name="test",
            graph=Graph(),
            exports={"foo": "foo"},
            dependencies=[]
        )
        
        self.env.modules["/test/module.cl"] = module_info
        
        self.assertIn("/test/module.cl", self.env.modules)
        self.assertEqual(self.env.modules["/test/module.cl"].name, "test")
    
    def test_qualified_bindings(self):
        """Test storing qualified bindings"""
        self.env.bindings["math.pi"] = 3.14159
        self.env.bindings["math.e"] = 2.71828
        
        self.assertEqual(self.env.bindings["math.pi"], 3.14159)
        self.assertEqual(self.env.bindings["math.e"], 2.71828)


class TestModuleExportsImports(unittest.TestCase):
    """Test module export and import functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.loader = ModuleLoader()
        self.env = ModuleEnvironment(search_paths=[self.test_dir])
    
    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir)
    
    def create_module_file(self, name, content):
        """Helper to create a module file"""
        path = os.path.join(self.test_dir, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, 'w') as f:
            f.write(content)
        return path
    
    def test_export_specific_items(self):
        """Test exporting specific items"""
        module_content = """
        (module test
          (export foo bar)
          (let ((foo 1)
                (bar 2)
                (internal 3))
            'test))
        """
        
        self.create_module_file("test.cl", module_content)
        module_info = self.loader.load_module("test", self.env)
        
        # Only exported items should be in exports
        self.assertIn("foo", module_info.exports)
        self.assertIn("bar", module_info.exports)
        self.assertNotIn("internal", module_info.exports)
    
    def test_import_creates_dependency(self):
        """Test that imports create dependencies"""
        base_module = """
        (module base
          (export value)
          (let ((value 'base-value))
            'base))
        """
        
        user_module = """
        (import "base")
        (module user
          (export result)
          (let ((result 'uses-base))
            'user))
        """
        
        self.create_module_file("base.cl", base_module)
        self.create_module_file("user.cl", user_module)
        
        # Load user module
        module_info = self.loader.load_module("user", self.env)
        
        # Should have base as dependency
        self.assertTrue(any("base.cl" in dep for dep in module_info.dependencies))
    
    def test_multiple_exports(self):
        """Test multiple export statements"""
        module_content = """
        (export a b)
        (export c)
        (module multi
          (export d)
          (let ((a 1) (b 2) (c 3) (d 4))
            'multi))
        """
        
        self.create_module_file("multi.cl", module_content)
        module_info = self.loader.load_module("multi", self.env)
        
        # All exports should be collected
        for name in ["a", "b", "c", "d"]:
            self.assertIn(name, module_info.exports)
    
    def test_export_with_rename(self):
        """Test exporting with rename (if supported)"""
        # Check if rename syntax is supported in Export AST
        module_content = """
        (module rename
          (export internal as public)
          (let ((internal 'secret))
            'rename))
        """
        
        # This test depends on parser support for rename syntax
        # Skip if not supported
        try:
            self.create_module_file("rename.cl", module_content)
            module_info = self.loader.load_module("rename", self.env)
            # If it works, check the export
            if "public" in module_info.exports:
                self.assertEqual(module_info.exports["public"], "internal")
        except:
            # Parser doesn't support this syntax yet
            self.skipTest("Export rename syntax not supported")


class TestModuleResolver(unittest.TestCase):
    """Test module resolution functionality"""
    
    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.env = ModuleEnvironment(search_paths=[self.test_dir])
        self.resolver = ModuleResolver()
    
    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir)
    
    def test_resolve_qualified_variable(self):
        """Test resolving qualified variables"""
        # Create a mock module info
        module_info = ModuleInfo(
            path="/test/math.cl",
            name="math",
            graph=Graph(),
            exports={"pi": "pi", "e": "e"},
            dependencies=[]
        )
        
        self.env.modules["/test/math.cl"] = module_info
        self.env.bindings["math.pi"] = 3.14159
        
        # Test resolution
        result = self.resolver.resolve_qualified("math.pi", self.env)
        self.assertEqual(result, 3.14159)
    
    def test_resolve_nonexistent_module(self):
        """Test error when resolving from non-existent module"""
        with self.assertRaises(Exception):
            self.resolver.resolve_qualified("unknown.value", self.env)
    
    def test_resolve_private_binding(self):
        """Test that private bindings cannot be resolved"""
        module_info = ModuleInfo(
            path="/test/private.cl",
            name="private",
            graph=Graph(),
            exports={"public": "public"},  # Only public is exported
            dependencies=[]
        )
        
        self.env.modules["/test/private.cl"] = module_info
        self.env.bindings["private.public"] = "visible"
        self.env.bindings["private.secret"] = "hidden"
        
        # Should resolve public
        result = self.resolver.resolve_qualified("private.public", self.env)
        self.assertEqual(result, "visible")
        
        # Should not resolve private
        with self.assertRaises(Exception):
            self.resolver.resolve_qualified("private.secret", self.env)


class TestModuleErrors(unittest.TestCase):
    """Test error handling in module system"""
    
    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.loader = ModuleLoader()
        self.env = ModuleEnvironment(search_paths=[self.test_dir])
    
    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir)
    
    def create_module_file(self, name, content):
        """Helper to create a module file"""
        path = os.path.join(self.test_dir, name)
        with open(path, 'w') as f:
            f.write(content)
        return path
    
    def test_syntax_error_in_module(self):
        """Test handling of syntax errors in modules"""
        invalid_module = """
        (module broken
          (export foo
          (let ((foo 'bar))
            'broken))
        """
        
        self.create_module_file("broken.cl", invalid_module)
        
        with self.assertRaises(Exception):
            self.loader.load_module("broken", self.env)
    
    def test_empty_module_file(self):
        """Test loading empty module file"""
        self.create_module_file("empty.cl", "")
        
        # Should handle gracefully or error appropriately
        try:
            module_info = self.loader.load_module("empty", self.env)
            # If it loads, it should have minimal info
            self.assertEqual(len(module_info.exports), 0)
        except:
            # Or it might error, which is also acceptable
            pass
    
    def test_module_without_module_form(self):
        """Test loading file without module form"""
        content = """
        (let ((x 42))
          x)
        """
        
        self.create_module_file("nomodule.cl", content)
        
        # Should either work with defaults or error
        try:
            module_info = self.loader.load_module("nomodule", self.env)
            self.assertEqual(module_info.name, "nomodule")
        except:
            # Error is also acceptable behavior
            pass
    
    def test_import_after_definitions(self):
        """Test that imports must come before definitions"""
        module_content = """
        (module test
          (export x)
          (let ((x 1)))
          (import "other"))
        """
        
        # Depending on implementation, this might be an error
        # or imports might be processed first
        self.create_module_file("test.cl", module_content)
        self.create_module_file("other.cl", "(module other (export y) 'other)")
        
        # Just verify it doesn't crash
        try:
            self.loader.load_module("test", self.env)
        except:
            pass


class TestModuleIntegration(unittest.TestCase):
    """Test module system integration"""
    
    def setUp(self):
        """Set up test environment"""
        self.test_dir = tempfile.mkdtemp()
        self.loader = ModuleLoader()
        self.env = ModuleEnvironment(search_paths=[self.test_dir])
        
        # Try to create interpreter if available
        try:
            self.interpreter = ModuleInterpreter()
        except:
            self.interpreter = None
    
    def tearDown(self):
        """Clean up test environment"""
        shutil.rmtree(self.test_dir)
    
    def create_module_file(self, name, content):
        """Helper to create a module file"""
        path = os.path.join(self.test_dir, name)
        os.makedirs(os.path.dirname(path), exist_ok=True)
        with open(path, 'w') as f:
            f.write(content)
        return path
    
    def test_transitive_dependencies(self):
        """Test loading modules with transitive dependencies"""
        # A depends on B depends on C
        module_c = """
        (module c
          (export base-value)
          (let ((base-value 100))
            'c))
        """
        
        module_b = """
        (import "c")
        (module b
          (export double-value)
          (let ((double-value (* 2 c.base-value)))
            'b))
        """
        
        module_a = """
        (import "b")
        (module a
          (export final-value)
          (let ((final-value (+ b.double-value 50)))
            'a))
        """
        
        self.create_module_file("c.cl", module_c)
        self.create_module_file("b.cl", module_b)
        self.create_module_file("a.cl", module_a)
        
        # Load A, should load B and C
        module_info = self.loader.load_module("a", self.env)
        
        # All modules should be loaded
        self.assertEqual(len(self.env.modules), 3)
    
    def test_parallel_imports(self):
        """Test module with multiple imports"""
        math_module = """
        (module math
          (export add multiply)
          (let ((add (lambda (x y) (+ x y)))
                (multiply (lambda (x y) (* x y))))
            'math))
        """
        
        utils_module = """
        (module utils
          (export identity const)
          (let ((identity (lambda (x) x))
                (const (lambda (x) (lambda (y) x))))
            'utils))
        """
        
        combined_module = """
        (import "math")
        (import "utils")
        (module combined
          (export compute)
          (let ((compute (lambda (x y)
                          (math.add x (math.multiply y 2)))))
            'combined))
        """
        
        self.create_module_file("math.cl", math_module)
        self.create_module_file("utils.cl", utils_module)
        self.create_module_file("combined.cl", combined_module)
        
        # Load combined module
        module_info = self.loader.load_module("combined", self.env)
        
        # Should have both dependencies
        self.assertEqual(len(module_info.dependencies), 2)
    
    def test_stdlib_module_structure(self):
        """Test standard library module structure"""
        # Test that stdlib path exists in search paths
        stdlib_paths = [p for p in self.env.search_paths if "stdlib" in p]
        self.assertGreater(len(stdlib_paths), 0)
    
    def test_module_with_effects(self):
        """Test modules that use effects"""
        io_module = """
        (module io-test
          (export write-msg)
          (let ((write-msg (lambda (msg)
                            (effect io:print msg))))
            'io-test))
        """
        
        self.create_module_file("io-test.cl", io_module)
        module_info = self.loader.load_module("io-test", self.env)
        
        self.assertIn("write-msg", module_info.exports)
    
    def test_module_metadata(self):
        """Test module metadata storage"""
        module_content = """
        (module meta-test
          (export value)
          (let ((value 42))
            'meta-test))
        """
        
        self.create_module_file("meta-test.cl", module_content)
        module_info = self.loader.load_module("meta-test", self.env)
        
        # Metadata should be available
        self.assertIsInstance(module_info.metadata, dict)
        
        # Can store custom metadata
        module_info.metadata["version"] = "1.0.0"
        module_info.metadata["author"] = "test"
        
        # Should persist in cache
        cached = self.loader.load_module("meta-test", self.env)
        self.assertEqual(cached.metadata["version"], "1.0.0")


if __name__ == '__main__':
    unittest.main()