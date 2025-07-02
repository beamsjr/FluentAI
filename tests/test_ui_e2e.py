"""
End-to-end tests for ClaudeLang UI system

Tests the complete pipeline from ClaudeLang source to JavaScript output.
"""

import unittest
import os
import sys
import tempfile
import json

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.parser.sexpr_parser import parse
from src.compiler.ui_compiler import UICompiler, CompilerOptions, OutputFormat
from src.core.ast import UIComponent, UIControlFlow, NodeType


class TestUIEndToEnd(unittest.TestCase):
    """End-to-end tests for UI compilation"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = UICompiler(CompilerOptions(
            output_format=OutputFormat.VANILLA_JS,
            module_format="esm"
        ))
    
    def test_parse_simple_component(self):
        """Test parsing a simple UI component"""
        source = '''
(ui:component "Button"
  {:text (prop :string :required true)}
  (lambda (props)
    (dom:h "button" {} [(dom:text (get props "text"))])))
'''
        graph = parse(source)
        
        # Find the UIComponent node
        ui_component = None
        for node in graph.nodes.values():
            if isinstance(node, UIComponent):
                ui_component = node
                break
        
        self.assertIsNotNone(ui_component)
        self.assertEqual(ui_component.name, "Button")
        self.assertIn("text", ui_component.props)
        self.assertTrue(ui_component.props["text"].required)
        self.assertEqual(ui_component.props["text"].prop_type, "string")
    
    def test_compile_component(self):
        """Test compiling a component to JavaScript"""
        source = '''
(ui:component "Button"
  {:text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button" 
      (set {} "onClick" (get props "onClick"))
      [(dom:text (get props "text"))])))
'''
        js_code = self.compiler.compile(source)
        
        # Check that component definition is generated
        self.assertIn("ClaudeLang.defineComponent('Button'", js_code)
        self.assertIn('"text": {"type": "string", "required": true', js_code)
        self.assertIn('"onClick": {"type": "function"', js_code)
        
    def test_compile_component_with_state(self):
        """Test compiling a component with reactive state"""
        source = '''
(ui:component "Counter"
  {:initial (prop :number :default 0)}
  (lambda (props)
    (let ((count (effect state:reactive-ref (get props "initial"))))
      (dom:h "div" {}
        [(dom:h "span" {} 
          [(dom:text (to-string (effect state:reactive-get count)))])
         (dom:h "button"
           (set {} "onClick" 
             (lambda () 
               (effect state:reactive-update count 
                 (lambda (n) (+ n 1)))))
           [(dom:text "+")])]))))
'''
        js_code = self.compiler.compile(source)
        
        # Check state operations are compiled
        self.assertIn("ClaudeLang.performEffect('STATE', 'reactive-ref'", js_code)
        self.assertIn("ClaudeLang.performEffect('STATE', 'reactive-get'", js_code)
        self.assertIn("ClaudeLang.performEffect('STATE', 'reactive-update'", js_code)
    
    def test_compile_ui_control_flow(self):
        """Test compiling UI control flow constructs"""
        source = '''
(let ((show (effect state:reactive-ref true))
      (items (effect state:reactive-ref [1 2 3])))
  (dom:h "div" {}
    [(ui:if (effect state:reactive-get show)
       (dom:h "p" {} [(dom:text "Visible")])
       (dom:h "p" {} [(dom:text "Hidden")]))
     (ui:for (effect state:reactive-get items)
       (lambda (item)
         (dom:h "li" {} [(dom:text (to-string item))])))
     (ui:when (effect state:reactive-get show)
       (dom:h "footer" {} [(dom:text "Footer")]))]))
'''
        graph = parse(source)
        
        # Check that UI control flow nodes are created
        has_if = False
        has_for = False
        has_when = False
        
        for node in graph.nodes.values():
            if isinstance(node, UIControlFlow):
                if node.flow_type == "if":
                    has_if = True
                elif node.flow_type == "for":
                    has_for = True
                elif node.flow_type == "when":
                    has_when = True
        
        self.assertTrue(has_if, "ui:if not parsed")
        self.assertTrue(has_for, "ui:for not parsed")
        self.assertTrue(has_when, "ui:when not parsed")
        
        # Compile and check output
        js_code = self.compiler.compile(source)
        self.assertIn("? ", js_code)  # Ternary for ui:if
        self.assertIn(".map(", js_code)  # Map for ui:for
    
    def test_compile_component_instantiation(self):
        """Test compiling component instantiation"""
        source = '''
(ui:component "Button"
  {:text (prop :string :required true)}
  (lambda (props)
    (dom:h "button" {} [(dom:text (get props "text"))])))

(dom:h "div" {}
  [(ui:create "Button" (set {} "text" "Click me"))])
'''
        js_code = self.compiler.compile(source)
        
        # Check component instantiation
        self.assertIn('ClaudeLang.primitives[\'ui:create\']("Button"', js_code)
        self.assertIn('ClaudeLang.primitives[\'set\']({}, "text", "Click me")', js_code)
    
    def test_compile_nested_components(self):
        """Test compiling nested component definitions"""
        source = '''
(ui:component "Icon"
  {:name (prop :string :required true)}
  (lambda (props)
    (dom:h "i" (set {} "class" (get props "name")) [])))

(ui:component "IconButton"
  {:icon (prop :string :required true)
   :text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button" 
      (set {} "onClick" (get props "onClick"))
      [(ui:create "Icon" (set {} "name" (get props "icon")))
       (dom:h "span" {} [(dom:text (get props "text"))])])))

(ui:create "IconButton" 
  (set (set {} "icon" "star") 
       "text" "Favorite"
       "onClick" (lambda () (effect io:print "Clicked"))))
'''
        js_code = self.compiler.compile(source)
        
        # Check both components are defined
        self.assertIn("ClaudeLang.defineComponent('Icon'", js_code)
        self.assertIn("ClaudeLang.defineComponent('IconButton'", js_code)
        
        # Check nested instantiation
        self.assertEqual(len(self.compiler.component_registry), 2)
    
    def test_compile_with_all_ui_primitives(self):
        """Test that all UI primitives are available"""
        source = '''
(let ((className (ui:class "base" {"active" true}))
      (style (ui:style {"color" "red" "fontSize" "16px"})))
  (dom:h "div" 
    (set (set {} "class" className) "style" style)
    [(dom:text "Styled")]))
'''
        js_code = self.compiler.compile(source)
        
        # UI primitives should be compiled as runtime calls
        self.assertIn("ClaudeLang.primitives['ui:class']", js_code)
        self.assertIn("ClaudeLang.primitives['ui:style']", js_code)
    
    def test_parse_map_literals(self):
        """Test parsing map literals with curly braces"""
        source = '''
(let ((props {:text "Hello" :count 42 :enabled true}))
  (dom:h "div" props []))
'''
        graph = parse(source)
        
        # The map should be compiled to set operations
        js_code = self.compiler.compile(source)
        self.assertIn('ClaudeLang.primitives[\'set\']', js_code)
    
    def test_compile_event_handlers(self):
        """Test compiling event handlers"""
        source = '''
(dom:h "input"
  (set (set {} "type" "text")
       "onChange" (lambda (e) 
                   (effect io:print (get (get e "target") "value"))))
  [])
'''
        js_code = self.compiler.compile(source)
        
        # Check event handler compilation
        self.assertIn('"onChange"', js_code)
        self.assertIn('(e) =>', js_code)
        self.assertIn('ClaudeLang.performEffect(\'IO\', \'print\'', js_code)
    
    def test_full_app_compilation(self):
        """Test compiling a complete app with multiple components"""
        source = '''
; Todo item component
(ui:component "TodoItem"
  {:text (prop :string :required true)
   :completed (prop :bool :default false)
   :onToggle (prop :function)}
  (lambda (props)
    (dom:h "li" 
      (set {} "class" (ui:class "todo" {"done" (get props "completed")}))
      [(dom:h "input"
         (set (set {} "type" "checkbox")
              "checked" (get props "completed")
              "onChange" (get props "onToggle"))
         [])
       (dom:h "span" {} [(dom:text (get props "text"))])])))

; Todo list component
(ui:component "TodoList"
  {:items (prop :list :default [])}
  (lambda (props)
    (dom:h "ul" {}
      (ui:for (get props "items")
        (lambda (item)
          (ui:create "TodoItem"
            (set (set {} "text" (get item "text"))
                 "completed" (get item "completed")
                 "onToggle" (lambda () (effect io:print "Toggle")))))))))

; Main app
(let ((todos [{:text "Learn ClaudeLang" :completed false}
              {:text "Build UI app" :completed true}]))
  (effect dom:render
    (ui:create "TodoList" (set {} "items" todos))
    "#app"))
'''
        js_code = self.compiler.compile(source)
        
        # Verify all components are compiled
        self.assertIn("ClaudeLang.defineComponent('TodoItem'", js_code)
        self.assertIn("ClaudeLang.defineComponent('TodoList'", js_code)
        
        # Verify ui:for compilation
        self.assertIn(".map(", js_code)
        
        # Verify final render call
        self.assertIn("ClaudeLang.performEffect('DOM', 'render'", js_code)
    
    def test_error_handling(self):
        """Test error handling for invalid UI code"""
        # Missing required prop type
        with self.assertRaises(SyntaxError):
            parse('(ui:component "Bad" {:text (prop)} (lambda (p) p))')
        
        # Invalid prop definition
        with self.assertRaises(SyntaxError):
            parse('(ui:component "Bad" {:text "not-a-prop"} (lambda (p) p))')
        
        # Missing component name
        with self.assertRaises(SyntaxError):
            parse('(ui:component)')


class TestUICompilerOutput(unittest.TestCase):
    """Test the quality of generated JavaScript"""
    
    def setUp(self):
        """Set up test environment"""
        self.compiler = UICompiler(CompilerOptions(
            output_format=OutputFormat.VANILLA_JS,
            minify=False,
            source_maps=False
        ))
    
    def test_output_is_valid_javascript(self):
        """Test that output is syntactically valid JavaScript"""
        source = '''
(ui:component "Test"
  {:value (prop :number)}
  (lambda (props)
    (dom:h "div" {} [(dom:text (to-string (get props "value")))])))
'''
        js_code = self.compiler.compile(source)
        
        # Basic syntax checks
        self.assertTrue(js_code.count('(') == js_code.count(')'))
        self.assertTrue(js_code.count('{') == js_code.count('}'))
        self.assertTrue(js_code.count('[') == js_code.count(']'))
    
    def test_output_formats(self):
        """Test different output formats"""
        source = '(dom:h "div" {} [(dom:text "Hello")])'
        
        # Test ES module format
        compiler_esm = UICompiler(CompilerOptions(
            output_format=OutputFormat.VANILLA_JS,
            module_format="esm"
        ))
        js_esm = compiler_esm.compile(source)
        self.assertNotIn("module.exports", js_esm)
        
        # Test CommonJS format
        compiler_cjs = UICompiler(CompilerOptions(
            output_format=OutputFormat.VANILLA_JS,
            module_format="commonjs"
        ))
        js_cjs = compiler_cjs.compile(source)
        self.assertIn("module.exports", js_cjs)
        
        # Test UMD format
        compiler_umd = UICompiler(CompilerOptions(
            output_format=OutputFormat.VANILLA_JS,
            module_format="umd"
        ))
        js_umd = compiler_umd.compile(source)
        self.assertIn("typeof define === 'function'", js_umd)


if __name__ == '__main__':
    unittest.main()