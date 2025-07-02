"""
Test UI functionality in ClaudeLang
"""

import unittest
from src.interpreter.interpreter import Interpreter
from src.effects.handlers import create_test_handler
from src.effects.dom_handler import VNode


class TestUI(unittest.TestCase):
    """Test UI primitives and DOM effects"""
    
    def setUp(self):
        self.handler = create_test_handler()
        self.interpreter = Interpreter(effect_handler=self.handler)
    
    def eval(self, code: str):
        """Helper to parse and evaluate code"""
        result = self.interpreter.eval(code)
        return result.data if hasattr(result, 'data') else result
    
    def test_create_vnode(self):
        """Test creating virtual DOM nodes"""
        code = """
        (dom:h "div" {"class" "container"} 
          [(dom:h "h1" {} [(dom:text "Hello")])])
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, VNode)
        self.assertEqual(result.tag, "div")
        self.assertEqual(result.props["class"], "container")
        self.assertEqual(len(result.children), 1)
        
        child = result.children[0]
        self.assertIsInstance(child, VNode)
        self.assertEqual(child.tag, "h1")
        self.assertEqual(child.children[0], "Hello")
    
    def test_text_node(self):
        """Test creating text nodes"""
        code = """(dom:text "Hello, World!")"""
        result = self.eval(code)
        self.assertEqual(result, "Hello, World!")
    
    def test_fragment(self):
        """Test creating fragments"""
        code = """
        (dom:fragment 
          [(dom:h "h1" {} [(dom:text "Title")])
           (dom:h "p" {} [(dom:text "Content")])])
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, VNode)
        self.assertEqual(result.tag, "fragment")
        self.assertEqual(len(result.children), 2)
    
    def test_conditional_rendering(self):
        """Test conditional rendering with ui:if"""
        code = """
        (let ((show true))
          (ui:if show
            (dom:h "div" {} [(dom:text "Visible")])
            (dom:h "div" {} [(dom:text "Hidden")])))
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, VNode)
        self.assertEqual(result.children[0], "Visible")
        
        # Test false condition
        code_false = """
        (let ((show false))
          (ui:if show
            (dom:h "div" {} [(dom:text "Visible")])
            (dom:h "div" {} [(dom:text "Hidden")])))
        """
        result_false = self.eval(code_false)
        self.assertEqual(result_false.children[0], "Hidden")
    
    def test_list_rendering(self):
        """Test list rendering with ui:for"""
        code = """
        (ui:for [1 2 3]
          (lambda (item idx)
            (dom:h "li" {} [(dom:text (to-string item))])))
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, list)
        self.assertEqual(len(result), 3)
        self.assertEqual(result[0].tag, "li")
        self.assertEqual(result[0].children[0], "1")
    
    def test_style_object(self):
        """Test creating style objects"""
        code = """
        (ui:style {"color" "red" 
                   "fontSize" "16px"
                   "marginTop" "10px"})
        """
        result = self.eval(code)
        self.assertIn("color: red", result)
        self.assertIn("font-size: 16px", result)
        self.assertIn("margin-top: 10px", result)
    
    def test_class_string(self):
        """Test creating class strings"""
        code = """
        (ui:class "base" 
                  ["extra" "classes"]
                  {"active" true
                   "disabled" false})
        """
        result = self.eval(code)
        self.assertIn("base", result)
        self.assertIn("extra", result)
        self.assertIn("classes", result)
        self.assertIn("active", result)
        self.assertNotIn("disabled", result)
    
    def test_dom_effects(self):
        """Test DOM effect operations"""
        code = """
        (let ((elem (effect dom:create-element "div" {"id" "test"} [])))
          (do
            (effect dom:set-attribute elem "class" "test-class")
            (effect dom:set-text elem "Test content")
            elem))
        """
        result = self.eval(code)
        
        # Should return element ID
        self.assertIsInstance(result, str)
        self.assertTrue(result.startswith("vnode_"))
    
    def test_reactive_ref(self):
        """Test reactive references"""
        code = """
        (let ((count (ui:ref 0)))
          {"type" (get count "type")
           "value" (get count "value")})
        """
        result = self.eval(code)
        
        self.assertEqual(result["type"], "ref")
        self.assertEqual(result["value"], 0)
    
    def test_computed_value(self):
        """Test computed values"""
        code = """
        (ui:computed 
          (lambda () 
            (+ 1 2)))
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, dict)
        self.assertEqual(result["type"], "computed")
        self.assertIsNotNone(result["fn"])
    
    def test_event_handler(self):
        """Test event handler creation"""
        code = """
        (ui:on "click" 
          (lambda (event) 
            (print "Clicked!")))
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, dict)
        self.assertEqual(result["type"], "event")
        self.assertEqual(result["name"], "click")
        self.assertIsNotNone(result["handler"])
    
    def test_portal(self):
        """Test portal rendering"""
        code = """
        (ui:portal
          (dom:h "div" {"class" "modal"} 
            [(dom:text "Modal content")])
          "#modal-root")
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, VNode)
        self.assertEqual(result.tag, "portal")
        self.assertEqual(result.props["target"], "#modal-root")
        self.assertEqual(len(result.children), 1)
    
    def test_nested_components(self):
        """Test nested component rendering"""
        code = """
        (let ((Button (lambda (props)
                       (dom:h "button" 
                         {"class" (get props "class")}
                         [(dom:text (get props "label"))]))))
          (dom:h "div" {}
            [(Button {"label" "Click me" "class" "primary"})
             (Button {"label" "Cancel" "class" "secondary"})]))
        """
        result = self.eval(code)
        
        self.assertIsInstance(result, VNode)
        self.assertEqual(result.tag, "div")
        self.assertEqual(len(result.children), 2)
        
        # Check button components
        btn1 = result.children[0]
        self.assertEqual(btn1.tag, "button")
        self.assertEqual(btn1.props["class"], "primary")
        self.assertEqual(btn1.children[0], "Click me")


class TestReactiveSystem(unittest.TestCase):
    """Test reactive state system"""
    
    def setUp(self):
        self.handler = create_test_handler()
        self.interpreter = Interpreter(effect_handler=self.handler)
    
    def eval(self, code: str):
        """Helper to parse and evaluate code"""
        result = self.interpreter.eval(code)
        return result.data if hasattr(result, 'data') else result
    
    def test_reactive_ref(self):
        """Test reactive reference creation and updates"""
        code = """
        (let ((count (effect reactive:ref 0)))
          (do
            (effect reactive:set count 42)
            (effect reactive:get count)))
        """
        result = self.eval(code)
        self.assertEqual(result, 42)
    
    def test_reactive_update(self):
        """Test reactive update with function"""
        code = """
        (let ((count (effect reactive:ref 10)))
          (do
            (effect reactive:update count (lambda (n) (+ n 5)))
            (effect reactive:get count)))
        """
        result = self.eval(code)
        self.assertEqual(result, 15)
    
    def test_computed_dependency(self):
        """Test computed values with dependencies"""
        code = """
        (let ((a (effect reactive:ref 2))
              (b (effect reactive:ref 3)))
          (let ((sum (effect reactive:computed
                      (lambda ()
                        (+ (effect reactive:get a)
                           (effect reactive:get b))))))
            (do
              (effect reactive:set a 5)
              (effect reactive:get-computed sum))))
        """
        result = self.eval(code)
        self.assertEqual(result, 8)
    
    def test_batch_updates(self):
        """Test batched reactive updates"""
        code = """
        (let ((a (effect reactive:ref 1))
              (b (effect reactive:ref 2))
              (c (effect reactive:ref 3)))
          (effect reactive:batch
            (lambda ()
              (do
                (effect reactive:set a 10)
                (effect reactive:set b 20)
                (effect reactive:set c 30)
                (+ (effect reactive:get a)
                   (effect reactive:get b)
                   (effect reactive:get c))))))
        """
        result = self.eval(code)
        self.assertEqual(result, 60)


if __name__ == "__main__":
    unittest.main()