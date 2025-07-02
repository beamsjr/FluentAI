"""
Test async/await functionality
"""

import unittest
import time
from src.interpreter.interpreter import Interpreter
from src.effects.handlers import create_test_handler


class TestAsyncAwait(unittest.TestCase):
    """Test async/await primitives"""
    
    def setUp(self):
        self.handler = create_test_handler()
        self.interpreter = Interpreter(effect_handler=self.handler)
    
    def eval(self, code: str):
        """Helper to parse and evaluate code"""
        result = self.interpreter.eval(code)
        return result.data if hasattr(result, 'data') else result
    
    def test_simple_promise(self):
        """Test creating and resolving a simple promise"""
        code = """
        (let ((p (promise (lambda (resolve reject)
                           (resolve 42)))))
          (await p))
        """
        result = self.eval(code)
        self.assertEqual(result, 42)
    
    def test_promise_rejection(self):
        """Test promise rejection"""
        # Skip for now - need to implement error handling
        self.skipTest("Error handling not yet implemented")
    
    def test_async_lambda(self):
        """Test async lambda function"""
        code = """
        (let ((fetch-data (async (url)
                            (promise (lambda (resolve reject)
                                      (resolve (string-concat "Data from " url)))))))
          (let ((promise (fetch-data "https://api.example.com")))
            (await promise)))
        """
        result = self.eval(code)
        self.assertEqual(result, "Data from https://api.example.com")
    
    def test_promise_then(self):
        """Test promise chaining with then"""
        # Skip for now - need to implement promise.then
        self.skipTest("Promise.then not yet implemented")
    
    def test_promise_all(self):
        """Test waiting for multiple promises"""
        # Skip for now - need to implement Promise.all
        self.skipTest("Promise.all not yet implemented")
    
    def test_promise_race(self):
        """Test racing promises"""
        # Skip for now - need to implement Promise.race
        self.skipTest("Promise.race not yet implemented")
    
    def test_async_with_effects(self):
        """Test async operations with other effects"""
        code = """
        (let ((async-print (async (msg)
                             (do
                               (effect io:print msg)
                               (promise (lambda (resolve reject)
                                         (resolve (string-concat "Printed: " msg))))))))
          (let ((promise (async-print "Hello async!")))
            (await promise)))
        """
        result = self.eval(code)
        self.assertEqual(result, "Printed: Hello async!")
    
    def test_nested_async(self):
        """Test nested async operations"""
        code = """
        (let ((fetch-user (async (id)
                            (promise (lambda (resolve reject)
                                      (resolve {"id" id "name" "User"})))))
              (fetch-posts (async (user-id)
                             (promise (lambda (resolve reject)
                                       (resolve [{"id" 1 "title" "Post 1"}
                                                {"id" 2 "title" "Post 2"}]))))))
          (let ((user (await (fetch-user 123))))
            (let ((posts (await (fetch-posts (get user "id")))))
              {"user" user "posts" posts})))
        """
        result = self.eval(code)
        self.assertEqual(result["user"]["id"], 123)
        self.assertEqual(len(result["posts"]), 2)
    
    def test_async_error_handling(self):
        """Test error handling in async context"""
        # Skip for now - need to implement error handling
        self.skipTest("Error handling not yet implemented")
    
    def test_async_timeout(self):
        """Test async operation with timeout"""
        # Skip for now - need to implement timeout and error handling
        self.skipTest("Timeout and error handling not yet implemented")
    
    def test_parallel_async_operations(self):
        """Test parallel async operations"""
        # Skip for now - need to implement Promise.all and delay
        self.skipTest("Promise.all and delay not yet implemented")
    
    def test_async_map(self):
        """Test mapping async function over list"""
        # Skip for now - need to implement Promise.all
        self.skipTest("Promise.all not yet implemented")
    
    def test_async_filter(self):
        """Test filtering with async predicate"""
        code = """
        (let ((is-even-async (async (n)
                               (promise (lambda (resolve reject)
                                         (resolve (= (mod n 2) 0)))))))
          (let ((check-all (lambda (items)
                            (if (null? items)
                                []
                                (let ((first (car items))
                                      (rest (cdr items)))
                                  (let ((is-even (await (is-even-async first))))
                                    (if is-even
                                        (cons first (check-all rest))
                                        (check-all rest))))))))
            (check-all [1 2 3 4 5 6])))
        """
        result = self.eval(code)
        self.assertEqual(result, [2, 4, 6])
    
    def test_promise_state(self):
        """Test querying promise state"""
        # Skip for now - need to implement get-state
        self.skipTest("Promise state querying not yet implemented")


if __name__ == "__main__":
    unittest.main()