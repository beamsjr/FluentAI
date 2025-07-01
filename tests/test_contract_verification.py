"""
Tests for Runtime Contract Verification
"""

import unittest
from src.interpreter.interpreter import Interpreter
from src.contracts.verification import ContractViolation, ContractViolationType


class TestContractVerification(unittest.TestCase):
    """Test runtime contract verification"""
    
    def setUp(self):
        """Set up test environment"""
        self.interpreter = Interpreter(enable_contracts=True)
    
    def test_precondition_success(self):
        """Test successful precondition check"""
        code = """
        (spec:contract divide
          :requires [(not= y 0)]
          :ensures [(= result (/ x y))])
        
        (let ((divide (lambda (x y) (/ x y))))
          (divide 10 2))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 5)
    
    def test_precondition_failure(self):
        """Test precondition violation"""
        code = """
        (spec:contract divide
          :requires [(not= y 0)]
          :ensures [(= result (/ x y))])
        
        (let ((divide (lambda (x y) (/ x y))))
          (divide 10 0))
        """
        
        with self.assertRaises(ContractViolation) as cm:
            self.interpreter.eval(code)
        
        self.assertEqual(cm.exception.violation_type, ContractViolationType.PRECONDITION)
        self.assertIn("divide", cm.exception.function_name)
    
    def test_postcondition_success(self):
        """Test successful postcondition check"""
        code = """
        (spec:contract abs
          :ensures [(>= result 0)])
        
        (let ((abs (lambda (x) (if (< x 0) (- x) x))))
          (abs -5))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 5)
    
    def test_postcondition_failure(self):
        """Test postcondition violation"""
        code = """
        (spec:contract always-positive
          :ensures [(> result 0)])
        
        (let ((always-positive (lambda (x) 0)))
          ; Bug: returns 0 instead of positive
          (always-positive 5))
        """
        
        with self.assertRaises(ContractViolation) as cm:
            self.interpreter.eval(code)
        
        self.assertEqual(cm.exception.violation_type, ContractViolationType.POSTCONDITION)
    
    def test_type_predicates(self):
        """Test type predicates in contracts"""
        code = """
        (spec:contract add-numbers
          :requires [(number? x) (number? y)]
          :ensures [(number? result)])
        
        (let ((add-numbers (lambda (x y) (+ x y))))
          (add-numbers 3 4))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 7)
    
    def test_list_predicates(self):
        """Test list predicates in contracts"""
        code = """
        (spec:contract sum-list
          :requires [(list? lst) (all number? lst)]
          :ensures [(number? result)])
        
        (let ((sum-list (lambda (lst) (fold + 0 lst))))
          (sum-list [1 2 3 4 5]))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 15)
    
    def test_pure_function_contract(self):
        """Test pure function contract"""
        code = """
        (spec:contract pure-add
          :pure true)
        
        (let ((pure-add (lambda (x y) (+ x y))))
          (pure-add 2 3))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 5)
    
    def test_impure_function_violation(self):
        """Test pure function using effects"""
        code = """
        (spec:contract supposed-to-be-pure
          :pure true)
        
        (let ((supposed-to-be-pure 
               (lambda (x)
                 (do
                   (io:print x)  ; Side effect!
                   x))))
          (supposed-to-be-pure 42))
        """
        
        # This should raise a purity violation
        # (Implementation depends on effect tracking)
    
    def test_contract_with_implies(self):
        """Test contract with logical implication"""
        code = """
        (spec:contract safe-divide
          :requires [(number? x) (number? y)]
          :ensures [(implies (not= y 0) (= result (/ x y)))
                    (implies (= y 0) (= result 0))])
        
        (let ((safe-divide (lambda (x y) (if (= y 0) 0 (/ x y)))))
          (list (safe-divide 10 2) (safe-divide 10 0)))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, [5, 0])
    
    def test_disable_contracts(self):
        """Test disabling contract verification"""
        self.interpreter.contract_verifier.disable()
        
        code = """
        (spec:contract divide
          :requires [(not= y 0)])
        
        (let ((divide (lambda (x y) (/ x y))))
          (divide 10 0))  ; Would violate if enabled
        """
        
        # Should not raise exception when disabled
        try:
            result = self.interpreter.eval(code)
            # Will get division by zero error instead
        except ContractViolation:
            self.fail("Contract should not be checked when disabled")
        except:
            pass  # Expected arithmetic error
    
    def test_nested_function_contracts(self):
        """Test contracts on nested function calls"""
        code = """
        (spec:contract positive
          :ensures [(> result 0)])
        
        (spec:contract double-positive
          :requires [(> x 0)]
          :ensures [(> result x)])
        
        (let ((positive (lambda (x) (if (> x 0) x 1)))
              (double-positive (lambda (x) (* 2 (positive x)))))
          (double-positive 5))
        """
        
        result = self.interpreter.eval(code)
        self.assertEqual(result.data, 10)


if __name__ == "__main__":
    unittest.main()