"""
Tests for Contract Proof Verification
"""

import unittest
from src.interpreter.interpreter import Interpreter
from src.parser.sexpr_parser import parse
from src.core.ast import Contract
from src.contracts.proof_verification import (
    ContractProofVerifier, 
    VerificationStrategy,
    verify_contract_statically
)


class TestContractProofVerification(unittest.TestCase):
    """Test static contract verification through proof generation"""
    
    def setUp(self):
        """Set up test environment"""
        self.interpreter = Interpreter(enable_contracts=True)
        self.verifier = ContractProofVerifier(self.interpreter)
    
    def test_simple_postcondition_proof(self):
        """Test proving a simple postcondition"""
        # Parse contract
        contract_code = """
        (spec:contract increment
          :ensures [(= result (+ x 1))])
        """
        contract_graph = parse(contract_code)
        contract_node = None
        for node in contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                contract_node = node
                break
        
        # Parse function
        function_code = """
        (lambda (x) (+ x 1))
        """
        function_graph = parse(function_code)
        
        # Verify contract
        proof = self.verifier.verify_contract(
            contract_node, 
            function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        # Check proof structure
        self.assertEqual(len(proof.postcondition_proofs), 1)
        self.assertIsNotNone(proof.postcondition_proofs[0])
        
        # Print proof for inspection
        print("\nPostcondition proof:")
        print(self.verifier.format_contract_proof(proof))
    
    def test_precondition_implication(self):
        """Test proving postcondition given precondition"""
        # Parse contract
        contract_code = """
        (spec:contract safe_divide
          :requires [(not= y 0)]
          :ensures [(= result (/ x y))])
        """
        contract_graph = parse(contract_code)
        contract_node = None
        for node in contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                contract_node = node
                break
        
        # Parse function
        function_code = """
        (lambda (x y) (/ x y))
        """
        function_graph = parse(function_code)
        
        # Verify contract
        proof = self.verifier.verify_contract(
            contract_node,
            function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        # Check that preconditions are considered
        self.assertEqual(len(proof.precondition_proofs), 1)
        self.assertEqual(len(proof.postcondition_proofs), 1)
        
        print("\nDivision contract proof:")
        print(self.verifier.format_contract_proof(proof))
    
    def test_purity_verification(self):
        """Test verifying function purity"""
        # Pure function
        pure_contract_code = """
        (spec:contract add
          :pure true
          :ensures [(= result (+ x y))])
        """
        pure_contract_graph = parse(pure_contract_code)
        pure_contract = None
        for node in pure_contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                pure_contract = node
                break
        
        pure_function_code = """
        (lambda (x y) (+ x y))
        """
        pure_function_graph = parse(pure_function_code)
        
        # Verify pure function
        pure_proof = self.verifier.verify_contract(
            pure_contract,
            pure_function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        self.assertIsNotNone(pure_proof.purity_proof)
        self.assertTrue(pure_proof.purity_proof.verified)
        
        # Impure function
        impure_contract_code = """
        (spec:contract print_and_add
          :pure true
          :ensures [(= result (+ x y))])
        """
        impure_contract_graph = parse(impure_contract_code)
        impure_contract = None
        for node in impure_contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                impure_contract = node
                break
        
        impure_function_code = """
        (lambda (x y) 
          (do
            (effect io print x)
            (+ x y)))
        """
        impure_function_graph = parse(impure_function_code)
        
        # Verify impure function
        impure_proof = self.verifier.verify_contract(
            impure_contract,
            impure_function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        self.assertIsNotNone(impure_proof.purity_proof)
        self.assertFalse(impure_proof.purity_proof.verified)
        
        print("\nPurity verification:")
        print("Pure function:", self.verifier.format_contract_proof(pure_proof))
        print("Impure function:", self.verifier.format_contract_proof(impure_proof))
    
    def test_invariant_proof(self):
        """Test proving loop/recursion invariants"""
        # Recursive function with invariant
        contract_code = """
        (spec:contract factorial
          :requires [(>= n 0)]
          :ensures [(>= result 1)]
          :invariant [(>= n 0)])
        """
        contract_graph = parse(contract_code)
        contract_node = None
        for node in contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                contract_node = node
                break
        
        # Recursive factorial
        function_code = """
        (lambda (n)
          (if (= n 0)
              1
              (* n (factorial (- n 1)))))
        """
        function_graph = parse(function_code)
        
        # Verify contract
        proof = self.verifier.verify_contract(
            contract_node,
            function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        self.assertEqual(len(proof.invariant_proofs), 1)
        self.assertIsNotNone(proof.invariant_proofs[0])
        
        print("\nInvariant proof:")
        print(self.verifier.format_contract_proof(proof))
    
    def test_bounded_verification_strategy(self):
        """Test bounded verification strategy"""
        contract_code = """
        (spec:contract abs
          :ensures [(>= result 0)])
        """
        contract_graph = parse(contract_code)
        contract_node = None
        for node in contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                contract_node = node
                break
        
        function_code = """
        (lambda (x) (if (< x 0) (- 0 x) x))
        """
        function_graph = parse(function_code)
        
        # Try bounded checking
        bounded_proof = self.verifier.verify_contract(
            contract_node,
            function_graph,
            VerificationStrategy.BOUNDED_CHECKING
        )
        
        # Should fall back to runtime for now
        self.assertFalse(bounded_proof.is_verified)
        
        print("\nBounded verification:")
        print(self.verifier.format_contract_proof(bounded_proof))
    
    def test_proof_completeness(self):
        """Test checking if proof is complete"""
        # Complete contract
        contract_code = """
        (spec:contract complete_function
          :requires [(> x 0)]
          :ensures [(> result 0)]
          :pure true)
        """
        contract_graph = parse(contract_code)
        contract_node = None
        for node in contract_graph.nodes.values():
            if hasattr(node, 'function_name'):
                contract_node = node
                break
        
        function_code = """
        (lambda (x) (* x 2))
        """
        function_graph = parse(function_code)
        
        proof = self.verifier.verify_contract(
            contract_node,
            function_graph,
            VerificationStrategy.PROOF_GENERATION
        )
        
        # Check completeness
        self.assertTrue(proof.is_complete)
        print("\nComplete proof:")
        print(self.verifier.format_contract_proof(proof))


if __name__ == "__main__":
    unittest.main()