"""
Tests for complete contract verification system

Tests SMT solving, symbolic execution, and bounded checking.
"""

import unittest
from unittest.mock import Mock, patch

from src.core.ast import Contract, Graph, NodeType
from src.contracts.proof_verification import (
    ContractProofVerifier, VerificationStrategy, ContractProof
)
from src.contracts.smt_solver import SMTSolver, SMTEncoder, SMTContext
from src.contracts.symbolic_execution import (
    SymbolicExecutor, SymbolicState, SymbolicValue, SymbolicValueType
)
from src.interpreter.interpreter import Interpreter
from tests.test_utils import TestNode


class TestSMTSolver(unittest.TestCase):
    """Test SMT solver integration"""
    
    def setUp(self):
        self.solver = SMTSolver()
        self.encoder = SMTEncoder()
    
    def test_encode_literal(self):
        """Test encoding literal values"""
        # Create graph with integer literal
        graph = Graph()
        node = TestNode.literal(42)
        graph.add_node(node)
        graph.root_id = node.id
        
        context = SMTContext()
        result = self.encoder.encode_graph(graph, context)
        
        # Result will be None if Z3 is not available
        if context.solver is not None:
            self.assertIsNotNone(result)
        else:
            self.assertIsNone(result)
    
    def test_encode_arithmetic(self):
        """Test encoding arithmetic expressions"""
        # Create graph: x + 5
        graph = Graph()
        
        # Variable x
        var_node = TestNode.variable('x')
        graph.add_node(var_node)
        
        # Literal 5
        lit_node = TestNode.literal(5)
        graph.add_node(lit_node)
        
        # Addition
        add_node = TestNode.binary_op('+', var_node.id, lit_node.id)
        graph.add_node(add_node)
        graph.root_id = add_node.id
        
        context = SMTContext()
        result = self.encoder.encode_graph(graph, context)
        
        # Variable should be created only if Z3 is available
        if context.solver is not None:
            self.assertIn('x', context.variables)
        else:
            self.assertEqual(len(context.variables), 0)
    
    @patch('src.contracts.smt_solver.HAS_Z3', False)
    def test_no_z3_available(self):
        """Test behavior when Z3 is not available"""
        solver = SMTSolver()
        graph = Graph()
        
        verified, reason = solver.prove_implication([], graph)
        self.assertFalse(verified)
        self.assertEqual(reason, "Z3 not available")


class TestSymbolicExecution(unittest.TestCase):
    """Test symbolic execution engine"""
    
    def setUp(self):
        self.executor = SymbolicExecutor()
    
    def test_execute_literal(self):
        """Test symbolic execution of literal"""
        graph = Graph()
        node = TestNode.literal(42)
        graph.add_node(node)
        graph.root_id = node.id
        
        states = self.executor.execute(graph)
        self.assertEqual(len(states), 1)
        
        # Result should be concrete
        state = states[0]
        result = self.executor._execute_node(
            graph.nodes[graph.root_id], graph, state, []
        )
        self.assertTrue(result.is_concrete())
        self.assertEqual(result.concrete_value, 42)
    
    def test_execute_variable(self):
        """Test symbolic execution of variable"""
        graph = Graph()
        node = TestNode.variable('x')
        graph.add_node(node)
        graph.root_id = node.id
        
        # Create initial state with symbolic value
        initial_state = SymbolicState()
        sym_val = SymbolicValue(
            value_type=SymbolicValueType.SYMBOLIC,
            symbol_name='x'
        )
        initial_state.bind('x', sym_val)
        
        states = self.executor.execute(graph, initial_state)
        self.assertEqual(len(states), 1)
    
    def test_execute_if_concrete(self):
        """Test symbolic execution of if with concrete condition"""
        graph = Graph()
        
        # Condition: true
        cond_node = TestNode.literal(True)
        graph.add_node(cond_node)
        
        # Then branch: 1
        then_node = TestNode.literal(1)
        graph.add_node(then_node)
        
        # Else branch: 2
        else_node = TestNode.literal(2)
        graph.add_node(else_node)
        
        # If node
        if_node = TestNode.if_node(cond_node.id, then_node.id, else_node.id)
        graph.add_node(if_node)
        graph.root_id = if_node.id
        
        states = self.executor.execute(graph)
        self.assertEqual(len(states), 1)
        
        # Should take then branch
        result = self.executor._execute_node(
            graph.nodes[graph.root_id], graph, states[0], []
        )
        self.assertTrue(result.is_concrete())
        self.assertEqual(result.concrete_value, 1)
    
    def test_execute_if_symbolic(self):
        """Test symbolic execution of if with symbolic condition"""
        graph = Graph()
        
        # Condition: symbolic variable
        cond_node = TestNode.variable('cond')
        graph.add_node(cond_node)
        
        # Then branch: 1
        then_node = TestNode.literal(1)
        graph.add_node(then_node)
        
        # Else branch: 2
        else_node = TestNode.literal(2)
        graph.add_node(else_node)
        
        # If node
        if_node = TestNode.if_node(cond_node.id, then_node.id, else_node.id)
        graph.add_node(if_node)
        graph.root_id = if_node.id
        
        # Execute with symbolic condition
        initial_state = SymbolicState()
        sym_cond = SymbolicValue(
            value_type=SymbolicValueType.SYMBOLIC,
            symbol_name='cond'
        )
        initial_state.bind('cond', sym_cond)
        
        final_states = []
        result = self.executor._execute_node(
            graph.nodes[graph.root_id], graph, initial_state, final_states
        )
        
        # Should create symbolic result (path split)
        self.assertTrue(result.is_symbolic())
    
    def test_effect_tracking(self):
        """Test effect tracking during symbolic execution"""
        graph = Graph()
        
        # Create print effect node
        func_node = TestNode.variable('print')
        graph.add_node(func_node)
        
        arg_node = TestNode.literal("Hello")
        graph.add_node(arg_node)
        
        app_node = TestNode.application(func_node.id, [arg_node.id])
        graph.add_node(app_node)
        graph.root_id = app_node.id
        
        initial_state = SymbolicState()
        states = self.executor.execute(graph, initial_state)
        
        # Should track print effect
        self.assertEqual(len(states), 1)
        self.assertEqual(len(states[0].effects), 1)
        effect_type, args = states[0].effects[0]
        self.assertEqual(effect_type, 'print')


class TestContractProofVerifier(unittest.TestCase):
    """Test complete contract proof verification"""
    
    def setUp(self):
        self.interpreter = Mock(spec=Interpreter)
        self.verifier = ContractProofVerifier(self.interpreter)
    
    def test_verify_with_smt_strategy(self):
        """Test verification using SMT solving"""
        # Create simple contract
        contract = Contract(
            function_name="add",
            preconditions=["pre1"],
            postconditions=["post1"],
            invariants=[],
            pure=True
        )
        
        # Create function graph
        graph = Graph()
        
        # Mock SMT solver behavior
        with patch('src.contracts.smt_solver.SMTSolver') as mock_smt:
            mock_solver = Mock()
            mock_solver.prove_implication.return_value = (True, "Proven")
            mock_solver.check_satisfiability.return_value = (True, {'x': 5})
            mock_smt.return_value = mock_solver
            
            proof = self.verifier.verify_contract(
                contract, graph, VerificationStrategy.SMT_SOLVING
            )
            
            self.assertIsInstance(proof, ContractProof)
            self.assertEqual(len(proof.postcondition_proofs), 1)
            self.assertTrue(proof.postcondition_proofs[0].verified)
    
    def test_verify_with_symbolic_execution(self):
        """Test verification using symbolic execution"""
        contract = Contract(
            function_name="pure_func",
            preconditions=[],
            postconditions=["post1"],
            invariants=[],
            pure=True
        )
        
        # Create function graph with lambda
        graph = Graph()
        lambda_node = TestNode.lambda_node(['x'])
        graph.add_node(lambda_node)
        graph.root_id = lambda_node.id
        
        proof = self.verifier.verify_contract(
            contract, graph, VerificationStrategy.SYMBOLIC_EXECUTION
        )
        
        self.assertIsInstance(proof, ContractProof)
        # Should verify purity (no effects)
        self.assertIsNotNone(proof.purity_proof)
        self.assertTrue(proof.purity_proof.verified)
    
    def test_create_subgraph(self):
        """Test subgraph creation"""
        # Create parent graph
        parent = Graph()
        
        # Add nodes
        node1 = TestNode.literal(1)
        parent.add_node(node1)
        
        node2 = TestNode.literal(2)
        parent.add_node(node2)
        
        node3 = TestNode.binary_op('+', node1.id, node2.id)
        parent.add_node(node3)
        
        # Create subgraph rooted at node3
        subgraph = self.verifier._create_subgraph(parent, node3.id)
        
        self.assertEqual(subgraph.root_id, node3.id)
        self.assertEqual(len(subgraph.nodes), 3)  # Should include all reachable nodes


class TestContractIntegration(unittest.TestCase):
    """Integration tests for contract system"""
    
    def test_factorial_contract_verification(self):
        """Test verification of factorial function contract"""
        # Create factorial contract
        contract = Contract(
            function_name="factorial",
            preconditions=["n_ge_0"],  # n >= 0
            postconditions=["result_positive"],  # result > 0
            invariants=[],
            pure=True
        )
        
        # Create precondition graph: n >= 0
        pre_graph = Graph()
        n_node = TestNode.variable('n')
        pre_graph.add_node(n_node)
        zero_node = TestNode.literal(0)
        pre_graph.add_node(zero_node)
        
        ge_node = TestNode.binary_op('>=', n_node.id, zero_node.id)
        pre_graph.add_node(ge_node)
        pre_graph.root_id = ge_node.id
        
        # Create postcondition graph: result > 0
        post_graph = Graph()
        result_node = TestNode.variable('result')
        post_graph.add_node(result_node)
        zero_node2 = TestNode.literal(0)
        post_graph.add_node(zero_node2)
        
        gt_node = TestNode.binary_op('>', result_node.id, zero_node2.id)
        post_graph.add_node(gt_node)
        post_graph.root_id = gt_node.id
        
        # Create function graph (simplified)
        func_graph = Graph()
        func_graph.nodes["n_ge_0"] = pre_graph.nodes[pre_graph.root_id]
        func_graph.nodes["result_positive"] = post_graph.nodes[post_graph.root_id]
        
        # Verify with different strategies
        interpreter = Mock(spec=Interpreter)
        verifier = ContractProofVerifier(interpreter)
        
        # Runtime monitoring (default)
        proof = verifier.verify_contract(contract, func_graph)
        self.assertIsInstance(proof, ContractProof)
        
        # Proof generation
        proof = verifier.verify_contract(
            contract, func_graph, VerificationStrategy.PROOF_GENERATION
        )
        self.assertIsInstance(proof, ContractProof)


if __name__ == "__main__":
    unittest.main()