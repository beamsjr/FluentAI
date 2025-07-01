#!/usr/bin/env python3
"""
ClaudeLang Contract Verification Tool

This tool statically verifies contracts in ClaudeLang source files
and generates proof reports.
"""

import argparse
import sys
import os
from pathlib import Path

# Add parent directory to path for imports
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.parser.sexpr_parser import parse
from src.interpreter.interpreter import Interpreter
from src.contracts.proof_verification import (
    ContractProofVerifier,
    VerificationStrategy
)
from src.semantic.proof_generation import ProofGenerator


def find_contracts_and_functions(graph):
    """Extract contracts and their associated functions from AST"""
    contracts = []
    functions = {}
    
    # First pass: collect all contracts
    for node_id, node in graph.nodes.items():
        if hasattr(node, 'function_name') and node.function_name:
            contracts.append((node_id, node))
    
    # Second pass: find functions (lambdas with matching names)
    for node_id, node in graph.nodes.items():
        if node.node_type.name == "LAMBDA":
            # Try to find the binding name
            # This is simplified - would need more sophisticated analysis
            functions[node_id] = node
    
    return contracts, functions


def verify_file(filename, strategy=VerificationStrategy.PROOF_GENERATION, verbose=False):
    """Verify all contracts in a file"""
    print(f"\nVerifying contracts in: {filename}")
    print("=" * 60)
    
    # Read and parse file
    with open(filename, 'r') as f:
        source = f.read()
    
    try:
        graph = parse(source)
    except Exception as e:
        print(f"Error parsing file: {e}")
        return False
    
    # Find contracts and functions
    contracts, functions = find_contracts_and_functions(graph)
    
    if not contracts:
        print("No contracts found in file.")
        return True
    
    print(f"Found {len(contracts)} contracts to verify.")
    
    # Create verifier
    interpreter = Interpreter(enable_contracts=False)  # Don't need runtime checking
    verifier = ContractProofVerifier(interpreter)
    proof_gen = ProofGenerator()
    
    all_verified = True
    
    # Verify each contract
    for i, (contract_id, contract) in enumerate(contracts):
        print(f"\n{i+1}. Verifying contract: {contract.function_name}")
        print("-" * 40)
        
        # Find associated function
        # This is simplified - in practice would match by name/binding
        function_graph = None
        for func_id, func_node in functions.items():
            # Simple heuristic - use first lambda found
            # Real implementation would match names properly
            function_graph = graph
            break
        
        if not function_graph:
            print(f"   WARNING: Could not find function implementation")
            all_verified = False
            continue
        
        # Verify contract
        try:
            proof = verifier.verify_contract(contract, function_graph, strategy)
            
            # Display results
            print(f"   Preconditions:  {len(proof.precondition_proofs)} verified")
            print(f"   Postconditions: {len(proof.postcondition_proofs)} verified")
            print(f"   Invariants:     {len(proof.invariant_proofs)} verified")
            
            if contract.pure:
                purity_status = "✓" if proof.purity_proof and proof.purity_proof.verified else "✗"
                print(f"   Purity:         {purity_status}")
            
            print(f"   Overall:        {'✓ VERIFIED' if proof.is_verified else '✗ NOT VERIFIED'}")
            
            if verbose and not proof.is_verified:
                print("\n   Unverified components:")
                for p in proof.precondition_proofs:
                    if not p.verified:
                        print(f"     - Precondition: {p.name}")
                for p in proof.postcondition_proofs:
                    if not p.verified:
                        print(f"     - Postcondition: {p.name}")
                for p in proof.invariant_proofs:
                    if not p.verified:
                        print(f"     - Invariant: {p.name}")
                if proof.purity_proof and not proof.purity_proof.verified:
                    print(f"     - Purity check failed")
            
            if verbose and proof.postcondition_proofs:
                print("\n   Sample proof:")
                theorem = proof.postcondition_proofs[0]
                print(proof_gen.format_proof(theorem))
            
            if not proof.is_verified:
                all_verified = False
                
        except Exception as e:
            print(f"   ERROR: {e}")
            if verbose:
                import traceback
                traceback.print_exc()
            all_verified = False
    
    print("\n" + "=" * 60)
    if all_verified:
        print("✓ All contracts verified successfully!")
    else:
        print("✗ Some contracts could not be verified.")
    
    return all_verified


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Verify contracts in ClaudeLang source files"
    )
    parser.add_argument(
        "files", 
        nargs="+", 
        help="ClaudeLang source files to verify"
    )
    parser.add_argument(
        "--strategy", 
        choices=["proof", "symbolic", "bounded", "runtime"],
        default="proof",
        help="Verification strategy (default: proof)"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Show detailed verification output"
    )
    parser.add_argument(
        "--generate-proof",
        action="store_true",
        help="Generate formal proof documents"
    )
    
    args = parser.parse_args()
    
    # Map strategy names
    strategy_map = {
        "proof": VerificationStrategy.PROOF_GENERATION,
        "symbolic": VerificationStrategy.SYMBOLIC_EXECUTION,
        "bounded": VerificationStrategy.BOUNDED_CHECKING,
        "runtime": VerificationStrategy.RUNTIME_MONITORING
    }
    strategy = strategy_map[args.strategy]
    
    all_success = True
    
    # Verify each file
    for filename in args.files:
        if not Path(filename).exists():
            print(f"Error: File '{filename}' not found")
            all_success = False
            continue
        
        success = verify_file(filename, strategy, args.verbose)
        all_success = all_success and success
    
    # Exit with appropriate code
    sys.exit(0 if all_success else 1)


if __name__ == "__main__":
    main()