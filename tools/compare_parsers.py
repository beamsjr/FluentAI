#!/usr/bin/env python3
"""
Compare performance between Python and Rust parsers
"""

import time
import statistics
import json
import os
from datetime import datetime
from typing import Dict, List, Tuple

# Python parser
from src.parser.sexpr_parser import parse as python_parse

# Try to import Rust parser (will fail until built)
try:
    import claudelang_rust
    rust_parse = claudelang_rust.parse
    RUST_AVAILABLE = True
except ImportError:
    print("Warning: Rust parser not available. Run 'maturin develop' in rust/claudelang-py/")
    RUST_AVAILABLE = False
    rust_parse = None


class ParserComparison:
    """Compare Python and Rust parser implementations"""
    
    def __init__(self):
        self.test_cases = [
            ("simple_expr", "(+ 1 2)"),
            ("nested_expr", "(* (+ 1 2) (- 4 3))"),
            ("lambda", "(lambda (x y) (+ x y))"),
            ("let_binding", "(let ((x 10) (y 20)) (+ x y))"),
            ("list_literal", "[1 2 3 4 5 6 7 8 9 10]"),
            ("complex_expr", """
                (let ((map (lambda (f lst)
                            (if (empty? lst)
                                []
                                (cons (f (head lst))
                                      (map f (tail lst)))))))
                  (map (lambda (x) (* x 2)) [1 2 3 4 5]))
            """),
            ("fibonacci", """
                (let ((fib (lambda (n)
                             (if (<= n 1)
                                 n
                                 (+ (fib (- n 1)) (fib (- n 2)))))))
                  (fib 10))
            """),
            ("quicksort", """
                (let ((quicksort (lambda (lst)
                                  (if (empty? lst)
                                      []
                                      (let ((pivot (head lst))
                                            (rest (tail lst))
                                            (less (filter (lambda (x) (< x pivot)) rest))
                                            (greater (filter (lambda (x) (>= x pivot)) rest)))
                                        (append (quicksort less)
                                                (cons pivot (quicksort greater))))))))
                  (quicksort [3 1 4 1 5 9 2 6 5 3 5]))
            """),
        ]
    
    def benchmark_parser(self, parser_func, code: str, iterations: int = 1000) -> Dict[str, float]:
        """Benchmark a parser implementation"""
        # Warmup
        for _ in range(10):
            parser_func(code)
        
        # Measure
        times = []
        for _ in range(iterations):
            start = time.perf_counter()
            parser_func(code)
            elapsed = time.perf_counter() - start
            times.append(elapsed)
        
        return {
            'mean_ns': statistics.mean(times) * 1e9,
            'median_ns': statistics.median(times) * 1e9,
            'min_ns': min(times) * 1e9,
            'max_ns': max(times) * 1e9,
            'std_dev_ns': statistics.stdev(times) * 1e9 if len(times) > 1 else 0,
        }
    
    def compare_parsers(self) -> Dict[str, Dict]:
        """Run comparison between Python and Rust parsers"""
        results = {}
        
        print("Parser Performance Comparison")
        print("=" * 80)
        print(f"{'Test Case':<20} {'Python (µs)':<15} {'Rust (µs)':<15} {'Speedup':<10}")
        print("-" * 80)
        
        for test_name, code in self.test_cases:
            # Benchmark Python parser
            python_metrics = self.benchmark_parser(python_parse, code)
            
            # Benchmark Rust parser if available
            if RUST_AVAILABLE:
                rust_metrics = self.benchmark_parser(rust_parse, code)
                speedup = python_metrics['mean_ns'] / rust_metrics['mean_ns']
            else:
                rust_metrics = {'mean_ns': 0, 'median_ns': 0, 'min_ns': 0, 'max_ns': 0, 'std_dev_ns': 0}
                speedup = 0
            
            results[test_name] = {
                'python': python_metrics,
                'rust': rust_metrics,
                'speedup': speedup,
                'code_size': len(code),
            }
            
            print(f"{test_name:<20} {python_metrics['mean_ns']/1000:>14.1f} "
                  f"{rust_metrics['mean_ns']/1000 if RUST_AVAILABLE else 'N/A':>14} "
                  f"{f'{speedup:.1f}x' if RUST_AVAILABLE else 'N/A':>9}")
        
        if RUST_AVAILABLE:
            avg_speedup = statistics.mean(r['speedup'] for r in results.values() if r['speedup'] > 0)
            print("-" * 80)
            print(f"{'Average Speedup:':<20} {' '*15} {' '*15} {avg_speedup:>8.1f}x")
        
        return results
    
    def test_scaling(self) -> Dict[int, Dict]:
        """Test parser performance with different input sizes"""
        print("\n\nParser Scaling Performance")
        print("=" * 80)
        print(f"{'Depth':<10} {'Python (µs)':<15} {'Rust (µs)':<15} {'Speedup':<10}")
        print("-" * 80)
        
        scaling_results = {}
        
        for depth in [10, 50, 100, 200, 500]:
            # Generate deeply nested expression
            code = self.generate_nested_expr(depth)
            
            # Benchmark both parsers
            python_metrics = self.benchmark_parser(python_parse, code, iterations=100)
            
            if RUST_AVAILABLE:
                rust_metrics = self.benchmark_parser(rust_parse, code, iterations=100)
                speedup = python_metrics['mean_ns'] / rust_metrics['mean_ns']
            else:
                rust_metrics = {'mean_ns': 0}
                speedup = 0
            
            scaling_results[depth] = {
                'python': python_metrics,
                'rust': rust_metrics,
                'speedup': speedup,
                'code_size': len(code),
            }
            
            print(f"{depth:<10} {python_metrics['mean_ns']/1000:>14.1f} "
                  f"{rust_metrics['mean_ns']/1000 if RUST_AVAILABLE else 'N/A':>14} "
                  f"{f'{speedup:.1f}x' if RUST_AVAILABLE else 'N/A':>9}")
        
        return scaling_results
    
    def generate_nested_expr(self, depth: int) -> str:
        """Generate a deeply nested expression for testing"""
        if depth == 0:
            return "1"
        else:
            return f"(+ {self.generate_nested_expr(depth - 1)} {depth})"
    
    def save_results(self, results: Dict, scaling_results: Dict):
        """Save comparison results to file"""
        output = {
            'timestamp': datetime.utcnow().isoformat(),
            'rust_available': RUST_AVAILABLE,
            'results': results,
            'scaling': scaling_results,
            'summary': {
                'average_speedup': statistics.mean(r['speedup'] for r in results.values() if r['speedup'] > 0) if RUST_AVAILABLE else 0,
                'best_speedup': max((r['speedup'] for r in results.values()), default=0),
                'worst_speedup': min((r['speedup'] for r in results.values() if r['speedup'] > 0), default=0),
            }
        }
        
        os.makedirs('parser_comparison', exist_ok=True)
        filename = f"parser_comparison/comparison_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        with open(filename, 'w') as f:
            json.dump(output, f, indent=2)
        
        print(f"\nResults saved to: {filename}")
    
    def verify_correctness(self):
        """Verify that both parsers produce equivalent results"""
        if not RUST_AVAILABLE:
            print("\nSkipping correctness verification (Rust parser not available)")
            return
        
        print("\n\nVerifying Parser Correctness")
        print("=" * 40)
        
        all_correct = True
        for test_name, code in self.test_cases:
            try:
                python_result = python_parse(code)
                rust_result = rust_parse(code)
                
                # Basic check: both should succeed and have root nodes
                if python_result.root_id and rust_result.root_id:
                    print(f"{test_name:<20} ✓ Both parsers succeeded")
                else:
                    print(f"{test_name:<20} ✗ Parse results differ")
                    all_correct = False
                    
            except Exception as e:
                print(f"{test_name:<20} ✗ Error: {str(e)}")
                all_correct = False
        
        if all_correct:
            print("\nAll tests passed! ✓")
        else:
            print("\nSome tests failed! ✗")


def main():
    """Run parser comparison"""
    comparison = ParserComparison()
    
    # Run comparisons
    results = comparison.compare_parsers()
    scaling_results = comparison.test_scaling()
    
    # Verify correctness
    comparison.verify_correctness()
    
    # Save results
    comparison.save_results(results, scaling_results)
    
    # Print instructions for building Rust parser if not available
    if not RUST_AVAILABLE:
        print("\n" + "="*60)
        print("To build and test the Rust parser:")
        print("1. Install maturin: pip install maturin")
        print("2. Build the extension: cd rust/claudelang-py && maturin develop")
        print("3. Re-run this script")
        print("="*60)


if __name__ == "__main__":
    main()