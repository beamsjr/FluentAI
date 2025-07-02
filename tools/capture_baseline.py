#!/usr/bin/env python3
"""
Capture comprehensive baseline performance metrics from current Python implementation.
These will be used to compare against Rust implementation progress.
"""

import json
import time
import statistics
import platform
import subprocess
import os
from datetime import datetime
from typing import Dict, List, Any, Callable

from src.parser.sexpr_parser import parse
from src.interpreter.interpreter import Interpreter
from src.vm.compiler import BytecodeCompiler
from src.vm.vm import VM
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer


class BaselineCapture:
    """Capture detailed performance baselines for all components"""
    
    def __init__(self):
        self.interpreter = Interpreter()
        self.compiler = BytecodeCompiler()
        self.vm = VM()
        self.optimizer = AdvancedGraphOptimizer()
        self.results = {}
        
    def measure_operation(self, name: str, operation: Callable, 
                         iterations: int = 1000, warmup: int = 100) -> Dict[str, float]:
        """Measure performance of a single operation"""
        # Warmup
        for _ in range(warmup):
            operation()
        
        # Measure
        times = []
        for _ in range(iterations):
            start = time.perf_counter()
            operation()
            elapsed = time.perf_counter() - start
            times.append(elapsed)
        
        return {
            'mean_ns': statistics.mean(times) * 1e9,
            'median_ns': statistics.median(times) * 1e9,
            'min_ns': min(times) * 1e9,
            'max_ns': max(times) * 1e9,
            'std_dev_ns': statistics.stdev(times) * 1e9 if len(times) > 1 else 0,
        }
    
    def capture_parser_baselines(self):
        """Capture parser performance baselines"""
        print("Capturing parser baselines...")
        
        test_cases = [
            ("simple_expr", "(+ 1 2)"),
            ("nested_expr", "(* (+ 1 2) (- 4 3))"),
            ("function_def", "(lambda (x y) (+ x y))"),
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
        ]
        
        parser_results = {}
        for name, code in test_cases:
            metrics = self.measure_operation(
                f"parse_{name}",
                lambda: parse(code),
                iterations=5000
            )
            parser_results[name] = metrics
            print(f"  {name}: {metrics['mean_ns']/1000:.1f}µs")
        
        self.results['parser'] = parser_results
    
    def capture_interpreter_baselines(self):
        """Capture interpreter performance baselines"""
        print("\nCapturing interpreter baselines...")
        
        test_cases = [
            ("arithmetic", "(+ (* 2 3) (* 4 5))"),
            ("fibonacci_10", """
                (let ((fib (lambda (n)
                             (if (<= n 1)
                                 n
                                 (+ (fib (- n 1)) (fib (- n 2)))))))
                  (fib 10))
            """),
            ("list_map", """
                (let ((map (lambda (f lst)
                            (if (empty? lst)
                                []
                                (cons (f (head lst))
                                      (map f (tail lst))))))
                      (double (lambda (x) (* x 2))))
                  (map double [1 2 3 4 5]))
            """),
            ("factorial_15", """
                (let ((fact (lambda (n)
                              (if (<= n 1)
                                  1
                                  (* n (fact (- n 1)))))))
                  (fact 15))
            """),
        ]
        
        interpreter_results = {}
        for name, code in test_cases:
            graph = parse(code)
            metrics = self.measure_operation(
                f"interpret_{name}",
                lambda: self.interpreter.interpret(graph),
                iterations=1000 if 'fibonacci' not in name else 100
            )
            interpreter_results[name] = metrics
            print(f"  {name}: {metrics['mean_ns']/1000:.1f}µs")
        
        self.results['interpreter'] = interpreter_results
    
    def capture_vm_baselines(self):
        """Capture VM performance baselines"""
        print("\nCapturing VM baselines...")
        
        test_cases = [
            ("arithmetic", "(+ (* 2 3) (* 4 5))"),
            ("function_call", """
                (let ((square (lambda (x) (* x x))))
                  (square 7))
            """),
            ("list_sum", """
                (let ((sum (lambda (lst)
                            (if (empty? lst)
                                0
                                (+ (head lst) (sum (tail lst)))))))
                  (sum [1 2 3 4 5 6 7 8 9 10]))
            """),
        ]
        
        vm_results = {}
        for name, code in test_cases:
            graph = parse(code)
            bytecode = self.compiler.compile(graph)
            metrics = self.measure_operation(
                f"vm_{name}",
                lambda: self.vm.execute(bytecode),
                iterations=2000
            )
            vm_results[name] = metrics
            print(f"  {name}: {metrics['mean_ns']/1000:.1f}µs")
        
        self.results['vm'] = vm_results
    
    def capture_optimization_baselines(self):
        """Capture optimization performance baselines"""
        print("\nCapturing optimization baselines...")
        
        test_cases = [
            ("constant_folding", "(+ 1 (+ 2 (+ 3 4)))"),
            ("dead_code", "(let ((x 10) (y 20)) x)"),
            ("inline_simple", """
                (let ((add1 (lambda (x) (+ x 1))))
                  (add1 (add1 (add1 5))))
            """),
        ]
        
        optimization_results = {}
        for name, code in test_cases:
            graph = parse(code)
            metrics = self.measure_operation(
                f"optimize_{name}",
                lambda: self.optimizer.optimize(graph),
                iterations=1000
            )
            optimization_results[name] = metrics
            print(f"  {name}: {metrics['mean_ns']/1000:.1f}µs")
        
        self.results['optimization'] = optimization_results
    
    def capture_end_to_end_baselines(self):
        """Capture end-to-end performance baselines"""
        print("\nCapturing end-to-end baselines...")
        
        test_cases = [
            ("hello_world", '(print "Hello, World!")'),
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
        
        e2e_results = {}
        for name, code in test_cases:
            # Measure full pipeline
            def full_pipeline():
                graph = parse(code)
                optimized = self.optimizer.optimize(graph)
                bytecode = self.compiler.compile(optimized)
                return self.vm.execute(bytecode)
            
            metrics = self.measure_operation(
                f"e2e_{name}",
                full_pipeline,
                iterations=500
            )
            e2e_results[name] = metrics
            print(f"  {name}: {metrics['mean_ns']/1000:.1f}µs")
        
        self.results['end_to_end'] = e2e_results
    
    def get_environment_info(self) -> Dict[str, Any]:
        """Get environment information"""
        try:
            commit_hash = subprocess.check_output(
                ['git', 'rev-parse', 'HEAD'],
                text=True
            ).strip()
        except:
            commit_hash = None
        
        return {
            'python_version': platform.python_version(),
            'os': f"{platform.system()} {platform.release()}",
            'cpu': platform.processor() or platform.machine(),
            'commit_hash': commit_hash,
            'timestamp': datetime.utcnow().isoformat(),
        }
    
    def save_baseline(self, filename: str = "baseline_performance.json"):
        """Save baseline results to file"""
        baseline_data = {
            'environment': self.get_environment_info(),
            'results': self.results,
            'summary': self._generate_summary(),
        }
        
        os.makedirs('performance_baselines', exist_ok=True)
        filepath = os.path.join('performance_baselines', filename)
        
        with open(filepath, 'w') as f:
            json.dump(baseline_data, f, indent=2)
        
        print(f"\nBaseline saved to: {filepath}")
    
    def _generate_summary(self) -> Dict[str, Any]:
        """Generate summary statistics"""
        summary = {}
        
        for category, results in self.results.items():
            category_summary = {
                'fastest_operation': min(results.items(), key=lambda x: x[1]['mean_ns'])[0],
                'slowest_operation': max(results.items(), key=lambda x: x[1]['mean_ns'])[0],
                'mean_time_us': statistics.mean(r['mean_ns'] for r in results.values()) / 1000,
            }
            summary[category] = category_summary
        
        return summary
    
    def print_summary(self):
        """Print performance summary"""
        print("\n" + "="*60)
        print("BASELINE PERFORMANCE SUMMARY")
        print("="*60)
        
        for category, results in self.results.items():
            print(f"\n{category.upper()}:")
            for operation, metrics in results.items():
                print(f"  {operation:<30} {metrics['mean_ns']/1000:>10.1f} µs")
        
        print("\nThese baselines will be used to measure Rust implementation improvements.")


def main():
    """Capture comprehensive performance baselines"""
    print("ClaudeLang Performance Baseline Capture")
    print("="*40)
    
    capture = BaselineCapture()
    
    # Run all baseline captures
    capture.capture_parser_baselines()
    capture.capture_interpreter_baselines()
    capture.capture_vm_baselines()
    capture.capture_optimization_baselines()
    capture.capture_end_to_end_baselines()
    
    # Save results
    capture.save_baseline()
    
    # Print summary
    capture.print_summary()


if __name__ == "__main__":
    main()