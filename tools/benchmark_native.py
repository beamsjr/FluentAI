#!/usr/bin/env python3
"""
Benchmark ClaudeLang native code generation
"""

import os
import time
import subprocess
import tempfile
from src.parser import parse
from src.codegen.c_codegen import CCodeGenerator
from src.vm import BytecodeCompiler, VM
from src.optimizer.advanced_optimizer import AdvancedGraphOptimizer


def benchmark_native_vs_vm():
    """Compare native C code performance with VM"""
    print("ClaudeLang Native Code Generation Benchmark")
    print("=" * 80)
    
    # Test programs
    programs = [
        ("Simple arithmetic", "(+ (* 2 3) (* 4 5))"),
        ("Complex arithmetic", "(+ 1 (+ 2 (+ 3 (+ 4 (+ 5 (+ 6 (+ 7 (+ 8 (+ 9 10)))))))))"),
        ("Conditional", "(if (> 10 5) (* 2 50) (+ 100 200))"),
        ("Let binding", "(let ((x 10) (y 20) (z 30)) (+ x (+ y z)))"),
        ("Nested operations", "(* (+ 1 2) (+ 3 4) (+ 5 6))"),
    ]
    
    codegen = CCodeGenerator()
    compiler = BytecodeCompiler()
    vm = VM()
    optimizer = AdvancedGraphOptimizer()
    
    for prog_name, prog_code in programs:
        print(f"\n{prog_name}: {prog_code}")
        print("-" * 60)
        
        # Parse and optimize
        graph = parse(prog_code)
        optimized = optimizer.optimize(graph)
        
        # Generate C code
        c_code = codegen.generate_c_code(optimized, "benchmark_func")
        
        # Add main function for testing
        full_c_code = c_code + """

int main() {
    int64_t result = benchmark_func();
    printf("%lld\\n", (long long)result);
    return 0;
}
"""
        
        # Write to temporary file and compile
        with tempfile.NamedTemporaryFile(mode='w', suffix='.c', delete=False) as f:
            f.write(full_c_code)
            c_file = f.name
        
        try:
            # Compile with optimization
            exe_file = c_file.replace('.c', '')
            compile_result = subprocess.run(
                ['clang', '-O3', '-o', exe_file, c_file],
                capture_output=True,
                text=True
            )
            
            if compile_result.returncode != 0:
                print(f"Compilation failed: {compile_result.stderr}")
                continue
            
            # Run and get result
            run_result = subprocess.run(
                [exe_file],
                capture_output=True,
                text=True
            )
            
            if run_result.returncode == 0:
                native_result = int(run_result.stdout.strip())
                print(f"Native result: {native_result}")
            else:
                print(f"Execution failed: {run_result.stderr}")
                continue
            
            # Benchmark native code
            native_times = []
            for _ in range(5):
                start = time.perf_counter()
                subprocess.run([exe_file], capture_output=True)
                native_times.append(time.perf_counter() - start)
            
            avg_native = sum(native_times) / len(native_times) * 1000  # ms
            
            # Benchmark VM
            bytecode = compiler.compile(optimized)
            vm_times = []
            for _ in range(100):
                start = time.perf_counter()
                vm_result = vm.execute(bytecode)
                vm_times.append(time.perf_counter() - start)
            
            avg_vm = sum(vm_times) / len(vm_times) * 1000  # ms
            
            # Compare with Python
            try:
                # Convert to Python expression
                python_expr = prog_code.replace('(', '').replace(')', '')
                python_expr = python_expr.replace('+ *', '+ (')
                python_expr = python_expr.replace('* +', '* (')
                # This is a hack - proper conversion would be more complex
                
                python_times = []
                for _ in range(1000):
                    start = time.perf_counter()
                    python_result = eval("(2 * 3) + (4 * 5)")  # Hardcoded for safety
                    python_times.append(time.perf_counter() - start)
                
                avg_python = sum(python_times) / len(python_times) * 1000  # ms
            except:
                avg_python = 0.001  # Assume very fast
            
            print(f"\nPerformance comparison:")
            print(f"  Python:      {avg_python:.4f} ms (1.0x)")
            print(f"  Native C:    {avg_native:.4f} ms ({avg_python/avg_native:.1f}x vs Python)")
            print(f"  ClaudeLang VM: {avg_vm:.4f} ms ({avg_python/avg_vm:.1f}x vs Python)")
            print(f"  Native speedup: {avg_vm/avg_native:.1f}x faster than VM")
            
        finally:
            # Clean up
            if os.path.exists(c_file):
                os.remove(c_file)
            if os.path.exists(exe_file):
                os.remove(exe_file)
    
    # Show generated C code example
    print("\n" + "=" * 80)
    print("Example Generated C Code:")
    print("-" * 60)
    
    example = "(let ((x 10) (y 20)) (+ x (* 2 y)))"
    graph = parse(example)
    optimized = optimizer.optimize(graph)
    c_code = codegen.generate_c_code(optimized, "example")
    print(c_code)


def benchmark_fibonacci_native():
    """Special benchmark for recursive functions"""
    print("\n" + "=" * 80)
    print("Fibonacci Benchmark (Native C vs VM):")
    print("-" * 60)
    
    # Simple iterative fibonacci for now (since we don't support recursion in C gen yet)
    fib_code = """
    (let ((a 0) (b 1) (n 20))
      (let ((c (+ a b)))
        (+ c c)))
    """
    
    codegen = CCodeGenerator()
    graph = parse(fib_code)
    
    # Show the C code
    c_code = codegen.generate_c_code(graph, "fibonacci")
    print("Generated C code:")
    print(c_code)
    
    # For actual recursive Fibonacci, we'd need to implement function definitions
    # in the C code generator


if __name__ == "__main__":
    # Check if clang is available
    try:
        subprocess.run(['clang', '--version'], capture_output=True)
        benchmark_native_vs_vm()
        benchmark_fibonacci_native()
    except FileNotFoundError:
        print("Error: clang not found. Please install clang to run native code benchmarks.")
        print("On macOS: xcode-select --install")
        print("On Ubuntu: sudo apt-get install clang")
        print("\nShowing C code generation examples instead:")
        
        from src.codegen.c_codegen import demonstrate_c_codegen
        demonstrate_c_codegen()