#!/usr/bin/env python3
"""Test script to compile the counter example"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.compiler.ui_compiler import UICompiler, CompilerOptions, OutputFormat

def main():
    # Read the counter example
    with open('examples/component_demo.ai', 'r') as f:
        source = f.read()
    
    # Create compiler with vanilla JS output
    options = CompilerOptions(
        output_format=OutputFormat.VANILLA_JS,
        module_format="iife"  # Immediately invoked function expression
    )
    
    compiler = UICompiler(options)
    
    try:
        # Compile the code
        js_code = compiler.compile(source)
        
        # Write to output file
        with open('examples/counter_compiled.js', 'w') as f:
            f.write(js_code)
        
        print("Successfully compiled counter example to examples/counter_compiled.js")
        
    except Exception as e:
        print(f"Compilation failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    main()