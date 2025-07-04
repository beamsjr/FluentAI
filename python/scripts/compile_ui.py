#!/usr/bin/env python3
"""
Compile ClaudeLang UI files to JavaScript

Usage:
    python compile_ui.py input.cl output.js [--format=react|vue|vanilla]
"""

import sys
import os
import argparse

# Add parent directory to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from src.compiler.ui_compiler import UICompiler, CompilerOptions, OutputFormat


def main():
    parser = argparse.ArgumentParser(description='Compile ClaudeLang UI to JavaScript')
    parser.add_argument('input', help='Input .cl file')
    parser.add_argument('output', help='Output .js file')
    parser.add_argument('--format', choices=['vanilla', 'react', 'vue'], 
                       default='vanilla', help='Output format')
    parser.add_argument('--minify', action='store_true', help='Minify output')
    parser.add_argument('--module', choices=['esm', 'commonjs', 'umd'], 
                       default='esm', help='Module format')
    
    args = parser.parse_args()
    
    # Set up compiler options
    format_map = {
        'vanilla': OutputFormat.VANILLA_JS,
        'react': OutputFormat.REACT,
        'vue': OutputFormat.VUE
    }
    
    options = CompilerOptions(
        output_format=format_map[args.format],
        minify=args.minify,
        module_format=args.module
    )
    
    # Read input file
    with open(args.input, 'r') as f:
        source = f.read()
    
    # Compile
    compiler = UICompiler(options)
    try:
        output = compiler.compile(source)
        
        # Write output
        with open(args.output, 'w') as f:
            f.write(output)
        
        print(f"Successfully compiled {args.input} to {args.output}")
        print(f"Format: {args.format}, Module: {args.module}")
        
    except Exception as e:
        print(f"Compilation error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()