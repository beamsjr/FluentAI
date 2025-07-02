#!/usr/bin/env python3
"""Test UI component compilation"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.compiler.ui_compiler import UICompiler, CompilerOptions, OutputFormat

def test_component_compilation():
    source = """
(ui:component "Button"
  {:text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button" 
      (set {} "onClick" (get props "onClick"))
      [(dom:text (get props "text"))])))
"""
    
    # Create compiler
    options = CompilerOptions(
        output_format=OutputFormat.VANILLA_JS,
        module_format="esm"
    )
    
    compiler = UICompiler(options)
    
    try:
        # Compile the code
        js_code = compiler.compile(source)
        
        print("=== Compiled JavaScript ===")
        print(js_code)
        
        # Check component registry
        print("\n=== Component Registry ===")
        for name, code in compiler.component_registry.items():
            print(f"Component: {name}")
            print(code)
            
    except Exception as e:
        print(f"Compilation failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_component_compilation()