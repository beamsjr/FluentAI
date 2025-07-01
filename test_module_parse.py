#!/usr/bin/env python3
from src.parser.sexpr_parser import parse

code = '''
(module math
  (export add multiply)
  (define add (lambda (x y) (+ x y)))
  (define multiply (lambda (x y) (* x y))))
'''

try:
    ast = parse(code)
    print("Parse successful!")
    print(f"Root node type: {ast.nodes[ast.root_id].node_type}")
except Exception as e:
    print(f"Parse error: {e}")
    import traceback
    traceback.print_exc()