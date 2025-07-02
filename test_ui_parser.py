#!/usr/bin/env python3
"""Test UI parsing"""

import sys
import os
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

from src.parser.sexpr_parser import parse

def test_ui_component():
    source = """
(ui:component "Button"
  {:text (prop :string :required true)
   :onClick (prop :function)}
  (lambda (props)
    (dom:h "button" 
      (set {} "onClick" (get props "onClick"))
      [(dom:text (get props "text"))])))
"""
    
    try:
        graph = parse(source)
        print(f"Successfully parsed! Root node: {graph.root_id}")
        
        # Check if we have a UIComponent node
        ui_component = None
        for node_id, node in graph.nodes.items():
            print(f"Node {node_id}: {type(node).__name__}")
            if hasattr(node, 'name') and hasattr(node, 'props'):
                ui_component = node
                break
        
        if ui_component:
            print(f"\nFound UIComponent: {ui_component.name}")
            print(f"Props: {ui_component.props}")
        else:
            print("\nNo UIComponent found")
            
    except Exception as e:
        print(f"Parse error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    test_ui_component()