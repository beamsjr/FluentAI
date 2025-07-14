#!/usr/bin/env python3
"""
Quick and dirty Continuum to FluentAI transpiler
This demonstrates how the lowering phase would work
"""

import re
import sys

def transpile_continuum(code):
    """Transform Continuum UI syntax into executable FluentAI code"""
    
    output = []
    state_fields = {}
    
    # Extract state fields
    state_pattern = r'public state_field (\w+):\s*(\w+)\s*=\s*(.+)'
    for match in re.finditer(state_pattern, code):
        name, type_, initial = match.groups()
        state_fields[name] = (type_, initial)
    
    # Generate state management code
    if state_fields:
        output.append("// Generated state management")
        for name, (type_, initial) in state_fields.items():
            output.append(f'let {name} = {{"value": {initial}}};')
        output.append("")
    
    # Extract surface definitions
    surface_pattern = r'public surface (\w+)\s*\{((?:[^{}]|\{[^}]*\})*)\}'
    surface_match = re.search(surface_pattern, code, re.DOTALL)
    
    if surface_match:
        surface_name = surface_match.group(1)
        surface_body = surface_match.group(2)
        
        output.append("// Generated UI rendering")
        output.append(f'let render_{surface_name} = () => {{')
        
        # Extract elements
        element_pattern = r'element (\w+)\s*\{([^}]+)\}'
        for elem_match in re.finditer(element_pattern, surface_body):
            elem_type = elem_match.group(1)
            elem_props = elem_match.group(2)
            
            # Extract properties
            content_match = re.search(r'content:\s*"([^"]+)"', elem_props)
            content_fstring = re.search(r'content:\s*f"([^"]+)"', elem_props)
            onclick_match = re.search(r'on_click:\s*disturb\s+(\w+)\((.*?)\)', elem_props)
            
            if content_fstring:
                # Handle f-strings by converting to string concatenation
                content = content_fstring.group(1)
                # Simple replacement of {var} with " + var.value + "
                content = re.sub(r'\{(\w+)\}', r'" + \\1.value + "', content)
                content = f'"{content}"'
            elif content_match:
                content = f'"{content_match.group(1)}"'
            else:
                content = '""'
            
            output.append(f'    // {elem_type} element')
            
            if elem_type == "text":
                output.append(f'    perform IO.print({content});')
            elif elem_type == "button" and onclick_match:
                field = onclick_match.group(1)
                expr = onclick_match.group(2)
                output.append(f'    perform IO.print({content} + " (clickable)");')
                output.append(f'    // on_click would update: {field}.value = {expr};')
        
        output.append('};')
        output.append('')
        output.append(f'// Render the UI')
        output.append(f'render_{surface_name}()')
    
    return '\n'.join(output)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r') as f:
            continuum_code = f.read()
    else:
        continuum_code = sys.stdin.read()
    
    fluentai_code = transpile_continuum(continuum_code)
    print(fluentai_code)