#!/usr/bin/env python3
import os
import re
import sys

def find_long_methods(filepath):
    """Find methods/functions over 50 lines in a Rust file."""
    try:
        with open(filepath, 'r') as f:
            content = f.read()
    except:
        return []
    
    # Pattern to match function definitions
    # Matches: fn name(...) { or async fn name(...) {
    fn_pattern = r'^\s*(pub\s+)?(async\s+)?(unsafe\s+)?(extern\s+"[^"]+"\s+)?fn\s+(\w+)'
    
    results = []
    lines = content.split('\n')
    
    i = 0
    while i < len(lines):
        line = lines[i]
        match = re.match(fn_pattern, line)
        
        if match:
            fn_name = match.group(5)
            start_line = i + 1  # 1-based line numbers
            
            # Find the opening brace
            brace_count = 0
            j = i
            found_start = False
            
            while j < len(lines):
                for char in lines[j]:
                    if char == '{':
                        brace_count += 1
                        found_start = True
                    elif char == '}' and found_start:
                        brace_count -= 1
                        if brace_count == 0:
                            end_line = j + 1
                            line_count = end_line - start_line + 1
                            
                            if line_count > 50:
                                # Get a preview of what the function does
                                preview_lines = lines[i:min(i+10, len(lines))]
                                preview = '\n'.join(preview_lines)
                                
                                results.append({
                                    'name': fn_name,
                                    'start': start_line,
                                    'end': end_line,
                                    'lines': line_count,
                                    'preview': preview
                                })
                            i = j
                            break
                j += 1
                if brace_count == 0 and found_start:
                    break
        i += 1
    
    return results

def main():
    # Core modules to search
    modules = [
        'fluentai-core',
        'fluentai-vm', 
        'fluentai-parser',
        'fluentai-interpreter',
        'fluentai-optimizer'
    ]
    
    all_results = []
    
    for root, dirs, files in os.walk('.'):
        # Skip target directory
        if 'target' in root:
            continue
            
        # Check if this is one of our target modules
        if any(module in root for module in modules):
            for file in files:
                if file.endswith('.rs'):
                    filepath = os.path.join(root, file)
                    results = find_long_methods(filepath)
                    
                    if results:
                        for result in results:
                            all_results.append({
                                'file': filepath,
                                'function': result['name'],
                                'lines': result['lines'],
                                'start': result['start'],
                                'preview': result['preview']
                            })
    
    # Sort by line count descending
    all_results.sort(key=lambda x: x['lines'], reverse=True)
    
    # Print results
    print(f"Found {len(all_results)} methods over 50 lines:\n")
    
    for result in all_results:
        print(f"File: {result['file']}")
        print(f"Method: {result['function']} (lines {result['start']}-{result['start']+result['lines']-1}, {result['lines']} lines)")
        print("Preview:")
        print(result['preview'])
        print("-" * 80)
        print()

if __name__ == "__main__":
    main()