#!/usr/bin/env python3
import os
import re
import sys

def find_long_methods(filepath):
    """Find methods/functions over 50 lines in a Rust file."""
    try:
        with open(filepath, 'r') as f:
            lines = f.readlines()
    except:
        return []
    
    # Pattern to match function definitions
    fn_pattern = r'^(\s*)(pub\s+)?(async\s+)?(unsafe\s+)?(extern\s+"[^"]+"\s+)?fn\s+(\w+)'
    
    results = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        match = re.match(fn_pattern, line)
        
        if match:
            indent = match.group(1)
            fn_name = match.group(6)
            start_line = i + 1  # 1-based line numbers
            
            # Find the end of the function by looking for the closing brace at the same indentation
            # or the next function at the same or lesser indentation
            j = i + 1
            brace_count = 0
            found_opening = False
            
            while j < len(lines):
                curr_line = lines[j]
                
                # Count braces
                for char in curr_line:
                    if char == '{':
                        brace_count += 1
                        found_opening = True
                    elif char == '}':
                        brace_count -= 1
                
                # Check if we've found the end
                if found_opening and brace_count == 0:
                    end_line = j + 1
                    line_count = end_line - start_line + 1
                    
                    if line_count > 50:
                        # Get a preview
                        preview_lines = lines[i:min(i+10, len(lines))]
                        preview = ''.join(preview_lines).rstrip()
                        
                        results.append({
                            'name': fn_name,
                            'start': start_line,
                            'end': end_line,
                            'lines': line_count,
                            'preview': preview
                        })
                    break
                    
                # Check if we hit another function at same level (safety check)
                next_match = re.match(fn_pattern, curr_line)
                if next_match and next_match.group(1) == indent and j > i + 1:
                    # We hit another function, something went wrong
                    break
                    
                j += 1
                
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
                                'end': result['end'],
                                'preview': result['preview']
                            })
    
    # Sort by line count descending
    all_results.sort(key=lambda x: x['lines'], reverse=True)
    
    # Print results
    print(f"Found {len(all_results)} methods over 50 lines:\n")
    
    for i, result in enumerate(all_results[:30]):  # Top 30
        print(f"{i+1}. File: {result['file']}")
        print(f"   Method: {result['function']} (lines {result['start']}-{result['end']}, {result['lines']} lines)")
        print(f"   Preview:")
        preview_lines = result['preview'].split('\n')[:5]
        for line in preview_lines:
            print(f"   {line}")
        print()

if __name__ == "__main__":
    main()