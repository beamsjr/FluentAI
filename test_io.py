#!/usr/bin/env python3
"""
Test ClaudeLang IO operations
"""

import os
import tempfile
from src.parser import parse
from src.interpreter import Interpreter
from src.stdlib import core, strings, io  # Import all stdlib modules


def test_file_operations():
    print("=== Testing File Operations ===")
    interpreter = Interpreter()
    
    # Create a temporary directory for testing
    with tempfile.TemporaryDirectory() as tmpdir:
        test_file = os.path.join(tmpdir, "test.txt")
        
        # Test file write
        code = f'(file-write "{test_file}" "Hello, ClaudeLang!")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Write file: {result.data} {'✓' if result.data is None else '✗'}")
        
        # Test file exists
        code = f'(file-exists? "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"File exists: {result.data} {'✓' if result.data == True else '✗'}")
        
        # Test file read
        code = f'(file-read "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Read file: {result.data} {'✓' if result.data == 'Hello, ClaudeLang!' else '✗'}")
        
        # Test file append
        code = f'(file-append "{test_file}" "\\nAppended line")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Append file: {result.data} {'✓' if result.data is None else '✗'}")
        
        # Test read after append
        code = f'(file-read "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        expected = "Hello, ClaudeLang!\\nAppended line"
        print(f"Read after append: {'✓' if result.data == expected else '✗'}")
        
        # Test file-read-lines
        code = f'(file-read-lines "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        expected_lines = ["Hello, ClaudeLang!", "Appended line"]
        print(f"Read lines: {result.data} {'✓' if result.data == expected_lines else '✗'}")
        
        # Test file-write-lines
        test_file2 = os.path.join(tmpdir, "test2.txt")
        code = f'(file-write-lines "{test_file2}" ["Line 1" "Line 2" "Line 3"])'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Write lines: {result.data} {'✓' if result.data is None else '✗'}")
        
        # Test delete
        code = f'(file-delete "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Delete file: {result.data} {'✓' if result.data is None else '✗'}")
        
        # Verify deletion
        code = f'(file-exists? "{test_file}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"File exists after delete: {result.data} {'✓' if result.data == False else '✗'}")
    
    print()


def test_directory_operations():
    print("=== Testing Directory Operations ===")
    interpreter = Interpreter()
    
    with tempfile.TemporaryDirectory() as tmpdir:
        # Test current directory
        code = '(current-directory)'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Current directory: {result.data}")
        
        # Test create directory
        new_dir = os.path.join(tmpdir, "test_dir")
        code = f'(dir-create "{new_dir}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Create directory: {result.data} {'✓' if result.data is None else '✗'}")
        
        # Create some files
        for i in range(3):
            with open(os.path.join(new_dir, f"file{i}.txt"), 'w') as f:
                f.write(f"Content {i}")
        
        # Test list directory
        code = f'(dir-list "{new_dir}")'
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"List directory: {sorted(result.data)} {'✓' if len(result.data) == 3 else '✗'}")
    
    print()


def test_json_operations():
    print("=== Testing JSON Operations ===")
    interpreter = Interpreter()
    
    # Test JSON stringify
    code = '(json-stringify ["name" "Claude" "age" 42])'
    graph = parse(code)
    result = interpreter.interpret(graph)
    print(f"JSON stringify list: {result.data}")
    
    # Test JSON parse
    json_str = '{\\"name\\": \\"Claude\\", \\"age\\": 42}'
    code = f'(json-parse "{json_str}")'
    graph = parse(code)
    result = interpreter.interpret(graph)
    print(f"JSON parse: {result.data}")
    
    # Test JSON error handling
    code = '(json-parse "invalid json")'
    graph = parse(code)
    result = interpreter.interpret(graph)
    print(f"JSON parse error: {'✓' if 'error' in result.data else '✗'}")
    
    print()


def test_io_with_effects():
    print("=== Testing IO with Effects ===")
    interpreter = Interpreter()
    
    # Example: Read config, process it, write result
    with tempfile.TemporaryDirectory() as tmpdir:
        config_file = os.path.join(tmpdir, "config.json")
        output_file = os.path.join(tmpdir, "output.txt")
        
        # Write config
        code = f'''
        (let ((config-data (json-stringify ["setting1" "value1" "setting2" "value2"])))
          (file-write "{config_file}" config-data))
        '''
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Write config: {'✓' if result.data is None else '✗'}")
        
        # Read, process, write
        code = f'''
        (let ((config-str (file-read "{config_file}"))
              (config (json-parse config-str))
              (output (string-join "\\n" config)))
          (file-write "{output_file}" output))
        '''
        graph = parse(code)
        result = interpreter.interpret(graph)
        print(f"Process config: {'✓' if result.data is None else '✗'}")
        
        # Verify output
        with open(output_file, 'r') as f:
            content = f.read()
        print(f"Output content: {content}")
    
    print()


if __name__ == "__main__":
    test_file_operations()
    test_directory_operations()
    test_json_operations()
    test_io_with_effects()