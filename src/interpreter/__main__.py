"""
ClaudeLang interpreter entry point with module support
"""

import sys
import os
from ..modules.module_system import ModuleInterpreter, ModuleEnvironment
from ..parser.sexpr_parser import parse


def main():
    if len(sys.argv) < 2:
        print("Usage: python -m src.interpreter <file.cl>")
        sys.exit(1)
    
    filename = sys.argv[1]
    
    # Set up module search paths
    file_dir = os.path.dirname(os.path.abspath(filename))
    env = ModuleEnvironment(search_paths=[
        os.path.join(file_dir, "modules"),
        file_dir,
        "modules",
        "."
    ])
    
    # Execute file
    interpreter = ModuleInterpreter()
    
    # Check if it's a module file or a script
    with open(filename, 'r') as f:
        source = f.read()
    
    # If it starts with (module, load as module
    if source.strip().startswith('(module'):
        result = interpreter.load_and_execute_module(filename, env)
    else:
        # Regular script with imports
        graph = parse(source)
        
        # Process imports
        from ..core.ast import NodeType, Import
        for node_id, node in graph.nodes.items():
            if isinstance(node, Import):
                module_path = node.module_path
                interpreter.load_and_execute_module(module_path, env)
        
        # Execute main graph
        from ..modules.module_system import ModuleInfo
        module_info = ModuleInfo(
            path=filename,
            name="__main__",
            graph=graph,
            exports={},
            dependencies=[]
        )
        result = interpreter.execute_module(module_info, graph, env)
    
    if result is not None and result != 0:
        print(result)


if __name__ == "__main__":
    main()