#!/usr/bin/env python3
"""
ClaudeLang Trace Analysis Tool

This tool runs ClaudeLang programs with execution tracing enabled
and generates documentation from the collected traces.
"""

import argparse
import sys
import os
from pathlib import Path
from typing import List, Optional

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from src.interpreter.trace_interpreter import TracingInterpreter
from src.docs.trace_analyzer import ExecutionTraceAnalyzer


def analyze_file(filename: str, output_format: str = 'markdown', 
                 export_json: Optional[str] = None, verbose: bool = False):
    """Analyze execution traces from a ClaudeLang file"""
    print(f"Analyzing: {filename}")
    print("=" * 60)
    
    # Read source file
    with open(filename, 'r') as f:
        source = f.read()
    
    # Create tracing interpreter
    analyzer = ExecutionTraceAnalyzer()
    interpreter = TracingInterpreter(trace_analyzer=analyzer)
    
    # Suppress output unless verbose
    if not verbose:
        import sys
        from io import StringIO
        old_stdout = sys.stdout
        sys.stdout = StringIO()
    
    try:
        # Execute the program
        print("\nExecuting program with tracing enabled...")
        result = interpreter.eval(source)
        
        if verbose:
            print(f"\nProgram result: {result.data if hasattr(result, 'data') else result}")
        
    except Exception as e:
        print(f"\nError during execution: {e}")
        if verbose:
            import traceback
            traceback.print_exc()
        return False
    
    finally:
        if not verbose:
            sys.stdout = old_stdout
    
    # Generate analysis
    print("\nAnalyzing execution traces...")
    patterns = analyzer.analyze_patterns()
    
    # Display summary statistics
    print(f"\nTrace Summary:")
    print(f"  Total events: {len(analyzer.traces)}")
    print(f"  Functions profiled: {len(analyzer.function_profiles)}")
    print(f"  Unique call paths: {len(analyzer.call_graph)}")
    print(f"  Branch points: {len(analyzer.branch_profiles)}")
    
    # Display top functions by call count
    if analyzer.function_profiles:
        print("\nTop Functions by Call Count:")
        sorted_funcs = sorted(analyzer.function_profiles.items(), 
                            key=lambda x: x[1].call_count, reverse=True)[:5]
        for name, profile in sorted_funcs:
            print(f"  {name}: {profile.call_count} calls, "
                  f"{profile.average_time:.4f}s avg time")
    
    # Display bottlenecks
    bottlenecks = patterns.get('bottlenecks', [])
    if bottlenecks:
        print("\nPerformance Bottlenecks:")
        for b in bottlenecks[:3]:
            print(f"  {b['function']}: {b['reason'].replace('_', ' ')}")
    
    # Display optimization opportunities
    opportunities = patterns.get('optimization_opportunities', [])
    if opportunities:
        print("\nOptimization Opportunities:")
        for opp in opportunities[:3]:
            print(f"  {opp['type'].replace('_', ' ').title()}: "
                  f"{opp.get('function', opp.get('condition'))}")
    
    # Generate documentation
    if output_format == 'markdown':
        doc = interpreter.generate_documentation()
        
        # Save to file
        output_file = filename.replace('.cl', '_trace_analysis.md')
        with open(output_file, 'w') as f:
            f.write(doc)
        print(f"\nDocumentation written to: {output_file}")
    
    # Export JSON if requested
    if export_json:
        interpreter.export_traces(export_json)
        print(f"Trace data exported to: {export_json}")
    
    return True


def analyze_multiple_files(files: List[str], output_dir: Optional[str] = None,
                          verbose: bool = False):
    """Analyze multiple files and generate combined report"""
    combined_analyzer = ExecutionTraceAnalyzer()
    
    print("Analyzing multiple files...")
    print("=" * 60)
    
    for filename in files:
        print(f"\nProcessing: {filename}")
        
        # Read and execute
        with open(filename, 'r') as f:
            source = f.read()
        
        # Use shared analyzer
        interpreter = TracingInterpreter(trace_analyzer=combined_analyzer)
        
        if not verbose:
            import sys
            from io import StringIO
            old_stdout = sys.stdout
            sys.stdout = StringIO()
        
        try:
            interpreter.eval(source)
        except Exception as e:
            print(f"  Error: {e}")
            continue
        finally:
            if not verbose:
                sys.stdout = old_stdout
    
    # Generate combined report
    print("\n" + "=" * 60)
    print("Combined Analysis Report")
    print("=" * 60)
    
    doc = combined_analyzer.generate_documentation()
    
    if output_dir:
        output_file = os.path.join(output_dir, 'combined_trace_analysis.md')
    else:
        output_file = 'combined_trace_analysis.md'
    
    with open(output_file, 'w') as f:
        f.write(doc)
    
    print(f"\nCombined documentation written to: {output_file}")


def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(
        description="Analyze ClaudeLang program execution traces"
    )
    parser.add_argument(
        "files",
        nargs="+",
        help="ClaudeLang source files to analyze"
    )
    parser.add_argument(
        "-f", "--format",
        choices=["markdown", "json"],
        default="markdown",
        help="Output format (default: markdown)"
    )
    parser.add_argument(
        "-o", "--output",
        help="Output directory for analysis results"
    )
    parser.add_argument(
        "--export-json",
        help="Export raw trace data to JSON file"
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Show program output during execution"
    )
    parser.add_argument(
        "--combine",
        action="store_true",
        help="Combine analysis from multiple files"
    )
    
    args = parser.parse_args()
    
    # Validate files exist
    for filename in args.files:
        if not Path(filename).exists():
            print(f"Error: File '{filename}' not found")
            sys.exit(1)
    
    # Process files
    if args.combine and len(args.files) > 1:
        analyze_multiple_files(args.files, args.output, args.verbose)
    else:
        # Analyze each file separately
        for filename in args.files:
            success = analyze_file(
                filename, 
                args.format,
                args.export_json,
                args.verbose
            )
            if not success:
                sys.exit(1)


if __name__ == "__main__":
    main()