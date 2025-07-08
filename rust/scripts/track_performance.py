#!/usr/bin/env python3
"""
Performance tracking script for FluentAI Rust implementation.
Runs benchmarks and stores results for historical tracking.
"""

import json
import subprocess
import datetime
import os
import sys
from pathlib import Path
from typing import Dict, List, Any

BENCHMARK_DIR = Path(__file__).parent.parent / "target" / "criterion"
RESULTS_DIR = Path(__file__).parent.parent / "benchmark_results"
RESULTS_FILE = RESULTS_DIR / "performance_history.json"

def run_benchmarks() -> Dict[str, Any]:
    """Run all benchmarks and collect results."""
    print("Running FluentAI Rust benchmarks...")
    
    results = {
        "timestamp": datetime.datetime.now().isoformat(),
        "commit": get_git_commit(),
        "benchmarks": {}
    }
    
    # Run parser benchmarks
    print("\n1. Running parser benchmarks...")
    parser_output = subprocess.run(
        ["cargo", "bench", "--bench", "parser", "--", "--output-format", "json"],
        capture_output=True,
        text=True,
        cwd=Path(__file__).parent.parent
    )
    results["benchmarks"]["parser"] = parse_criterion_output(parser_output.stdout)
    
    # Run VM benchmarks
    print("\n2. Running VM benchmarks...")
    vm_output = subprocess.run(
        ["cargo", "bench", "--bench", "vm", "--", "--output-format", "json"],
        capture_output=True,
        text=True,
        cwd=Path(__file__).parent.parent
    )
    results["benchmarks"]["vm"] = parse_criterion_output(vm_output.stdout)
    
    # Run comprehensive benchmarks
    print("\n3. Running comprehensive benchmarks...")
    comprehensive_output = subprocess.run(
        ["cargo", "bench", "--bench", "comprehensive_benchmark", "--", "--warm-up-time", "1", "--measurement-time", "3"],
        capture_output=True,
        text=True,
        cwd=Path(__file__).parent.parent
    )
    results["benchmarks"]["comprehensive"] = parse_comprehensive_output(comprehensive_output.stdout)
    
    return results

def get_git_commit() -> str:
    """Get current git commit hash."""
    try:
        result = subprocess.run(
            ["git", "rev-parse", "HEAD"],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except:
        return "unknown"

def parse_criterion_output(output: str) -> Dict[str, Any]:
    """Parse criterion benchmark output."""
    # For now, just extract key metrics from the output
    metrics = {}
    lines = output.split('\n')
    
    for line in lines:
        if "time:" in line and "[" in line:
            # Extract benchmark name and time
            parts = line.split()
            if len(parts) > 2:
                name = parts[0]
                time_str = parts[-2]
                try:
                    # Convert to nanoseconds
                    if "µs" in time_str:
                        time_ns = float(time_str.replace("µs", "")) * 1000
                    elif "ms" in time_str:
                        time_ns = float(time_str.replace("ms", "")) * 1_000_000
                    elif "ns" in time_str:
                        time_ns = float(time_str.replace("ns", ""))
                    else:
                        time_ns = 0
                    
                    metrics[name] = {"time_ns": time_ns}
                except:
                    pass
    
    return metrics

def parse_comprehensive_output(output: str) -> Dict[str, Any]:
    """Parse comprehensive benchmark output."""
    metrics = {}
    
    # Look for the performance summary section
    in_summary = False
    for line in output.split('\n'):
        if "FluentAI Performance Comparison" in line:
            in_summary = True
        elif in_summary and "AVERAGE" in line:
            parts = line.split()
            if len(parts) >= 6:
                try:
                    metrics["average"] = {
                        "parse_ns": float(parts[1]),
                        "compile_ns": float(parts[2]),
                        "vm_ns": float(parts[3]),
                        "total_ns": float(parts[4]),
                        "throughput": float(parts[5])
                    }
                except:
                    pass
    
    return metrics

def save_results(results: Dict[str, Any]):
    """Save benchmark results to history file."""
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
    
    # Load existing history
    history = []
    if RESULTS_FILE.exists():
        with open(RESULTS_FILE, 'r') as f:
            history = json.load(f)
    
    # Add new results
    history.append(results)
    
    # Keep only last 100 results
    if len(history) > 100:
        history = history[-100:]
    
    # Save updated history
    with open(RESULTS_FILE, 'w') as f:
        json.dump(history, f, indent=2)
    
    print(f"\nResults saved to {RESULTS_FILE}")

def print_summary(results: Dict[str, Any]):
    """Print a summary of the benchmark results."""
    print("\n" + "="*60)
    print("PERFORMANCE SUMMARY")
    print("="*60)
    
    comprehensive = results["benchmarks"].get("comprehensive", {})
    if "average" in comprehensive:
        avg = comprehensive["average"]
        print(f"\nAverage Performance:")
        print(f"  Parser:     {avg['parse_ns']:.1f} ns")
        print(f"  Compiler:   {avg['compile_ns']:.1f} ns")
        print(f"  VM:         {avg['vm_ns']:.1f} ns")
        print(f"  Total:      {avg['total_ns']:.1f} ns")
        print(f"  Throughput: {avg['throughput']:.0f} ops/second")
    
    print(f"\nCommit: {results['commit'][:8]}")
    print(f"Time:   {results['timestamp']}")

def compare_with_baseline():
    """Compare current results with baseline."""
    if not RESULTS_FILE.exists():
        print("\nNo baseline results found for comparison.")
        return
    
    with open(RESULTS_FILE, 'r') as f:
        history = json.load(f)
    
    if len(history) < 2:
        print("\nNot enough history for comparison.")
        return
    
    current = history[-1]
    baseline = history[-2]
    
    print("\n" + "="*60)
    print("PERFORMANCE COMPARISON")
    print("="*60)
    
    curr_avg = current["benchmarks"].get("comprehensive", {}).get("average", {})
    base_avg = baseline["benchmarks"].get("comprehensive", {}).get("average", {})
    
    if curr_avg and base_avg:
        for metric in ["parse_ns", "compile_ns", "vm_ns", "total_ns"]:
            if metric in curr_avg and metric in base_avg:
                current_val = curr_avg[metric]
                baseline_val = base_avg[metric]
                change = ((current_val - baseline_val) / baseline_val) * 100
                
                symbol = "🔴" if change > 5 else "🟢" if change < -5 else "🟡"
                print(f"{symbol} {metric}: {current_val:.1f} ns ({change:+.1f}%)")

def main():
    """Main entry point."""
    results = run_benchmarks()
    save_results(results)
    print_summary(results)
    compare_with_baseline()

if __name__ == "__main__":
    main()