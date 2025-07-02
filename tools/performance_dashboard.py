#!/usr/bin/env python3
"""
Performance Dashboard - Real-time comparison between Python and Rust implementations
"""

import json
import os
import time
from datetime import datetime
from typing import Dict, List, Optional, Tuple
import matplotlib.pyplot as plt
import matplotlib.animation as animation
from matplotlib.figure import Figure
import numpy as np


class PerformanceDashboard:
    """Interactive dashboard for performance comparison"""
    
    def __init__(self, baseline_file: str, rust_results_dir: str = "rust_performance"):
        self.baseline_file = baseline_file
        self.rust_results_dir = rust_results_dir
        self.baseline_data = self._load_baseline()
        self.fig = None
        self.axes = []
        
    def _load_baseline(self) -> Dict:
        """Load Python baseline data"""
        with open(self.baseline_file, 'r') as f:
            return json.load(f)
    
    def _load_rust_results(self) -> Dict:
        """Load latest Rust performance results"""
        # This will be populated as Rust components are implemented
        rust_results = {}
        
        if os.path.exists(self.rust_results_dir):
            for file in os.listdir(self.rust_results_dir):
                if file.endswith('.json'):
                    with open(os.path.join(self.rust_results_dir, file), 'r') as f:
                        data = json.load(f)
                        component = data.get('component', 'unknown')
                        rust_results[component] = data
        
        return rust_results
    
    def create_dashboard(self):
        """Create the dashboard layout"""
        self.fig = plt.figure(figsize=(16, 10))
        self.fig.suptitle('ClaudeLang Performance: Python vs Rust', fontsize=16)
        
        # Create subplots
        gs = self.fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)
        
        # Parser performance
        self.ax_parser = self.fig.add_subplot(gs[0, 0])
        self.ax_parser.set_title('Parser Performance')
        
        # VM performance
        self.ax_vm = self.fig.add_subplot(gs[0, 1])
        self.ax_vm.set_title('VM Performance')
        
        # End-to-end performance
        self.ax_e2e = self.fig.add_subplot(gs[0, 2])
        self.ax_e2e.set_title('End-to-End Performance')
        
        # Overall speedup
        self.ax_speedup = self.fig.add_subplot(gs[1, :])
        self.ax_speedup.set_title('Overall Speedup Factor')
        
        # Memory usage
        self.ax_memory = self.fig.add_subplot(gs[2, 0])
        self.ax_memory.set_title('Memory Usage')
        
        # Latency distribution
        self.ax_latency = self.fig.add_subplot(gs[2, 1])
        self.ax_latency.set_title('Latency Distribution')
        
        # Progress tracker
        self.ax_progress = self.fig.add_subplot(gs[2, 2])
        self.ax_progress.set_title('Migration Progress')
        
        self.axes = [
            self.ax_parser, self.ax_vm, self.ax_e2e,
            self.ax_speedup, self.ax_memory, self.ax_latency,
            self.ax_progress
        ]
    
    def update_parser_chart(self, ax):
        """Update parser performance comparison"""
        ax.clear()
        ax.set_title('Parser Performance')
        
        baseline_parser = self.baseline_data['results'].get('parser', {})
        rust_results = self._load_rust_results()
        rust_parser = rust_results.get('parser', {}).get('results', {})
        
        operations = list(baseline_parser.keys())
        python_times = [baseline_parser[op]['mean_ns']/1000 for op in operations]
        
        x = np.arange(len(operations))
        width = 0.35
        
        bars1 = ax.bar(x - width/2, python_times, width, label='Python', color='blue', alpha=0.7)
        
        if rust_parser:
            rust_times = [rust_parser.get(op, {}).get('mean_ns', 0)/1000 for op in operations]
            bars2 = ax.bar(x + width/2, rust_times, width, label='Rust', color='green', alpha=0.7)
            
            # Add speedup labels
            for i, (py, rs) in enumerate(zip(python_times, rust_times)):
                if rs > 0:
                    speedup = py / rs
                    ax.text(i, max(py, rs) + 10, f'{speedup:.1f}x', 
                           ha='center', fontsize=8, color='red')
        
        ax.set_xlabel('Operation')
        ax.set_ylabel('Time (µs)')
        ax.set_xticks(x)
        ax.set_xticklabels(operations, rotation=45, ha='right')
        ax.legend()
        ax.grid(True, alpha=0.3)
    
    def update_speedup_chart(self, ax):
        """Update overall speedup chart"""
        ax.clear()
        ax.set_title('Overall Speedup Factor (Rust vs Python)')
        
        categories = ['Parser', 'Interpreter', 'VM', 'Optimizer', 'End-to-End']
        current_speedups = []
        target_speedups = []
        
        rust_results = self._load_rust_results()
        
        # Calculate actual speedups
        for category in categories:
            category_lower = category.lower().replace('-', '_')
            baseline = self.baseline_data['results'].get(category_lower, {})
            rust = rust_results.get(category_lower, {}).get('results', {})
            
            if baseline and rust:
                # Average speedup for the category
                speedups = []
                for op in baseline:
                    if op in rust and rust[op].get('mean_ns', 0) > 0:
                        speedup = baseline[op]['mean_ns'] / rust[op]['mean_ns']
                        speedups.append(speedup)
                
                avg_speedup = np.mean(speedups) if speedups else 0
                current_speedups.append(avg_speedup)
            else:
                current_speedups.append(0)
        
        # Target speedups
        target_speedups = [100, 50, 30, 20, 10]  # Expected improvements
        
        x = np.arange(len(categories))
        width = 0.35
        
        bars1 = ax.bar(x - width/2, current_speedups, width, 
                       label='Current', color='green', alpha=0.7)
        bars2 = ax.bar(x + width/2, target_speedups, width, 
                       label='Target', color='orange', alpha=0.5)
        
        # Add value labels
        for i, (current, target) in enumerate(zip(current_speedups, target_speedups)):
            if current > 0:
                ax.text(i - width/2, current + 1, f'{current:.1f}x', 
                       ha='center', fontsize=8)
            ax.text(i + width/2, target + 1, f'{target}x', 
                   ha='center', fontsize=8, alpha=0.7)
        
        ax.set_xlabel('Component')
        ax.set_ylabel('Speedup Factor')
        ax.set_xticks(x)
        ax.set_xticklabels(categories)
        ax.legend()
        ax.grid(True, alpha=0.3)
        ax.set_yscale('log')
    
    def update_progress_chart(self, ax):
        """Update migration progress"""
        ax.clear()
        ax.set_title('Rust Migration Progress')
        
        components = [
            ('Parser', 0),  # Not started
            ('Lexer', 0),
            ('AST', 0),
            ('VM', 0),
            ('GC', 0),
            ('JIT', 0),
            ('LSP', 0),
            ('Python Bindings', 0),
        ]
        
        # Check actual progress
        rust_results = self._load_rust_results()
        for i, (name, _) in enumerate(components):
            if name.lower() in rust_results:
                # Estimate progress based on test coverage
                tests_passed = rust_results[name.lower()].get('tests_passed', 0)
                total_tests = rust_results[name.lower()].get('total_tests', 100)
                progress = (tests_passed / total_tests) * 100 if total_tests > 0 else 0
                components[i] = (name, progress)
        
        names = [c[0] for c in components]
        progress = [c[1] for c in components]
        
        bars = ax.barh(names, progress, color='green', alpha=0.7)
        
        # Add percentage labels
        for i, (bar, pct) in enumerate(zip(bars, progress)):
            if pct > 0:
                ax.text(pct + 1, i, f'{pct:.0f}%', 
                       va='center', fontsize=8)
        
        ax.set_xlabel('Progress (%)')
        ax.set_xlim(0, 105)
        ax.grid(True, alpha=0.3, axis='x')
    
    def animate(self, frame):
        """Animation update function"""
        self.update_parser_chart(self.ax_parser)
        self.update_speedup_chart(self.ax_speedup)
        self.update_progress_chart(self.ax_progress)
        
        # Update other charts as data becomes available
        # self.update_vm_chart(self.ax_vm)
        # self.update_e2e_chart(self.ax_e2e)
        # self.update_memory_chart(self.ax_memory)
        # self.update_latency_chart(self.ax_latency)
        
        return self.axes
    
    def run(self):
        """Run the dashboard"""
        self.create_dashboard()
        
        # Create animation that updates every 5 seconds
        ani = animation.FuncAnimation(
            self.fig, self.animate, interval=5000, blit=False
        )
        
        plt.tight_layout()
        plt.show()


def generate_comparison_report(baseline_file: str, rust_results_dir: str = "rust_performance"):
    """Generate a static comparison report"""
    with open(baseline_file, 'r') as f:
        baseline = json.load(f)
    
    print("ClaudeLang Performance Comparison Report")
    print("=" * 60)
    print(f"Generated: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
    print()
    
    # Current status
    print("Current Performance (Python Baseline):")
    print("-" * 40)
    
    for category, results in baseline['results'].items():
        print(f"\n{category.upper()}:")
        for op, metrics in results.items():
            print(f"  {op:<30} {metrics['mean_ns']/1000:>10.1f} µs")
    
    print("\n\nProjected Improvements with Rust:")
    print("-" * 40)
    print("Component            Current        Target       Speedup")
    print("-" * 60)
    
    projections = [
        ("Parser", 240.0, 5.0, 48),
        ("VM Execution", 4.88, 0.1, 49),
        ("LSP Response", 100.0, 5.0, 20),
        ("JIT Compilation", 1000.0, 50.0, 20),
        ("Memory Usage", 100, 20, 5),
    ]
    
    for component, current, target, speedup in projections:
        print(f"{component:<20} {current:>7.1f} µs    {target:>7.1f} µs    {speedup:>5}x")
    
    print("\n\nImplementation Roadmap:")
    print("-" * 40)
    print("Phase 1 (Weeks 1-2): Parser - Expected 50x speedup")
    print("Phase 2 (Weeks 3-4): VM - Expected 30x speedup")
    print("Phase 3 (Weeks 5-6): LSP - Expected 20x speedup")
    print("Phase 4 (Weeks 7-8): JIT - Expected 10x speedup")
    print("\nTotal Expected Improvement: 10-20x overall performance")


def main():
    """Main entry point"""
    import argparse
    
    parser = argparse.ArgumentParser(description='Performance comparison dashboard')
    parser.add_argument('--baseline', default='performance_baselines/baseline_performance.json',
                       help='Path to baseline performance file')
    parser.add_argument('--rust-dir', default='rust_performance',
                       help='Directory containing Rust performance results')
    parser.add_argument('--report', action='store_true',
                       help='Generate static report instead of dashboard')
    
    args = parser.parse_args()
    
    if args.report:
        generate_comparison_report(args.baseline, args.rust_dir)
    else:
        dashboard = PerformanceDashboard(args.baseline, args.rust_dir)
        dashboard.run()


if __name__ == "__main__":
    main()