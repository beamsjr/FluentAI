#!/usr/bin/env python3

"""
FluentAI Coverage Summary Generator

This script generates detailed coverage summaries and tracks coverage trends over time.
It can process Tarpaulin JSON output and generate various reports including:
- Per-crate coverage breakdown
- File-level coverage details
- Coverage trends over time
- Coverage badges
- Markdown reports for GitHub
"""

import json
import sys
import os
import argparse
from datetime import datetime
from pathlib import Path
from typing import Dict, List, Tuple, Optional
import subprocess

# ANSI color codes
RED = '\033[0;31m'
GREEN = '\033[0;32m'
YELLOW = '\033[1;33m'
BLUE = '\033[0;34m'
NC = '\033[0m'  # No Color


def load_coverage_data(json_path: str) -> dict:
    """Load coverage data from Tarpaulin JSON output."""
    with open(json_path, 'r') as f:
        return json.load(f)


def get_crate_from_path(file_path: str) -> str:
    """Extract crate name from file path."""
    parts = file_path.split('/')
    if 'src' in parts:
        src_index = parts.index('src')
        if src_index > 0:
            return parts[src_index - 1]
    return 'unknown'


def calculate_coverage_stats(data: dict) -> Tuple[Dict[str, dict], float, int, int]:
    """Calculate coverage statistics from Tarpaulin data."""
    crate_coverage = {}
    total_lines = 0
    covered_lines = 0
    
    if 'files' in data:
        for file_info in data['files']:
            file_path = file_info['path']
            file_covered = sum(1 for line in file_info['covered'] if line > 0)
            file_total = len(file_info['covered'])
            
            crate_name = get_crate_from_path(file_path)
            
            if crate_name not in crate_coverage:
                crate_coverage[crate_name] = {
                    'covered': 0,
                    'total': 0,
                    'files': []
                }
            
            crate_coverage[crate_name]['covered'] += file_covered
            crate_coverage[crate_name]['total'] += file_total
            crate_coverage[crate_name]['files'].append({
                'path': file_path,
                'covered': file_covered,
                'total': file_total,
                'percentage': (file_covered / file_total * 100) if file_total > 0 else 0
            })
            
            total_lines += file_total
            covered_lines += file_covered
    
    overall_coverage = (covered_lines / total_lines * 100) if total_lines > 0 else 0
    
    return crate_coverage, overall_coverage, covered_lines, total_lines


def generate_badge_svg(coverage: float) -> str:
    """Generate SVG badge for coverage percentage."""
    color = '#4c1' if coverage >= 80 else '#dfb317' if coverage >= 60 else '#e05d44'
    coverage_text = f"{coverage:.1f}%"
    
    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="114" height="20">
    <linearGradient id="b" x2="0" y2="100%">
        <stop offset="0" stop-color="#bbb" stop-opacity=".1"/>
        <stop offset="1" stop-opacity=".1"/>
    </linearGradient>
    <mask id="a">
        <rect width="114" height="20" rx="3" fill="#fff"/>
    </mask>
    <g mask="url(#a)">
        <path fill="#555" d="M0 0h63v20H0z"/>
        <path fill="{color}" d="M63 0h51v20H63z"/>
        <path fill="url(#b)" d="M0 0h114v20H0z"/>
    </g>
    <g fill="#fff" text-anchor="middle" font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="11">
        <text x="31.5" y="15" fill="#010101" fill-opacity=".3">coverage</text>
        <text x="31.5" y="14">coverage</text>
        <text x="87.5" y="15" fill="#010101" fill-opacity=".3">{coverage_text}</text>
        <text x="87.5" y="14">{coverage_text}</text>
    </g>
</svg>"""


def generate_markdown_report(crate_coverage: dict, overall_coverage: float, 
                           covered_lines: int, total_lines: int) -> str:
    """Generate Markdown report for coverage."""
    report = f"""# FluentAI Coverage Report

Generated on: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}

## Overall Coverage

- **Total Coverage**: {overall_coverage:.2f}%
- **Lines Covered**: {covered_lines:,} / {total_lines:,}

## Coverage by Crate

| Crate | Coverage | Lines Covered | Total Lines |
|-------|----------|---------------|-------------|
"""
    
    # Sort crates by coverage percentage
    sorted_crates = sorted(
        crate_coverage.items(),
        key=lambda x: x[1]['covered'] / x[1]['total'] if x[1]['total'] > 0 else 0,
        reverse=True
    )
    
    for crate, stats in sorted_crates:
        if stats['total'] > 0:
            coverage = stats['covered'] / stats['total'] * 100
            emoji = "🟢" if coverage >= 80 else "🟡" if coverage >= 60 else "🔴"
            report += f"| {crate} | {emoji} {coverage:.1f}% | {stats['covered']:,} | {stats['total']:,} |\n"
    
    # Add most/least covered files
    all_files = []
    for crate, stats in crate_coverage.items():
        for file_info in stats['files']:
            all_files.append((file_info['path'], file_info['percentage']))
    
    all_files.sort(key=lambda x: x[1], reverse=True)
    
    if all_files:
        report += "\n## Top 5 Best Covered Files\n\n"
        for path, coverage in all_files[:5]:
            report += f"- `{path}`: {coverage:.1f}%\n"
        
        report += "\n## Top 5 Files Needing Attention\n\n"
        uncovered = [f for f in all_files if f[1] < 100]
        for path, coverage in uncovered[-5:]:
            report += f"- `{path}`: {coverage:.1f}%\n"
    
    return report


def save_coverage_history(coverage_data: dict, history_file: str = "coverage-history.json"):
    """Save coverage data to history file for tracking trends."""
    history = []
    
    if os.path.exists(history_file):
        with open(history_file, 'r') as f:
            history = json.load(f)
    
    # Calculate overall coverage
    _, overall_coverage, covered_lines, total_lines = calculate_coverage_stats(coverage_data)
    
    history.append({
        'timestamp': datetime.now().isoformat(),
        'overall_coverage': overall_coverage,
        'covered_lines': covered_lines,
        'total_lines': total_lines
    })
    
    # Keep only last 100 entries
    history = history[-100:]
    
    with open(history_file, 'w') as f:
        json.dump(history, f, indent=2)


def print_summary(crate_coverage: dict, overall_coverage: float, 
                  covered_lines: int, total_lines: int, verbose: bool = False):
    """Print coverage summary to console."""
    print(f"\n{GREEN}Coverage Summary:{NC}")
    print(f"{'='*60}")
    print(f"{'Crate':<30} {'Coverage':>10} {'Lines':>18}")
    print(f"{'-'*60}")
    
    # Sort by coverage percentage
    sorted_crates = sorted(
        crate_coverage.items(),
        key=lambda x: x[1]['covered'] / x[1]['total'] if x[1]['total'] > 0 else 0,
        reverse=True
    )
    
    for crate, stats in sorted_crates:
        if stats['total'] > 0:
            coverage = stats['covered'] / stats['total'] * 100
            color = GREEN if coverage >= 80 else YELLOW if coverage >= 60 else RED
            print(f"{crate:<30} {color}{coverage:>9.2f}%{NC} {stats['covered']:>8,}/{stats['total']:<8,}")
    
    print(f"{'-'*60}")
    
    color = GREEN if overall_coverage >= 80 else YELLOW if overall_coverage >= 60 else RED
    print(f"{'TOTAL':<30} {color}{overall_coverage:>9.2f}%{NC} {covered_lines:>8,}/{total_lines:<8,}")
    
    if verbose:
        print(f"\n{BLUE}File Details:{NC}")
        for crate, stats in sorted_crates:
            if stats['files']:
                print(f"\n{crate}:")
                for file_info in sorted(stats['files'], key=lambda x: x['percentage']):
                    color = GREEN if file_info['percentage'] >= 80 else YELLOW if file_info['percentage'] >= 60 else RED
                    print(f"  {file_info['path']}: {color}{file_info['percentage']:.1f}%{NC}")


def main():
    parser = argparse.ArgumentParser(description='Generate FluentAI coverage summary')
    parser.add_argument('json_file', help='Tarpaulin JSON output file')
    parser.add_argument('-v', '--verbose', action='store_true', help='Show file-level details')
    parser.add_argument('-m', '--markdown', help='Generate Markdown report to file')
    parser.add_argument('-b', '--badge', help='Generate SVG badge to file')
    parser.add_argument('-H', '--history', action='store_true', help='Save to coverage history')
    parser.add_argument('--min-coverage', type=float, help='Minimum coverage threshold')
    
    args = parser.parse_args()
    
    # Load coverage data
    try:
        data = load_coverage_data(args.json_file)
    except Exception as e:
        print(f"{RED}Error loading coverage data: {e}{NC}")
        sys.exit(1)
    
    # Calculate statistics
    crate_coverage, overall_coverage, covered_lines, total_lines = calculate_coverage_stats(data)
    
    # Print summary
    print_summary(crate_coverage, overall_coverage, covered_lines, total_lines, args.verbose)
    
    # Generate Markdown report if requested
    if args.markdown:
        report = generate_markdown_report(crate_coverage, overall_coverage, covered_lines, total_lines)
        with open(args.markdown, 'w') as f:
            f.write(report)
        print(f"\n{GREEN}Markdown report saved to: {args.markdown}{NC}")
    
    # Generate badge if requested
    if args.badge:
        badge_svg = generate_badge_svg(overall_coverage)
        with open(args.badge, 'w') as f:
            f.write(badge_svg)
        print(f"{GREEN}Coverage badge saved to: {args.badge}{NC}")
    
    # Save to history if requested
    if args.history:
        save_coverage_history(data)
        print(f"{GREEN}Coverage history updated{NC}")
    
    # Check minimum coverage threshold
    if args.min_coverage:
        if overall_coverage < args.min_coverage:
            print(f"\n{RED}ERROR: Coverage {overall_coverage:.2f}% is below minimum threshold of {args.min_coverage}%{NC}")
            sys.exit(1)
        else:
            print(f"\n{GREEN}SUCCESS: Coverage {overall_coverage:.2f}% meets minimum threshold of {args.min_coverage}%{NC}")


if __name__ == '__main__':
    main()