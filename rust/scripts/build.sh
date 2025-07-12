#!/bin/bash
# Build script for FluentAI Rust components

set -e

echo "Building FluentAI Rust components..."
echo "===================================="

# Build all Rust crates
echo "Building core libraries..."
cargo build --release

# Run tests
echo -e "\nRunning tests..."
cargo test --all

# Build Python bindings
echo -e "\nBuilding Python bindings..."
cd fluentai-py

# Check if maturin is installed
if ! command -v maturin &> /dev/null; then
    echo "Installing maturin..."
    pip install maturin
fi

# Build and install Python extension
maturin develop --release

cd ..

# Run benchmarks
echo -e "\nRunning benchmarks..."
cargo bench --no-run

echo -e "\nBuild complete! To run benchmarks:"
echo "  cargo bench"
echo -e "\nTo test Python bindings:"
echo "  cd .. && python tools/compare_parsers.py"