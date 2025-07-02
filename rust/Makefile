.PHONY: all build test bench bench-quick bench-full clean fmt check release

# Default target
all: build test

# Build all crates
build:
	cargo build --all-features

# Build release version
release:
	cargo build --release --all-features

# Run all tests
test:
	cargo test --all-features

# Run tests in release mode
test-release:
	cargo test --release --all-features

# Run quick benchmarks (for development)
bench-quick:
	cargo bench -- --warm-up-time 1 --measurement-time 2

# Run standard benchmarks
bench:
	cargo bench

# Run full benchmarks with detailed output
bench-full:
	cargo bench -- --output-format bencher | tee benchmark_results.txt
	@echo "\n=== Benchmark Summary ==="
	@grep -E "(Parser:|VM:|End-to-End:|Throughput:)" benchmark_results.txt || true

# Run comprehensive benchmark only
bench-comprehensive:
	cargo bench --bench comprehensive_benchmark -- --warm-up-time 1 --measurement-time 3

# Track performance over time
track-performance:
	./scripts/track_performance.py

# Format code
fmt:
	cargo fmt --all

# Run clippy
check:
	cargo clippy --all-targets --all-features -- -D warnings

# Clean build artifacts
clean:
	cargo clean
	rm -f benchmark_results.txt

# Install development tools
install-tools:
	cargo install cargo-criterion
	cargo install cargo-flamegraph
	cargo install cargo-tarpaulin

# Generate test coverage
coverage:
	cargo tarpaulin --out Html --all-features

# Profile parser performance
profile-parser:
	cargo flamegraph --bench parser -- --bench

# Profile VM performance  
profile-vm:
	cargo flamegraph --bench vm -- --bench

# Run integration tests
integration-test: release
	@echo "Running parser integration test..."
	@echo "(+ 1 2)" | cargo run --release --example test_parser
	@echo "\nRunning VM integration test..."
	cargo run --release --example test_vm

# Build documentation
doc:
	cargo doc --all-features --no-deps --open

# Run everything (build, test, bench)
ci: fmt check build test bench-quick

# Help target
help:
	@echo "Available targets:"
	@echo "  make build          - Build debug version"
	@echo "  make release        - Build release version"
	@echo "  make test           - Run all tests"
	@echo "  make bench          - Run standard benchmarks"
	@echo "  make bench-quick    - Run quick benchmarks"
	@echo "  make bench-full     - Run full benchmarks with output"
	@echo "  make track-performance - Track performance over time"
	@echo "  make fmt            - Format code"
	@echo "  make check          - Run clippy lints"
	@echo "  make clean          - Clean build artifacts"
	@echo "  make doc            - Build and open documentation"
	@echo "  make ci             - Run full CI pipeline"