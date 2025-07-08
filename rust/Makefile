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
	@cd .. && ./coverage.sh

# Generate coverage with HTML report and open
coverage-open:
	@cd .. && ./coverage.sh -O

# Generate coverage summary only
coverage-summary:
	@cd .. && ./coverage.sh -s

# Generate coverage for specific crate
coverage-crate:
	@if [ -z "$(CRATE)" ]; then \
		echo "Usage: make coverage-crate CRATE=fluentai-core"; \
		exit 1; \
	fi
	@cd .. && ./coverage.sh -c $(CRATE)

# Generate coverage report in multiple formats
coverage-full:
	@cd .. && ./coverage.sh -f Html
	@cd .. && ./coverage.sh -f Json -o coverage-report/coverage.json
	@cd .. && ./coverage.sh -f Lcov -o coverage-report/lcov.info

# Generate coverage for CI (with minimum threshold)
coverage-ci:
	@cd .. && ./coverage.sh -f Lcov -m 75 -u

# Compare coverage with previous report
coverage-compare:
	@if [ -z "$(PREV)" ]; then \
		echo "Usage: make coverage-compare PREV=path/to/previous.json"; \
		exit 1; \
	fi
	@cd .. && ./coverage.sh -f Json -C $(PREV)

# Quick coverage check for core components
coverage-quick:
	cargo tarpaulin --config .tarpaulin.toml --profile fluentai-quick

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
	@echo ""
	@echo "Coverage targets:"
	@echo "  make coverage       - Generate HTML coverage report"
	@echo "  make coverage-open  - Generate and open HTML report"
	@echo "  make coverage-summary - Show coverage summary only"
	@echo "  make coverage-crate CRATE=name - Coverage for specific crate"
	@echo "  make coverage-full  - Generate reports in all formats"
	@echo "  make coverage-quick - Quick coverage for core components"
	@echo "  make coverage-ci    - CI coverage with minimum threshold"
	@echo "  make coverage-compare PREV=path - Compare with previous"