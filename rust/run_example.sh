#!/bin/bash
# Script to run FluentAI examples

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m' # No Color

echo -e "${BLUE}🚀 FluentAI Example Runner${NC}"
echo "========================="

# Check if cargo is available
if ! command -v cargo &> /dev/null; then
    echo -e "${RED}❌ Error: Cargo not found. Please install Rust.${NC}"
    exit 1
fi

# Build the CLI if needed
echo -e "${BLUE}📦 Building FluentAI CLI...${NC}"
cargo build --release -p fluentai-cli

if [ $? -ne 0 ]; then
    echo -e "${RED}❌ Build failed${NC}"
    exit 1
fi

# Path to the CLI binary
CLI_BIN="./target/release/fluentai"

# Check if example file is provided
if [ $# -eq 0 ]; then
    echo -e "${RED}❌ Please provide an example to run${NC}"
    echo "Usage: ./run_example.sh <example_name>"
    echo ""
    echo "Available examples:"
    echo "  - runnable_demo"
    echo "  - continuum_compilation_demo"
    echo ""
    echo "Example: ./run_example.sh runnable_demo"
    exit 1
fi

EXAMPLE_NAME=$1
EXAMPLE_FILE="examples/${EXAMPLE_NAME}.flc"

# Check if example exists
if [ ! -f "$EXAMPLE_FILE" ]; then
    echo -e "${RED}❌ Example file not found: $EXAMPLE_FILE${NC}"
    exit 1
fi

echo -e "${GREEN}✅ Running example: $EXAMPLE_NAME${NC}"
echo ""

# Run the example
$CLI_BIN run "$EXAMPLE_FILE"