#!/bin/bash

# FluentAI Code Coverage Script
# Generates comprehensive code coverage reports for the entire workspace

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
OUTPUT_DIR="coverage-report"
FORMAT="Html"
FEATURES="all-features"
VERBOSE=false
CRATE=""
OPEN_REPORT=false
SUMMARY_ONLY=false
COMPARE_WITH=""
UPLOAD_CODECOV=false
MIN_COVERAGE=0

# Function to display usage
usage() {
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Options:"
    echo "  -o, --output DIR        Output directory for coverage reports (default: coverage-report)"
    echo "  -f, --format FORMAT     Output format: Html, Xml, Json, Lcov (default: Html)"
    echo "  -c, --crate CRATE       Run coverage for specific crate only"
    echo "  -F, --features FEATURES Features to enable (default: all-features)"
    echo "  -v, --verbose           Enable verbose output"
    echo "  -O, --open              Open HTML report in browser after generation"
    echo "  -s, --summary           Generate summary only (no detailed reports)"
    echo "  -C, --compare PATH      Compare with previous coverage report"
    echo "  -u, --upload-codecov    Upload results to Codecov"
    echo "  -m, --min-coverage PCT  Minimum coverage percentage required (fails if not met)"
    echo "  -h, --help              Display this help message"
    echo ""
    echo "Examples:"
    echo "  $0                                    # Generate HTML report for all crates"
    echo "  $0 -c fluentai-core -O               # Generate and open report for fluentai-core"
    echo "  $0 -f Json -o coverage.json           # Generate JSON report"
    echo "  $0 -s                                 # Generate summary only"
    echo "  $0 -m 80                              # Require minimum 80% coverage"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        -o|--output)
            OUTPUT_DIR="$2"
            shift 2
            ;;
        -f|--format)
            FORMAT="$2"
            shift 2
            ;;
        -c|--crate)
            CRATE="$2"
            shift 2
            ;;
        -F|--features)
            FEATURES="$2"
            shift 2
            ;;
        -v|--verbose)
            VERBOSE=true
            shift
            ;;
        -O|--open)
            OPEN_REPORT=true
            shift
            ;;
        -s|--summary)
            SUMMARY_ONLY=true
            shift
            ;;
        -C|--compare)
            COMPARE_WITH="$2"
            shift 2
            ;;
        -u|--upload-codecov)
            UPLOAD_CODECOV=true
            shift
            ;;
        -m|--min-coverage)
            MIN_COVERAGE="$2"
            shift 2
            ;;
        -h|--help)
            usage
            exit 0
            ;;
        *)
            echo "Unknown option: $1"
            usage
            exit 1
            ;;
    esac
done

# Check if cargo-tarpaulin is installed
if ! command -v cargo-tarpaulin &> /dev/null; then
    echo -e "${RED}Error: cargo-tarpaulin is not installed${NC}"
    echo "Install it with: cargo install cargo-tarpaulin"
    exit 1
fi

# Change to rust directory
cd "$(dirname "$0")/rust"

# Create output directory
mkdir -p "$OUTPUT_DIR"

# Build command
CMD="cargo tarpaulin"

# Add format
CMD="$CMD --out $FORMAT"

# Add features
if [[ "$FEATURES" == "all-features" ]]; then
    CMD="$CMD --all-features"
elif [[ -n "$FEATURES" ]]; then
    CMD="$CMD --features $FEATURES"
fi

# Add crate filter
if [[ -n "$CRATE" ]]; then
    CMD="$CMD --package $CRATE"
else
    CMD="$CMD --workspace"
fi

# Add verbose flag
if [[ "$VERBOSE" == true ]]; then
    CMD="$CMD --verbose"
fi

# Add output directory for HTML
if [[ "$FORMAT" == "Html" ]]; then
    CMD="$CMD --output-dir $OUTPUT_DIR"
fi

# Additional options
CMD="$CMD --timeout 300 --follow-exec --ignore-panics"

echo -e "${BLUE}Running code coverage analysis...${NC}"
echo "Command: $CMD"

# Run coverage
if [[ "$SUMMARY_ONLY" == true ]]; then
    # For summary, we need JSON output
    TEMP_JSON=$(mktemp)
    cargo tarpaulin --out Json --all-features --workspace --timeout 300 --follow-exec --ignore-panics > "$TEMP_JSON" 2>&1
    
    # Parse and display summary
    echo -e "\n${GREEN}Coverage Summary:${NC}"
    python3 - "$TEMP_JSON" <<'EOF'
import json
import sys

with open(sys.argv[1], 'r') as f:
    data = json.load(f)

if 'files' in data:
    total_lines = 0
    covered_lines = 0
    crate_coverage = {}
    
    for file_info in data['files']:
        file_path = file_info['path']
        file_covered = sum(1 for line in file_info['covered'] if line > 0)
        file_total = len(file_info['covered'])
        
        # Extract crate name from path
        parts = file_path.split('/')
        if 'src' in parts:
            src_index = parts.index('src')
            if src_index > 0:
                crate_name = parts[src_index - 1]
                if crate_name not in crate_coverage:
                    crate_coverage[crate_name] = {'covered': 0, 'total': 0}
                crate_coverage[crate_name]['covered'] += file_covered
                crate_coverage[crate_name]['total'] += file_total
        
        total_lines += file_total
        covered_lines += file_covered
    
    # Print crate-by-crate coverage
    print(f"{'Crate':<30} {'Coverage':>10} {'Lines':>15}")
    print("-" * 58)
    
    for crate, stats in sorted(crate_coverage.items()):
        if stats['total'] > 0:
            coverage = (stats['covered'] / stats['total']) * 100
            print(f"{crate:<30} {coverage:>9.2f}% {stats['covered']:>7}/{stats['total']:<7}")
    
    print("-" * 58)
    overall_coverage = (covered_lines / total_lines * 100) if total_lines > 0 else 0
    print(f"{'TOTAL':<30} {overall_coverage:>9.2f}% {covered_lines:>7}/{total_lines:<7}")
    
    # Write to file for later use
    with open('coverage-summary.txt', 'w') as summary:
        summary.write(f"Overall Coverage: {overall_coverage:.2f}%\n")
        summary.write(f"Lines Covered: {covered_lines}/{total_lines}\n")
EOF
    
    rm "$TEMP_JSON"
else
    # Run full coverage
    eval "$CMD"
    
    # Generate additional formats if requested
    if [[ "$FORMAT" == "Html" && "$UPLOAD_CODECOV" == true ]]; then
        echo -e "\n${BLUE}Generating LCOV format for Codecov...${NC}"
        cargo tarpaulin --out Lcov --all-features --workspace --timeout 300 --follow-exec --ignore-panics > lcov.info
    fi
fi

# Compare with previous report if specified
if [[ -n "$COMPARE_WITH" && -f "$COMPARE_WITH" ]]; then
    echo -e "\n${BLUE}Comparing with previous coverage...${NC}"
    python3 - "$OUTPUT_DIR/tarpaulin-report.json" "$COMPARE_WITH" <<'EOF'
import json
import sys

def load_coverage(path):
    with open(path, 'r') as f:
        data = json.load(f)
    total = sum(len(f['covered']) for f in data['files'])
    covered = sum(sum(1 for l in f['covered'] if l > 0) for f in data['files'])
    return covered / total * 100 if total > 0 else 0

current = load_coverage(sys.argv[1])
previous = load_coverage(sys.argv[2])
diff = current - previous

color = '\033[0;32m' if diff >= 0 else '\033[0;31m'
nc = '\033[0m'
sign = '+' if diff >= 0 else ''

print(f"Previous coverage: {previous:.2f}%")
print(f"Current coverage:  {current:.2f}%")
print(f"Difference:        {color}{sign}{diff:.2f}%{nc}")
EOF
fi

# Check minimum coverage
if [[ $MIN_COVERAGE -gt 0 ]]; then
    # Extract coverage percentage from the output
    if [[ -f "coverage-summary.txt" ]]; then
        COVERAGE=$(grep "Overall Coverage:" coverage-summary.txt | awk '{print $3}' | sed 's/%//')
    else
        # Try to get from tarpaulin output
        COVERAGE=$(cargo tarpaulin --print-summary 2>&1 | grep -E "Coverage" | tail -1 | grep -oE "[0-9]+\.[0-9]+" | head -1)
    fi
    
    if [[ -n "$COVERAGE" ]]; then
        if (( $(echo "$COVERAGE < $MIN_COVERAGE" | bc -l) )); then
            echo -e "${RED}ERROR: Coverage ${COVERAGE}% is below minimum threshold of ${MIN_COVERAGE}%${NC}"
            exit 1
        else
            echo -e "${GREEN}SUCCESS: Coverage ${COVERAGE}% meets minimum threshold of ${MIN_COVERAGE}%${NC}"
        fi
    fi
fi

# Upload to Codecov if requested
if [[ "$UPLOAD_CODECOV" == true && -f "lcov.info" ]]; then
    echo -e "\n${BLUE}Uploading to Codecov...${NC}"
    if command -v codecov &> /dev/null; then
        codecov -f lcov.info
    else
        echo -e "${YELLOW}Warning: codecov CLI not found. Skipping upload.${NC}"
        echo "Install with: pip install codecov"
    fi
fi

# Open report if requested
if [[ "$OPEN_REPORT" == true && "$FORMAT" == "Html" ]]; then
    REPORT_PATH="$OUTPUT_DIR/tarpaulin-report.html"
    if [[ -f "$REPORT_PATH" ]]; then
        echo -e "\n${GREEN}Opening coverage report...${NC}"
        if [[ "$OSTYPE" == "darwin"* ]]; then
            open "$REPORT_PATH"
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            xdg-open "$REPORT_PATH"
        else
            echo "Report generated at: $REPORT_PATH"
        fi
    fi
fi

echo -e "\n${GREEN}Coverage report generated successfully!${NC}"