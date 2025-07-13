#!/bin/bash

# Find all files with parse() calls that aren't parse_flc
echo "Updating parse() calls to parse_flc()..."

# Update parse( to parse_flc( in all Rust files
find . -name "*.rs" -type f -not -path "./target/*" -exec grep -l "parse(" {} \; | while read -r file; do
    # Skip if it already uses parse_flc
    if grep -q "parse(" "$file" | grep -v "parse_flc"; then
        echo "Updating: $file"
        # Replace parse( with parse_flc( but be careful not to replace parse_flc(
        sed -i '' 's/\bparse(/parse_flc(/g' "$file"
        # Fix any double replacements (parse_flc_flc -> parse_flc)
        sed -i '' 's/parse_flc_flc(/parse_flc(/g' "$file"
    fi
done

echo "Done updating parse() calls!"

# Check if there are any remaining issues
echo -e "\nChecking for any remaining parse-related issues..."
grep -r "use fluentai_parser::parse" . --include="*.rs" --exclude-dir=target | head -5

echo -e "\nChecking for parse() calls that might have been missed..."
grep -r "\bparse(" . --include="*.rs" --exclude-dir=target | grep -v "parse_flc" | grep -v "parse_" | head -10