#!/bin/bash

# Find all files with the old parse import (excluding target directory)
files=$(find . -name "*.rs" -type f -not -path "./target/*" -exec grep -l "use fluentai_parser::parse;" {} \;)

echo "Found $(echo "$files" | wc -l) files with old parse import"

# Remove the old import from each file
for file in $files; do
    echo "Removing old parse import from: $file"
    # Remove the line with the old import
    sed -i '' '/^use fluentai_parser::parse;$/d' "$file"
    
    # Also check if there are any remaining references to parse( that aren't parse_flc(
    if grep -q "parse(" "$file" | grep -v "parse_flc"; then
        echo "  WARNING: File still contains parse() calls that may need updating"
    fi
done

echo "Done! Removed old parse imports."

# Show any remaining parse() calls that might need attention
echo -e "\nFiles that may still have parse() calls to update:"
grep -r "parse(" . --include="*.rs" --exclude-dir=target | grep -v "parse_flc" | grep -v "parse_" | head -20