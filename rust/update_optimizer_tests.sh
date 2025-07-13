#!/bin/bash

# Update all S-expression parser usage to FLC parser in optimizer tests
find fluentai-optimizer/tests -name "*.rs" -type f -exec sed -i '' \
    -e 's/use fluentai_parser::parse;/use fluentai_parser::parse_flc;/g' \
    -e 's/parse(/parse_flc(/g' \
    {} \;

echo "Updated parser imports in all optimizer test files"

# Show which files were updated
echo "Files that may need syntax updates:"
grep -l "parse_flc" fluentai-optimizer/tests/*.rs | xargs grep -l '"(let\|"(+\|"(-\|"(\*\|"(lambda\|"(if\|"(define\|"(print\|"(async\|"(chan'