#!/usr/bin/env bash

# Set text colors
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

echo "Starting Deployment Verification Tests"
echo "======================================"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"

passed=0
failed=0

# Iterate through all test scripts starting with a number
for test_script in $(ls "$SCRIPT_DIR"/[0-9]*.sh 2>/dev/null | sort); do
    test_name=$(basename "$test_script")
    echo -n "Running $test_name... "
    
    # Run the script and capture both stdout and stderr
    output=$(bash "$test_script" 2>&1)
    exit_code=$?
    
    if [ $exit_code -eq 0 ]; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
    else
        echo -e "${RED}FAIL${NC}"
        # Print output nicely indented
        echo "  Output:"
        while IFS= read -r line; do
            echo "    $line"
        done <<< "$output"
        ((failed++))
    fi
done

echo "======================================"
echo -e "Results: ${GREEN}$passed passed${NC}, ${RED}$failed failed${NC}"

if [ $failed -gt 0 ]; then
    exit 1
fi
exit 0
