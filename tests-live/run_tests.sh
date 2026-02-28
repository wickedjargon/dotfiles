#!/bin/sh

# Run all deployment verification tests.
# Usage: ./tests-live/run_tests.sh <username>

GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m'

TARGET_USER="$1"

if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

echo "Deployment verification for user '$TARGET_USER'"
echo "============================================================"

SCRIPT_DIR="$(cd "$(dirname "$0")" >/dev/null 2>&1 && pwd)"

passed=0
failed=0

for test_script in "$SCRIPT_DIR"/*.sh; do
    [ -f "$test_script" ] || continue
    # Skip self
    [ "$(basename "$test_script")" = "run_tests.sh" ] && continue

    test_name=$(basename "$test_script")
    printf "Running %s... " "$test_name"

    output=$(sh "$test_script" "$TARGET_USER" 2>&1)
    exit_code=$?

    if [ $exit_code -eq 0 ]; then
        printf '%sPASS%s\n' "$GREEN" "$NC"
        passed=$((passed + 1))
    else
        printf '%sFAIL%s\n' "$RED" "$NC"
        echo "  Output:"
        printf '%s\n' "$output" | while IFS= read -r line; do
            echo "    $line"
        done
        failed=$((failed + 1))
    fi
done

echo "=============================="
printf 'Results: %s%s passed%s, %s%s failed%s\n' \
    "$GREEN" "$passed" "$NC" "$RED" "$failed" "$NC"

if [ $failed -gt 0 ]; then
    exit 1
fi
exit 0
