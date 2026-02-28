#!/bin/sh

# Verify: apt package manager is in a healthy state after mass install.

echo "Testing apt health..."
FAILED=0

# Check for partially installed or broken packages
audit_output=$(dpkg --audit 2>&1)
if [ -n "$audit_output" ]; then
    echo "  [FAIL] dpkg --audit found issues:"
    echo "$audit_output" | while IFS= read -r line; do echo "    $line"; done
    FAILED=1
fi

# Verify dependency tree is consistent
if ! check_output=$(apt-get check 2>&1); then
    echo "  [FAIL] apt-get check found broken dependencies:"
    echo "$check_output" | while IFS= read -r line; do echo "    $line"; done
    FAILED=1
fi

[ $FAILED -eq 1 ] && exit 1
echo "Apt health verified."
exit 0
