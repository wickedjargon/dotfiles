#!/bin/sh

# Verify: every tracked Arch package (pacman + AUR) is installed.
#
# This test only runs where pacman exists (i.e. inside the archbox
# container). On the Debian host it is a no-op and passes, so it is safe
# to include in the shared run_tests.sh sweep.

if ! command -v pacman >/dev/null 2>&1; then
    echo "pacman not present (not in archbox) — skipping."
    exit 0
fi

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
FAILED=0

check_list() {
    label="$1"
    file="$2"
    echo "Testing $label packages..."
    while IFS= read -r line; do
        pkg=$(echo "$line" | sed 's/#.*//' | xargs)
        [ -z "$pkg" ] && continue
        if ! pacman -Q "$pkg" >/dev/null 2>&1; then
            echo "  [FAIL] Missing package: $pkg"
            FAILED=1
        fi
    done < "$file"
}

check_list "pacman" "$SCRIPT_DIR/packages/arch-pacman-packages.txt"
check_list "AUR" "$SCRIPT_DIR/packages/arch-aur-packages.txt"

[ $FAILED -eq 1 ] && exit 1
echo "All Arch packages verified."
exit 0
