#!/usr/bin/env bash
# arch-sync-packages.sh — Report (or fix) drift between the tracked Arch
# package lists and what is actually installed in the archbox container.
#
# Run from INSIDE archbox (where pacman is available).
#
# Usage:
#     ./scripts/arch-sync-packages.sh           # report drift only (default)
#     ./scripts/arch-sync-packages.sh --write   # rewrite the list files from
#                                               # the live system, then review
#                                               # the git diff before committing
#
# Definitions:
#   - "Explicit" pacman packages  = pacman -Qqe  (minus foreign/AUR, minus lib32-*)
#   - "Foreign"  (AUR) packages   = pacman -Qqm  (minus *-debug, minus yay)
#
# lib32-* packages are owned by setup-steam.sh and are excluded here so they
# do not churn the pacman list.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." >/dev/null 2>&1 && pwd)"
PACMAN_FILE="$REPO_ROOT/packages/arch-pacman-packages.txt"
AUR_FILE="$REPO_ROOT/packages/arch-aur-packages.txt"

if ! command -v pacman >/dev/null 2>&1; then
    echo "ERROR: pacman not found. Run this inside the archbox container." >&2
    exit 1
fi

WRITE=0
[ "${1:-}" = "--write" ] && WRITE=1

# Strip comments/blank lines and sort a package-list file.
listed() { sed 's/#.*//' "$1" | awk 'NF' | sort -u; }

# Live explicit native packages: explicitly installed, from official repos,
# minus the lib32-* libraries owned by setup-steam.sh.
live_pacman() {
    pacman -Qqen | sort -u | grep -v '^lib32-'
}

# Live explicit AUR packages: explicitly installed AND foreign. Using -Qqem
# (not -Qqm) excludes dependency-only foreign packages such as gtk2/libtermkey.
# Also drop *-debug split packages and yay (bootstrapped by the installer).
live_aur() {
    pacman -Qqem | sort -u | grep -v -- '-debug$' | grep -vx 'yay'
}

report() {
    local name="$1" file="$2" live="$3" rc=0
    echo "=== $name ==="
    local missing extra
    missing=$(comm -23 <(echo "$live") <(listed "$file") || true)   # installed, not listed
    extra=$(comm -13 <(echo "$live") <(listed "$file") || true)     # listed, not installed
    if [ -n "$missing" ]; then
        echo "  Installed but NOT listed (add to $(basename "$file")):"
        echo "$missing" | sed 's/^/    + /'
        rc=1
    fi
    if [ -n "$extra" ]; then
        echo "  Listed but NOT installed (remove, or install later):"
        echo "$extra" | sed 's/^/    - /'
        rc=1
    fi
    [ $rc -eq 0 ] && echo "  In sync."
    return $rc
}

PAC_LIVE="$(live_pacman)"
AUR_LIVE="$(live_aur)"

if [ $WRITE -eq 1 ]; then
    {
        head -n 5 "$PACMAN_FILE"   # preserve the comment header
        echo "$PAC_LIVE"
    } > "$PACMAN_FILE.tmp" && mv "$PACMAN_FILE.tmp" "$PACMAN_FILE"
    {
        head -n 3 "$AUR_FILE"
        echo "$AUR_LIVE"
    } > "$AUR_FILE.tmp" && mv "$AUR_FILE.tmp" "$AUR_FILE"
    echo "Lists rewritten from the live system. Review with: git diff packages/"
    exit 0
fi

RC=0
report "pacman" "$PACMAN_FILE" "$PAC_LIVE" || RC=1
echo
report "AUR" "$AUR_FILE" "$AUR_LIVE" || RC=1
echo
if [ $RC -eq 0 ]; then
    echo "Package lists are in sync with archbox."
else
    echo "Drift detected. Re-run with --write to update the lists from live state."
fi
exit $RC
