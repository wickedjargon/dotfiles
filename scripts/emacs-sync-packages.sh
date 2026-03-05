#!/usr/bin/env bash
# emacs-sync-packages.sh — Pre-clone all straight.el packages
#
# Usage: ./scripts/emacs-sync-packages.sh
#
# Runs an Emacs batch process that parses init.el, resolves all
# straight.el package recipes, and clones each repository with
# error handling. Packages that fail (e.g. due to a host outage)
# are skipped and reported at the end.
#
# This ensures that when you actually start Emacs, no cloning
# is needed and a single host being down won't block your init.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SYNC_EL="${SCRIPT_DIR}/emacs-sync-packages.el"

if [ ! -f "$SYNC_EL" ]; then
    echo "Error: emacs-sync-packages.el not found at $SYNC_EL"
    exit 1
fi

if command -v emacs &>/dev/null; then
    EMACS_CMD="emacs"
elif command -v distrobox-host-exec &>/dev/null; then
    EMACS_CMD="distrobox-host-exec emacs"
else
    echo "Error: emacs not found (tried direct and distrobox-host-exec)"
    exit 1
fi

$EMACS_CMD --batch --load "$SYNC_EL" 2>&1

exit_code=$?
if [ $exit_code -ne 0 ]; then
    echo ""
    echo "Some packages failed to sync. Re-run this script when"
    echo "connectivity to the failed hosts is restored."
fi

exit $exit_code
