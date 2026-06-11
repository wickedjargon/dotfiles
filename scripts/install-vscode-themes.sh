#!/usr/bin/env bash
# install-vscode-themes.sh
#
# Install the VS Code color themes used by the `theme` light/dark switcher.
#
# Both "GitHub Dark Colorblind (Beta)" and "GitHub Light Colorblind (Beta)"
# (see ~/.local/bin/theme -> switch_vscode) ship in a single extension:
# GitHub.github-vscode-theme. Installing it provides both themes.
#
# Usage:
#   Run from inside the distrobox (Arch) container where visual-studio-code-bin
#   is installed, as a normal user (not root).
#   $ ./scripts/install-vscode-themes.sh
#
# Idempotent: skips installation if the extension is already present.

set -euo pipefail

EXTENSION="GitHub.github-vscode-theme"

if ! command -v code >/dev/null 2>&1; then
    echo "Error: 'code' not found on PATH."
    echo "Run this from inside the archbox container (visual-studio-code-bin)."
    exit 1
fi

# code --list-extensions reports ids lowercased.
if code --list-extensions 2>/dev/null | grep -qix "$EXTENSION"; then
    echo "$EXTENSION already installed — nothing to do."
    exit 0
fi

echo "Installing $EXTENSION (GitHub color themes) ..."
code --install-extension "$EXTENSION"
echo "Done. Themes 'GitHub Dark Colorblind (Beta)' and 'GitHub Light Colorblind (Beta)' are available."
