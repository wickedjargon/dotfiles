#!/bin/sh
#
# install-claude-code - Install Claude Code via Anthropic's native installer
#
# Installs a self-contained binary to ~/.local/bin/claude. The binary
# self-updates after installation, so an existing install is left alone.
# Login (`claude` first run) remains a manual post-deploy step.
#

set -eu

INSTALL_URL="https://claude.ai/install.sh"

if [ -x "$HOME/.local/bin/claude" ]; then
    echo "Claude Code is already installed at ~/.local/bin/claude (self-updating), skipping."
    exit 0
fi

echo "Installing Claude Code..."
i=1
while [ "$i" -le 3 ]; do
    echo "Attempt $i/3..."
    if curl -fsSL "$INSTALL_URL" | bash; then
        echo "Done! Claude Code installed successfully."
        echo "Run 'claude' to log in."
        exit 0
    fi
    echo "Install failed, retrying in 2s..."
    sleep 2
    i=$((i + 1))
done

echo "Error: Failed to install Claude Code" >&2
exit 1
