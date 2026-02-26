#!/bin/sh

SCRIPT_DIR="$( cd "$( dirname "$0" )/.." >/dev/null 2>&1 && pwd )"
FAILED=0

# Find the target user
TARGET_USER="$1"

if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

if [ -z "$TARGET_USER" ]; then
    echo "Could not find a regular user to test."
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"

echo "Testing remaining system configurations..."

# 1. Test Tor Browser installation (if script exists)
if [ -f "$SCRIPT_DIR/scripts/install-debian-tor-browser.sh" ]; then
    tor_dir="$HOME_DIR/.local/src/tor-browser"
    
    if [ ! -d "$tor_dir" ]; then
        echo "  [FAIL] Tor Browser directory missing: $tor_dir"
        FAILED=1
    else
        # Verify ownership
        owner=$(stat -c '%U' "$tor_dir")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] Tor Browser directory is owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi
        
        # Verify executable existence
        if [ ! -f "$tor_dir/Browser/start-tor-browser" ]; then
            echo "  [FAIL] Tor Browser executable missing in $tor_dir"
            FAILED=1
        fi
        
        # Verify symlink in ~/.local/bin
        if [ ! -L "$HOME_DIR/.local/bin/tor-browser" ]; then
            if [ ! -f "$HOME_DIR/.local/bin/tor-browser" ]; then
                # Some scripts copy rather than symlink, check if it exists at all
                echo "  [FAIL] Tor Browser not linked or copied to ~/.local/bin/tor-browser"
                FAILED=1
            fi
        fi
    fi
fi

# 2. Test systemd services (if systemd is running)
if command -v systemctl >/dev/null 2>&1 && systemctl is-system-running >/dev/null 2>&1; then
    # Check keyd if package is installed
    if dpkg-query -W -f='${Status}' keyd 2>/dev/null | grep -q "install ok installed"; then
        if ! systemctl is-enabled keyd >/dev/null 2>&1; then
            echo "  [FAIL] keyd service is installed but not enabled"
            FAILED=1
        fi
        if ! systemctl is-active keyd >/dev/null 2>&1; then
            echo "  [FAIL] keyd service is enabled but not active/running"
            FAILED=1
        fi
    fi
    
    # Check kbdrate if package is installed
    if dpkg-query -W -f='${Status}' kbd 2>/dev/null | grep -q "install ok installed"; then
        if ! systemctl is-enabled kbdrate >/dev/null 2>&1; then
            # kbdrate might be a oneshot service that fails if run blindly, so we only strictly test enable status
            echo "  [FAIL] kbdrate service is installed but not enabled"
            FAILED=1
        fi
    fi
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "Remaining system configurations verified successfully."
exit 0
