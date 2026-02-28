#!/bin/sh

# Verify: systemd services (keyd, kbdrate) and user systemd service files.
# Also verifies Tor Browser installation.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
HOME_DIR="/home/$TARGET_USER"
FAILED=0

# ── Tor Browser ──────────────────────────────────────────────────

echo "Testing Tor Browser..."

if [ -f "$SCRIPT_DIR/scripts/install-debian-tor-browser.sh" ]; then
    tor_dir="$HOME_DIR/.local/src/tor-browser"

    if [ ! -d "$tor_dir" ]; then
        echo "  [FAIL] Tor Browser directory missing: $tor_dir"
        FAILED=1
    else
        owner=$(stat -c '%U' "$tor_dir")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] $tor_dir owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi

        if [ ! -f "$tor_dir/Browser/start-tor-browser" ]; then
            echo "  [FAIL] Tor Browser executable missing"
            FAILED=1
        fi

        if [ ! -L "$HOME_DIR/.local/bin/tor-browser" ] && \
           [ ! -f "$HOME_DIR/.local/bin/tor-browser" ]; then
            echo "  [FAIL] tor-browser not in ~/.local/bin/"
            FAILED=1
        fi
    fi
fi

# ── System services ─────────────────────────────────────────────

echo "Testing system services..."

if command -v systemctl >/dev/null 2>&1 && systemctl is-system-running >/dev/null 2>&1; then
    # keyd
    if dpkg-query -W -f='${Status}' keyd 2>/dev/null | grep -q "install ok installed"; then
        if ! systemctl is-enabled keyd >/dev/null 2>&1; then
            echo "  [FAIL] keyd service not enabled"
            FAILED=1
        fi
        if ! systemctl is-active keyd >/dev/null 2>&1; then
            echo "  [FAIL] keyd service not active"
            FAILED=1
        fi
    fi

    # kbdrate
    if dpkg-query -W -f='${Status}' kbd 2>/dev/null | grep -q "install ok installed"; then
        if ! systemctl is-enabled kbdrate >/dev/null 2>&1; then
            echo "  [FAIL] kbdrate service not enabled"
            FAILED=1
        fi
    fi
fi

# ── User systemd services ───────────────────────────────────────

echo "Testing user systemd services..."

user_systemd_dir="$HOME_DIR/.config/systemd/user"
if [ -d "$user_systemd_dir" ]; then
    fail_flag=$(mktemp)
    echo 0 > "$fail_flag"

    find "$user_systemd_dir" -type f -name "*.service" | while read -r svc; do
        owner=$(stat -c '%U' "$svc")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] $(basename "$svc") owned by $owner instead of $TARGET_USER"
            echo 1 > "$fail_flag"
        fi
    done

    svc_failed=$(cat "$fail_flag")
    rm -f "$fail_flag"
    [ "$svc_failed" -eq 1 ] && FAILED=1
fi

[ $FAILED -eq 1 ] && exit 1
echo "Services verified."
exit 0
