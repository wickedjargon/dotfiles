#!/bin/sh

# Verify: font configuration is applied and fonts are available.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

FAILED=0
echo "Testing font configuration..."

# 1. Check user.conf symlink
user_conf_link="/etc/fonts/conf.d/50-user.conf"
user_conf_target="/usr/share/fontconfig/conf.avail/50-user.conf"

if [ -f "$user_conf_target" ]; then
    if [ ! -L "$user_conf_link" ]; then
        echo "  [FAIL] $user_conf_link is not a symlink to $user_conf_target"
        FAILED=1
    elif [ "$(readlink -f "$user_conf_link")" != "$(readlink -f "$user_conf_target")" ]; then
        echo "  [FAIL] $user_conf_link points to wrong target"
        FAILED=1
    fi
fi

# 2. Check font availability in cache
if ! command -v fc-list >/dev/null 2>&1; then
    echo "  [FAIL] fc-list not found (fontconfig is missing)"
    FAILED=1
else
    # Check for expected fonts from packages list
    fonts="Noto Color Emoji,DejaVu Sans"
    IFS=','
    for font in $fonts; do
        if ! fc-list | grep -qi "$font"; then
            echo "  [FAIL] $font not found in fc-list"
            FAILED=1
        fi
    done
    unset IFS
fi

[ $FAILED -eq 1 ] && exit 1
echo "Fonts and fontconfig verified."
exit 0
