#!/bin/sh

# Verify: Firefox extensions policy and user.js are deployed.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "$0")/.." >/dev/null 2>&1 && pwd)"
HOME_DIR="/home/$TARGET_USER"
FIREFOX_DIR="$HOME_DIR/.mozilla/firefox"
USERJS_SRC="$SCRIPT_DIR/firefox/firefox-user.js"
FAILED=0

echo "Testing Firefox extensions policy..."

POLICIES_JSON=""
for p in /usr/share/firefox-esr/distribution/policies.json \
         /usr/share/firefox/distribution/policies.json \
         /etc/firefox-esr/policies/policies.json \
         /etc/firefox/policies/policies.json; do
    if [ -f "$p" ]; then
        POLICIES_JSON="$p"
        break
    fi
done

if [ -z "$POLICIES_JSON" ]; then
    echo "  [FAIL] policies.json not found"
    FAILED=1
else
    if ! grep -q '"ExtensionSettings"' "$POLICIES_JSON"; then
        echo "  [FAIL] policies.json missing ExtensionSettings"
        FAILED=1
    fi
    if ! grep -q 'uBlock0@raymondhill.net' "$POLICIES_JSON"; then
        echo "  [FAIL] uBlock Origin not in policies.json"
        FAILED=1
    fi
fi

echo "Testing Firefox user.js..."

profiles=$(find "$FIREFOX_DIR" -maxdepth 1 -type d -name "*.default*" 2>/dev/null)

if [ -z "$profiles" ]; then
    echo "  [FAIL] No Firefox profiles found in $FIREFOX_DIR"
    FAILED=1
else
    for profile in $profiles; do
        userjs="$profile/user.js"
        if [ ! -f "$userjs" ]; then
            echo "  [FAIL] Missing user.js in $(basename "$profile")"
            FAILED=1
            continue
        fi

        owner=$(stat -c '%U' "$userjs")
        if [ "$owner" != "$TARGET_USER" ]; then
            echo "  [FAIL] $userjs owned by $owner instead of $TARGET_USER"
            FAILED=1
        fi

        if [ -f "$USERJS_SRC" ] && ! cmp -s "$USERJS_SRC" "$userjs"; then
            echo "  [FAIL] $userjs content does not match source"
            FAILED=1
        fi
    done
fi

[ $FAILED -eq 1 ] && exit 1
echo "Firefox verified."
exit 0
