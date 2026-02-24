#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
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
FIREFOX_DIR="$HOME_DIR/.mozilla/firefox"
POLICIES_JSON=""
for p in "/usr/share/firefox-esr/distribution/policies.json" \
         "/usr/share/firefox/distribution/policies.json" \
         "/etc/firefox-esr/policies/policies.json" \
         "/etc/firefox/policies/policies.json"; do
    if [ -f "$p" ]; then
        POLICIES_JSON="$p"
        break
    fi
done
USERJS_SRC="$SCRIPT_DIR/firefox/firefox-user.js"

echo "Testing Firefox extensions configuration..."

if [ -z "$POLICIES_JSON" ]; then
    echo "  [FAIL] Firefox policies.json not found in standard system locations."
    FAILED=1
else
    # Simple check: Does it contain "ExtensionSettings" ensuring our script ran?
    if ! grep -q '"ExtensionSettings"' "$POLICIES_JSON"; then
        echo "  [FAIL] Firefox policies.json does not contain ExtensionSettings"
        FAILED=1
    fi
    # Check for a specific extension you know should be there, e.g. uBlock Origin
    if ! grep -q 'uBlock0@raymondhill.net' "$POLICIES_JSON"; then
        echo "  [FAIL] uBlock Origin not found in Firefox policies.json"
        FAILED=1
    fi
fi

echo "Testing Firefox user.js deployment..."

# Find all generated profile directories
profiles=$(find "$FIREFOX_DIR" -maxdepth 1 -type d -name "*.default*")

if [ -z "$profiles" ]; then
    echo "  [FAIL] No Firefox *.default* profiles found in $FIREFOX_DIR"
    FAILED=1
else
    for profile in $profiles; do
        userjs_dest="$profile/user.js"
        
        if [ ! -f "$userjs_dest" ]; then
            echo "  [FAIL] Missing user.js in profile: $profile"
            FAILED=1
        else
            # Verify ownership
            owner=$(stat -c '%U' "$userjs_dest")
            if [ "$owner" != "$TARGET_USER" ]; then
                echo "  [FAIL] $userjs_dest is owned by $owner instead of $TARGET_USER"
                FAILED=1
            fi
            
            # Verify content matches source firefox-user.js
            if [ -f "$USERJS_SRC" ]; then
                # Compare ignoring whitespace/newlines using md5sum or diff
                if ! cmp -s "$USERJS_SRC" "$userjs_dest"; then
                    echo "  [FAIL] Content of $userjs_dest does not exactly match $USERJS_SRC"
                    FAILED=1
                fi
            fi
        fi
    done
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "Firefox extensions and user.js verified successfully."
exit 0
