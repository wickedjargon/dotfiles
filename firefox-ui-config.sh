#!/bin/sh
# Automates Firefox UI configuration:
# 1. Enables Compact Density
# 2. Enables Native Vertical Tabs (Sidebar Revamp)

set -e

echo "Detected profiles in ~/.mozilla/firefox/"

for full_profile_path in ~/.mozilla/firefox/*.default* ~/.mozilla/firefox/*.default-release*; do
    [ -d "$full_profile_path" ] || continue
    echo "Configuring profile: $full_profile_path"

    USER_JS="$full_profile_path/user.js"
    echo "Updating $USER_JS..."
    
    # helper to append or replace preference
    set_pref() {
        pref_name="$1"
        pref_value="$2"
        if [ -f "$USER_JS" ] && grep -q "$pref_name" "$USER_JS"; then
            # Replace existing
            sed -i "s|^user_pref(\"$pref_name\".*|user_pref(\"$pref_name\", $pref_value);|" "$USER_JS"
        else
            # Append
            echo "user_pref(\"$pref_name\", $pref_value);" >> "$USER_JS"
        fi
    }

    # 1. Compact Density
    set_pref "browser.compactmode.show" "true"
    set_pref "browser.uidensity" "1"

    # 2. Native Vertical Tabs (Sidebar)
    set_pref "sidebar.revamp" "true"
    set_pref "sidebar.verticalTabs" "true"
    set_pref "sidebar.main.tools" "\"\""  # Cleanup tools menu if needed

    echo "  - Applied preferences for Compact Mode and Native Vertical Tabs."
done

echo "Firefox UI configuration complete. Please restart Firefox."
