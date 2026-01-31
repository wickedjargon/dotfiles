# Automates Firefox UI configuration:
# 1. Enables Compact Density

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

    # 2. Compact Density (Confirmed working)
    set_pref "browser.compactmode.show" "true"
    set_pref "browser.uidensity" "1"

    echo "  - Applied preferences for Compact Mode."
done

echo "Firefox UI configuration complete. Please restart Firefox."
