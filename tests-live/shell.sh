#!/bin/sh

# Verify: login shell, .bashrc/.profile syntax, ~/.local/bin in PATH.

TARGET_USER="$1"
if [ -z "$TARGET_USER" ]; then
    echo "Usage: $0 <username>"
    exit 1
fi

HOME_DIR="/home/$TARGET_USER"
FAILED=0

echo "Testing shell environment..."

# Login shell
login_shell=$(getent passwd "$TARGET_USER" | cut -d: -f7)
if [ "$login_shell" != "/bin/bash" ]; then
    echo "  [FAIL] Login shell is $login_shell instead of /bin/bash"
    FAILED=1
fi

# .bashrc syntax
if [ -f "$HOME_DIR/.bashrc" ]; then
    if ! bashrc_err=$(sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.bashrc" 2>&1); then
        echo "  [FAIL] Syntax error in .bashrc"
        echo "$bashrc_err" | while IFS= read -r line; do echo "    $line"; done
        FAILED=1
    fi
fi

# .profile syntax
if [ -f "$HOME_DIR/.profile" ]; then
    if ! profile_err=$(sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.profile" 2>&1); then
        echo "  [FAIL] Syntax error in .profile"
        echo "$profile_err" | while IFS= read -r line; do echo "    $line"; done
        FAILED=1
    fi
fi

# ~/.local/bin in PATH (colon-delimited match to avoid substring false positives)
user_path=$(sudo -u "$TARGET_USER" bash -l -c 'echo $PATH' 2>/dev/null)
local_bin="$HOME_DIR/.local/bin"
found=false
IFS=':'
for dir in $user_path; do
    if [ "$dir" = "$local_bin" ]; then
        found=true
        break
    fi
done
unset IFS

if [ "$found" = false ]; then
    echo "  [FAIL] ~/.local/bin not in PATH"
    echo "         PATH: $user_path"
    FAILED=1
fi

[ $FAILED -eq 1 ] && exit 1
echo "Shell environment verified."
exit 0
