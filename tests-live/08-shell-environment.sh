#!/bin/sh

FAILED=0

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
echo "Testing shell environment..."

# 1. Check default login shell
login_shell=$(getent passwd "$TARGET_USER" | cut -d: -f7)
if [ "$login_shell" != "/bin/bash" ]; then
    echo "  [FAIL] Default login shell for $TARGET_USER is $login_shell instead of /bin/bash"
    FAILED=1
fi

# 2. Check if .bashrc and .profile have syntax errors
bashrc_err=$(sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.bashrc" 2>&1)
if [ $? -ne 0 ]; then
    echo "  [FAIL] Syntax error in .bashrc"
    echo "$bashrc_err" | while read -r line; do echo "    $line"; done
    FAILED=1
fi

profile_err=$(sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.profile" 2>&1)
if [ $? -ne 0 ]; then
    echo "  [FAIL] Syntax error in .profile"
    echo "$profile_err" | while read -r line; do echo "    $line"; done
    FAILED=1
fi

# 3. Check if ~/.local/bin is in PATH for the user via login shell
user_path_login=$(sudo -u "$TARGET_USER" bash -l -c 'echo $PATH' 2>/dev/null)
case "$user_path_login" in
    *"$HOME_DIR/.local/bin"*) ;;
    *)
        echo "  [FAIL] ~/.local/bin is not in PATH for $TARGET_USER"
        echo "         PATH is: $user_path_login"
        FAILED=1
        ;;
esac

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "Shell environment verified successfully."
exit 0
