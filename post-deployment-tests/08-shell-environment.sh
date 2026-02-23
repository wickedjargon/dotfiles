#!/usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." &> /dev/null && pwd )"
FAILED=0

TARGET_USER=$(awk -F':' -v "limit=1000" '{ if ( $3 >= limit && $3 < 65534 && $6 ~ /^\/home\// ) print $1 }' /etc/passwd | head -n 1)

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
if sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.bashrc" 2>/tmp/bashrc_err; then
    :
else
    echo "  [FAIL] Syntax error in .bashrc"
    while read -r line; do echo "    $line"; done < /tmp/bashrc_err
    FAILED=1
fi

if sudo -u "$TARGET_USER" bash -n "$HOME_DIR/.profile" 2>/tmp/profile_err; then
    :
else
    echo "  [FAIL] Syntax error in .profile"
    while read -r line; do echo "    $line"; done < /tmp/profile_err
    FAILED=1
fi

# 3. Check if ~/.local/bin is in PATH for the user via login shell
user_path_login=$(sudo -u "$TARGET_USER" bash -l -c 'echo $PATH' 2>/dev/null)
if [[ "$user_path_login" != *"$HOME_DIR/.local/bin"* ]]; then
    echo "  [FAIL] ~/.local/bin is not in PATH for $TARGET_USER"
    echo "         PATH is: $user_path_login"
    FAILED=1
fi

if [ $FAILED -eq 1 ]; then
    exit 1
fi

echo "Shell environment verified successfully."
exit 0
