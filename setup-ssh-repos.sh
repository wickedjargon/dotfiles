#!/bin/bash
# Post-deployment script to convert wickedjargon repos to SSH remotes
# Run this as a regular user (NOT with sudo) after deploy.py completes

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== SSH Repository Setup Script ===${NC}"
echo ""

# Check 1: Verify we're NOT running as root
if [ "$EUID" -eq 0 ]; then
    echo -e "${RED}ERROR: This script should NOT be run with sudo or as root.${NC}"
    echo "Please run as your regular user: ./setup-ssh-repos.sh"
    exit 1
fi

# Check 2: Look for SSH keys
echo -e "${BLUE}Checking for SSH keys...${NC}"
SSH_KEY=""
for key in id_ed25519 id_rsa id_ecdsa; do
    if [ -f ~/.ssh/$key ]; then
        SSH_KEY=~/.ssh/$key
        echo -e "${GREEN}✓ Found SSH key: $key${NC}"
        break
    fi
done

if [ -z "$SSH_KEY" ]; then
    echo -e "${RED}✗ No SSH key found in ~/.ssh/${NC}"
    echo "Please generate an SSH key or copy your existing key to ~/.ssh/"
    echo "To generate: ssh-keygen -t ed25519 -C \"your_email@example.com\""
    exit 1
fi

# Check 3: Verify SSH key has access to GitHub
echo ""
echo -e "${BLUE}Testing SSH access to GitHub...${NC}"
if ssh -T git@github.com 2>&1 | grep -q "successfully authenticated"; then
    echo -e "${GREEN}✓ SSH key has access to GitHub${NC}"
    GITHUB_USER=$(ssh -T git@github.com 2>&1 | grep -oP 'Hi \K[^!]+')
    echo -e "  Authenticated as: ${GREEN}$GITHUB_USER${NC}"
else
    echo -e "${RED}✗ SSH key does not have access to GitHub${NC}"
    echo "Please add your SSH public key to your GitHub account:"
    echo ""
    echo "1. Copy your public key:"
    echo "   cat ${SSH_KEY}.pub"
    echo ""
    echo "2. Add it to GitHub at: https://github.com/settings/keys"
    exit 1
fi

# Check 4: Find and convert wickedjargon repos
echo ""
echo -e "${BLUE}Looking for wickedjargon repositories...${NC}"
echo ""

REPOS_CONVERTED=0
REPOS_SKIPPED=0

# Wickedjargon repos we're looking for
WICKEDJARGON_REPOS=("emacs" "dwm" "dmenu" "dwmblocks")

# Search locations
SEARCH_PATHS=(
    ~/.local/src
    ~/.config
    ~/
)

convert_repo() {
    local repo_path="$1"
    local repo_name=$(basename "$repo_path")
    
    # Check if it's a git repository
    if [ ! -d "$repo_path/.git" ]; then
        return
    fi
    
    # Get current remote URL
    local current_url=$(git -C "$repo_path" remote get-url origin 2>/dev/null || echo "")
    
    if [ -z "$current_url" ]; then
        return
    fi
    
    # Check if it's a wickedjargon HTTPS repo
    if echo "$current_url" | grep -q "https://github.com/wickedjargon/"; then
        # Convert to SSH URL
        local ssh_url=$(echo "$current_url" | sed 's|https://github.com/|git@github.com:|')
        
        echo -e "${YELLOW}Converting: $repo_name${NC}"
        echo "  From: $current_url"
        echo "  To:   $ssh_url"
        
        # Change the remote URL
        if git -C "$repo_path" remote set-url origin "$ssh_url"; then
            echo -e "  ${GREEN}✓ Converted successfully${NC}"
            ((REPOS_CONVERTED++))
        else
            echo -e "  ${RED}✗ Failed to convert${NC}"
        fi
        echo ""
    elif echo "$current_url" | grep -q "git@github.com:wickedjargon/"; then
        echo -e "${GREEN}Already SSH: $repo_name${NC}"
        echo "  $current_url"
        echo ""
        ((REPOS_SKIPPED++))
    fi
}

# Search for wickedjargon repos
for search_path in "${SEARCH_PATHS[@]}"; do
    if [ ! -d "$search_path" ]; then
        continue
    fi
    
    for repo_name in "${WICKEDJARGON_REPOS[@]}"; do
        # Try direct path
        if [ -d "$search_path/$repo_name" ]; then
            convert_repo "$search_path/$repo_name"
        fi
        
        # Try with dot prefix (e.g., .emacs.d)
        if [ -d "$search_path/.$repo_name.d" ]; then
            convert_repo "$search_path/.$repo_name.d"
        fi
    done
done

# Summary
echo -e "${BLUE}=== Summary ===${NC}"
echo -e "Repositories converted to SSH: ${GREEN}$REPOS_CONVERTED${NC}"
echo -e "Repositories already using SSH: ${GREEN}$REPOS_SKIPPED${NC}"
echo ""

if [ $REPOS_CONVERTED -gt 0 ]; then
    echo -e "${GREEN}✓ Setup complete! Your wickedjargon repos now use SSH.${NC}"
else
    echo -e "${YELLOW}No repositories were converted.${NC}"
fi
