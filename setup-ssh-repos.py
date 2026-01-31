#!/usr/bin/env python3
"""
Post-deployment script to convert wickedjargon repos to SSH remotes.
Run this as a regular user (NOT with sudo) after deploy.py completes.
Ensure you've copied your SSH keys to the new system before running this script.
"""

import os
import sys
import subprocess
from pathlib import Path

# ANSI color codes
class Colors:
    RED = '\033[0;31m'
    GREEN = '\033[0;32m'
    YELLOW = '\033[1;33m'
    BLUE = '\033[0;34m'
    NC = '\033[0m'  # No Color

def print_section(msg):
    """Print a section header"""
    print(f"{Colors.BLUE}=== {msg} ==={Colors.NC}")

def print_success(msg):
    """Print success message"""
    print(f"{Colors.GREEN}✓ {msg}{Colors.NC}")

def print_error(msg):
    """Print error message"""
    print(f"{Colors.RED}✗ {msg}{Colors.NC}")

def print_warning(msg):
    """Print warning message"""
    print(f"{Colors.YELLOW}{msg}{Colors.NC}")

def check_not_root():
    """Verify we're not running as root"""
    if os.geteuid() == 0:
        print_error("This script should NOT be run with sudo or as root.")
        print("Please run as your regular user: ./setup-ssh-repos.py")
        sys.exit(1)

def find_ssh_key():
    """Find SSH key in ~/.ssh/"""
    ssh_dir = Path.home() / '.ssh'
    key_names = ['id_ed25519', 'id_rsa', 'id_ecdsa']

    for key_name in key_names:
        key_path = ssh_dir / key_name
        if key_path.exists():
            print_success(f"Found SSH key: {key_name}")
            return key_path

    return None

def test_github_access():
    """Test if SSH key has access to GitHub"""
    try:
        result = subprocess.run(
            ['ssh', '-T', 'git@github.com'],
            capture_output=True,
            text=True,
            timeout=10
        )
        # SSH to GitHub always returns 1, but with success message
        if 'successfully authenticated' in result.stderr:
            # Extract username from message like "Hi username! You've successfully..."
            for line in result.stderr.split('\n'):
                if 'Hi ' in line:
                    username = line.split('Hi ')[1].split('!')[0]
                    print_success("SSH key has access to GitHub")
                    print(f"  Authenticated as: {Colors.GREEN}{username}{Colors.NC}")
                    return True
        return False
    except (subprocess.TimeoutExpired, subprocess.CalledProcessError, Exception):
        return False

def get_remote_url(repo_path):
    """Get the origin remote URL for a git repo"""
    try:
        result = subprocess.run(
            ['git', '-C', str(repo_path), 'remote', 'get-url', 'origin'],
            capture_output=True,
            text=True,
            check=True,
            timeout=5
        )
        return result.stdout.strip()
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return None

def set_remote_url(repo_path, new_url):
    """Set the origin remote URL for a git repo"""
    try:
        subprocess.run(
            ['git', '-C', str(repo_path), 'remote', 'set-url', 'origin', new_url],
            check=True,
            capture_output=True,
            timeout=5
        )
        return True
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return False

def convert_https_to_ssh(https_url):
    """Convert HTTPS GitHub URL to SSH"""
    if https_url.startswith('https://github.com/'):
        return https_url.replace('https://github.com/', 'git@github.com:')
    return https_url

def is_wickedjargon_repo(url):
    """Check if URL is a wickedjargon repo"""
    return 'github.com/wickedjargon/' in url

def convert_repo(repo_path):
    """Convert a single repo from HTTPS to SSH"""
    repo_name = repo_path.name

    # Check if it's a git repo
    if not (repo_path / '.git').is_dir():
        return None

    # Get current remote URL
    current_url = get_remote_url(repo_path)
    if not current_url:
        return None

    # Check if it's a wickedjargon repo
    if not is_wickedjargon_repo(current_url):
        return None

    # Check if already SSH
    if current_url.startswith('git@github.com:'):
        print_success(f"Already SSH: {repo_name}")
        print(f"  {current_url}")
        print()
        return 'already_ssh'

    # Convert HTTPS to SSH
    ssh_url = convert_https_to_ssh(current_url)

    print_warning(f"Converting: {repo_name}")
    print(f"  From: {current_url}")
    print(f"  To:   {ssh_url}")

    if set_remote_url(repo_path, ssh_url):
        print_success("  Converted successfully")
        print()
        return 'converted'
    else:
        print_error("  Failed to convert")
        print()
        return 'failed'


def main():
    print_section("SSH Repository Setup Script")
    print()

    # Check 1: Not running as root
    check_not_root()

    # Check 2: Find SSH key
    print_section("Checking for SSH keys")
    ssh_key = find_ssh_key()

    if not ssh_key:
        print_error("No SSH key found in ~/.ssh/")
        print("Please generate an SSH key or copy your existing key to ~/.ssh/")
        print('To generate: ssh-keygen -t ed25519 -C "your_email@example.com"')
        sys.exit(1)

    print()

    # Check 3: Test GitHub access
    print_section("Testing SSH access to GitHub")
    if not test_github_access():
        print_error("SSH key does not have access to GitHub")
        print("Please add your SSH public key to your GitHub account:")
        print()
        print("1. Copy your public key:")
        print(f"   cat {ssh_key}.pub")
        print()
        print("2. Add it to GitHub at: https://github.com/settings/keys")
        sys.exit(1)

    print()

    # Check 4: Find and convert repos
    print_section("Looking for wickedjargon repositories")
    print()

    repos_converted = 0
    repos_skipped = 0
    repos_failed = 0

    # Wickedjargon repos to look for
    repo_names = ['dmenu', '.emacs.d']

    # Search locations
    search_paths = [
        Path.home() / '.local' / 'src',
        Path.home()
    ]

    for search_path in search_paths:
        if not search_path.exists():
            continue

        for repo_name in repo_names:
            repo_path = search_path / repo_name
            if repo_path.is_dir():
                result = convert_repo(repo_path)
                if result == 'converted':
                    repos_converted += 1
                elif result == 'already_ssh':
                    repos_skipped += 1
                elif result == 'failed':
                    repos_failed += 1

    # Summary
    print_section("Summary")
    print(f"Repositories converted to SSH: {Colors.GREEN}{repos_converted}{Colors.NC}")
    print(f"Repositories already using SSH: {Colors.GREEN}{repos_skipped}{Colors.NC}")
    if repos_failed > 0:
        print(f"Repositories that failed: {Colors.RED}{repos_failed}{Colors.NC}")
    print()

    if repos_converted > 0:
        print_success("Setup complete! Your wickedjargon repos now use SSH.")
    else:
        print_warning("No repositories were converted.")


if __name__ == '__main__':
    main()
