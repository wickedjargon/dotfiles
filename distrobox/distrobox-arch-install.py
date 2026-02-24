#!/usr/bin/env python3
import subprocess
import os
import sys
from datetime import datetime

# Configuration
PACMAN_FILE = "distrobox-arch-pacman-pkglist.txt"
AUR_FILE = "distrobox-arch-aur-pkglist.txt"
LOG_FILE = f"/tmp/arch_install_{datetime.now().strftime('%Y%m%d_%H%M%S')}.log"

def log_message(message, to_file_only=False):
    """Prints to console and appends to the log file."""
    timestamp = datetime.now().strftime("%Y-%m-%d %H:%M:%S")
    formatted = f"[{timestamp}] {message}"
    if not to_file_only:
        print(message)
    with open(LOG_FILE, "a") as f:
        f.write(formatted + "\n")

def install_packages(file_path, command_prefix):
    """Generic installer that processes a list of packages."""
    if not os.path.exists(file_path):
        log_message(f"CRITICAL: File {file_path} not found. Skipping.")
        return

    with open(file_path, "r") as f:
        packages = [line.strip() for line in f if line.strip()]

    log_message(f"--- Starting installation from {file_path} ---")
    
    failed_packages = []

    for pkg in packages:
        log_message(f"Installing: {pkg}...", to_file_only=True)
        # Construct the command: e.g., sudo pacman -S --needed --noconfirm pkgname
        cmd = command_prefix + [pkg]
        
        try:
            # We use check=True to catch non-zero exit codes
            # capture_output=True keeps the console clean, but logs the errors
            result = subprocess.run(cmd, capture_output=True, text=True, check=True)
            log_message(f" [SUCCESS] {pkg}")
        except subprocess.CalledProcessError as e:
            log_message(f" [FAILED]  {pkg}")
            log_message(f"Error details for {pkg}:\n{e.stderr}", to_file_only=True)
            failed_packages.append(pkg)

    return failed_packages

def main():
    # Ensure log file starts clean
    with open(LOG_FILE, "w") as f:
        f.write(f"Archbox Restore Log - T430s\n{'='*30}\n")

    log_message(f"Log file initialized at: {LOG_FILE}")

    # 1. Install Native Packages (Pacman)
    # Using --noconfirm for automation; remove if you want to manual approve each
    pacman_cmd = ["sudo", "pacman", "-S", "--needed", "--noconfirm"]
    failed_pacman = install_packages(PACMAN_FILE, pacman_cmd)

    # 2. Install AUR Packages (Yay)
    # Note: yay should NOT be run as root/sudo
    yay_cmd = ["yay", "-S", "--needed", "--noconfirm"]
    failed_aur = install_packages(AUR_FILE, yay_cmd)

    # Final Summary
    log_message("\n" + "="*30)
    log_message("INSTALLATION SUMMARY")
    log_message("="*30)
    
    if not failed_pacman and not failed_aur:
        log_message("All systems nominal. All packages installed successfully.")
    else:
        if failed_pacman:
            log_message(f"Failed Pacman packages: {', '.join(failed_pacman)}")
        if failed_aur:
            log_message(f"Failed AUR packages: {', '.join(failed_aur)}")
        log_message(f"Check {LOG_FILE} for full error traces.")

if __name__ == "__main__":
    try:
        main()
    except KeyboardInterrupt:
        log_message("\nInstallation interrupted by user. Exiting.")
        sys.exit(1)