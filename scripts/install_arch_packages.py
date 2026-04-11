#!/usr/bin/env python3
"""
install_arch_packages.py

Installs official (pacman) and AUR packages on an Arch Linux system.
- Reads package lists from packages/arch-pacman-packages.txt and
  packages/arch-aur-packages.txt relative to the repo root.
- Bootstraps yay from source if not already installed.
- Skips packages that are already installed.
- Reports any packages that failed to install.

Must be run as a normal user (not root).
"""

import os
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PACMAN_PACKAGES_FILE = REPO_ROOT / "packages" / "arch-pacman-packages.txt"
AUR_PACKAGES_FILE = REPO_ROOT / "packages" / "arch-aur-packages.txt"

YAY_GIT_URL = "https://aur.archlinux.org/yay.git"


def read_package_list(filepath: Path) -> list[str]:
    """Read a package list file, returning non-empty, non-comment lines."""
    if not filepath.exists():
        print(f"ERROR: Package list not found: {filepath}")
        sys.exit(1)
    with open(filepath) as f:
        packages = []
        for line in f:
            stripped = line.strip()
            if stripped and not stripped.startswith("#"):
                packages.append(stripped)
    return packages


def is_installed(package: str) -> bool:
    """Check if a package is already installed via pacman."""
    result = subprocess.run(
        ["pacman", "-Q", package],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return result.returncode == 0


def install_pacman_packages(packages: list[str]) -> list[str]:
    """Install official repo packages via sudo pacman. Returns failed packages."""
    to_install = [p for p in packages if not is_installed(p)]

    if not to_install:
        print("All pacman packages are already installed.")
        return []

    print(f"\nInstalling {len(to_install)} pacman package(s):")
    for p in to_install:
        print(f"  - {p}")

    result = subprocess.run(
        ["sudo", "pacman", "-S", "--noconfirm", "--needed", *to_install]
    )

    if result.returncode != 0:
        # Figure out which ones actually failed by re-checking
        failed = [p for p in to_install if not is_installed(p)]
        return failed

    return []


def install_yay() -> bool:
    """Bootstrap yay from source via git clone + makepkg. Returns True on success."""
    if is_installed("yay"):
        print("yay is already installed.")
        return True

    print("\nBootstrapping yay from source...")
    with tempfile.TemporaryDirectory(prefix="yay-build-") as tmpdir:
        yay_dir = os.path.join(tmpdir, "yay")
        clone_result = subprocess.run(
            ["git", "clone", YAY_GIT_URL, yay_dir],
        )
        if clone_result.returncode != 0:
            print("ERROR: Failed to clone yay repository.")
            return False

        build_result = subprocess.run(
            ["makepkg", "-si", "--noconfirm"],
            cwd=yay_dir,
        )
        if build_result.returncode != 0:
            print("ERROR: Failed to build/install yay.")
            return False

    print("yay installed successfully.")
    return True


def install_aur_packages(packages: list[str]) -> list[str]:
    """Install AUR packages via yay. Returns failed packages."""
    to_install = [p for p in packages if not is_installed(p)]

    if not to_install:
        print("All AUR packages are already installed.")
        return []

    print(f"\nInstalling {len(to_install)} AUR package(s):")
    for p in to_install:
        print(f"  - {p}")

    failed = []
    for pkg in to_install:
        result = subprocess.run(
            ["yay", "-S", "--noconfirm", "--needed", pkg]
        )
        if result.returncode != 0 or not is_installed(pkg):
            failed.append(pkg)

    return failed


def main():
    # Refuse to run as root
    if os.geteuid() == 0:
        print("ERROR: Do not run this script as root.")
        print("Run as a normal user; sudo will be invoked where needed.")
        sys.exit(1)

    print("=== Arch Package Installer ===\n")

    # --- Pacman packages ---
    print(f"Reading pacman packages from: {PACMAN_PACKAGES_FILE}")
    pacman_packages = read_package_list(PACMAN_PACKAGES_FILE)
    print(f"Found {len(pacman_packages)} package(s) in list.")

    pacman_failed = install_pacman_packages(pacman_packages)

    # --- yay ---
    yay_ok = install_yay()

    # --- AUR packages ---
    aur_failed: list[str] = []
    if yay_ok:
        print(f"\nReading AUR packages from: {AUR_PACKAGES_FILE}")
        aur_packages = read_package_list(AUR_PACKAGES_FILE)
        print(f"Found {len(aur_packages)} package(s) in list.")

        aur_failed = install_aur_packages(aur_packages)
    else:
        print("\nSkipping AUR packages because yay is not available.")
        aur_failed = ["(all AUR packages skipped — yay not installed)"]

    # --- Report ---
    all_failed = pacman_failed + aur_failed
    print("\n" + "=" * 40)
    if all_failed:
        print("The following packages FAILED to install:")
        for p in all_failed:
            print(f"  ✗ {p}")
        sys.exit(1)
    else:
        print("All packages installed successfully. ✓")


if __name__ == "__main__":
    main()
