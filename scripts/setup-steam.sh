#!/usr/bin/env bash
# setup-steam.sh
#
# Install and configure Steam inside a Distrobox (Arch Linux) container.
#
# What this script does:
#   1. Enables the [multilib] repository (required for 32-bit Steam libs).
#   2. Installs Steam and its runtime dependencies via pacman.
#   3. Exports Steam to the host desktop so it appears in your app launcher.
#
# Usage:
#   Run from inside the distrobox container as a normal user (not root).
#   $ ./scripts/setup-steam.sh
#
# Prerequisites:
#   - An Arch Linux distrobox container.
#   - sudo access within the container.

set -euo pipefail

# ── Colours ──────────────────────────────────────────────────────────────────
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BOLD='\033[1m'
RESET='\033[0m'

info()  { printf "${GREEN}[INFO]${RESET}  %s\n" "$*"; }
warn()  { printf "${YELLOW}[WARN]${RESET}  %s\n" "$*"; }
error() { printf "${RED}[ERROR]${RESET} %s\n" "$*" >&2; }

# ── Sanity checks ───────────────────────────────────────────────────────────
if [[ $EUID -eq 0 ]]; then
    error "Do not run this script as root."
    echo  "Run as a normal user; sudo will be invoked where needed."
    exit 1
fi

if ! command -v pacman &>/dev/null; then
    error "pacman not found — this script is for Arch Linux only."
    exit 1
fi

# ── Step 1: Enable [multilib] ───────────────────────────────────────────────
# Steam is a 32-bit application and needs the multilib repo for lib32-* deps.
PACMAN_CONF="/etc/pacman.conf"

if grep -qE '^\[multilib\]' "$PACMAN_CONF"; then
    info "[multilib] is already enabled."
elif grep -qE '^#\[multilib\]' "$PACMAN_CONF"; then
    info "Uncommenting [multilib] repository..."
    sudo sed -i \
        '/^#\[multilib\]/,/^#Include/ s/^#//' \
        "$PACMAN_CONF"
    info "[multilib] uncommented in $PACMAN_CONF."
else
    info "Adding [multilib] repository..."
    printf '\n[multilib]\nInclude = /etc/pacman.d/mirrorlist\n' \
        | sudo tee -a "$PACMAN_CONF" > /dev/null
    info "[multilib] appended to $PACMAN_CONF."
fi

# ── Step 2: Sync repos ──────────────────────────────────────────────────────
info "Synchronising package databases..."
sudo pacman -Sy --noconfirm

# ── Step 3: Install Steam and dependencies ──────────────────────────────────
# Core Steam package + GPU drivers + Vulkan + common deps.
# Many of these are already in your arch-pacman-packages.txt; --needed
# ensures nothing is re-installed unnecessarily.
PACKAGES=(
    # Steam itself
    steam

    # GPU / Vulkan drivers (already in your pacman list — kept for clarity)
    mesa
    vulkan-intel
    vulkan-radeon

    # 32-bit GPU / Vulkan libraries (multilib)
    lib32-mesa
    lib32-vulkan-intel
    lib32-vulkan-radeon

    # 32-bit system libraries Steam commonly needs
    lib32-glibc
    lib32-gcc-libs
    lib32-libx11
    lib32-libxss
    lib32-alsa-plugins
    lib32-libpulse
    lib32-openal
    lib32-nss
    lib32-sqlite
    lib32-libcurl-gnutls

    # Fonts (many games need Windows-compatible fonts)
    ttf-liberation

    # Networking (for some Steam features / game downloads)
    xdg-utils
    xdg-user-dirs
)

info "Installing Steam and dependencies..."
sudo pacman -S --noconfirm --needed "${PACKAGES[@]}"

# ── Step 4: Export Steam to host via distrobox ───────────────────────────────
if command -v distrobox-export &>/dev/null; then
    info "Exporting Steam to the host desktop..."
    distrobox-export --app steam 2>/dev/null || warn "distrobox-export failed (non-fatal)."
else
    warn "distrobox-export not found — skipping host-side app export."
    warn "You can launch Steam from inside the container with: steam"
fi

# ── Step 5: Verify ──────────────────────────────────────────────────────────
echo ""
if pacman -Q steam &>/dev/null; then
    printf '%b%bSteam installed successfully.%b\n' "${GREEN}" "${BOLD}" "${RESET}"
    echo ""
    info "To launch Steam:"
    info "  • From the host: look for 'Steam' in your application launcher."
    info "  • From inside the container: run 'steam'"
    echo ""
    info "On first launch Steam will download its own runtime updates."
else
    error "Steam does not appear to be installed. Check the errors above."
    exit 1
fi
