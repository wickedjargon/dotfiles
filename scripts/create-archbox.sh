#!/usr/bin/env bash
# create-archbox.sh — Create (or recreate) the archbox Distrobox container.
#
# Run this from the HOST (not inside a container). It builds the container
# from archbox.ini, then prints the remaining package-install steps, which
# must be run from INSIDE archbox as your normal user.
#
# Usage:
#     ./scripts/create-archbox.sh          # create if missing
#     ./scripts/create-archbox.sh --replace # destroy and recreate

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." >/dev/null 2>&1 && pwd)"
MANIFEST="$REPO_ROOT/packages/archbox.ini"

GREEN='\033[0;32m'; YELLOW='\033[1;33m'; RED='\033[0;31m'; RESET='\033[0m'
info()  { printf "${GREEN}[INFO]${RESET}  %s\n" "$*"; }
warn()  { printf "${YELLOW}[WARN]${RESET}  %s\n" "$*"; }
error() { printf "${RED}[ERROR]${RESET} %s\n" "$*" >&2; }

if [ ! -f "$MANIFEST" ]; then
    error "Manifest not found: $MANIFEST"
    exit 1
fi

if ! command -v distrobox >/dev/null 2>&1; then
    error "distrobox not found on the host. Install it first."
    exit 1
fi

if [ -f /run/.containerenv ] || [ -f /.dockerenv ]; then
    error "This looks like inside a container. Run create-archbox.sh on the HOST."
    exit 1
fi

REPLACE_FLAG=""
if [ "${1:-}" = "--replace" ]; then
    REPLACE_FLAG="--replace"
    warn "Replacing any existing archbox container..."
fi

info "Assembling archbox from $MANIFEST ..."
distrobox assemble create --file "$MANIFEST" $REPLACE_FLAG

cat <<EOF

${GREEN}archbox container created.${RESET}

Next steps (run these from INSIDE archbox, as your normal user):

    distrobox enter archbox

    # 1. Install pacman + AUR packages (bootstraps yay):
    python3 ~/d/projects/dotfiles/scripts/install_arch_packages.py

    # 2. Install + export Steam (multilib lib32 libraries):
    ~/d/projects/dotfiles/scripts/setup-steam.sh

Your home directory is shared with the host, so your dotfiles are already
present inside the container — no separate overlay deploy is needed.
EOF
