#!/bin/sh
#
# install-tor-browser - Download and install the latest stable Tor Browser
#
# Installs to ~/.local/src/tor-browser/ and creates a symlink at ~/.local/bin/tor-browser
#

set -eu

TOR_DIST_URL="https://www.torproject.org/dist/torbrowser"
INSTALL_DIR="$HOME/.local/src/tor-browser"
SYMLINK_PATH="$HOME/.local/bin/tor-browser"

# Get latest stable version (exclude alpha/beta versions with 'a' or 'b' suffix)
get_latest_version() {
    retries=3
    i=1
    while [ "$i" -le "$retries" ]; do
        if version=$(curl -sL "$TOR_DIST_URL/" | grep -oE '[0-9]+\.[0-9]+(\.[0-9]+)?/' | tr -d '/' | grep -vE '[ab]' | sort -V | tail -1); then
            if [ -n "$version" ]; then
                echo "$version"
                return 0
            fi
        fi
        echo "Failed to fetch version (attempt $i/$retries)" >&2
        sleep 2
        i=$((i + 1))
    done
    return 1
}

echo "Fetching latest Tor Browser version..."
VERSION=$(get_latest_version)

if [ -z "$VERSION" ]; then
    echo "Error: Could not determine latest version" >&2
    exit 1
fi

echo "Latest stable version: $VERSION"

# Map the machine architecture to Tor's Linux desktop build names. Tor only
# publishes x86_64 and i686 tarballs for Linux desktop; the aarch64/armv7
# builds in the dist listing are Android APKs, not desktop browsers. Fail
# loudly on anything else rather than silently installing a wrong binary.
case "$(uname -m)" in
    x86_64 | amd64) ARCH=x86_64 ;;
    i686 | i386) ARCH=i686 ;;
    *)
        echo "Error: no Tor Browser desktop build for architecture '$(uname -m)'." >&2
        echo "Tor only ships x86_64 and i686 tarballs for Linux." >&2
        exit 1
        ;;
esac

TARBALL="tor-browser-linux-$ARCH-$VERSION.tar.xz"
DOWNLOAD_URL="$TOR_DIST_URL/$VERSION/$TARBALL"
TEMP_FILE=$(mktemp --suffix=.tar.xz)

cleanup() {
    rm -f "$TEMP_FILE"
}
trap cleanup EXIT

echo "Downloading $TARBALL..."
download_success=false
i=1
while [ "$i" -le 3 ]; do
    echo "Attempt $i/3..."
    if curl -fL "$DOWNLOAD_URL" -o "$TEMP_FILE"; then
        download_success=true
        break
    fi
    echo "Download failed, retrying in 2s..."
    sleep 2
    i=$((i + 1))
done

if [ "$download_success" != "true" ]; then
    echo "Error: Failed to download Tor Browser" >&2
    exit 1
fi

echo "Extracting to $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR"
rm -rf "${INSTALL_DIR:?}"/*
tar -xJf "$TEMP_FILE" -C "$INSTALL_DIR" --strip-components=1

echo "Creating symlink at $SYMLINK_PATH..."
mkdir -p "$(dirname "$SYMLINK_PATH")"
ln -sf "$INSTALL_DIR/Browser/start-tor-browser" "$SYMLINK_PATH"

echo "Done! Tor Browser $VERSION installed successfully."
echo "Run 'tor-browser' to launch."
