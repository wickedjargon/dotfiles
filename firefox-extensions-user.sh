#!/bin/sh
# Automated Firefox extension installation (User Level)
# Installs all extensions without requiring root privileges.

set -e

# The directory where user-level extensions are stored in Firefox
EXT_DIR="$HOME/.mozilla/extensions/{ec8030f7-c20a-464f-9b0e-13a3a9e97384}"
mkdir -p "$EXT_DIR"

echo "Downloading extensions..."

# uBlock Origin
curl -L -o "$EXT_DIR/uBlock0@raymondhill.net.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi"

# Dark Reader
curl -L -o "$EXT_DIR/addon@darkreader.org.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/darkreader/latest.xpi"

# Vimium
curl -L -o "$EXT_DIR/{d7742d87-e61d-4b78-b8a1-b469842139fa}.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi"

# SponsorBlock
curl -L -o "$EXT_DIR/sponsorBlocker@ajay.app.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/sponsorblock/latest.xpi"

# I Still Don't Care About Cookies
curl -L -o "$EXT_DIR/idcac-pub@guus.ninja.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/latest.xpi"

# Read Aloud
curl -L -o "$EXT_DIR/read-aloud@ken.loomis.org.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/read-aloud/latest.xpi"

# Defund Wikipedia
curl -L -o "$EXT_DIR/{ea2673bc-51ef-48a0-b81e-6b73e4f08cb5}.xpi" \
    "https://addons.mozilla.org/firefox/downloads/latest/defund-wikipedia/latest.xpi"

echo ""
echo "Firefox extensions downloaded to $EXT_DIR"
echo "They will be prompted for installation the next time you open Firefox:"
echo "  - uBlock Origin (ad blocker)"
echo "  - Dark Reader (dark mode)"
echo "  - Vimium (keyboard navigation)"
echo "  - SponsorBlock (skip YouTube sponsors)"
echo "  - I Still Don't Care About Cookies"
echo "  - Read Aloud"
echo "  - Defund Wikipedia"
echo ""
echo "Note: You must approve their installation inside Firefox."
