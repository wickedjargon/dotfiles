#!/bin/sh
# Automated Firefox extension installation via Firefox Enterprise Policies
# Installs all extensions listed in README.md

set -e

# Create the policies directory for both standard and ESR
mkdir -p /etc/firefox/policies
mkdir -p /etc/firefox-esr/policies

# Create the policies.json file with all extensions
cat > /tmp/policies.json << 'EOF'
{
  "policies": {
    "ExtensionSettings": {
      "uBlock0@raymondhill.net": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi",
        "private_browsing": true
      },
      "addon@darkreader.org": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/darkreader/latest.xpi",
        "private_browsing": true
      },
      "{d7742d87-e61d-4b78-b8a1-b469842139fa}": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/vimium-ff/latest.xpi",
        "private_browsing": true
      },
      "sponsorBlocker@ajay.app": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/sponsorblock/latest.xpi",
        "private_browsing": true
      },
      "idcac-pub@guus.ninja": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/istilldontcareaboutcookies/latest.xpi",
        "private_browsing": true
      },
      "{ddc62400-f22d-4dd3-8b4a-05837de53c2e}": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/read-aloud/latest.xpi",
        "private_browsing": true
      },
      "{9d6e7f41-8d33-4145-a164-5ca4358c7960}": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/defund-wikipedia/latest.xpi",
        "private_browsing": true
      }
    }
  }
}
EOF

cp /tmp/policies.json /etc/firefox/policies/policies.json
cp /tmp/policies.json /etc/firefox-esr/policies/policies.json
rm /tmp/policies.json

echo "Firefox extensions policy installed to /etc/firefox/policies/ and /etc/firefox-esr/policies/"
echo "Extensions will be automatically installed on next Firefox launch:"
echo "  - uBlock Origin (ad blocker)"
echo "  - Dark Reader (dark mode for websites)"
echo "  - Vimium (keyboard navigation)"
echo "  - SponsorBlock (skip YouTube sponsors)"
echo "  - I Still Don't Care About Cookies (remove cookie banners)"
echo "  - Read Aloud (text-to-speech)"
echo "  - Defund Wikipedia (remove donation banners)"
echo ""
echo "Note: Extensions are enabled in private browsing mode by default"
