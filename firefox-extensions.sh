#!/bin/sh
# Automated Firefox extension installation via Firefox Enterprise Policies
# Installs all extensions listed in README.md

set -e

# Create the policies directory
mkdir -p /etc/firefox/policies

# Create the policies.json file with all extensions
cat > /etc/firefox/policies/policies.json << 'EOF'
{
  "policies": {
    "ExtensionSettings": {
      "uBlock0@raymondhill.net": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi",
        "private_browsing": true
      },
      "{82ac2b38-ddfa-4cb5-b2f1-c39e1beb95bd}": {
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
      "read-aloud@ken.loomis.org": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/read-aloud/latest.xpi",
        "private_browsing": true
      },
      "{ea2673bc-51ef-48a0-b81e-6b73e4f08cb5}": {
        "installation_mode": "normal_installed",
        "install_url": "https://addons.mozilla.org/firefox/downloads/latest/defund-wikipedia/latest.xpi",
        "private_browsing": true
      }
    }
  }
}
EOF

echo "Firefox extensions policy installed to /etc/firefox/policies/policies.json"
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
