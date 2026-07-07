"""Tests for the browser default-switcher script."""

import pytest
from helpers import get_bin_path, import_script

browser_mod = import_script(get_bin_path("browser"))

MIMEAPPS_CHROME = """\
[Default Applications]
text/x-python=emacs-client.desktop
x-scheme-handler/http=google-chrome.desktop
x-scheme-handler/https=google-chrome.desktop
x-scheme-handler/chrome=google-chrome.desktop
text/html=google-chrome.desktop
application/x-extension-htm=google-chrome.desktop
application/x-extension-html=google-chrome.desktop
application/x-extension-shtml=google-chrome.desktop
application/xhtml+xml=google-chrome.desktop
application/x-extension-xhtml=google-chrome.desktop
application/x-extension-xht=google-chrome.desktop
application/pdf=org.pwmt.zathura.desktop

[Added Associations]
x-scheme-handler/http=google-chrome.desktop;
x-scheme-handler/https=google-chrome.desktop;
x-scheme-handler/chrome=google-chrome.desktop;
text/html=google-chrome.desktop;
application/x-extension-htm=google-chrome.desktop;
application/x-extension-html=google-chrome.desktop;
application/x-extension-shtml=google-chrome.desktop;
application/xhtml+xml=google-chrome.desktop;
application/x-extension-xhtml=google-chrome.desktop;
application/x-extension-xht=google-chrome.desktop;
"""

ROFI_WEB_CHROME = (
    '#!/bin/sh\n'
    'selection=$(printf "Google Chrome\\nFirefox\\nFirefox Private\\nTor Browser'
    '\\nBookmarks" | rofi -dmenu -i -p "Web")\n'
)


@pytest.fixture
def fake_home(tmp_path, monkeypatch):
    """Fake HOME with mimeapps.list and rofi-web set to Chrome as default."""
    home = tmp_path / "home"
    config = home / ".config"
    config.mkdir(parents=True)
    (config / "mimeapps.list").write_text(MIMEAPPS_CHROME)

    scripts = home / ".local" / "scripts"
    scripts.mkdir(parents=True)
    (scripts / "rofi-web").write_text(ROFI_WEB_CHROME)

    monkeypatch.setattr(browser_mod, "HOME", str(home))
    monkeypatch.setattr(browser_mod, "MIMEAPPS", str(config / "mimeapps.list"))
    monkeypatch.setattr(browser_mod, "ROFI_WEB", str(scripts / "rofi-web"))
    return home


class TestReadCurrent:
    def test_reads_chrome(self, fake_home):
        assert browser_mod.read_current() == "chrome"

    def test_reads_firefox_after_switch(self, fake_home):
        browser_mod.apply_browser("firefox")
        assert browser_mod.read_current() == "firefox"

    def test_missing_file_returns_none(self, fake_home, monkeypatch):
        monkeypatch.setattr(
            browser_mod, "MIMEAPPS", str(fake_home / "nonexistent")
        )
        assert browser_mod.read_current() is None


class TestSwitchMimeapps:
    def test_switch_to_firefox_updates_all_handlers(self, fake_home):
        browser_mod.switch_mimeapps(browser_mod.BROWSERS["firefox"])
        content = (fake_home / ".config" / "mimeapps.list").read_text()
        assert "google-chrome.desktop" not in content
        # Default section: bare value; Added section: trailing semicolon kept.
        assert "x-scheme-handler/http=firefox-esr.desktop\n" in content
        assert "x-scheme-handler/http=firefox-esr.desktop;\n" in content

    def test_non_browser_entries_untouched(self, fake_home):
        browser_mod.switch_mimeapps(browser_mod.BROWSERS["firefox"])
        content = (fake_home / ".config" / "mimeapps.list").read_text()
        assert "text/x-python=emacs-client.desktop" in content
        assert "application/pdf=org.pwmt.zathura.desktop" in content

    def test_roundtrip_restores_original(self, fake_home):
        browser_mod.switch_mimeapps(browser_mod.BROWSERS["firefox"])
        browser_mod.switch_mimeapps(browser_mod.BROWSERS["chrome"])
        content = (fake_home / ".config" / "mimeapps.list").read_text()
        assert content == MIMEAPPS_CHROME

    def test_drift_warns(self, fake_home, capsys):
        (fake_home / ".config" / "mimeapps.list").write_text(
            "[Default Applications]\napplication/pdf=org.pwmt.zathura.desktop\n"
        )
        browser_mod.switch_mimeapps(browser_mod.BROWSERS["firefox"])
        assert "⚠" in capsys.readouterr().out


class TestSwitchRofiWeb:
    def test_switch_to_firefox_reorders_menu(self, fake_home):
        browser_mod.switch_rofi_web(browser_mod.BROWSERS["firefox"])
        content = (fake_home / ".local" / "scripts" / "rofi-web").read_text()
        assert r"Firefox\nFirefox Private\nGoogle Chrome\nTor Browser" in content

    def test_already_first_is_noop(self, fake_home, capsys):
        browser_mod.switch_rofi_web(browser_mod.BROWSERS["chrome"])
        content = (fake_home / ".local" / "scripts" / "rofi-web").read_text()
        assert content == ROFI_WEB_CHROME
        assert "already first" in capsys.readouterr().out

    def test_roundtrip_restores_original(self, fake_home):
        browser_mod.switch_rofi_web(browser_mod.BROWSERS["firefox"])
        browser_mod.switch_rofi_web(browser_mod.BROWSERS["chrome"])
        content = (fake_home / ".local" / "scripts" / "rofi-web").read_text()
        assert content == ROFI_WEB_CHROME

    def test_drift_warns(self, fake_home, capsys):
        (fake_home / ".local" / "scripts" / "rofi-web").write_text(
            '#!/bin/sh\nprintf "Something Else" | rofi -dmenu\n'
        )
        browser_mod.switch_rofi_web(browser_mod.BROWSERS["firefox"])
        assert "⚠" in capsys.readouterr().out
