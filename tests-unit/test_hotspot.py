"""Tests for the hotspot CLI (nmcli WiFi sharing)."""

import subprocess

import pytest

from helpers import get_bin_path, import_script

hotspot = import_script(get_bin_path("hotspot"))


def _completed(stdout="", returncode=0):
    return subprocess.CompletedProcess([], returncode, stdout, "")


# ── Credentials ─────────────────────────────────────────────────────


class TestGeneratePassword:
    def test_default_length_is_wpa_valid(self):
        pw = hotspot.generate_password()
        assert 8 <= len(pw) <= 63

    def test_no_ambiguous_characters(self):
        for _ in range(20):
            pw = hotspot.generate_password()
            assert not set(pw) & set("0O1lI")

    def test_passwords_differ(self):
        assert hotspot.generate_password() != hotspot.generate_password()


class TestQrEscape:
    def test_special_chars_escaped(self):
        assert hotspot.qr_escape('a;b,c:d"e') == r'a\;b\,c\:d\"e'

    def test_backslash_escaped_first(self):
        assert hotspot.qr_escape(r"a\;b") == r"a\\\;b"

    def test_plain_value_unchanged(self):
        assert hotspot.qr_escape("MyNet123") == "MyNet123"


class TestDefaultSsid:
    def test_ends_with_hotspot(self):
        assert hotspot.default_ssid().endswith("-hotspot")


# ── nmcli parsing ───────────────────────────────────────────────────


class TestGetProfile:
    def test_parses_fields(self, monkeypatch):
        stdout = (
            "802-11-wireless.ssid:MyNet\n"
            "802-11-wireless-security.psk:s3cretpass\n"
            "802-11-wireless.band:bg\n"
            "connection.interface-name:wlan0\n"
        )
        monkeypatch.setattr(hotspot, "run",
                            lambda cmd, timeout=30: _completed(stdout))
        profile = hotspot.get_profile()
        assert profile == {"ssid": "MyNet", "password": "s3cretpass",
                           "band": "bg", "iface": "wlan0"}

    def test_missing_profile_is_none(self, monkeypatch):
        monkeypatch.setattr(
            hotspot, "run",
            lambda cmd, timeout=30: _completed(returncode=10))
        assert hotspot.get_profile() is None


# ── on ──────────────────────────────────────────────────────────────


class TestCmdOn:
    def _setup(self, monkeypatch, calls, profile=None):
        monkeypatch.setattr(hotspot, "is_active", lambda: False)
        monkeypatch.setattr(hotspot, "get_wifi_device", lambda: "wlan0")
        monkeypatch.setattr(hotspot, "get_profile", lambda: profile)
        monkeypatch.setattr(
            hotspot, "run",
            lambda cmd, timeout=30: calls.append(cmd) or _completed())

    def test_creates_hotspot_with_given_credentials(self, monkeypatch):
        calls = []
        self._setup(monkeypatch, calls)
        hotspot.cmd_on(["MyNet", "s3cretpass"])
        create = next(c for c in calls if "hotspot" in c)
        assert "ssid" in create and "MyNet" in create
        assert "password" in create and "s3cretpass" in create
        assert "wlan0" in create

    def test_generates_password_when_omitted(self, monkeypatch, capsys):
        calls = []
        self._setup(monkeypatch, calls)
        hotspot.cmd_on(["MyNet"])
        create = next(c for c in calls if "hotspot" in c)
        pw = create[create.index("password") + 1]
        assert 8 <= len(pw) <= 63
        assert pw in capsys.readouterr().out

    def test_short_password_rejected(self, monkeypatch):
        calls = []
        self._setup(monkeypatch, calls)
        with pytest.raises(SystemExit):
            hotspot.cmd_on(["MyNet", "short"])
        assert not any("hotspot" in c for c in calls)

    def test_bare_on_reuses_saved_profile(self, monkeypatch):
        calls = []
        profile = {"ssid": "Old", "password": "oldpass123",
                   "band": "", "iface": "wlan0"}
        self._setup(monkeypatch, calls, profile=profile)
        hotspot.cmd_on([])
        # Reuse path brings the profile up instead of recreating it
        assert ["nmcli", "connection", "up", "Hotspot"] in calls
        assert not any("wifi" in c for c in calls)

    def test_already_active_warns(self, monkeypatch, capsys):
        monkeypatch.setattr(hotspot, "is_active", lambda: True)
        hotspot.cmd_on(["MyNet"])
        assert "already" in capsys.readouterr().out


# ── clients ─────────────────────────────────────────────────────────


class TestClientMacs:
    def test_parses_station_dump(self, monkeypatch):
        monkeypatch.setattr(hotspot, "which", lambda name: "/usr/sbin/iw")
        stdout = (
            "Station aa:bb:cc:dd:ee:ff (on wlan0)\n"
            "\tsignal: -40 dBm\n"
            "Station 11:22:33:44:55:66 (on wlan0)\n"
        )
        monkeypatch.setattr(hotspot, "run",
                            lambda cmd, timeout=30: _completed(stdout))
        macs = hotspot.client_macs("wlan0")
        assert macs == ["aa:bb:cc:dd:ee:ff", "11:22:33:44:55:66"]

    def test_missing_iw_returns_none(self, monkeypatch):
        monkeypatch.setattr(hotspot, "which", lambda name: None)
        assert hotspot.client_macs("wlan0") is None
