"""Tests for the update CLI (apt output parsing)."""

from helpers import get_bin_path, import_script

update = import_script(get_bin_path("update"))


class TestParseAptUpgradable:
    def test_counts_upgraded_and_new(self):
        text = ("Reading package lists...\n"
                "3 upgraded, 1 newly installed, 0 to remove and "
                "0 not upgraded.\n")
        assert update.parse_apt_upgradable(text) == 4

    def test_zero_pending(self):
        text = "0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.\n"
        assert update.parse_apt_upgradable(text) == 0

    def test_unparseable_is_none(self):
        assert update.parse_apt_upgradable("garbage") is None
