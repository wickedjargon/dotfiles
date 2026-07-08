"""Tests for the font CLI (family parsing and file detection)."""

import subprocess

from helpers import get_bin_path, import_script

font = import_script(get_bin_path("font"))


def _completed(stdout="", returncode=0):
    return subprocess.CompletedProcess([], returncode, stdout, "")


class TestIsFontFile:
    def test_common_extensions(self):
        assert font.is_font_file("a.ttf")
        assert font.is_font_file("b.OTF")
        assert font.is_font_file("c.woff2")

    def test_non_fonts_rejected(self):
        assert not font.is_font_file("a.txt")
        assert not font.is_font_file("archive.zip")


class TestFontFamilies:
    def test_dedup_and_sort(self, monkeypatch):
        stdout = ("DejaVu Sans\n"
                  "JetBrains Mono,JetBrains Mono Regular\n"
                  "DejaVu Sans\n")
        monkeypatch.setattr(font, "run",
                            lambda cmd, timeout=60: _completed(stdout))
        assert font.font_families() == [
            "DejaVu Sans", "JetBrains Mono", "JetBrains Mono Regular"]

    def test_filter_is_case_insensitive(self, monkeypatch):
        stdout = "DejaVu Sans Mono\nArial\n"
        monkeypatch.setattr(font, "run",
                            lambda cmd, timeout=60: _completed(stdout))
        assert font.font_families(["MONO"]) == ["DejaVu Sans Mono"]

    def test_fc_list_failure_is_empty(self, monkeypatch):
        monkeypatch.setattr(
            font, "run",
            lambda cmd, timeout=60: _completed(returncode=1))
        assert font.font_families() == []
