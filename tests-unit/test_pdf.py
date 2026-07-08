"""Tests for the pdf CLI (range parsing and output guards)."""

import pytest

from helpers import get_bin_path, import_script

pdf = import_script(get_bin_path("pdf"))


class TestParsePageRange:
    def test_single_page(self):
        assert pdf.parse_page_range("7") == (7, 7)

    def test_range(self):
        assert pdf.parse_page_range("5-10") == (5, 10)

    def test_reversed_range_invalid(self):
        assert pdf.parse_page_range("10-5") is None

    def test_zero_page_invalid(self):
        assert pdf.parse_page_range("0") is None

    def test_garbage_invalid(self):
        assert pdf.parse_page_range("five") is None
        assert pdf.parse_page_range("5-") is None


class TestMergeGuards:
    def test_existing_output_dies(self, tmp_path, monkeypatch):
        a = tmp_path / "a.pdf"
        b = tmp_path / "b.pdf"
        out = tmp_path / "out.pdf"
        for f in (a, b, out):
            f.write_text("x")
        with pytest.raises(SystemExit):
            pdf.cmd_merge([str(a), str(b), str(out)])

    def test_too_few_args_dies(self):
        with pytest.raises(SystemExit):
            pdf.cmd_merge(["only.pdf", "two.pdf"])

    def test_missing_input_dies(self, tmp_path):
        with pytest.raises(SystemExit):
            pdf.cmd_merge([str(tmp_path / "nope.pdf"),
                           str(tmp_path / "also-nope.pdf"),
                           str(tmp_path / "out.pdf")])


class TestCompressGuards:
    def test_bad_preset_dies(self, tmp_path, monkeypatch):
        monkeypatch.setattr(pdf, "which", lambda t: "/usr/bin/gs")
        f = tmp_path / "doc.pdf"
        f.write_text("x")
        with pytest.raises(SystemExit):
            pdf.cmd_compress([str(f), "maximum"])
