"""Tests for the img CLI (naming and validation logic)."""

import pytest

from helpers import get_bin_path, import_script

img = import_script(get_bin_path("img"))


class TestOutName:
    def test_tag_before_extension(self):
        assert img.out_name("photo.jpg", "800") == "photo-800.jpg"

    def test_new_extension(self):
        assert img.out_name("shot.heic", "", new_ext="jpg") == "shot.jpg"

    def test_path_preserved(self):
        assert img.out_name("/tmp/a/photo.png", "clean") == \
            "/tmp/a/photo-clean.png"

    def test_extension_with_dot(self):
        assert img.out_name("x.png", "", new_ext=".webp") == "x.webp"


class TestValidResizeSpec:
    def test_percent(self):
        assert img.valid_resize_spec("50%")

    def test_width(self):
        assert img.valid_resize_spec("1920")

    def test_geometry(self):
        assert img.valid_resize_spec("800x600")

    def test_garbage_rejected(self):
        assert not img.valid_resize_spec("big")
        assert not img.valid_resize_spec("50%%")
        assert not img.valid_resize_spec("x600")


class TestGuards:
    def test_missing_file_dies(self):
        with pytest.raises(SystemExit):
            img.check_files(["/nonexistent/nope.jpg"])

    def test_no_files_dies(self):
        with pytest.raises(SystemExit):
            img.check_files([])

    def test_existing_output_dies(self, tmp_path):
        dest = tmp_path / "photo-800.jpg"
        dest.write_text("x")
        with pytest.raises(SystemExit):
            img._guard_output(str(dest))
