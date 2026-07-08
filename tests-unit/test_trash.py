"""Tests for the trash CLI (XDG trash round trips)."""

import os

import pytest

from helpers import get_bin_path, import_script

trash = import_script(get_bin_path("trash"))


@pytest.fixture
def trash_home(tmp_path, monkeypatch):
    """Isolated trash directory plus a working dir with test files."""
    monkeypatch.setattr(trash, "TRASH_DIR", str(tmp_path / "Trash"))
    work = tmp_path / "work"
    work.mkdir()
    return work


class TestTrashAndList:
    def test_trash_moves_file_and_writes_info(self, trash_home):
        f = trash_home / "doc.txt"
        f.write_text("hello")
        trash.cmd_trash([str(f)])

        assert not f.exists()
        assert os.path.exists(os.path.join(trash.files_dir(), "doc.txt"))
        with open(os.path.join(trash.info_dir(),
                               "doc.txt.trashinfo")) as fh:
            content = fh.read()
        assert "[Trash Info]" in content
        assert f"Path={f}" in content

    def test_path_is_percent_encoded(self, trash_home):
        f = trash_home / "my file.txt"
        f.write_text("x")
        trash.cmd_trash([str(f)])
        with open(os.path.join(trash.info_dir(),
                               "my file.txt.trashinfo")) as fh:
            content = fh.read()
        assert "my%20file.txt" in content
        # and it decodes back
        original, date = trash.parse_trashinfo(content)
        assert original == str(f)
        assert date is not None

    def test_collision_gets_unique_name(self, trash_home):
        for text in ("one", "two"):
            f = trash_home / "doc.txt"
            f.write_text(text)
            trash.cmd_trash([str(f)])
        names = sorted(os.listdir(trash.files_dir()))
        assert names == ["doc.2.txt", "doc.txt"]

    def test_refuses_home_directory(self, trash_home):
        with pytest.raises(SystemExit):
            trash.cmd_trash([os.path.expanduser("~")])


class TestRestore:
    def test_round_trip(self, trash_home):
        f = trash_home / "doc.txt"
        f.write_text("hello")
        trash.cmd_trash([str(f)])
        trash.cmd_restore(["doc.txt"])
        assert f.read_text() == "hello"
        assert trash.entries() == []

    def test_refuses_overwriting_existing(self, trash_home):
        f = trash_home / "doc.txt"
        f.write_text("old")
        trash.cmd_trash([str(f)])
        f.write_text("new")  # something reappeared at the original path
        with pytest.raises(SystemExit):
            trash.cmd_restore(["doc.txt"])
        assert f.read_text() == "new"

    def test_unknown_name_dies(self, trash_home):
        with pytest.raises(SystemExit):
            trash.cmd_restore(["nope.txt"])


class TestEmpty:
    def test_empty_by_age_keeps_recent(self, trash_home):
        f = trash_home / "recent.txt"
        f.write_text("x")
        trash.cmd_trash([str(f)])
        trash.cmd_empty(["30"])  # nothing is 30 days old
        assert len(trash.entries()) == 1

    def test_directories_are_trashable(self, trash_home):
        d = trash_home / "project"
        d.mkdir()
        (d / "a.txt").write_text("x")
        trash.cmd_trash([str(d)])
        assert not d.exists()
        restored = trash.entries()
        assert restored[0][0] == "project"
