"""Tests for the dsync CLI (planning, manifest, git inventory, prune)."""

import os
import subprocess

import pytest

from helpers import get_bin_path, import_script

dsync = import_script(get_bin_path("dsync"))

OVERLAY_REL = dsync.OVERLAY_REL


def make_repo(root, files, ignored=None):
    """A git repo with an overlay containing `files` (rel -> content)."""
    overlay = root / OVERLAY_REL
    for rel, content in files.items():
        path = overlay / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
    if ignored:
        (root / ".gitignore").write_text("\n".join(ignored) + "\n")
    subprocess.run(["git", "init", "-q"], cwd=root, check=True)
    subprocess.run(["git", "-C", str(root), "config", "user.email", "t@t"],
                   check=True)
    subprocess.run(["git", "-C", str(root), "config", "user.name", "t"],
                   check=True)
    subprocess.run(["git", "-C", str(root), "add", "-A"], check=True)
    subprocess.run(["git", "-C", str(root), "commit", "-q", "-m", "init"],
                   check=True)
    return overlay


class TestExcluded:
    def test_theme_mode_dir_excluded(self):
        assert dsync.excluded(".config/theme-mode")
        assert dsync.excluded(".config/theme-mode/current")

    def test_similar_prefix_not_excluded(self):
        assert not dsync.excluded(".config/theme-mode-extra")

    def test_normal_paths_kept(self):
        assert not dsync.excluded(".bashrc")
        assert not dsync.excluded(".config/emacs/init.el")


class TestFilesDiffer:
    def test_identical(self, tmp_path):
        a, b = tmp_path / "a", tmp_path / "b"
        a.write_text("same")
        b.write_text("same")
        assert not dsync.files_differ(a, b)

    def test_different_content_same_size(self, tmp_path):
        a, b = tmp_path / "a", tmp_path / "b"
        a.write_text("aaaa")
        b.write_text("bbbb")
        assert dsync.files_differ(a, b)

    def test_different_size(self, tmp_path):
        a, b = tmp_path / "a", tmp_path / "b"
        a.write_text("short")
        b.write_text("much longer content")
        assert dsync.files_differ(a, b)

    def test_symlink_vs_file(self, tmp_path):
        a, b = tmp_path / "a", tmp_path / "b"
        a.write_text("x")
        b.symlink_to(a)
        assert dsync.files_differ(a, b)

    def test_symlinks_same_target(self, tmp_path):
        t = tmp_path / "t"
        t.write_text("x")
        a, b = tmp_path / "a", tmp_path / "b"
        a.symlink_to(t)
        b.symlink_to(t)
        assert not dsync.files_differ(a, b)


class TestManifest:
    def test_roundtrip(self, tmp_path):
        path = tmp_path / "state" / "manifest"
        dsync.write_manifest(path, [".bashrc", ".config/x"])
        assert dsync.read_manifest(path) == [".bashrc", ".config/x"]

    def test_missing_reads_empty(self, tmp_path):
        assert dsync.read_manifest(tmp_path / "nope") == []


class TestBuildPlan:
    def test_classification(self, tmp_path):
        overlay = tmp_path / "overlay"
        home = tmp_path / "home"
        for rel, content in ((".new", "n"), (".changed", "v2"),
                             (".same", "s")):
            p = overlay / rel
            p.parent.mkdir(parents=True, exist_ok=True)
            p.write_text(content)
        home.mkdir()
        (home / ".changed").write_text("v1")
        (home / ".same").write_text("s")
        (home / ".stale").write_text("old")

        plan = dsync.build_plan(overlay, home,
                                [".new", ".changed", ".same"],
                                [".changed", ".same", ".stale"])
        assert plan["new"] == [".new"]
        assert plan["changed"] == [".changed"]
        assert plan["same"] == [".same"]
        assert plan["stale"] == [".stale"]

    def test_stale_requires_presence_in_home(self, tmp_path):
        overlay = tmp_path / "overlay"
        home = tmp_path / "home"
        overlay.mkdir()
        home.mkdir()
        plan = dsync.build_plan(overlay, home, [], [".already-gone"])
        assert plan["stale"] == []

    def test_excluded_manifest_entry_never_stale(self, tmp_path):
        overlay = tmp_path / "overlay"
        home = tmp_path / "home"
        overlay.mkdir()
        (home / ".config/theme-mode").mkdir(parents=True)
        (home / ".config/theme-mode/current").write_text("dark")
        plan = dsync.build_plan(overlay, home, [],
                                [".config/theme-mode/current"])
        assert plan["stale"] == []


class TestCopyFile:
    def test_creates_parents_and_preserves_mode(self, tmp_path):
        src = tmp_path / "src" / "bin" / "tool"
        src.parent.mkdir(parents=True)
        src.write_text("#!/bin/sh\n")
        src.chmod(0o755)
        dst = tmp_path / "home" / "bin" / "tool"
        dsync.copy_file(src, dst)
        assert dst.read_text() == "#!/bin/sh\n"
        assert os.access(dst, os.X_OK)

    def test_replaces_symlink_with_file(self, tmp_path):
        target = tmp_path / "target"
        target.write_text("t")
        src = tmp_path / "src"
        src.write_text("real")
        dst = tmp_path / "dst"
        dst.symlink_to(target)
        dsync.copy_file(src, dst)
        assert not dst.is_symlink()
        assert dst.read_text() == "real"


class TestOverlayFiles:
    def test_tracked_untracked_and_ignored(self, tmp_path):
        overlay = make_repo(tmp_path, {".bashrc": "b"},
                            ignored=["*.pyc"])
        (overlay / ".config").mkdir()
        (overlay / ".config/new.conf").write_text("untracked")
        (overlay / "junk.pyc").write_text("ignored")
        assert dsync.overlay_files(tmp_path) == \
            [".bashrc", ".config/new.conf"]

    def test_excluded_paths_filtered(self, tmp_path):
        make_repo(tmp_path, {".bashrc": "b",
                             ".config/theme-mode/current": "dark"})
        assert dsync.overlay_files(tmp_path) == [".bashrc"]

    def test_deleted_from_worktree_skipped(self, tmp_path):
        overlay = make_repo(tmp_path, {".bashrc": "b", ".gone": "x"})
        (overlay / ".gone").unlink()
        assert dsync.overlay_files(tmp_path) == [".bashrc"]


class TestParseDeletedPaths:
    LOG = "\n".join([
        "",
        f"{OVERLAY_REL}/.config/old.conf",
        f"{OVERLAY_REL}/.local/bin/gone-tool",
        f"{OVERLAY_REL}/.readded",
        "unrelated/other.txt",
        "",
    ])

    def test_extracts_overlay_deletions(self):
        rels = dsync.parse_deleted_paths(self.LOG, set())
        assert rels == [".config/old.conf", ".local/bin/gone-tool",
                        ".readded"]

    def test_readded_files_not_candidates(self):
        rels = dsync.parse_deleted_paths(self.LOG, {".readded"})
        assert ".readded" not in rels

    def test_paths_outside_overlay_ignored(self):
        rels = dsync.parse_deleted_paths(self.LOG, set())
        assert "unrelated/other.txt" not in rels
        assert "other.txt" not in rels


class TestDeletedPaths:
    def test_git_history_deletion_found(self, tmp_path):
        overlay = make_repo(tmp_path, {".bashrc": "b", ".old": "x"})
        (overlay / ".old").unlink()
        subprocess.run(["git", "-C", str(tmp_path), "add", "-A"],
                       check=True)
        subprocess.run(["git", "-C", str(tmp_path), "commit", "-q",
                        "-m", "remove .old"], check=True)
        current = dsync.overlay_files(tmp_path)
        assert dsync.deleted_paths(tmp_path, current) == [".old"]
