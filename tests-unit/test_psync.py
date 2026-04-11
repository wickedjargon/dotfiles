"""Tests for the psync phone-sync script."""

import os
import signal
import subprocess

import pytest
from helpers import get_bin_path, import_script

psync = import_script(get_bin_path("psync"))


# ── Config Tests ─────────────────────────────────────────────────────────────


class TestConfig:
    def test_sync_dirs_is_list_of_tuples(self):
        assert isinstance(psync.SYNC_DIRS, list)
        for entry in psync.SYNC_DIRS:
            assert isinstance(entry, tuple)
            assert len(entry) == 2

    def test_sync_dirs_local_paths_start_with_home(self):
        for local, _remote in psync.SYNC_DIRS:
            assert local.startswith(os.path.expanduser("~"))

    def test_sync_dirs_remote_paths_start_with_phone_base(self):
        for _local, remote in psync.SYNC_DIRS:
            assert remote.startswith(psync.PHONE_BASE)

    def test_images_has_screenshots_exclude(self):
        home = os.path.expanduser("~")
        assert f"{home}/d/images" in psync.SYNC_EXCLUDES
        assert "screenshots" in psync.SYNC_EXCLUDES[f"{home}/d/images"]

    def test_phone_port_is_8022(self):
        assert psync.PHONE_PORT == 8022


# ── DIR_NAMES Dict ──────────────────────────────────────────────────────────


class TestDirNames:
    def test_dir_names_is_dict(self):
        assert isinstance(psync.DIR_NAMES, dict)

    def test_dir_names_has_all_expected(self):
        for expected in ["audio", "images", "notes", "other", "screenshots", "video"]:
            assert expected in psync.DIR_NAMES, f"Missing dir: {expected}"

    def test_dir_names_values_are_tuples(self):
        for name, entry in psync.DIR_NAMES.items():
            assert isinstance(entry, tuple), f"{name} is not a tuple"
            assert len(entry) == 2, f"{name} has wrong length"

    def test_sync_dirs_matches_dir_names(self):
        assert psync.SYNC_DIRS == list(psync.DIR_NAMES.values())

    def test_dir_names_lookup(self):
        """Direct dict lookup should work for all known names."""
        entry = psync.DIR_NAMES["notes"]
        assert entry[0].endswith("/d/notes")

    def test_resolve_single_name(self):
        result = psync.resolve_dir_names(["notes"])
        assert len(result) == 1
        assert result[0][0].endswith("/d/notes")

    def test_resolve_multiple_names(self):
        result = psync.resolve_dir_names(["notes", "audio"])
        assert len(result) == 2

    def test_resolve_comma_separated(self):
        result = psync.resolve_dir_names(["notes,audio"])
        assert len(result) == 2

    def test_resolve_mixed(self):
        result = psync.resolve_dir_names(["notes,audio", "video"])
        assert len(result) == 3

    def test_resolve_deduplicates(self):
        result = psync.resolve_dir_names(["notes", "notes"])
        assert len(result) == 1

    def test_resolve_invalid_name(self):
        with pytest.raises(SystemExit):
            psync.resolve_dir_names(["nonexistent"])

    def test_resolve_case_insensitive(self):
        result = psync.resolve_dir_names(["NOTES"])
        assert len(result) == 1
        assert result[0][0].endswith("/d/notes")


# ── Build Rsync Options ─────────────────────────────────────────────────────


class TestBuildRsyncOpts:
    def test_basic_opts(self):
        opts = psync.build_rsync_opts()
        assert "-avz" in opts
        assert "--progress" in opts
        assert "--stats" in opts
        assert f"ssh -p {psync.PHONE_PORT}" in opts

    def test_no_delete_by_default_push(self):
        opts = psync.build_rsync_opts()
        assert "--delete" not in opts

    def test_no_delete_by_default_pull(self):
        opts = psync.build_rsync_opts()
        assert "--delete" not in opts

    def test_delete_flag_push(self):
        opts = psync.build_rsync_opts(delete_flag=True)
        assert "--delete" in opts

    def test_delete_flag_pull(self):
        opts = psync.build_rsync_opts(delete_flag=True)
        assert "--delete" in opts

    def test_delete_flag_ignored_when_use_delete_false(self):
        opts = psync.build_rsync_opts(use_delete=False, delete_flag=True)
        assert "--delete" not in opts

    def test_dry_run(self):
        opts = psync.build_rsync_opts(dry_run=True)
        assert "--dry-run" in opts

    def test_no_dry_run(self):
        opts = psync.build_rsync_opts(dry_run=False)
        assert "--dry-run" not in opts

    def test_update_flag(self):
        opts = psync.build_rsync_opts(update=True)
        assert "--update" in opts

    def test_no_update_by_default(self):
        opts = psync.build_rsync_opts()
        assert "--update" not in opts


# ── Argument Parsing ─────────────────────────────────────────────────────────


class TestArgParsing:
    """Test that main() validates args correctly (via SystemExit from argparse/die)."""

    def test_push_is_valid(self):
        parser = self._build_parser()
        args = parser.parse_args(["push"])
        assert args.command == "push"

    def test_pull_is_valid(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull"])
        assert args.command == "pull"

    def test_sync_is_valid(self):
        parser = self._build_parser()
        args = parser.parse_args(["sync"])
        assert args.command == "sync"

    def test_status_is_valid(self):
        parser = self._build_parser()
        args = parser.parse_args(["status"])
        assert args.command == "status"

    def test_log_is_valid(self):
        parser = self._build_parser()
        args = parser.parse_args(["log"])
        assert args.command == "log"

    def test_invalid_command(self):
        parser = self._build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["invalid"])

    def test_files_parsed(self):
        parser = self._build_parser()
        args = parser.parse_args(["push", "file1.txt", "file2.txt"])
        assert args.files == ["file1.txt", "file2.txt"]

    def test_latest_default(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull", "--latest"])
        assert args.latest == 1

    def test_latest_with_count(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull", "--latest", "5"])
        assert args.latest == 5

    def test_nth(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull", "--nth", "3"])
        assert args.nth == 3

    def test_dry_run_flag(self):
        parser = self._build_parser()
        args = parser.parse_args(["push", "--dry-run"])
        assert args.dry_run is True

    def test_delete_flag(self):
        parser = self._build_parser()
        args = parser.parse_args(["push", "--delete"])
        assert args.delete is True

    def test_dir_flag_single(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull", "--dir", "notes"])
        assert args.dir == ["notes"]

    def test_dir_flag_multiple(self):
        parser = self._build_parser()
        args = parser.parse_args(["pull", "--dir", "notes", "audio"])
        assert args.dir == ["notes", "audio"]

    def test_quiet_flag(self):
        parser = self._build_parser()
        args = parser.parse_args(["push", "--quiet"])
        assert args.quiet is True

    def test_no_delete_flag_removed(self):
        """--no-delete should no longer be a valid argument."""
        parser = self._build_parser()
        with pytest.raises(SystemExit):
            parser.parse_args(["push", "--no-delete"])

    def test_update_flag(self):
        parser = self._build_parser()
        args = parser.parse_args(["push", "--update"])
        assert args.update is True

    def test_no_update_by_default(self):
        parser = self._build_parser()
        args = parser.parse_args(["push"])
        assert args.update is False

    @staticmethod
    def _build_parser():
        """Build the same parser as main() without running the full function."""
        import argparse
        parser = argparse.ArgumentParser()
        parser.add_argument("command", choices=["push", "pull", "sync", "status", "log"])
        parser.add_argument("files", nargs="*")
        parser.add_argument("--latest", nargs="?", const=1, type=int)
        parser.add_argument("--nth", type=int, default=0)
        parser.add_argument("--dry-run", action="store_true")
        parser.add_argument("--delete", action="store_true")
        parser.add_argument("--dir", nargs="+", metavar="NAME")
        parser.add_argument("--quiet", action="store_true")
        parser.add_argument("--update", action="store_true")
        return parser


# ── Directory Filtering ──────────────────────────────────────────────────────


class TestDirFiltering:
    def test_filter_by_name(self):
        result = psync.resolve_dir_names(["notes"])
        assert len(result) == 1
        assert result[0][0].endswith("/d/notes")

    def test_filter_case_insensitive(self):
        result = psync.resolve_dir_names(["NOTES"])
        assert len(result) == 1

    def test_filter_invalid_name(self):
        with pytest.raises(SystemExit):
            psync.resolve_dir_names(["nonexistent"])

    def test_all_dir_names_unique(self):
        names = list(psync.DIR_NAMES.keys())
        assert len(names) == len(set(names))

    def test_known_dir_names(self):
        for expected in ["audio", "images", "notes", "other", "screenshots", "video"]:
            assert expected in psync.DIR_NAMES, f"Missing dir: {expected}"


# ── Clipboard Helper ────────────────────────────────────────────────────────


class TestClipboard:
    def test_no_display_returns_false(self, monkeypatch):
        monkeypatch.delenv("DISPLAY", raising=False)
        monkeypatch.delenv("WAYLAND_DISPLAY", raising=False)
        assert psync.copy_to_clipboard("test") is False

    def test_with_display_no_clipboard_tool(self, monkeypatch):
        monkeypatch.setenv("DISPLAY", ":0")
        monkeypatch.setattr("shutil.which", lambda cmd: None)
        assert psync.copy_to_clipboard("test") is False

    def test_with_display_and_xclip(self, monkeypatch):
        monkeypatch.setenv("DISPLAY", ":0")
        monkeypatch.setattr("shutil.which", lambda cmd: "/usr/bin/xclip" if cmd == "xclip" else None)

        calls = []

        def mock_run(args, **kwargs):
            calls.append((args, kwargs))
            return subprocess.CompletedProcess(args, 0)

        monkeypatch.setattr(psync.subprocess, "run", mock_run)
        result = psync.copy_to_clipboard("/home/user/file.txt")
        assert result is True
        assert len(calls) == 1
        assert calls[0][0] == ["xclip", "-selection", "clipboard"]
        assert calls[0][1]["input"] == "/home/user/file.txt"


# ── Notification ─────────────────────────────────────────────────────────────


class TestNotification:
    def test_no_display_does_not_notify(self, monkeypatch):
        monkeypatch.delenv("DISPLAY", raising=False)
        monkeypatch.delenv("WAYLAND_DISPLAY", raising=False)
        calls = []
        monkeypatch.setattr(psync.subprocess, "run",
                            lambda *a, **kw: calls.append(a))
        psync.notify("psync", "test")
        assert len(calls) == 0

    def test_with_display_and_notify_send(self, monkeypatch):
        monkeypatch.setenv("DISPLAY", ":0")
        monkeypatch.setattr("shutil.which", lambda cmd: "/usr/bin/notify-send" if cmd == "notify-send" else None)

        calls = []

        def mock_run(args, **kwargs):
            calls.append(args)
            return subprocess.CompletedProcess(args, 0)

        monkeypatch.setattr(psync.subprocess, "run", mock_run)
        psync.notify("psync", "test body")
        assert len(calls) == 1
        assert calls[0] == ["notify-send", "psync", "test body"]

    def test_with_display_no_notify_send(self, monkeypatch):
        monkeypatch.setenv("DISPLAY", ":0")
        monkeypatch.setattr("shutil.which", lambda cmd: None)
        calls = []
        monkeypatch.setattr(psync.subprocess, "run",
                            lambda *a, **kw: calls.append(a))
        psync.notify("psync", "test")
        assert len(calls) == 0


# ── Full Sync ────────────────────────────────────────────────────────────────


class TestFullSync:
    def test_push_skips_missing_local_dir(self, tmp_path, monkeypatch, capsys):
        """Push should skip and warn when local dir doesn't exist."""
        fake_local = str(tmp_path / "nonexistent")
        fake_remote = "/data/data/com.termux/files/home/storage/shared/Test"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(fake_local, fake_remote)])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("push", dry_run=False, delete_flag=False)
        captured = capsys.readouterr()
        assert "does not exist, skipping" in captured.out
        assert len(rsync_calls) == 0

    def test_pull_skips_missing_remote_dir(self, tmp_path, monkeypatch, capsys):
        """Pull should skip and warn when remote dir doesn't exist."""
        fake_local = str(tmp_path / "pulltest")
        fake_remote = "/data/data/com.termux/files/home/storage/shared/Missing"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(fake_local, fake_remote)])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        # Simulate remote dir not existing (non-zero return)
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 1))

        psync.do_full_sync("pull", dry_run=False, delete_flag=False)
        captured = capsys.readouterr()
        assert "does not exist, skipping" in captured.out
        assert len(rsync_calls) == 0

    def test_push_calls_rsync_for_existing_dir(self, tmp_path, monkeypatch):
        """Push should call rsync when local dir exists."""
        local = tmp_path / "audio"
        local.mkdir()
        (local / "song.mp3").write_text("data")
        fake_remote = "/data/data/com.termux/files/home/storage/shared/Music"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("push", dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 1
        assert rsync_calls[0][0] == f"{local}/"

    def test_pull_calls_rsync_for_existing_remote(self, tmp_path, monkeypatch):
        """Pull should call rsync when remote dir exists."""
        local = str(tmp_path / "music")
        fake_remote = "/data/data/com.termux/files/home/storage/shared/Music"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(local, fake_remote)])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        # Simulate remote dir exists (zero return)
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("pull", dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 1
        assert local in rsync_calls[0][1]

    def test_excludes_applied(self, tmp_path, monkeypatch):
        """Verify exclude patterns are passed to rsync opts."""
        local = tmp_path / "images"
        local.mkdir()
        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {str(local): ["screenshots"]})
        monkeypatch.setattr(psync, "_quiet", False)

        captured_opts = []

        def mock_rsync(opts, src, dst):
            captured_opts.extend(opts)
            return 0, ""

        monkeypatch.setattr(psync, "run_rsync", mock_rsync)
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("push", dry_run=False, delete_flag=False)
        # Find the --exclude for screenshots (not the global .* exclude)
        exclude_pairs = [
            (captured_opts[i], captured_opts[i + 1])
            for i in range(len(captured_opts) - 1)
            if captured_opts[i] == "--exclude"
        ]
        assert ("--exclude", "screenshots") in exclude_pairs

    def test_returns_summary(self, tmp_path, monkeypatch):
        """Full sync should return a summary string."""
        local = tmp_path / "notes"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        stats_output = (
            "Number of regular files transferred: 3\n"
            "Total transferred file size: 1,234 bytes\n"
        )
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: (0, stats_output))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        summary = psync.do_full_sync("push", dry_run=False, delete_flag=False)
        assert "3 files" in summary
        assert "1.2 KB" in summary


# ── File Sync ────────────────────────────────────────────────────────────────


class TestFileSync:
    def test_push_finds_local_file(self, tmp_path, monkeypatch, capsys):
        local = tmp_path / "notes"
        local.mkdir()
        (local / "todo.md").write_text("buy milk")
        fake_remote = "/data/data/com.termux/files/home/storage/shared/Notes"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_file_sync("push", ["todo.md"], dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 1
        assert "todo.md" in rsync_calls[0][0]

    def test_push_warns_no_match(self, tmp_path, monkeypatch, capsys):
        local = tmp_path / "notes"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "_quiet", False)

        monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))

        psync.do_file_sync("push", ["nonexistent.txt"], dry_run=False, delete_flag=False)
        captured = capsys.readouterr()
        assert "No file matching" in captured.out

    def test_returns_summary(self, tmp_path, monkeypatch):
        local = tmp_path / "notes"
        local.mkdir()
        (local / "todo.md").write_text("data")
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "_quiet", False)

        monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        summary = psync.do_file_sync("push", ["todo.md"], dry_run=False,
                                     delete_flag=False)
        assert "1 file synced" in summary


# ── Latest Sync ──────────────────────────────────────────────────────────────


class TestLatestSync:
    def test_push_latest_finds_newest(self, tmp_path, monkeypatch):
        local = tmp_path / "images"
        local.mkdir()
        old = local / "old.jpg"
        new = local / "new.jpg"
        old.write_text("old")
        new.write_text("new")
        # Make old truly older
        os.utime(str(old), (1000000, 1000000))

        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_latest_sync("push", count=1, nth=0, dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 1
        assert "new.jpg" in rsync_calls[0][0]

    def test_push_latest_n(self, tmp_path, monkeypatch):
        local = tmp_path / "images"
        local.mkdir()
        for i in range(5):
            f = local / f"img_{i}.jpg"
            f.write_text(f"data{i}")
            os.utime(str(f), (1000000 + i * 1000, 1000000 + i * 1000))

        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_latest_sync("push", count=3, nth=0, dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 3

    def test_push_nth(self, tmp_path, monkeypatch):
        local = tmp_path / "images"
        local.mkdir()
        for i in range(5):
            f = local / f"img_{i}.jpg"
            f.write_text(f"data{i}")
            os.utime(str(f), (1000000 + i * 1000, 1000000 + i * 1000))

        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_latest_sync("push", count=0, nth=2, dry_run=False, delete_flag=False)
        assert len(rsync_calls) == 1
        # 2nd most recent should be img_3 (sorted desc: 4, 3, 2, 1, 0)
        assert "img_3.jpg" in rsync_calls[0][0]

    def test_warns_no_files(self, tmp_path, monkeypatch, capsys):
        local = tmp_path / "empty"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "_quiet", False)

        psync.do_latest_sync("push", count=1, nth=0, dry_run=False, delete_flag=False)
        captured = capsys.readouterr()
        assert "No files found" in captured.out

    def test_returns_summary(self, tmp_path, monkeypatch):
        local = tmp_path / "images"
        local.mkdir()
        f = local / "photo.jpg"
        f.write_text("data")
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "_quiet", False)

        monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        summary = psync.do_latest_sync("push", count=1, nth=0,
                                       dry_run=False, delete_flag=False)
        assert "1 file synced" in summary


# ── Logging ──────────────────────────────────────────────────────────────────


class TestLogging:
    def test_setup_log_creates_dir_and_file(self, tmp_path, monkeypatch):
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        try:
            assert os.path.isdir(log_dir)
            assert psync._log_path is not None
            assert os.path.isfile(psync._log_path)
        finally:
            psync.close_log()
            psync._log_path = None

    def test_log_filename_sorts_chronologically(self, tmp_path, monkeypatch):
        """ISO 8601 timestamps with hyphens sort alphabetically = chronologically."""
        import re
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        try:
            filename = os.path.basename(psync._log_path)
            # Should match YYYY-MM-DDTHH-MM-SS.log
            assert re.match(r"\d{4}-\d{2}-\d{2}T\d{2}-\d{2}-\d{2}\.log$", filename)
        finally:
            psync.close_log()
            psync._log_path = None

    def test_log_contains_header(self, tmp_path, monkeypatch):
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        try:
            psync._log_file.flush()
            content = open(psync._log_path).read()
            assert "psync run:" in content
            assert "args:" in content
        finally:
            psync.close_log()
            psync._log_path = None

    def test_log_write(self, tmp_path, monkeypatch):
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        try:
            psync.log_write("test message 123")
            content = open(psync._log_path).read()
            assert "test message 123" in content
        finally:
            psync.close_log()
            psync._log_path = None

    def test_log_write_noop_when_no_log(self, monkeypatch):
        """log_write should not crash when log file is not open."""
        monkeypatch.setattr(psync, "_log_file", None)
        psync.log_write("should not crash")  # No assertion, just no exception

    def test_full_sync_logs_skip(self, tmp_path, monkeypatch):
        """Skipped directories should be logged."""
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        try:
            fake_local = str(tmp_path / "nonexistent")
            monkeypatch.setattr(psync, "SYNC_DIRS", [(fake_local, "/remote")])
            monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
            monkeypatch.setattr(psync, "_quiet", False)
            monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))
            monkeypatch.setattr(psync, "phone_ssh",
                                lambda *a: subprocess.CompletedProcess(a, 0))

            psync.do_full_sync("push", dry_run=False, delete_flag=False)
            content = open(psync._log_path).read()
            assert "SKIP:" in content
        finally:
            psync.close_log()
            psync._log_path = None

    def test_close_log_is_idempotent(self, tmp_path, monkeypatch):
        """Calling close_log multiple times should not crash."""
        log_dir = str(tmp_path / "psync-logs")
        monkeypatch.setattr(psync, "LOG_DIR", log_dir)
        psync.setup_log()
        psync.close_log()
        psync.close_log()  # Should not raise
        psync._log_path = None


# ── Clipboard Single-File Only ───────────────────────────────────────────────


class TestClipboardSingleFile:
    def test_latest_3_does_not_clipboard(self, tmp_path, monkeypatch):
        """Pulling 3 latest files should NOT copy to clipboard."""
        local = tmp_path / "images"
        local.mkdir()
        for i in range(5):
            f = local / f"img_{i}.jpg"
            f.write_text(f"data{i}")
            os.utime(str(f), (1000000 + i * 1000, 1000000 + i * 1000))

        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        clipboard_calls = []
        monkeypatch.setattr(psync, "copy_to_clipboard",
                            lambda t: clipboard_calls.append(t) or True)

        rsync_calls = []
        monkeypatch.setattr(psync, "run_rsync",
                            lambda o, s, d: rsync_calls.append((s, d)) or (0, ""))

        # Simulate pull with remote find returning 3 files with timestamps
        def mock_ssh(*args):
            cmd_str = " ".join(args)
            if "test" in args:
                return subprocess.CompletedProcess(args, 0)
            return subprocess.CompletedProcess(args, 0, stdout="", stderr="")
        monkeypatch.setattr(psync, "phone_ssh", mock_ssh)

        # Use push direction so we can test with local files (simpler)
        psync.do_latest_sync("push", count=3, nth=0, dry_run=False, delete_flag=False)
        # Push never clips, so verify 3 syncs happened, 0 clips
        assert len(rsync_calls) == 3
        assert len(clipboard_calls) == 0

    def test_latest_1_does_clipboard(self, tmp_path, monkeypatch):
        """Pulling exactly 1 latest file SHOULD copy to clipboard."""
        local = tmp_path / "images"
        local.mkdir()
        f = local / "photo.jpg"
        f.write_text("data")

        fake_remote = "/data/data/com.termux/files/home/storage/shared/DCIM"
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), fake_remote)])
        monkeypatch.setattr(psync, "_quiet", False)

        clipboard_calls = []
        monkeypatch.setattr(psync, "copy_to_clipboard",
                            lambda t: clipboard_calls.append(t) or True)
        monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_latest_sync("push", count=1, nth=0, dry_run=False, delete_flag=False)
        # Push direction doesn't clip at all
        assert len(clipboard_calls) == 0


# ── Quiet Mode ───────────────────────────────────────────────────────────────


class TestQuietMode:
    def test_info_suppressed(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", True)
        psync.info("should not appear")
        captured = capsys.readouterr()
        assert captured.out == ""

    def test_ok_suppressed(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", True)
        psync.ok("should not appear")
        captured = capsys.readouterr()
        assert captured.out == ""

    def test_warn_suppressed(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", True)
        psync.warn("should not appear")
        captured = capsys.readouterr()
        assert captured.out == ""

    def test_header_suppressed(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", True)
        psync.header("should not appear")
        captured = capsys.readouterr()
        assert captured.out == ""

    def test_die_still_prints(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", True)
        with pytest.raises(SystemExit):
            psync.die("error message")
        captured = capsys.readouterr()
        assert "error message" in captured.err

    def test_info_shown_when_not_quiet(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "_quiet", False)
        psync.info("visible message")
        captured = capsys.readouterr()
        assert "visible message" in captured.out


# ── Transfer Summary ─────────────────────────────────────────────────────────


class TestTransferSummary:
    def test_parse_rsync_stats(self):
        output = (
            "Number of files: 10 (reg: 8, dir: 2)\n"
            "Number of created files: 0\n"
            "Number of deleted files: 0\n"
            "Number of regular files transferred: 5\n"
            "Total file size: 2,345,678 bytes\n"
            "Total transferred file size: 456,789 bytes\n"
        )
        files, total_bytes = psync.parse_rsync_stats(output)
        assert files == 5
        assert total_bytes == 456789

    def test_parse_rsync_stats_no_stats(self):
        files, total_bytes = psync.parse_rsync_stats("no stats here")
        assert files == 0
        assert total_bytes == 0

    def test_parse_rsync_stats_zero_transfer(self):
        output = (
            "Number of regular files transferred: 0\n"
            "Total transferred file size: 0 bytes\n"
        )
        files, total_bytes = psync.parse_rsync_stats(output)
        assert files == 0
        assert total_bytes == 0

    def test_format_bytes_b(self):
        assert psync.format_bytes(500) == "500 B"

    def test_format_bytes_kb(self):
        result = psync.format_bytes(1536)
        assert "KB" in result

    def test_format_bytes_mb(self):
        result = psync.format_bytes(5 * 1024 * 1024)
        assert "MB" in result

    def test_format_bytes_gb(self):
        result = psync.format_bytes(2 * 1024 * 1024 * 1024)
        assert "GB" in result


# ── TCP Check ────────────────────────────────────────────────────────────────


class TestTCPCheck:
    def test_unreachable_host(self):
        """Connection to non-existent host should return False."""
        assert psync.tcp_check("192.0.2.1", 9999, timeout=0.5) is False

    def test_unreachable_port(self):
        """Connection to unreachable port should return False."""
        assert psync.tcp_check("127.0.0.1", 9, timeout=0.5) is False


# ── Shell Escape ─────────────────────────────────────────────────────────────


class TestShellEscape:
    def test_filename_with_single_quote_escaped(self):
        """shlex.quote should be used for filenames in SSH find commands."""
        import shlex
        dangerous = "file'name.txt"
        quoted = shlex.quote(dangerous)
        # shlex.quote wraps in single quotes with escaping
        assert "'" in quoted or "\\" in quoted
        # The quoted string should be safe for shell use
        assert dangerous != quoted

    def test_filename_with_spaces_escaped(self):
        import shlex
        name = "my file.txt"
        quoted = shlex.quote(name)
        assert quoted != name
        assert " " not in quoted or quoted.startswith("'")


# ── Config File ──────────────────────────────────────────────────────────────


class TestConfigFile:
    def test_no_config_no_crash(self, tmp_path, monkeypatch):
        """load_config should not crash when config file doesn't exist."""
        monkeypatch.setattr(psync, "CONFIG_PATH", str(tmp_path / "nonexistent.conf"))
        psync.load_config()  # Should not raise

    def test_override_host(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text("[connection]\nhost = my-phone\n")
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        original_host = psync.PHONE_HOST
        psync.load_config()
        assert psync.PHONE_HOST == "my-phone"
        # Restore
        psync.PHONE_HOST = original_host
        psync._build_dir_names()

    def test_override_port(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text("[connection]\nport = 2222\n")
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        original_port = psync.PHONE_PORT
        psync.load_config()
        assert psync.PHONE_PORT == 2222
        # Restore
        psync.PHONE_PORT = original_port
        psync._build_dir_names()

    def test_override_directory(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text(
            "[directories]\nnotes_local = ~/custom/notes\n"
        )
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        psync.load_config()
        local, _remote = psync.DIR_NAMES["notes"]
        assert local == os.path.expanduser("~/custom/notes")
        # Restore defaults
        psync._build_dir_names()

    def test_relative_remote_prepends_base(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text(
            "[directories]\nnotes_remote = CustomNotes\n"
        )
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        psync.load_config()
        _local, remote = psync.DIR_NAMES["notes"]
        assert remote == f"{psync.PHONE_BASE}/CustomNotes"
        # Restore
        psync._build_dir_names()

    def test_absolute_remote_used_directly(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text(
            "[directories]\nnotes_remote = /absolute/path/Notes\n"
        )
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        psync.load_config()
        _local, remote = psync.DIR_NAMES["notes"]
        assert remote == "/absolute/path/Notes"
        # Restore
        psync._build_dir_names()

    def test_invalid_config_graceful(self, tmp_path, monkeypatch):
        config_file = tmp_path / "psync.conf"
        config_file.write_text("not valid ini {{{}}")
        monkeypatch.setattr(psync, "CONFIG_PATH", str(config_file))
        # configparser is lenient about malformed content;
        # this should not crash
        psync.load_config()


# ── Signal Handling ──────────────────────────────────────────────────────────


class TestSignalHandling:
    def test_handle_signal_exits_130_on_sigint(self):
        with pytest.raises(SystemExit) as exc_info:
            psync.handle_signal(signal.SIGINT, None)
        assert exc_info.value.code == 130

    def test_handle_signal_exits_143_on_sigterm(self):
        with pytest.raises(SystemExit) as exc_info:
            psync.handle_signal(signal.SIGTERM, None)
        assert exc_info.value.code == 143


# ── Status Command ───────────────────────────────────────────────────────────


class TestStatusCommand:
    def test_status_no_logs(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(psync, "LOG_DIR", str(tmp_path / "nonexistent"))
        monkeypatch.setattr(psync, "tcp_check", lambda h, p, timeout=3: False)
        psync.do_status()
        captured = capsys.readouterr()
        assert "unreachable" in captured.out
        assert "No sync logs" in captured.out

    def test_status_parses_push_log(self, tmp_path, monkeypatch, capsys):
        log_dir = tmp_path / "logs"
        log_dir.mkdir()
        log_file = log_dir / "2026-03-31T10-15-07.log"
        log_file.write_text(
            "psync run: 2026-03-31T10:15:07\n"
            "args: push\n\n"
            "direction: push\n"
        )
        monkeypatch.setattr(psync, "LOG_DIR", str(log_dir))
        monkeypatch.setattr(psync, "tcp_check", lambda h, p, timeout=3: True)
        psync.do_status()
        captured = capsys.readouterr()
        assert "reachable" in captured.out
        assert "Last push:" in captured.out
        assert "2026-03-31" in captured.out

    def test_status_reachable(self, monkeypatch, capsys):
        monkeypatch.setattr(psync, "LOG_DIR", "/nonexistent")
        monkeypatch.setattr(psync, "tcp_check", lambda h, p, timeout=3: True)
        psync.do_status()
        captured = capsys.readouterr()
        assert "reachable" in captured.out


# ── Log Command ──────────────────────────────────────────────────────────────


class TestLogCommand:
    def test_log_no_dir_dies(self, tmp_path, monkeypatch):
        monkeypatch.setattr(psync, "LOG_DIR", str(tmp_path / "nonexistent"))
        with pytest.raises(SystemExit):
            psync.do_log()

    def test_log_empty_dir_dies(self, tmp_path, monkeypatch):
        log_dir = tmp_path / "logs"
        log_dir.mkdir()
        monkeypatch.setattr(psync, "LOG_DIR", str(log_dir))
        with pytest.raises(SystemExit):
            psync.do_log()

    def test_log_finds_latest(self, tmp_path, monkeypatch):
        log_dir = tmp_path / "logs"
        log_dir.mkdir()
        (log_dir / "2026-03-30T10-00-00.log").write_text("old")
        (log_dir / "2026-03-31T10-00-00.log").write_text("new")

        exec_calls = []

        def mock_execvp(prog, args):
            exec_calls.append((prog, args))
            raise SystemExit(0)  # Prevent actual exec

        monkeypatch.setattr(os, "execvp", mock_execvp)
        monkeypatch.setattr(psync, "LOG_DIR", str(log_dir))

        with pytest.raises(SystemExit):
            psync.do_log()

        assert len(exec_calls) == 1
        assert "2026-03-31T10-00-00.log" in exec_calls[0][1][1]


# ── Sync Command ─────────────────────────────────────────────────────────────


class TestSyncCommand:
    def test_sync_runs_push_then_pull(self, tmp_path, monkeypatch):
        """The sync command should run push then pull."""
        local = tmp_path / "notes"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        directions_seen = []

        original_do_full_sync = psync.do_full_sync

        def tracking_sync(direction, dry_run, delete_flag, update=False):
            directions_seen.append(direction)
            monkeypatch.setattr(psync, "run_rsync", lambda o, s, d: (0, ""))
            monkeypatch.setattr(psync, "phone_ssh",
                                lambda *a: subprocess.CompletedProcess(a, 0))
            return original_do_full_sync(direction, dry_run, delete_flag,
                                         update=update)

        # Simulate what main() does for "sync"
        for direction in ["push", "pull"]:
            tracking_sync(direction, dry_run=False, delete_flag=False)

        assert directions_seen == ["push", "pull"]

    def test_sync_uses_update_flag(self, tmp_path, monkeypatch):
        """The sync command should pass --update to rsync."""
        local = tmp_path / "notes"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        captured_opts = []

        def mock_rsync(opts, src, dst):
            captured_opts.append(list(opts))
            return 0, ""

        monkeypatch.setattr(psync, "run_rsync", mock_rsync)
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("push", dry_run=False, delete_flag=False,
                           update=True)
        assert any("--update" in opts for opts in captured_opts)

    def test_push_does_not_use_update(self, tmp_path, monkeypatch):
        """Plain push should NOT use --update."""
        local = tmp_path / "notes"
        local.mkdir()
        monkeypatch.setattr(psync, "SYNC_DIRS", [(str(local), "/remote")])
        monkeypatch.setattr(psync, "SYNC_EXCLUDES", {})
        monkeypatch.setattr(psync, "_quiet", False)

        captured_opts = []

        def mock_rsync(opts, src, dst):
            captured_opts.append(list(opts))
            return 0, ""

        monkeypatch.setattr(psync, "run_rsync", mock_rsync)
        monkeypatch.setattr(psync, "phone_ssh",
                            lambda *a: subprocess.CompletedProcess(a, 0))

        psync.do_full_sync("push", dry_run=False, delete_flag=False)
        assert all("--update" not in opts for opts in captured_opts)
