"""Tests for the dl CLI (yt-dlp wrapper)."""

from types import SimpleNamespace

import pytest

from helpers import get_bin_path, import_script

dl = import_script(get_bin_path("dl"))


class TestBuildCommand:
    def test_video_targets_video_dir(self):
        cmd = dl.build_command(["https://example.com/v"])
        assert cmd[0] == "yt-dlp"
        assert "--no-playlist" in cmd
        assert "-x" not in cmd
        out = cmd[cmd.index("-o") + 1]
        assert out.startswith(dl.VIDEO_DIR)

    def test_audio_targets_audio_dir_with_extract(self):
        cmd = dl.build_command(["https://example.com/v"], audio=True)
        assert "-x" in cmd
        out = cmd[cmd.index("-o") + 1]
        assert out.startswith(dl.AUDIO_DIR)

    def test_urls_come_last(self):
        urls = ["https://a.example/1", "https://b.example/2"]
        cmd = dl.build_command(urls)
        assert cmd[-2:] == urls

    def test_paths_file_records_final_filepaths(self):
        cmd = dl.build_command(["https://a.example/1"], paths_file="/tmp/p")
        i = cmd.index("--print-to-file")
        assert cmd[i + 1 : i + 3] == ["after_move:filepath", "/tmp/p"]
        assert cmd[-1] == "https://a.example/1"

    def test_no_paths_file_by_default(self):
        assert "--print-to-file" not in dl.build_command(["https://a.example/1"])


class TestValidateUrls:
    def test_http_and_https_accepted(self):
        dl.validate_urls(["http://a.example", "https://b.example"])

    def test_non_url_dies(self):
        with pytest.raises(SystemExit):
            dl.validate_urls(["notaurl"])

    def test_dirs_match_psync_layout(self):
        assert dl.VIDEO_DIR.endswith("d/video")
        assert dl.AUDIO_DIR.endswith("d/audio")


LIST_OUTPUT = (
    "ID           | NAME       | STATUS  | IMAGE\n"
    "abc123def456 | archbox    | Up      | docker.io/library/archlinux:latest\n"
)


class TestContainerPrefix:
    def _setup(self, monkeypatch, in_container=False, have_distrobox=True,
               list_rc=0, list_out=LIST_OUTPUT):
        monkeypatch.setattr(
            dl.os.path, "exists",
            lambda p: in_container if p == "/run/.containerenv" else False,
        )
        monkeypatch.setattr(
            dl, "which",
            lambda t: "/usr/bin/distrobox" if (t == "distrobox" and have_distrobox) else None,
        )
        monkeypatch.setattr(
            dl.subprocess, "run",
            lambda *a, **k: SimpleNamespace(returncode=list_rc, stdout=list_out),
        )

    def test_host_with_archbox_routes_through_container(self, monkeypatch):
        self._setup(monkeypatch)
        assert dl.container_prefix() == ["distrobox", "enter", "archbox", "--"]

    def test_inside_container_runs_directly(self, monkeypatch):
        self._setup(monkeypatch, in_container=True)
        assert dl.container_prefix() == []

    def test_no_distrobox_runs_directly(self, monkeypatch):
        self._setup(monkeypatch, have_distrobox=False)
        assert dl.container_prefix() == []

    def test_missing_archbox_runs_directly(self, monkeypatch):
        other = "id1 | other | Up | img\n"
        self._setup(monkeypatch, list_out=other)
        assert dl.container_prefix() == []

    def test_list_failure_runs_directly(self, monkeypatch):
        self._setup(monkeypatch, list_rc=1, list_out="")
        assert dl.container_prefix() == []


class TestToolAvailable:
    def test_empty_prefix_uses_which(self, monkeypatch):
        monkeypatch.setattr(dl, "which", lambda t: "/usr/bin/yt-dlp")
        assert dl.tool_available([], "yt-dlp")
        monkeypatch.setattr(dl, "which", lambda t: None)
        assert not dl.tool_available([], "yt-dlp")

    def test_prefix_checks_inside_container(self, monkeypatch):
        seen = {}

        def fake_run(cmd, **kwargs):
            seen["cmd"] = cmd
            return SimpleNamespace(returncode=0)

        monkeypatch.setattr(dl.subprocess, "run", fake_run)
        prefix = ["distrobox", "enter", "archbox", "--"]
        assert dl.tool_available(prefix, "ffmpeg")
        assert seen["cmd"][:4] == prefix
        assert "command -v ffmpeg" in seen["cmd"]
