"""Tests for the dl CLI (yt-dlp wrapper)."""

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


class TestValidateUrls:
    def test_http_and_https_accepted(self):
        dl.validate_urls(["http://a.example", "https://b.example"])

    def test_non_url_dies(self):
        with pytest.raises(SystemExit):
            dl.validate_urls(["notaurl"])

    def test_dirs_match_psync_layout(self):
        assert dl.VIDEO_DIR.endswith("d/video")
        assert dl.AUDIO_DIR.endswith("d/audio")
