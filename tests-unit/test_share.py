"""Tests for the share CLI (address and URL logic)."""

import subprocess

from helpers import get_bin_path, import_script

share = import_script(get_bin_path("share"))


class TestBuildUrl:
    def test_directory_url(self):
        assert share.build_url("100.64.0.1", 8123) == \
            "http://100.64.0.1:8123/"

    def test_file_url_quotes_spaces(self):
        url = share.build_url("100.64.0.1", 8123, "my report.pdf")
        assert url == "http://100.64.0.1:8123/my%20report.pdf"


class TestPickAddress:
    def test_prefers_tailscale(self, monkeypatch):
        def fake_run(cmd, **kwargs):
            assert cmd[0] == "tailscale"
            return subprocess.CompletedProcess(cmd, 0, "100.64.0.5\n", "")

        monkeypatch.setattr(share.subprocess, "run", fake_run)
        assert share.pick_address() == ("100.64.0.5", "tailnet")

    def test_falls_back_when_tailscale_missing(self, monkeypatch):
        def fake_run(cmd, **kwargs):
            raise FileNotFoundError

        monkeypatch.setattr(share.subprocess, "run", fake_run)
        ip, scope = share.pick_address()
        # Whatever the environment, the fallback never claims tailnet
        assert scope in ("LAN", "all interfaces")


class TestSingleFileHandler:
    def test_handler_binds_filename(self, tmp_path):
        f = tmp_path / "report.pdf"
        f.write_bytes(b"%PDF-1.4")
        handler = share.make_file_handler(str(f))
        # The generated class closes over the file; sanity-check the
        # pieces the HTTP methods rely on.
        assert callable(handler.do_GET)
        assert callable(handler.do_HEAD)
