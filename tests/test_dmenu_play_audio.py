"""Tests for dmenu-play-audio script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import tempfile

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-play-audio')


class TestDmenuPlayAudio(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_load_dmenu_config_valid(self):
        """Test loading a valid dmenu config file."""
        config_content = '''DMENU_BASIC_ARGS="-nb #000000 -nf #ffffff -sb #222222 -sf #ffffff -l 12"
DMENU_FONT="monospace-10"
'''
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            original_expanduser = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path

            args, font = self.script.load_dmenu_config()

            self.script.os.path.expanduser = original_expanduser

            self.assertEqual(font, "monospace-10")
            self.assertIn("-nb", args)
            self.assertIn("#000000", args)
            self.assertIn("-l", args)
            self.assertIn("12", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        """Test behavior when config file doesn't exist."""
        original_expanduser = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/path'

        args, font = self.script.load_dmenu_config()

        self.script.os.path.expanduser = original_expanduser

        self.assertEqual(args, [])
        self.assertEqual(font, "")

    @patch('subprocess.run')
    def test_run_mpc_success(self, mock_run):
        """Test run_mpc returns True on success."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = ""
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        result = self.script.run_mpc(["status"])

        self.assertTrue(result)
        mock_run.assert_called_once_with(
            ["mpc", "status"],
            capture_output=True,
            text=True
        )

    @patch('subprocess.run')
    def test_run_mpc_failure(self, mock_run):
        """Test run_mpc returns False and calls notify-send on failure."""
        mock_proc_fail = MagicMock()
        mock_proc_fail.returncode = 1
        mock_proc_fail.stderr = "connection refused"

        mock_notify = MagicMock()

        def side_effect(cmd, **kwargs):
            if cmd[0] == "mpc":
                return mock_proc_fail
            if cmd[0] == "notify-send":
                return mock_notify
            return MagicMock()

        mock_run.side_effect = side_effect

        result = self.script.run_mpc(["play"])

        self.assertFalse(result)
        notify_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "notify-send"]
        self.assertEqual(len(notify_calls), 1)
        self.assertIn("MPD Error", notify_calls[0][0][0])

    @patch('subprocess.run')
    def test_run_mpc_missing(self, mock_run):
        """Test run_mpc returns False and calls notify-send when mpc not found."""
        def side_effect(cmd, **kwargs):
            if cmd[0] == "mpc":
                raise FileNotFoundError("mpc not found")
            return MagicMock()

        mock_run.side_effect = side_effect

        result = self.script.run_mpc(["status"])

        self.assertFalse(result)
        notify_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "notify-send"]
        self.assertEqual(len(notify_calls), 1)
        self.assertIn("mpc not found", notify_calls[0][0][0][2])


if __name__ == '__main__':
    unittest.main()
