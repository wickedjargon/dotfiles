"""Tests for the wifi CLI in .local/bin."""

import unittest
from unittest.mock import patch, MagicMock

from helpers import get_bin_path, import_script

wifi = import_script(get_bin_path("wifi"))


class TestSavedConnections(unittest.TestCase):
    @patch.object(wifi, "nmcli")
    def test_colon_in_profile_name(self, mock_nmcli):
        """Profile names with escaped colons must parse and unescape."""
        mock_nmcli.return_value = MagicMock(
            returncode=0,
            stdout=(r"Home\:5GHz:uuid-1:802-11-wireless" + "\n"
                    "Cafe:uuid-2:802-11-wireless\n"
                    "Wired:uuid-3:802-3-ethernet\n"),
        )
        conns = wifi.get_saved_connections()
        names = [c["name"] for c in conns]
        self.assertEqual(names, ["Home:5GHz", "Cafe"])

    @patch.object(wifi, "nmcli")
    def test_nmcli_failure_returns_empty(self, mock_nmcli):
        mock_nmcli.return_value = MagicMock(returncode=1, stdout="")
        self.assertEqual(wifi.get_saved_connections(), [])


class TestConnectWithPassword(unittest.TestCase):
    @patch("builtins.input", return_value="hunter2")
    @patch.object(wifi.subprocess, "run")
    def test_password_via_stdin_not_argv(self, mock_run, _mock_input):
        """The password goes to nmcli on stdin (--ask), never argv."""
        mock_run.return_value = MagicMock(returncode=0, stdout="", stderr="")
        wifi._connect_with_password("MyNet")
        cmd = mock_run.call_args[0][0]
        self.assertNotIn("hunter2", cmd)
        self.assertIn("--ask", cmd)
        self.assertEqual(mock_run.call_args[1]["input"], "hunter2\n")


if __name__ == "__main__":
    unittest.main()
