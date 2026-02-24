"""Tests for wifi-rescan script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import subprocess

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('wifi-rescan')


class TestWifiRescan(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    @patch('subprocess.run')
    def test_get_current_ssid_connected(self, mock_run):
        mock_run.return_value = MagicMock(stdout="MyNetwork\n")
        result = self.script.get_current_ssid()
        self.assertEqual(result, "MyNetwork")

    @patch('subprocess.run')
    def test_get_current_ssid_not_connected(self, mock_run):
        mock_run.return_value = MagicMock(stdout="")
        result = self.script.get_current_ssid()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_current_ssid_timeout(self, mock_run):
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="iwgetid", timeout=5)
        result = self.script.get_current_ssid()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_current_ssid_missing_command(self, mock_run):
        mock_run.side_effect = FileNotFoundError()
        result = self.script.get_current_ssid()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_notify(self, mock_run):
        self.script.notify("WiFi", "test message")
        mock_run.assert_called_once_with(["notify-send", "WiFi", "test message"])

    @patch('time.sleep')
    @patch('subprocess.run')
    def test_main_detects_new_connection(self, mock_run, mock_sleep):
        call_count = [0]

        def run_side_effect(cmd, **kwargs):
            mock = MagicMock()
            if cmd[0] == "iwgetid":
                call_count[0] += 1
                if call_count[0] <= 2:
                    mock.stdout = ""
                else:
                    mock.stdout = "PhoneHotspot\n"
            elif cmd[0] == "nmcli":
                pass
            elif cmd[0] == "notify-send":
                pass
            return mock

        mock_run.side_effect = run_side_effect
        self.script.main()

        notify_calls = [
            c for c in mock_run.call_args_list
            if c[0][0][0] == "notify-send"
        ]
        last_notify = notify_calls[-1][0][0]
        self.assertEqual(last_notify[1], "WiFi")
        self.assertIn("PhoneHotspot", last_notify[2])

    @patch('time.sleep')
    @patch('subprocess.run')
    def test_main_no_connection(self, mock_run, mock_sleep):
        def run_side_effect(cmd, **kwargs):
            mock = MagicMock()
            if cmd[0] == "iwgetid":
                mock.stdout = ""
            return mock

        mock_run.side_effect = run_side_effect
        self.script.main()

        notify_calls = [
            c for c in mock_run.call_args_list
            if c[0][0][0] == "notify-send"
        ]
        last_notify = notify_calls[-1][0][0]
        self.assertIn("No connection", last_notify[2])

    @patch('time.sleep')
    @patch('subprocess.run')
    def test_main_already_connected_same_network(self, mock_run, mock_sleep):
        def run_side_effect(cmd, **kwargs):
            mock = MagicMock()
            if cmd[0] == "iwgetid":
                mock.stdout = "HomeWiFi\n"
            return mock

        mock_run.side_effect = run_side_effect
        self.script.main()

        notify_calls = [
            c for c in mock_run.call_args_list
            if c[0][0][0] == "notify-send"
        ]
        last_notify = notify_calls[-1][0][0]
        self.assertIn("HomeWiFi", last_notify[2])


if __name__ == '__main__':
    unittest.main()
