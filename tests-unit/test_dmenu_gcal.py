"""Tests for dmenu-gcal (Google Calendar quick-add frontend).

Mocks the google.* and googleapiclient.* imports so the test suite
runs without the Google API packages installed.
"""

import sys
import unittest
from unittest.mock import patch, MagicMock, mock_open
import os
import subprocess

# ── Mock the Google library imports before loading the script ──────────────
_google_mocks = {}
for mod_name in [
    "google",
    "google.oauth2",
    "google.oauth2.credentials",
    "google.auth",
    "google.auth.transport",
    "google.auth.transport.requests",
    "google_auth_oauthlib",
    "google_auth_oauthlib.flow",
    "googleapiclient",
    "googleapiclient.discovery",
]:
    _google_mocks[mod_name] = MagicMock()

# Wire up nested attribute access so `from google.oauth2.credentials import Credentials` works
_google_mocks["google"].oauth2 = _google_mocks["google.oauth2"]
_google_mocks["google.oauth2"].credentials = _google_mocks["google.oauth2.credentials"]
_google_mocks["google"].auth = _google_mocks["google.auth"]
_google_mocks["google.auth"].transport = _google_mocks["google.auth.transport"]
_google_mocks["google.auth.transport"].requests = _google_mocks["google.auth.transport.requests"]
_google_mocks["google_auth_oauthlib"].flow = _google_mocks["google_auth_oauthlib.flow"]
_google_mocks["googleapiclient"].discovery = _google_mocks["googleapiclient.discovery"]

sys.modules.update(_google_mocks)

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path("dmenu-gcal")


def _load_module():
    if not os.path.exists(SCRIPT_PATH):
        raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
    return import_script(SCRIPT_PATH)


class TestLoadDmenuConfig(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    def test_parses_config_file(self):
        config = (
            'DMENU_BASIC_ARGS="-nb #000 -nf #fff"\n'
            'DMENU_FONT="monospace-12"\n'
        )
        with patch("builtins.open", mock_open(read_data=config)):
            with patch.object(os.path, "exists", return_value=True):
                args, font = self.mod.load_dmenu_config()
        self.assertEqual(args, ["-nb", "#000", "-nf", "#fff"])
        self.assertEqual(font, "monospace-12")

    def test_missing_config_returns_defaults(self):
        with patch.object(os.path, "exists", return_value=False):
            args, font = self.mod.load_dmenu_config()
        self.assertEqual(args, [])
        self.assertEqual(font, "")


class TestDmenuPrompt(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    @patch("subprocess.run")
    def test_returns_user_input(self, mock_run):
        mock_run.return_value = MagicMock(stdout="Dentist next Tuesday at 10am\n")
        result = self.mod.dmenu_prompt(["-i"], "monospace-10")
        self.assertEqual(result, "Dentist next Tuesday at 10am")

    @patch("subprocess.run")
    def test_includes_font_in_command(self, mock_run):
        mock_run.return_value = MagicMock(stdout="test\n")
        self.mod.dmenu_prompt([], "monospace-14")
        cmd = mock_run.call_args[0][0]
        self.assertIn("-fn", cmd)
        self.assertIn("monospace-14", cmd)

    @patch("subprocess.run")
    def test_no_font_when_empty(self, mock_run):
        mock_run.return_value = MagicMock(stdout="test\n")
        self.mod.dmenu_prompt([], "")
        cmd = mock_run.call_args[0][0]
        self.assertNotIn("-fn", cmd)

    @patch("subprocess.run", side_effect=subprocess.CalledProcessError(1, "dmenu"))
    def test_returns_none_on_cancel(self, mock_run):
        result = self.mod.dmenu_prompt([], "")
        self.assertIsNone(result)


class TestFormatEventTime(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    def test_datetime_format(self):
        event = {"start": {"dateTime": "2026-02-10T10:00:00-08:00"}}
        self.assertEqual(self.mod.format_event_time(event), "2026-02-10T10:00:00-08:00")

    def test_date_only_format(self):
        event = {"start": {"date": "2026-02-10"}}
        self.assertEqual(self.mod.format_event_time(event), "2026-02-10")

    def test_no_start_returns_unknown(self):
        self.assertEqual(self.mod.format_event_time({}), "unknown time")


class TestGetCalendarService(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    @patch("subprocess.run")
    def test_missing_credentials_notifies_and_exits(self, mock_run):
        with patch("pathlib.Path.exists", return_value=False):
            with self.assertRaises(SystemExit):
                self.mod.get_calendar_service()
        mock_run.assert_called_once()
        call_args = mock_run.call_args[0][0]
        self.assertEqual(call_args[0], "notify-send")
        self.assertIn("credentials.json", call_args[-1])


class TestAddEvent(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    def test_calls_quick_add_with_text(self):
        mock_service = MagicMock()
        expected = {"summary": "Dentist", "start": {"dateTime": "2026-02-10T10:00:00"}}
        mock_service.events.return_value.quickAdd.return_value.execute.return_value = expected
        result = self.mod.add_event(mock_service, "Dentist next Tuesday at 10am")
        mock_service.events.return_value.quickAdd.assert_called_with(
            calendarId="primary", text="Dentist next Tuesday at 10am"
        )
        self.assertEqual(result, expected)


class TestMain(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.mod = _load_module()

    @patch("subprocess.run")
    def test_exits_silently_on_dmenu_cancel(self, mock_run):
        mock_run.side_effect = subprocess.CalledProcessError(1, "dmenu")
        with patch.object(os.path, "exists", return_value=True):
            with patch("builtins.open", mock_open(read_data="")):
                with self.assertRaises(SystemExit) as ctx:
                    self.mod.main()
                self.assertEqual(ctx.exception.code, 0)

    @patch("subprocess.run")
    def test_successful_event_creation(self, mock_run):
        dmenu_result = MagicMock(stdout="Meeting tomorrow at 3pm\n")
        mock_run.return_value = dmenu_result

        mock_event = {
            "summary": "Meeting",
            "start": {"dateTime": "2026-02-10T15:00:00-08:00"},
        }

        with patch.object(os.path, "exists", return_value=True):
            with patch("builtins.open", mock_open(read_data="")):
                with patch.object(self.mod, "get_calendar_service") as _:
                    with patch.object(self.mod, "add_event", return_value=mock_event):
                        self.mod.main()

        # Last call to subprocess.run should be notify-send with event details
        last_call = mock_run.call_args_list[-1]
        notify_args = last_call[0][0]
        self.assertEqual(notify_args[0], "notify-send")
        self.assertIn("Meeting", notify_args[2])

    @patch("subprocess.run")
    def test_api_error_notifies_and_exits(self, mock_run):
        dmenu_result = MagicMock(stdout="Bad event\n")
        mock_run.return_value = dmenu_result

        with patch.object(os.path, "exists", return_value=True):
            with patch("builtins.open", mock_open(read_data="")):
                with patch.object(
                    self.mod,
                    "get_calendar_service",
                    side_effect=Exception("API broke"),
                ):
                    with self.assertRaises(SystemExit) as ctx:
                        self.mod.main()
                    self.assertEqual(ctx.exception.code, 1)

        last_call = mock_run.call_args_list[-1]
        notify_args = last_call[0][0]
        self.assertEqual(notify_args[0], "notify-send")
        self.assertIn("API broke", notify_args[2])


if __name__ == "__main__":
    unittest.main()
