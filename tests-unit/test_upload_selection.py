"""Tests for upload-selection script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import subprocess

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('upload-selection')


class TestUploadSelection(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    @patch('subprocess.run')
    def test_get_selection_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="hello world"
        )
        result = self.script.get_selection()
        self.assertEqual(result, "hello world")
        mock_run.assert_called_once_with(
            ["xclip", "-o", "-selection", "primary"],
            capture_output=True, text=True, timeout=2,
        )

    @patch('subprocess.run')
    def test_get_selection_empty(self, mock_run):
        mock_run.return_value = MagicMock(returncode=0, stdout="  ")
        result = self.script.get_selection()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_selection_nonzero_exit(self, mock_run):
        mock_run.return_value = MagicMock(returncode=1, stdout="")
        result = self.script.get_selection()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_selection_timeout(self, mock_run):
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="xclip", timeout=2)
        result = self.script.get_selection()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_get_selection_xclip_missing(self, mock_run):
        mock_run.side_effect = FileNotFoundError()
        result = self.script.get_selection()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_copy_to_clipboard_success(self, mock_run):
        mock_run.return_value = MagicMock(returncode=0)
        result = self.script.copy_to_clipboard("test")
        self.assertTrue(result)
        mock_run.assert_called_once_with(
            ["xclip", "-selection", "clipboard"],
            input="test", text=True,
        )

    @patch('subprocess.run')
    def test_copy_to_clipboard_failure(self, mock_run):
        mock_run.return_value = MagicMock(returncode=1)
        result = self.script.copy_to_clipboard("test")
        self.assertFalse(result)

    @patch('subprocess.run')
    def test_upload_to_0x0_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://0x0.st/abc.txt\n"
        )
        result = self.script.upload_to_0x0("hello")
        self.assertEqual(result, "https://0x0.st/abc.txt")

    @patch('subprocess.run')
    def test_upload_to_0x0_http_url(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="http://0x0.st/abc.txt\n"
        )
        result = self.script.upload_to_0x0("hello")
        self.assertEqual(result, "http://0x0.st/abc.txt")

    @patch('subprocess.run')
    def test_upload_to_0x0_invalid_response(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="<html>Error</html>"
        )
        result = self.script.upload_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_to_0x0_curl_failure(self, mock_run):
        mock_run.return_value = MagicMock(returncode=22, stdout="")
        result = self.script.upload_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_to_0x0_timeout(self, mock_run):
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="curl", timeout=30)
        result = self.script.upload_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_to_0x0_curl_missing(self, mock_run):
        mock_run.side_effect = FileNotFoundError()
        result = self.script.upload_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_to_0x0_uses_correct_curl_flags(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://0x0.st/x.txt\n"
        )
        self.script.upload_to_0x0("data")
        cmd = mock_run.call_args[0][0]
        self.assertIn("-fsS", cmd)
        self.assertIn("--connect-timeout", cmd)
        self.assertIn("--retry", cmd)
        self.assertIn("--retry-all-errors", cmd)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://0x0.st/img.png\n"
        )
        result = self.script.upload_file_to_0x0("/tmp/test.png")
        self.assertEqual(result, "https://0x0.st/img.png")
        cmd = mock_run.call_args[0][0]
        self.assertIn("-F", cmd)
        self.assertIn("file=@/tmp/test.png", cmd)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_invalid_response(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="rate limited"
        )
        result = self.script.upload_file_to_0x0("/tmp/test.png")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_failure(self, mock_run):
        mock_run.return_value = MagicMock(returncode=22, stdout="")
        result = self.script.upload_file_to_0x0("/tmp/test.png")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_notify(self, mock_run):
        self.script.notify("Title", "Message")
        mock_run.assert_called_once_with(["notify-send", "Title", "Message"])


if __name__ == '__main__':
    unittest.main()
