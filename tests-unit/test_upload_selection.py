"""Tests for the unified upload script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import subprocess

from helpers import import_script, get_bin_path

SCRIPT_PATH = get_bin_path('upload')


class TestUpload(unittest.TestCase):
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
            input="test", text=True, timeout=5,
        )

    @patch('subprocess.run')
    def test_copy_to_clipboard_xclip_missing(self, mock_run):
        mock_run.side_effect = FileNotFoundError()
        result = self.script.copy_to_clipboard("test")
        self.assertFalse(result)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://0x0.st/abc.txt\n"
        )
        result = self.script.upload_file_to_0x0("/tmp/test.txt")
        self.assertEqual(result, "https://0x0.st/abc.txt")

    @patch('subprocess.run')
    def test_upload_file_to_0x0_http_url(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="http://0x0.st/abc.txt\n"
        )
        result = self.script.upload_file_to_0x0("/tmp/test.txt")
        self.assertEqual(result, "http://0x0.st/abc.txt")

    @patch('subprocess.run')
    def test_upload_file_to_0x0_invalid_response(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="<html>Error</html>"
        )
        result = self.script.upload_file_to_0x0("/tmp/test.txt")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_curl_failure(self, mock_run):
        mock_run.return_value = MagicMock(returncode=22, stdout="")
        result = self.script.upload_file_to_0x0("/tmp/test.txt")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_file_to_0x0_timeout(self, mock_run):
        mock_run.side_effect = subprocess.TimeoutExpired(cmd="curl", timeout=30)
        result = self.script.upload_file_to_0x0("/tmp/test.txt")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_text_to_0x0_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://0x0.st/xyz.txt\n"
        )
        result = self.script.upload_text_to_0x0("hello world")
        self.assertEqual(result, "https://0x0.st/xyz.txt")
        cmd = mock_run.call_args[0][0]
        self.assertIn("file=@-", cmd)

    @patch('subprocess.run')
    def test_upload_text_to_0x0_invalid_response(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="rate limited"
        )
        result = self.script.upload_text_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_text_to_0x0_failure(self, mock_run):
        mock_run.return_value = MagicMock(returncode=22, stdout="")
        result = self.script.upload_text_to_0x0("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_text_to_u_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://u.fftp.io/abc123\n"
        )
        with patch.object(self.script, 'read_api_key', return_value="testkey"):
            result = self.script.upload_text_to_u("hello")
        self.assertEqual(result, "https://u.fftp.io/abc123")

    @patch('subprocess.run')
    def test_upload_text_to_u_no_api_key(self, mock_run):
        with patch.object(self.script, 'read_api_key', return_value=None):
            result = self.script.upload_text_to_u("hello")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_upload_file_to_u_success(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://u.fftp.io/def456\n"
        )
        with patch.object(self.script, 'read_api_key', return_value="testkey"):
            result = self.script.upload_file_to_u("/tmp/test.png")
        self.assertEqual(result, "https://u.fftp.io/def456")

    @patch('subprocess.run')
    def test_upload_file_to_u_preserve_filename(self, mock_run):
        mock_run.return_value = MagicMock(
            returncode=0, stdout="https://u.fftp.io/test.png\n"
        )
        with patch.object(self.script, 'read_api_key', return_value="testkey"):
            result = self.script.upload_file_to_u("/tmp/test.png",
                                                   preserve_filename=True)
        self.assertEqual(result, "https://u.fftp.io/test.png")
        cmd = mock_run.call_args[0][0]
        self.assertIn("preserve_filename=true", cmd)

    @patch('subprocess.run')
    def test_upload_file_to_u_no_api_key(self, mock_run):
        with patch.object(self.script, 'read_api_key', return_value=None):
            result = self.script.upload_file_to_u("/tmp/test.png")
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_notify(self, mock_run):
        mock_run.return_value = MagicMock(returncode=0)
        self.script.notify("Title", "Message")
        mock_run.assert_called_once_with(
            ["notify-send", "Title", "Message"],
            capture_output=True, timeout=5,
        )


if __name__ == '__main__':
    unittest.main()
