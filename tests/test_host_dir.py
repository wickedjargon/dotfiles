"""Tests for host-dir script."""

import unittest
from unittest.mock import patch, MagicMock
import os

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('host-dir')


class TestHostDir(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    # ── find_available_port ──────────────────────────

    @patch('socket.socket')
    def test_find_port_default(self, mock_socket_cls):
        """Port 9000 is available — should return 9000."""
        mock_sock = MagicMock()
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        result = self.script.find_available_port(start=9000)
        self.assertEqual(result, 9000)

    @patch('socket.socket')
    def test_find_port_increments(self, mock_socket_cls):
        """Port 9000 is taken, 9001 is free — should return 9001."""
        mock_sock = MagicMock()
        mock_sock.bind.side_effect = [OSError("in use"), None]
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        result = self.script.find_available_port(start=9000)
        self.assertEqual(result, 9001)

    @patch('socket.socket')
    def test_find_port_exhausted(self, mock_socket_cls):
        """All ports taken — should raise RuntimeError."""
        mock_sock = MagicMock()
        mock_sock.bind.side_effect = OSError("in use")
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        with self.assertRaises(RuntimeError):
            self.script.find_available_port(start=9000, max_attempts=3)

    # ── get_local_ip ─────────────────────────────────

    @patch('socket.socket')
    def test_get_local_ip_socket(self, mock_socket_cls):
        """UDP connect trick succeeds — should return that IP."""
        mock_sock = MagicMock()
        mock_sock.getsockname.return_value = ("192.168.1.42", 0)
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        result = self.script.get_local_ip()
        self.assertEqual(result, "192.168.1.42")

    @patch('subprocess.run')
    @patch('socket.socket')
    def test_get_local_ip_fallback(self, mock_socket_cls, mock_run):
        """Socket method fails — should fall back to hostname -I."""
        mock_sock = MagicMock()
        mock_sock.connect.side_effect = OSError("no route")
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        mock_run.return_value = MagicMock(
            returncode=0, stdout="10.0.0.5 fd00::1\n"
        )

        result = self.script.get_local_ip()
        self.assertEqual(result, "10.0.0.5")

    @patch('subprocess.run')
    @patch('socket.socket')
    def test_get_local_ip_all_fail(self, mock_socket_cls, mock_run):
        """Both methods fail — should return 127.0.0.1."""
        mock_sock = MagicMock()
        mock_sock.connect.side_effect = OSError("no route")
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        mock_run.side_effect = FileNotFoundError()

        result = self.script.get_local_ip()
        self.assertEqual(result, "127.0.0.1")

    @patch('subprocess.run')
    @patch('socket.socket')
    def test_get_local_ip_hostname_empty(self, mock_socket_cls, mock_run):
        """Socket fails, hostname -I returns empty — should return 127.0.0.1."""
        mock_sock = MagicMock()
        mock_sock.connect.side_effect = OSError("no route")
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        mock_run.return_value = MagicMock(returncode=0, stdout="  \n")

        result = self.script.get_local_ip()
        self.assertEqual(result, "127.0.0.1")

    @patch('subprocess.run')
    @patch('socket.socket')
    def test_get_local_ip_127_skipped(self, mock_socket_cls, mock_run):
        """Socket returns 127.x.x.x — should fall back to hostname -I."""
        mock_sock = MagicMock()
        mock_sock.getsockname.return_value = ("127.0.1.1", 0)
        mock_socket_cls.return_value.__enter__ = MagicMock(return_value=mock_sock)
        mock_socket_cls.return_value.__exit__ = MagicMock(return_value=False)

        mock_run.return_value = MagicMock(
            returncode=0, stdout="192.168.0.10\n"
        )

        result = self.script.get_local_ip()
        self.assertEqual(result, "192.168.0.10")


if __name__ == '__main__':
    unittest.main()
