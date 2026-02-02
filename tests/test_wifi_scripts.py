
import unittest
from unittest.mock import patch, MagicMock
import sys
import os
import importlib.util
import importlib.machinery

# Helper to import script without .py extension
def import_script(path):
    name = os.path.basename(path)
    loader = importlib.machinery.SourceFileLoader(name, path)
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module

# Paths to scripts
BIN_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin'))
WIFI_DISCONNECT_PATH = os.path.join(BIN_DIR, 'wifi-disconnect')



class TestWifiDisconnect(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(WIFI_DISCONNECT_PATH):
            raise unittest.SkipTest(f"Script not found: {WIFI_DISCONNECT_PATH}")
        cls.script = import_script(WIFI_DISCONNECT_PATH)

    @patch('subprocess.run')
    def test_get_active_wifi_connection_found(self, mock_run):
        # Mock successful nmcli output
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = "MyWifi:802-11-wireless:wlan0\nOtherConn:ethernet:eth0"
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        result = self.script.get_active_wifi_connection()
        self.assertEqual(result, "MyWifi")

    @patch('subprocess.run')
    def test_get_active_wifi_connection_none(self, mock_run):
        # Mock no active connection
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = ""
        mock_run.return_value = mock_proc

        result = self.script.get_active_wifi_connection()
        self.assertIsNone(result)

    @patch('subprocess.run')
    def test_disconnect_wifi(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_run.return_value = mock_proc

        success, output = self.script.disconnect_wifi("MyWifi")
        self.assertTrue(success)
        mock_run.assert_called_with(
            "nmcli connection down 'MyWifi'",
            shell=True,
            capture_output=True,
            text=True
        )



if __name__ == '__main__':
    unittest.main()
