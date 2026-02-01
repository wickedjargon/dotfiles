
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

ROFI_WIFI_PATH = os.path.join(BIN_DIR, 'rofi-wifi')

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

class WifiScriptBase:
    """Base class for testing rofi-wifi script."""
    
    script_path = None # Set in subclass

    @classmethod
    def setUpClass(cls):
        if not cls.script_path or not os.path.exists(cls.script_path):
            raise unittest.SkipTest(f"Script not found: {cls.script_path}")
        cls.script = import_script(cls.script_path)

    @patch('subprocess.run')
    def test_get_wifi_list(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        # SSID,SIGNAL,SECURITY
        mock_proc.stdout = """HomeWifi:80:WPA2
HomeWifi:40:WPA2
GuestWifi:90:--
Weiird\\:SSID:50:WPA2
"""
        mock_run.return_value = mock_proc

        networks = self.script.get_wifi_list()
        
        self.assertEqual(len(networks), 3)
        home = next(n for n in networks if n['ssid'] == 'HomeWifi')
        self.assertEqual(home['signal'], '80')
        guest = next(n for n in networks if n['ssid'] == 'GuestWifi')
        self.assertTrue(guest['is_open'])
        weird = next(n for n in networks if n['ssid'] == 'Weiird:SSID')
        self.assertEqual(weird['ssid'], 'Weiird:SSID')

    @patch('subprocess.run')
    def test_has_saved_connection_true(self, mock_run):
        mock_list = MagicMock()
        mock_list.returncode = 0
        mock_list.stdout = "HomeWifi\nOfficeWifi"
        
        mock_type = MagicMock()
        mock_type.returncode = 0
        mock_type.stdout = "802-11-wireless"

        mock_run.side_effect = [mock_list, mock_type]

        self.assertTrue(self.script.has_saved_connection("HomeWifi"))

    @patch('subprocess.run')
    def test_has_saved_connection_false(self, mock_run):
        mock_list = MagicMock()
        mock_list.returncode = 0
        mock_list.stdout = "OfficeWifi"
        mock_run.side_effect = [mock_list]

        self.assertFalse(self.script.has_saved_connection("HomeWifi"))

    @patch('subprocess.run')
    def test_connect_to_network_open(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = "Success"
        mock_run.return_value = mock_proc

        success, output = self.script.connect_to_network("OpenWifi")
        
        self.assertTrue(success)
        # Should delete existing first, then connect
        # We can inspect the last call or all calls
        mock_run.assert_called_with(
            "nmcli device wifi connect 'OpenWifi'",
            shell=True,
            capture_output=True,
            text=True
        )

    @patch('subprocess.run')
    def test_connect_to_network_password(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = "Success"
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        success, output = self.script.connect_to_network("SecureWifi", "s3cret")
        
        self.assertTrue(success)
        mock_run.assert_called_with(
            "nmcli device wifi connect 'SecureWifi' password 's3cret'",
            shell=True,
            capture_output=True,
            text=True
        )

class TestRofiWifi(WifiScriptBase, unittest.TestCase):
    script_path = ROFI_WIFI_PATH

    @patch('subprocess.Popen')
    def test_show_menu(self, mock_popen):
        mock_process = MagicMock()
        mock_process.communicate.return_value = ("Selected\n", "")
        mock_popen.return_value = mock_process
        
        result = self.script.show_menu("Prompt:", ["Item 1", "Item 2"])
        
        self.assertEqual(result, "Selected")
        args, kwargs = mock_popen.call_args
        self.assertEqual(args[0][0], "rofi")
        self.assertIn("-dmenu", args[0])

if __name__ == '__main__':
    unittest.main()
