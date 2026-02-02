"""Tests for dmenu-connect-to-network script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import tempfile

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-connect-to-network')


class TestDmenuConnect(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_load_dmenu_config_valid(self):
        """Test loading a valid dmenu config file."""
        config_content = '''
DMENU_BASIC_ARGS="-nb #000000 -nf #ffffff -sb #222222 -sf #ffffff -l 12"
DMENU_FONT="monospace-10"
'''
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            with patch.object(self.script.os.path, 'expanduser', return_value=config_path):
                with patch.object(self.script.os.path, 'exists', return_value=True):
                    # We need to mock at the right level - let's just call directly
                    pass
            
            # Direct test with temp file
            original_expanduser = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path
            
            args, font = self.script.load_dmenu_config()
            
            self.script.os.path.expanduser = original_expanduser
            
            self.assertEqual(font, "monospace-10")
            self.assertIn("-nb", args)
            self.assertIn("#000000", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        """Test behavior when config file doesn't exist."""
        with patch.object(self.script.os.path, 'expanduser', return_value='/nonexistent/path'):
            with patch.object(self.script.os.path, 'exists', return_value=False):
                args, font = self.script.load_dmenu_config()
                
        self.assertEqual(args, [])
        self.assertIsNone(font)

    @patch('subprocess.run')
    def test_get_wifi_list_success(self, mock_run):
        """Test parsing WiFi list from nmcli output."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = """MyNetwork:85:WPA2
OtherNetwork:60:WPA2
OpenNetwork:45:--
:30:WPA2"""  # Empty SSID should be skipped
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        networks = self.script.get_wifi_list()

        self.assertEqual(len(networks), 3)
        
        # Check first network
        my_net = next(n for n in networks if n['ssid'] == 'MyNetwork')
        self.assertEqual(my_net['signal'], '85')
        self.assertEqual(my_net['security'], 'WPA2')
        self.assertFalse(my_net['is_open'])
        
        # Check open network
        open_net = next(n for n in networks if n['ssid'] == 'OpenNetwork')
        self.assertTrue(open_net['is_open'])

    @patch('subprocess.run')
    def test_get_wifi_list_with_colon_in_ssid(self, mock_run):
        """Test parsing WiFi networks with colons in SSID names."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        # nmcli escapes colons as \:
        mock_proc.stdout = r"My\:Network\:Name:75:WPA2"
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        networks = self.script.get_wifi_list()

        self.assertEqual(len(networks), 1)
        # The script should unescape the colons
        self.assertEqual(networks[0]['ssid'], 'My:Network:Name')

    @patch('subprocess.run')
    def test_get_wifi_list_deduplication(self, mock_run):
        """Test that duplicate SSIDs keep only the strongest signal."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = """DupeNetwork:50:WPA2
DupeNetwork:80:WPA2
DupeNetwork:30:WPA2"""
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        networks = self.script.get_wifi_list()

        self.assertEqual(len(networks), 1)
        self.assertEqual(networks[0]['signal'], '80')  # Strongest signal kept

    @patch('subprocess.run')
    def test_has_saved_connection_true(self, mock_run):
        """Test detection of saved WiFi connection."""
        def run_side_effect(cmd, **kwargs):
            mock_proc = MagicMock()
            if 'connection show' in cmd and 'connection.type' not in cmd:
                mock_proc.returncode = 0
                mock_proc.stdout = "MyNetwork\nOtherConn"
            elif 'connection.type' in cmd:
                mock_proc.returncode = 0
                mock_proc.stdout = "connection.type:802-11-wireless"
            mock_proc.stderr = ""
            return mock_proc
            
        mock_run.side_effect = run_side_effect

        result = self.script.has_saved_connection("MyNetwork")
        self.assertTrue(result)

    @patch('subprocess.run')
    def test_has_saved_connection_false(self, mock_run):
        """Test when no saved connection exists."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = "SomeOtherNetwork"
        mock_proc.stderr = ""
        mock_run.return_value = mock_proc

        result = self.script.has_saved_connection("MyNetwork")
        self.assertFalse(result)


if __name__ == '__main__':
    unittest.main()
