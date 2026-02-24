"""Tests for dmenu-network-manager script."""

import unittest
from unittest.mock import patch, MagicMock
import sys
import os

# Mock gi and its components BEFORE importing the script
# This is necessary because the script imports gi at the top level
mock_gi = MagicMock()
mock_gi.require_version = MagicMock()
sys.modules['gi'] = mock_gi

mock_gi_repository = MagicMock()
sys.modules['gi.repository'] = mock_gi_repository

mock_glib = MagicMock()
mock_nm = MagicMock()
# Setup some constants that might be used
mock_nm.DeviceType.WIFI = 1
mock_nm.utils_ssid_to_utf8 = lambda x: x.decode('utf-8') if isinstance(x, bytes) else str(x)
# Add flags if needed, though we might not test everything that uses them yet
ap_flags = MagicMock()
ap_flags.PRIVACY = 1
setattr(mock_nm, '80211ApFlags', ap_flags)
ap_sec_flags = MagicMock()
ap_sec_flags.KEY_MGMT_PSK = 2
ap_sec_flags.KEY_MGMT_SAE = 4
ap_sec_flags.KEY_MGMT_802_1X = 8
ap_sec_flags.KEY_MGMT_OWE = 16
setattr(mock_nm, '80211ApSecurityFlags', ap_sec_flags)

sys.modules['gi.repository.GLib'] = mock_glib
sys.modules['gi.repository.NM'] = mock_nm

# Now we can safely import the script using the helper
from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-network-manager')

class TestDmenuNetworkManager(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        
        # Patch subprocess.run during import to prevent actual command execution
        with patch('subprocess.run') as mock_run:
            mock_proc = MagicMock()
            mock_proc.returncode = 0
            mock_proc.stdout = ''
            mock_run.return_value = mock_proc
            # Also patch os.path.expanduser to avoid filesystem access if needed
            with patch('os.path.expanduser', return_value='/tmp/fake/config'):
                cls.script = import_script(SCRIPT_PATH)

    def setUp(self):
        # Reset config to default before each test if we modify it
        # But for now just testing standalone functions mostly
        pass

    def test_cli_args_stripping(self):
        """Test stripping of prompt related arguments."""
        # Test -p flag stripping
        args = ["-p", "Prompt", "other_arg"]
        result = self.script.cli_args(args)
        self.assertEqual(result, ["other_arg"])

        # Test --prompt flag stripping
        args = ["--prompt", "Prompt Text", "-x"]
        result = self.script.cli_args(args)
        self.assertEqual(result, ["-x"])

        # Test -d / -dmenu stripping
        args = ["-d", "something"]
        result = self.script.cli_args(args)
        self.assertEqual(result, ["something"])

        # Test -pText format
        args = ["-pPrompt", "remaining"]
        result = self.script.cli_args(args)
        self.assertEqual(result, ["remaining"])

        # Test normal args preservation
        args = ["-nb", "#000", "-nf", "#fff"]
        result = self.script.cli_args(args)
        self.assertEqual(result, ["-nb", "#000", "-nf", "#fff"])

    @patch('subprocess.run')
    def test_load_dmenu_config(self, mock_run):
        """Test loading dmenu config from file."""
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = '-nb #222 -nf #aaa\nmonospace-12'
        mock_run.return_value = mock_proc

        with patch.object(self.script, 'expanduser', return_value='/tmp/fake/config'):
            basic_args, font = self.script.load_dmenu_config()

        self.assertEqual(basic_args, ['-nb', '#222', '-nf', '#aaa'])
        self.assertEqual(font, 'monospace-12')

    @patch('subprocess.run')
    def test_load_dmenu_config_failure(self, mock_run):
        """Test graceful failure when config load fails."""
        mock_proc = MagicMock()
        mock_proc.returncode = 1
        mock_run.return_value = mock_proc

        with patch.object(self.script, 'expanduser', return_value='/tmp/fake/config'):
            basic_args, font = self.script.load_dmenu_config()

        self.assertEqual(basic_args, [])
        self.assertIsNone(font)

    def test_config_defaults(self):
        """Verify critical default configuration values."""
        conf = self.script.Config()
        self.assertEqual(conf.dmenu_command, "dmenu")
        self.assertEqual(conf.active_chars, "==")
        self.assertTrue(conf.highlight)
        self.assertEqual(conf.prompt, "Networks")
        self.assertEqual(conf.rescan_delay, 5)

    def test_dmenu_cmd_construction_dmenu(self):
        """Test command construction for standard dmenu."""
        # Setup Config mock
        with patch.object(self.script, 'CONF') as mock_conf:
            mock_conf.dmenu_command = "dmenu"
            mock_conf.highlight = False
            mock_conf.obscure = False
            
            # Setup module level constants
            with patch.object(self.script, 'DMENU_BASIC_ARGS', ['-b']), \
                 patch.object(self.script, 'DMENU_FONT', 'font-12'), \
                 patch.object(self.script.sys, 'argv', ['script_name', 'extra_arg']):
                
                cmd = self.script.dmenu_cmd("MyPrompt")
                
                # Should contain: dmenu command, basic args, font arg, extra args from sys.argv, prompt args
                self.assertEqual(cmd[0], "dmenu")
                self.assertIn("-b", cmd)
                self.assertIn("font-12", cmd)
                self.assertIn("extra_arg", cmd)
                self.assertIn("-p", cmd)
                self.assertIn("MyPrompt", cmd)

    def test_dmenu_cmd_construction_rofi(self):
        """Test command construction for rofi."""
        with patch.object(self.script, 'CONF') as mock_conf:
            mock_conf.dmenu_command = "rofi"
            mock_conf.highlight = True
            mock_conf.obscure = False
            
            with patch.object(self.script.sys, 'argv', ['script']):
                # Test with active lines for highlighting
                cmd = self.script.dmenu_cmd("RofiPrompt", active_lines=[1, 3])
                
                self.assertEqual(cmd[0], "rofi")
                self.assertIn("-dmenu", cmd)
                self.assertIn("-p", cmd)
                self.assertIn("RofiPrompt", cmd)
                # Check highlight args
                self.assertIn("-a", cmd)
                self.assertIn("1,3", cmd)

    def test_dmenu_cmd_passphrase(self):
        """Test passphrase prompt command construction."""
        with patch.object(self.script, 'CONF') as mock_conf:
            mock_conf.dmenu_command = "rofi"
            mock_conf.highlight = False
            mock_conf.obscure = True
            mock_conf.obscure_color = "#111"

            with patch.object(self.script.sys, 'argv', ['script']):
                cmd = self.script.dmenu_cmd("Passphrase")
                
                self.assertIn("-password", cmd)

if __name__ == '__main__':
    unittest.main()
