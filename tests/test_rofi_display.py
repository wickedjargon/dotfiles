import unittest
from unittest.mock import MagicMock, patch, call
import sys
import os
import importlib.util
import importlib.machinery

def import_script(path):
    name = os.path.basename(path)
    loader = importlib.machinery.SourceFileLoader(name, path)
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module

SCRIPT_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/rofi-display'))

class TestDisplayManager(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def setUp(self):
        self.manager = self.script.DisplayManager()
        self.manager.notify = MagicMock()
        self.manager.get_xrandr_output = MagicMock()

    def test_get_internal(self):
        self.manager.get_xrandr_output.return_value = """
eDP-1 connected primary 1920x1080+0+0
DP-1 disconnected
HDMI-1 connected 1920x1080+0+0
"""
        self.assertEqual(self.manager.get_internal(), "eDP-1")

    def test_get_externals(self):
        self.manager.get_xrandr_output.return_value = """
eDP-1 connected primary
DP-1 disconnected
HDMI-1 connected
DP-2 connected
"""
        # Internal is eDP-1. Externals are HDMI-1, DP-2.
        externals = self.manager.get_externals()
        self.assertEqual(sorted(externals), sorted(["HDMI-1", "DP-2"]))

    @patch('subprocess.run')
    def test_apply_config_extend_right(self, mock_run):
        self.manager.get_internal = MagicMock(return_value="eDP-1")
        mock_run.return_value.returncode = 0
        
        self.manager.apply_config("extend-right", "HDMI-1")
        
        expected_cmd = ["xrandr", "--output", "eDP-1", "--auto", "--output", "HDMI-1", "--auto", "--right-of", "eDP-1"]
        mock_run.assert_called_with(expected_cmd)
        self.manager.notify.assert_called_with("Extended to HDMI-1 (right)")

    @patch('subprocess.run')
    def test_apply_config_laptop_only(self, mock_run):
        self.manager.get_internal = MagicMock(return_value="eDP-1")
        self.manager.get_externals = MagicMock(return_value=["HDMI-1", "DP-2"])
        mock_run.return_value.returncode = 0
        
        self.manager.apply_config("laptop-only", "HDMI-1") # external arg ignored but passed
        
        # Should turn off HDMI-1 and DP-2, turn on eDP-1
        # Order implies sequential extends
        args = mock_run.call_args[0][0]
        self.assertEqual(args[0], "xrandr")
        self.assertIn("--output", args)
        self.assertIn("HDMI-1", args)
        self.assertIn("--off", args)
        self.assertIn("DP-2", args)
        # Ensure internal is auto
        idx = args.index("eDP-1")
        self.assertEqual(args[idx+1], "--auto")
        
        self.manager.notify.assert_called_with("Using laptop display only")

if __name__ == '__main__':
    unittest.main()
