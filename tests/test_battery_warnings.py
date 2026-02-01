import unittest
from unittest.mock import MagicMock, patch
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

SCRIPT_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/battery-warnings'))

class TestBatteryMonitor(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def setUp(self):
        # Create a monitor with mocked notify and play_sound
        self.monitor = self.script.BatteryMonitor()
        self.monitor.notify = MagicMock()
        self.monitor.play_sound = MagicMock()
        
        # Mock the get_file_content method
        self.monitor.get_file_content = MagicMock()

    def test_battery_critical(self):
        self.monitor.get_file_content.side_effect = lambda f: "Discharging" if f == "status" else "5"
        
        self.monitor.check_battery()
        
        self.monitor.notify.assert_called_with("critical", "Battery Critical", "Level is at 5%.")
        self.monitor.play_sound.assert_called()
        self.assertTrue(self.monitor.sent_10)
        self.assertTrue(self.monitor.sent_20)

    def test_battery_low(self):
        self.monitor.get_file_content.side_effect = lambda f: "Discharging" if f == "status" else "15"
        
        self.monitor.check_battery()
        
        self.monitor.notify.assert_called_with("normal", "Battery Low", "Level is at 15%.")
        self.monitor.play_sound.assert_called()
        self.assertFalse(self.monitor.sent_10)
        self.assertTrue(self.monitor.sent_20)

    def test_battery_recovering(self):
        # Setup: previously notified
        self.monitor.sent_10 = True
        self.monitor.sent_20 = True
        
        self.monitor.get_file_content.side_effect = lambda f: "Charging" if f == "status" else "25"
        
        self.monitor.check_battery()
        
        self.monitor.notify.assert_called_with("low", "Charging", "Battery recovering...", expire_time=2000)
        self.assertFalse(self.monitor.sent_10)
        self.assertFalse(self.monitor.sent_20)

    def test_no_double_notify(self):
        # Already notified critical
        self.monitor.sent_10 = True
        self.monitor.get_file_content.side_effect = lambda f: "Discharging" if f == "status" else "5"
        
        self.monitor.check_battery()
        
        self.monitor.notify.assert_not_called()

    @patch('subprocess.run')
    def test_notify(self, mock_run):
        mon = self.script.BatteryMonitor()
        mon.notify("u", "t", "m", expire_time=100)
        mock_run.assert_called_with(["notify-send", "-u", "u", "-h", "string:x-canonical-private-synchronous:bat_alert", "-t", "100", "t", "m"])

    @patch('subprocess.Popen')
    def test_play_sound(self, mock_popen):
        mon = self.script.BatteryMonitor()
        mon.play_sound()
        mock_popen.assert_called_with(["paplay", "/usr/share/sounds/freedesktop/stereo/suspend-error.oga"])

if __name__ == '__main__':
    unittest.main()
