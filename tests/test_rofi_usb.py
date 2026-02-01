import unittest
from unittest.mock import MagicMock, patch
import sys
import os
import importlib.util
import importlib.machinery
import json

def import_script(path):
    name = os.path.basename(path)
    loader = importlib.machinery.SourceFileLoader(name, path)
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module

SCRIPT_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/rofi-usb'))

class TestUsbManager(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def setUp(self):
        self.manager = self.script.UsbManager()
        self.manager.notify = MagicMock()

    @patch('subprocess.run')
    def test_get_partitions(self, mock_run):
        # Mock lsblk output with hierarchy
        mock_output = {
            "blockdevices": [
                {
                    "name": "/dev/sda",
                    "type": "disk",
                    "rm": False,
                    "children": [
                        {"name": "/dev/sda1", "type": "part", "rm": False}
                    ]
                },
                {
                    "name": "/dev/sdb",
                    "type": "disk",
                    "rm": True,
                    "children": [
                        {"name": "/dev/sdb1", "type": "part", "rm": True, "label": "MyUSB", "size": "16G", "mountpoint": None}
                    ]
                }
            ]
        }
        mock_run.return_value.stdout = json.dumps(mock_output)
        mock_run.return_value.returncode = 0

        parts = self.manager.get_partitions()
        self.assertEqual(len(parts), 1)
        self.assertEqual(parts[0]["name"], "/dev/sdb1")
        self.assertEqual(parts[0]["label"], "MyUSB")

    @patch('subprocess.run')
    def test_mount(self, mock_run):
        mock_run.return_value.stdout = "Mounted /dev/sdb1 at /media/user/MyUSB"
        mock_run.return_value.returncode = 0
        
        self.manager.mount("/dev/sdb1")
        
        args = mock_run.call_args[0][0]
        self.assertEqual(args, ["udisksctl", "mount", "-b", "/dev/sdb1"])
        self.manager.notify.assert_called_with("Mounted /dev/sdb1 at /media/user/MyUSB")

    @patch('subprocess.Popen')
    def test_open_drive(self, mock_popen):
        with patch('shutil.which', return_value="/usr/bin/xdg-open"):
            self.manager.open_drive("/media/user/MyUSB")
            mock_popen.assert_called_with(["xdg-open", "/media/user/MyUSB"])

if __name__ == '__main__':
    unittest.main()
