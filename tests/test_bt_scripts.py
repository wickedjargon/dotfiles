
import unittest
from unittest.mock import patch, MagicMock
import sys
import os
import importlib.util
import importlib.machinery
import time

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
BT_CONNECT_PATH = os.path.join(BIN_DIR, 'bluetooth-connect')
BT_DISCONNECT_PATH = os.path.join(BIN_DIR, 'bluetooth-disconnect')

class TestBtConnect(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(BT_CONNECT_PATH):
            raise unittest.SkipTest(f"Script not found: {BT_CONNECT_PATH}")
        cls.script = import_script(BT_CONNECT_PATH)

    @patch('subprocess.run')
    @patch('time.sleep')
    def test_check_connected(self, mock_sleep, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = """Device 3C:B0:ED:A7:6A:88 (public)
	Name: Nothing Headphone (1)
	Alias: Nothing Headphone (1)
	Class: 0x00240404
	Icon: audio-card
	Paired: yes
	Bonded: yes
	Trusted: yes
	Blocked: no
	Connected: yes
	LegacyPairing: no
"""
        mock_run.return_value = mock_proc

        result = self.script.check_connected("3C:B0:ED:A7:6A:88")
        self.assertTrue(result)

    @patch('subprocess.run')
    @patch('time.sleep')
    def test_find_sink(self, mock_sleep, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        # pactl list short sinks output
        mock_proc.stdout = "44	bluez_output.3C_B0_ED_A7_6A_88.1	module-bluez5-device.c	s16le 2ch 44100Hz	SUSPENDED\n"
        mock_run.return_value = mock_proc

        result = self.script.find_sink("3C:B0:ED:A7:6A:88")
        self.assertEqual(result, "bluez_output.3C_B0_ED_A7_6A_88.1")

class TestBtDisconnect(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(BT_DISCONNECT_PATH):
            raise unittest.SkipTest(f"Script not found: {BT_DISCONNECT_PATH}")
        cls.script = import_script(BT_DISCONNECT_PATH)

    @patch('subprocess.run')
    @patch('time.sleep')
    def test_check_disconnected(self, mock_sleep, mock_run):
        mock_proc = MagicMock()
        mock_proc.returncode = 0
        mock_proc.stdout = """Device 3C:B0:ED:A7:6A:88
	Connected: no
"""
        mock_run.return_value = mock_proc

        result = self.script.check_disconnected("3C:B0:ED:A7:6A:88")
        self.assertTrue(result)

if __name__ == '__main__':
    unittest.main()
