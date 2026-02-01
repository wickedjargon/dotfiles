import unittest
from unittest.mock import MagicMock, patch, call
import sys
import os
import importlib.util
import importlib.machinery
import json
import subprocess

def import_script(path):
    name = os.path.basename(path)
    loader = importlib.machinery.SourceFileLoader(name, path)
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module

SCRIPT_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/bspwm-deck'))

class TestBspwmDeck(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def setUp(self):
        self.deck = self.script.BspwmDeck()

    @patch('subprocess.check_output')
    @patch('subprocess.run')
    def test_unhide_mode(self, mock_run, mock_output):
        # Setup: Has hidden nodes
        # Calls:
        # 1. focused -> 0x1
        # 2. master -> 0x1
        # 3. layout -> tiled
        # 4. hidden -> 0x2
        
        mock_output.side_effect = [
            "0x1", # focused
            "0x1", # master
            json.dumps({"layout": "tiled"}), # layout
            "0x2", # hidden nodes
        ]
        
        self.deck.run()
        
        # Verify unhide call
        mock_run.assert_called_with(["bspc", "node", "0x2", "-g", "hidden=off"])

    @patch('subprocess.check_output')
    @patch('subprocess.run')
    def test_deck_mode(self, mock_run, mock_output):
        # Setup: No hidden nodes, mix of windows
        # 0x1: Master (and Focused)
        # 0x2: Stack Window 1
        # 0x3: Stack Window 2
        
        mock_output.side_effect = [
            "0x1", # focused
            "0x1", # master
            json.dumps({"layout": "monocle"}), # layout (should switch to tiled)
            "",    # hidden nodes (empty)
            "0x1\n0x2\n0x3", # visible nodes
        ]
        
        self.deck.run()
        
        # 1. Switch to tiled
        mock_run.assert_any_call(["bspc", "desktop", "-l", "tiled"])
        
        # 2. Hide 0x2 and 0x3
        mock_run.assert_any_call(["bspc", "node", "0x2", "-g", "hidden=on"])
        mock_run.assert_any_call(["bspc", "node", "0x3", "-g", "hidden=on"])
        
        # 3. Ensure 0x1 not hidden
        # We can check call args list to ensure 0x1 is NOT present in hidden=on calls
        for call_args in mock_run.call_args_list:
             args = call_args[0][0]
             if "hidden=on" in args:
                 self.assertNotEqual(args[2], "0x1")

    @patch('subprocess.check_output')
    @patch('subprocess.run')
    def test_focused_stack_mode(self, mock_run, mock_output):
        # Setup: 
        # 0xM: Master
        # 0xF: Focused (Stack)
        # 0xS: Other Stack
        
        mock_output.side_effect = [
            "0xF", # focused
            "0xM", # master
            json.dumps({"layout": "tiled"}),
            "",    # hidden
            "0xM\n0xF\n0xS", # visible
        ]
        
        self.deck.run()
        
        # Should only hide 0xS
        mock_run.assert_called_with(["bspc", "node", "0xS", "-g", "hidden=on"])
        
        # Verify M and F not hidden
        for call_args in mock_run.call_args_list:
             args = call_args[0][0]
             if "hidden=on" in args:
                 self.assertNotIn("0xM", args)
                 self.assertNotIn("0xF", args)

if __name__ == '__main__':
    unittest.main()
