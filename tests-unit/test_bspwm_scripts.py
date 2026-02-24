import unittest
from unittest.mock import MagicMock, patch, mock_open
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

ARROW_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/bspwm-smart-arrow'))
CANCEL_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), '../.local/bin/bspwm-smart-cancel'))

class TestBspwmScripts(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(ARROW_PATH):
            raise unittest.SkipTest(f"Script not found: {ARROW_PATH}")
        cls.arrow_script = import_script(ARROW_PATH)
        cls.cancel_script = import_script(CANCEL_PATH)

    @patch('subprocess.run')
    @patch('subprocess.check_output')
    @patch('subprocess.check_call')
    def test_arrow_allowed(self, mock_check_call, mock_check_output, mock_run):
        arrow = self.arrow_script.BspwmSmartArrow()
        
        # Setup: 1 node, fullscreen=False, layout=monocle (should be allowed if count=1)
        mock_check_output.side_effect = [
            "0x001\n", # query nodes
            json.dumps({"layout": "monocle"}) # query layout
        ]
        mock_check_call.side_effect = subprocess.CalledProcessError(1, "cmd") # is_fullscreen returns false
        
        with patch('builtins.open', mock_open()) as mock_file:
            arrow.run("east")
            
            # Check state save
            mock_file.assert_called_with("/tmp/bspwm_smart_arrow_state.json", 'w')
            handle = mock_file()
            # verify json dump? mocking write calls is tricky with json.dump
            
            # Check commands
            # Ensure tiled
            mock_run.assert_any_call(["bspc", "desktop", "-l", "tiled"])
            # Preselect
            mock_run.assert_any_call(["bspc", "node", "-p", "east"])

    @patch('subprocess.run')
    @patch('subprocess.check_output')
    @patch('subprocess.check_call')
    def test_arrow_blocked(self, mock_check_call, mock_check_output, mock_run):
        arrow = self.arrow_script.BspwmSmartArrow()
        
        # Setup: 2 nodes, layout=monocle -> Blocked
        mock_check_output.side_effect = [
            "0x001\n0x002\n", # query nodes
            json.dumps({"layout": "monocle"})
        ]
        mock_check_call.side_effect = subprocess.CalledProcessError(1, "cmd")
        
        arrow.notify = MagicMock()
        
        arrow.run("east")
        
        arrow.notify.assert_called_with("Split blocked: Multiple windows in Fullscreen/Monocle mode.")
        # Should NOT ensure tiled or preselect
        for call_args in mock_run.call_args_list:
            args = call_args[0][0]
            self.assertNotIn("-p", args)
            self.assertNotIn("-l", args)

    @patch('subprocess.run')
    @patch('subprocess.check_output')
    @patch('subprocess.check_call')
    def test_cancel_restore(self, mock_check_call, mock_check_output, mock_run):
        cancel = self.cancel_script.BspwmSmartCancel()
        
        # Current state: Tiled, Not Fullscreen
        mock_check_output.return_value = json.dumps({"layout": "tiled"})
        mock_check_call.side_effect = subprocess.CalledProcessError(1, "cmd")
        
        # Saved state: Monocle, Fullscreen
        state_data = json.dumps({"WAS_FULLSCREEN": True, "PREV_LAYOUT": "monocle"})
        
        with patch('builtins.open', mock_open(read_data=state_data)), \
             patch('os.path.exists', return_value=True), \
             patch('os.remove') as mock_remove:
            
            cancel.run()
            
            # Cancel preselect
            mock_run.assert_any_call(["bspc", "node", "-p", "cancel"])
            
            # Restore layout
            mock_run.assert_any_call(["bspc", "desktop", "-l", "monocle"])
            
            # Restore fullscreen
            mock_run.assert_any_call(["bspc", "node", "focused", "-t", "~fullscreen"])
            
            mock_remove.assert_called_with("/tmp/bspwm_smart_arrow_state.json")

if __name__ == '__main__':
    unittest.main()
