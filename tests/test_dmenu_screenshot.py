
import unittest
from unittest.mock import patch, MagicMock, ANY
import sys
import os
import subprocess
from importlib.machinery import SourceFileLoader

# Load module from path
TEST_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(TEST_DIR)
SCRIPT_PATH = os.path.join(PROJECT_ROOT, 'root', 'home', 'new-user', '.local', 'bin', 'dmenu-screenshot')

if not os.path.exists(SCRIPT_PATH):
    raise FileNotFoundError(f"Could not find script at {SCRIPT_PATH}")

# Helper to load the module, patching the dynamic import inside it
def load_module_safely():
    # We need to patch the _import_upload_selection function immediately after loading?
    # Or better, patch importlib.machinery.SourceFileLoader inside the module?
    # Since the script calls _import_upload_selection() at module level (line 23),
    # we have to mock it BEFORE it executes.
    
    # Actually, SourceFileLoader.load_module() executes the module.
    # We can mock importlib in the context where we load the module?
    # A bit tricky.
    
    # Approach: Mock importlib.machinery.SourceFileLoader locally before loading script?
    # But the script imports it.
    
    # Let's rely on patching `importlib.machinery.SourceFileLoader` which the script imports.
    pass

# We can pre-patch the dynamic import mechanism used by the script
with patch('importlib.machinery.SourceFileLoader') as mock_loader:
    # Setup the mock module that would be returned
    mock_upload_mod = MagicMock()
    mock_upload_mod.upload_file_to_0x0 = MagicMock(return_value="http://0x0.st/test.png")
    mock_upload_mod.copy_to_clipboard = MagicMock()
    mock_upload_mod.notify = MagicMock()
    
    # When loader.load_module() doesn't exist in newer python (exec_module is used), 
    # but check script: uses exec_module.
    # The script calls:
    # loader = SourceFileLoader(...)
    # spec = spec_from_loader(...)
    # mod = module_from_spec(...)
    # loader.exec_module(mod)
    
    # We need to ensure that when the script runs `loader.exec_module(mod)`, 
    # `mod` gets populated with our mock functions.
    
    # This is getting complex to mock "perfectly" at module level execution.
    # Alternative: Use SourceFileLoader to load it, but catch the ImportError if upload-selection is missing?
    # No, we want to mock it.
    
    # Simplest way: The script runs `_import_upload_selection` at top level.
    # We can mock `importlib.machinery` in `sys.modules`?
    pass

# Let's try to just load it and patch the `_upload_mod` attribute after loading if possible, 
# OR patch `importlib` during the load.

class TestDmenuScreenshot(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # We patch importlib stuff globally for the load
        patcher1 = patch('importlib.machinery.SourceFileLoader')
        patcher2 = patch('importlib.util.spec_from_loader')
        patcher3 = patch('importlib.util.module_from_spec')
        
        MockLoader = patcher1.start()
        mock_spec_from_loader = patcher2.start()
        mock_module_from_spec = patcher3.start()
        
        # Ensure we clean up patches
        cls.addClassCleanup(patcher1.stop)
        cls.addClassCleanup(patcher2.stop)
        cls.addClassCleanup(patcher3.stop)
        
        # Setup the mock module that would be returned
        cls.mock_upload_mod = MagicMock()
        cls.mock_upload_mod.upload_file_to_0x0.return_value = "http://fake.url/img.png"
        cls.mock_upload_mod.copy_to_clipboard = MagicMock()
        cls.mock_upload_mod.notify = MagicMock()
        
        # Make module_from_spec return our mock module
        mock_module_from_spec.return_value = cls.mock_upload_mod
        
        # Use the REAL SourceFileLoader to load the script itself
        cls.dmenu_screenshot = SourceFileLoader("dmenu_screenshot", SCRIPT_PATH).load_module()


    def test_run_dmenu(self):
        with patch('subprocess.Popen') as mock_popen:
            process = MagicMock()
            process.communicate.return_value = (b"Selected Option\n", b"")
            mock_popen.return_value = process
            
            result = self.dmenu_screenshot.run_dmenu(["Opt1", "Opt2"], "Prompt:")
            self.assertEqual(result, "Selected Option")
            
            # Verify usage
            mock_popen.assert_called()
            args = mock_popen.call_args[0][0]
            self.assertEqual(args[0], "dmenu")
            self.assertIn("Prompt:", args)

    @patch('subprocess.run')
    @patch('os.path.exists')
    def test_take_screenshot_success(self, mock_exists, mock_run):
        mock_exists.return_value = True
        
        # Call
        filepath = self.dmenu_screenshot.take_screenshot(rect=True)
        
        # Verify
        self.assertIsNotNone(filepath)
        self.assertIn("screenshot-", filepath)
        # Check command
        mock_run.assert_called()
        cmd = mock_run.call_args[0][0]
        self.assertEqual(cmd[0], "maim")
        self.assertIn("-s", cmd) # rect=True

    @patch('subprocess.run')
    def test_take_screenshot_full(self, mock_run):
        with patch('os.path.exists', return_value=True):
            self.dmenu_screenshot.take_screenshot(rect=False)
            
        cmd = mock_run.call_args[0][0]
        self.assertEqual(cmd[0], "maim")
        self.assertNotIn("-s", cmd)

    @patch('subprocess.run')
    def test_take_screenshot_fail(self, mock_run):
        # Simulate maim failing
        mock_run.side_effect = subprocess.CalledProcessError(1, "maim")
        
        result = self.dmenu_screenshot.take_screenshot()
        self.assertIsNone(result)

    @patch('dmenu_screenshot.run_dmenu')
    @patch('dmenu_screenshot.take_screenshot')
    def test_main_save(self, mock_take_shot, mock_dmenu):
        # Setup
        mock_dmenu.side_effect = ["Full Screenshot", "Save to /tmp/ (Clip Path)"]
        mock_take_shot.return_value = "/tmp/shot.png"
        
        self.dmenu_screenshot.main()
        
        self.mock_upload_mod.copy_to_clipboard.assert_called_with("/tmp/shot.png")
        self.mock_upload_mod.notify.assert_called()

    @patch('dmenu_screenshot.run_dmenu')
    @patch('dmenu_screenshot.take_screenshot')
    def test_main_upload_success(self, mock_take_shot, mock_dmenu):
        # Setup
        mock_dmenu.side_effect = ["Region Screenshot", "Upload to 0x0.st (Clip URL)"]
        mock_take_shot.return_value = "/tmp/shot.png"
        self.mock_upload_mod.upload_file_to_0x0.return_value = "http://url"
        
        self.dmenu_screenshot.main()
        
        self.mock_upload_mod.upload_file_to_0x0.assert_called_with("/tmp/shot.png")
        self.mock_upload_mod.copy_to_clipboard.assert_called_with("http://url")

    @patch('dmenu_screenshot.run_dmenu')
    @patch('dmenu_screenshot.take_screenshot')
    def test_main_upload_fail(self, mock_take_shot, mock_dmenu):
        # Setup
        mock_dmenu.side_effect = ["Full Screenshot", "Upload to 0x0.st (Clip URL)"]
        mock_take_shot.return_value = "/tmp/shot.png"
        self.mock_upload_mod.upload_file_to_0x0.return_value = None
        
        self.dmenu_screenshot.main()
        
        self.mock_upload_mod.notify.assert_called_with("Upload Failed", ANY)

if __name__ == "__main__":
    unittest.main()
