
import unittest
from unittest.mock import patch, MagicMock, mock_open
import sys
import os
import subprocess
from pathlib import Path
from importlib.machinery import SourceFileLoader

# Load module from path
TEST_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(TEST_DIR)
SCRIPT_PATH = os.path.join(PROJECT_ROOT, 'root', 'home', 'new-user', '.local', 'bin', 'dmenu-open-project')

if not os.path.exists(SCRIPT_PATH):
    raise FileNotFoundError(f"Could not find script at {SCRIPT_PATH}")

# Use SourceFileLoader directly to handle file without .py extension
dmenu_open_project = SourceFileLoader("dmenu_open_project", SCRIPT_PATH).load_module()

class TestDmenuOpenProject(unittest.TestCase):

    def test_get_directories(self):
        # Patch os.scandir where it is used
        with patch("dmenu_open_project.os.scandir") as mock_scandir:
            entry1 = MagicMock()
            entry1.name = "project_a"
            entry1.is_dir.return_value = True
            entry1.path = "/home/user/d/projects/project_a"
            
            entry2 = MagicMock()
            entry2.name = "file.txt"
            entry2.is_dir.return_value = False
            
            entry3 = MagicMock()
            entry3.name = "project_b"
            entry3.is_dir.return_value = True
            entry3.path = "/home/user/d/projects/project_b"
            
            # The script iterates directly over os.scandir() result (no context manager)
            mock_scandir.return_value = [entry3, entry2, entry1]
            
            # Patch Path.exists on the class used by the module
            with patch("dmenu_open_project.Path.exists", return_value=True):
                # Pass a directory that conforms to what we expect
                dirs = dmenu_open_project.get_directories(dmenu_open_project.Path("/some/path"))
                
            self.assertEqual(len(dirs), 2)
            self.assertEqual(dirs[0][0], "project_a")
            self.assertEqual(dirs[1][0], "project_b")

    def test_get_directories_not_exists(self):
        with patch.object(Path, "exists", return_value=False):
            dirs = dmenu_open_project.get_directories(Path("/nonexistent"))
            self.assertEqual(dirs, [])

    @patch("subprocess.run")
    def test_dmenu_select(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.stdout = "project_a\n"
        mock_run.return_value = mock_proc
        
        options = [("project_a", "/path/a"), ("project_b", "/path/b")]
        result = dmenu_open_project.dmenu_select(options, [], "")
        self.assertEqual(result, "project_a")
        
        # Verify input passed to dmenu
        args = mock_run.call_args
        # input is passed via 'input' kwarg
        self.assertIn("project_a\nproject_b", args[1]['input'])

    @patch("dmenu_open_project.get_directories")
    @patch("dmenu_open_project.load_dmenu_config")
    @patch("dmenu_open_project.dmenu_select")
    @patch("subprocess.Popen")
    def test_main_success(self, mock_popen, mock_select, mock_load, mock_get_dirs):
        # Setup mocks
        mock_get_dirs.return_value = [("proj1", "/path/p1"), ("proj2", "/path/p2")]
        mock_load.return_value = (["-nb", "#000"], "font")
        mock_select.return_value = "proj1"
        
        dmenu_open_project.main()
        
        # Verify emacs opened with correct path
        mock_popen.assert_called()
        cmd = mock_popen.call_args[0][0]
        self.assertEqual(cmd[0], "emacsclient")
        self.assertIn("/path/p1", cmd)

    @patch("dmenu_open_project.get_directories")
    @patch("subprocess.run")
    def test_main_no_projects(self, mock_run, mock_get_dirs):
        mock_get_dirs.return_value = []
        
        dmenu_open_project.main()
        
        # Verify notification
        mock_run.assert_called()
        cmd = mock_run.call_args[0][0]
        self.assertEqual(cmd[0], "notify-send")
        self.assertIn("Error", cmd)

    @patch("dmenu_open_project.get_directories")
    @patch("dmenu_open_project.load_dmenu_config")
    @patch("dmenu_open_project.dmenu_select")
    @patch("subprocess.Popen")
    def test_main_cancel(self, mock_popen, mock_select, mock_load, mock_get_dirs):
        mock_get_dirs.return_value = [("proj1", "/path/p1")]
        mock_load.return_value = ([], "")
        mock_select.return_value = None # User cancelled
        
        dmenu_open_project.main()
        
        # Verify NO execution
        mock_popen.assert_not_called()

if __name__ == "__main__":
    unittest.main()
