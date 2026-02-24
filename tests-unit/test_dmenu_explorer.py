
import unittest
from unittest.mock import patch, MagicMock, mock_open
import os
import subprocess
from pathlib import Path
from importlib.machinery import SourceFileLoader

# Load module from path
TEST_DIR = os.path.dirname(os.path.abspath(__file__))
PROJECT_ROOT = os.path.dirname(TEST_DIR)
SCRIPT_PATH = os.path.join(PROJECT_ROOT, 'root', 'home', 'new-user', '.local', 'bin', 'dmenu-explorer')

if not os.path.exists(SCRIPT_PATH):
    raise FileNotFoundError(f"Could not find script at {SCRIPT_PATH}")

# Use SourceFileLoader directly to handle file without .py extension
dmenu_explorer = SourceFileLoader("dmenu_explorer", SCRIPT_PATH).load_module()

class TestDmenuExplorer(unittest.TestCase):

    def test_load_dmenu_config_valid(self):
        config_content = 'DMENU_BASIC_ARGS="-nb #000 -nf #fff"\nDMENU_FONT="mono-10"\n'
        with patch("builtins.open", mock_open(read_data=config_content)), \
             patch("os.path.exists", return_value=True):
            args, font = dmenu_explorer.load_dmenu_config()
            self.assertEqual(font, "mono-10")
            self.assertIn("-nb", args)
            self.assertIn("#000", args)

    def test_load_dmenu_config_missing(self):
        with patch("os.path.exists", return_value=False):
            args, font = dmenu_explorer.load_dmenu_config()
            self.assertEqual(args, [])
            self.assertEqual(font, "")

    @patch("subprocess.run")
    def test_dmenu_select_success(self, mock_run):
        mock_proc = MagicMock()
        mock_proc.stdout = "Selected Option\n"
        mock_run.return_value = mock_proc
        
        result = dmenu_explorer.dmenu_select(["Option 1", "Selected Option"], [], "")
        self.assertEqual(result, "Selected Option")

    @patch("subprocess.run")
    def test_dmenu_select_cancel(self, mock_run):
        mock_run.side_effect = subprocess.CalledProcessError(1, "dmenu")
        result = dmenu_explorer.dmenu_select(["Option"], [], "")
        self.assertIsNone(result)

    def test_list_directory(self):
        # Mock os.scandir
        with patch("os.scandir") as mock_scandir:
            # Create mock entries
            entry1 = MagicMock()
            entry1.name = "file.txt"
            entry1.is_dir.return_value = False
            entry1.path = "/path/file.txt"
            
            entry2 = MagicMock()
            entry2.name = "folder"
            entry2.is_dir.return_value = True
            entry2.path = "/path/folder"
            
            # Context manager for scandir
            mock_scandir.return_value.__enter__.return_value = [entry1, entry2]
            
            options, mapping = dmenu_explorer.list_directory(Path("/path"))
            
            self.assertIn("..", options)
            self.assertIn("folder/", options)
            self.assertIn("file.txt", options)
            
            # Mapping check
            self.assertEqual(mapping["file.txt"], Path("/path/file.txt"))
            self.assertEqual(mapping["folder/"], Path("/path/folder"))

    @patch("subprocess.Popen")
    def test_open_file_text(self, mock_popen):
        path = Path("/path/to/file.txt")
        with patch.object(Path, "resolve", return_value=path):
            dmenu_explorer.open_file(path)
            mock_popen.assert_called()
            args = mock_popen.call_args[0][0]
            self.assertEqual(args[0], "emacsclient")
            self.assertIn(str(path), args)

    @patch("subprocess.Popen")
    def test_open_file_image(self, mock_popen):
        path = Path("/path/to/image.png")
        with patch.object(Path, "resolve", return_value=path):
            dmenu_explorer.open_file(path)
            mock_popen.assert_called()
            args = mock_popen.call_args[0][0]
            self.assertEqual(args[0], "sxiv")
            self.assertIn(str(path), args)
            
    @patch("subprocess.Popen")
    def test_open_file_video(self, mock_popen):
        path = Path("/path/to/video.mp4")
        with patch.object(Path, "resolve", return_value=path):
            dmenu_explorer.open_file(path)
            mock_popen.assert_called()
            args = mock_popen.call_args[0][0]
            self.assertEqual(args[0], "mpv")

    @patch("subprocess.Popen")
    @patch("dmenu_explorer.load_dmenu_config", return_value=([], ""))
    @patch("dmenu_explorer.subprocess.run")
    def test_open_project(self, mock_run, mock_load_config, mock_popen):
        # Mock dmenu selection for project opener
        mock_proc = MagicMock()
        mock_proc.stdout = "emacsclient\n"
        mock_run.return_value = mock_proc
        
        path = Path("/home/user/d/projects/myproject")
        dmenu_explorer.open_project(path)
        
        mock_popen.assert_called()
        args = mock_popen.call_args[0][0]
        self.assertEqual(args[0], "emacsclient")
        self.assertIn(str(path), args)

if __name__ == "__main__":
    unittest.main()
