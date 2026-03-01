"""Tests for deploy-dotfiles script."""

import os
import shutil
import tempfile
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

from helpers import get_script_path, import_script

SCRIPT_PATH = get_script_path("deploy-dotfiles")


class TestDeployDotfiles(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_deploy_missing_dotfiles_dir_exits(self):
        original_dir = self.script.DOTFILES_DIR
        self.script.DOTFILES_DIR = Path("/nonexistent/dotfiles/dir")

        with patch.object(self.script.shutil, "which", return_value=None):
            with self.assertRaises(SystemExit) as ctx:
                self.script.deploy()

        self.script.DOTFILES_DIR = original_dir
        self.assertEqual(ctx.exception.code, 1)

    @patch("subprocess.run")
    def test_deploy_valid_dir_calls_rsync(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            # Create dotfiles directory structure: dotfiles-overlay/home/new-user/.bashrc
            dotfiles_dir = Path(tmpdir, "dotfiles-overlay", "home", "new-user")
            dotfiles_dir.mkdir(parents=True)
            src_file = dotfiles_dir / ".bashrc"
            src_file.touch()

            original_dir = self.script.DOTFILES_DIR
            self.script.DOTFILES_DIR = dotfiles_dir

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, "which", return_value=None):
                self.script.deploy()

            self.script.DOTFILES_DIR = original_dir

            rsync_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "rsync"]
            self.assertTrue(len(rsync_calls) > 0)
            rsync_cmd = rsync_calls[0][0][0]
            self.assertIn("-av", rsync_cmd)
        finally:
            shutil.rmtree(tmpdir)

    @patch("subprocess.run")
    def test_directory_sources_get_trailing_slash(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            # Create dotfiles directory structure with a subdirectory
            dotfiles_dir = Path(tmpdir, "dotfiles-overlay", "home", "new-user")
            dotfiles_dir.mkdir(parents=True)
            src_dir = dotfiles_dir / ".config"
            src_dir.mkdir()

            original_dir = self.script.DOTFILES_DIR
            self.script.DOTFILES_DIR = dotfiles_dir

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, "which", return_value=None):
                self.script.deploy()

            self.script.DOTFILES_DIR = original_dir

            rsync_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "rsync"]
            self.assertTrue(len(rsync_calls) > 0)
            src_arg = rsync_calls[0][0][0][-2]
            self.assertTrue(src_arg.endswith("/"))
        finally:
            shutil.rmtree(tmpdir)

    @patch("subprocess.run")
    def test_file_sources_no_trailing_slash(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            # Create dotfiles directory structure with a file
            dotfiles_dir = Path(tmpdir, "dotfiles-overlay", "home", "new-user")
            dotfiles_dir.mkdir(parents=True)
            src_file = dotfiles_dir / ".bashrc"
            src_file.touch()

            original_dir = self.script.DOTFILES_DIR
            self.script.DOTFILES_DIR = dotfiles_dir

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, "which", return_value=None):
                self.script.deploy()

            self.script.DOTFILES_DIR = original_dir

            rsync_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "rsync"]
            self.assertTrue(len(rsync_calls) > 0)
            src_arg = rsync_calls[0][0][0][-2]
            self.assertFalse(src_arg.endswith("/"))
        finally:
            shutil.rmtree(tmpdir)

    @patch("subprocess.run")
    def test_deploys_all_items_in_directory(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            # Create dotfiles directory structure with multiple items
            dotfiles_dir = Path(tmpdir, "dotfiles-overlay", "home", "new-user")
            dotfiles_dir.mkdir(parents=True)
            (dotfiles_dir / ".bashrc").touch()
            (dotfiles_dir / ".xinitrc").touch()
            (dotfiles_dir / ".config").mkdir()

            original_dir = self.script.DOTFILES_DIR
            self.script.DOTFILES_DIR = dotfiles_dir

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, "which", return_value=None):
                self.script.deploy()

            self.script.DOTFILES_DIR = original_dir

            rsync_calls = [c for c in mock_run.call_args_list if c[0][0][0] == "rsync"]
            self.assertEqual(len(rsync_calls), 3)
        finally:
            shutil.rmtree(tmpdir)


if __name__ == "__main__":
    unittest.main()
