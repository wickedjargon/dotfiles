"""Tests for deploy-dotfiles script."""

import unittest
from unittest.mock import patch, MagicMock, call
import os
import sys
import tempfile
import shutil
from pathlib import Path

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('deploy-dotfiles')


class TestDeployDotfiles(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_deploy_missing_manifest_exits(self):
        original_manifest = self.script.MANIFEST_FILE
        self.script.MANIFEST_FILE = Path("/nonexistent/manifest/file")

        with patch.object(self.script.shutil, 'which', return_value=None):
            with self.assertRaises(SystemExit) as ctx:
                self.script.deploy()

        self.script.MANIFEST_FILE = original_manifest
        self.assertEqual(ctx.exception.code, 1)

    @patch('subprocess.run')
    def test_deploy_valid_manifest_calls_rsync(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            src_file = Path(tmpdir, "repo", ".bashrc")
            src_file.parent.mkdir(parents=True)
            src_file.touch()

            manifest = Path(tmpdir, "manifest")
            manifest.write_text("/.bashrc\n")

            original_manifest = self.script.MANIFEST_FILE
            original_repo = self.script.REPO_PATH
            self.script.MANIFEST_FILE = manifest
            self.script.REPO_PATH = Path(tmpdir, "repo")

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, 'which', return_value=None):
                self.script.deploy()

            self.script.MANIFEST_FILE = original_manifest
            self.script.REPO_PATH = original_repo

            rsync_calls = [c for c in mock_run.call_args_list
                           if c[0][0][0] == 'rsync']
            self.assertTrue(len(rsync_calls) > 0)
            rsync_cmd = rsync_calls[0][0][0]
            self.assertIn('-av', rsync_cmd)
        finally:
            shutil.rmtree(tmpdir)

    @patch('subprocess.run')
    def test_directory_sources_get_trailing_slash(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            src_dir = Path(tmpdir, "repo", ".config")
            src_dir.mkdir(parents=True)

            manifest = Path(tmpdir, "manifest")
            manifest.write_text("/.config/\n")

            original_manifest = self.script.MANIFEST_FILE
            original_repo = self.script.REPO_PATH
            self.script.MANIFEST_FILE = manifest
            self.script.REPO_PATH = Path(tmpdir, "repo")

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, 'which', return_value=None):
                self.script.deploy()

            self.script.MANIFEST_FILE = original_manifest
            self.script.REPO_PATH = original_repo

            rsync_calls = [c for c in mock_run.call_args_list
                           if c[0][0][0] == 'rsync']
            self.assertTrue(len(rsync_calls) > 0)
            src_arg = rsync_calls[0][0][0][2]
            self.assertTrue(src_arg.endswith('/'))
        finally:
            shutil.rmtree(tmpdir)

    @patch('subprocess.run')
    def test_file_sources_no_trailing_slash(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            src_file = Path(tmpdir, "repo", ".bashrc")
            src_file.parent.mkdir(parents=True)
            src_file.touch()

            manifest = Path(tmpdir, "manifest")
            manifest.write_text("/.bashrc\n")

            original_manifest = self.script.MANIFEST_FILE
            original_repo = self.script.REPO_PATH
            self.script.MANIFEST_FILE = manifest
            self.script.REPO_PATH = Path(tmpdir, "repo")

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, 'which', return_value=None):
                self.script.deploy()

            self.script.MANIFEST_FILE = original_manifest
            self.script.REPO_PATH = original_repo

            rsync_calls = [c for c in mock_run.call_args_list
                           if c[0][0][0] == 'rsync']
            self.assertTrue(len(rsync_calls) > 0)
            src_arg = rsync_calls[0][0][0][2]
            self.assertFalse(src_arg.endswith('/'))
        finally:
            shutil.rmtree(tmpdir)

    @patch('subprocess.run')
    def test_skips_missing_source_files(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            manifest = Path(tmpdir, "manifest")
            manifest.write_text("/.nonexistent_file\n")

            original_manifest = self.script.MANIFEST_FILE
            original_repo = self.script.REPO_PATH
            self.script.MANIFEST_FILE = manifest
            self.script.REPO_PATH = Path(tmpdir, "repo")

            Path(tmpdir, "repo").mkdir(exist_ok=True)

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, 'which', return_value=None):
                self.script.deploy()

            self.script.MANIFEST_FILE = original_manifest
            self.script.REPO_PATH = original_repo

            rsync_calls = [c for c in mock_run.call_args_list
                           if len(c[0][0]) > 0 and c[0][0][0] == 'rsync']
            self.assertEqual(len(rsync_calls), 0)
        finally:
            shutil.rmtree(tmpdir)

    @patch('subprocess.run')
    def test_comments_and_empty_lines_skipped(self, mock_run):
        tmpdir = tempfile.mkdtemp()
        try:
            src_file = Path(tmpdir, "repo", ".bashrc")
            src_file.parent.mkdir(parents=True)
            src_file.touch()

            manifest = Path(tmpdir, "manifest")
            manifest.write_text("# This is a comment\n\n/.bashrc\n\n# Another comment\n")

            original_manifest = self.script.MANIFEST_FILE
            original_repo = self.script.REPO_PATH
            self.script.MANIFEST_FILE = manifest
            self.script.REPO_PATH = Path(tmpdir, "repo")

            mock_run.return_value = MagicMock(returncode=0)

            with patch.object(self.script.shutil, 'which', return_value=None):
                self.script.deploy()

            self.script.MANIFEST_FILE = original_manifest
            self.script.REPO_PATH = original_repo

            rsync_calls = [c for c in mock_run.call_args_list
                           if c[0][0][0] == 'rsync']
            self.assertEqual(len(rsync_calls), 1)
        finally:
            shutil.rmtree(tmpdir)


if __name__ == '__main__':
    unittest.main()
