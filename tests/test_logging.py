
import unittest
import sys
import os
import shutil
import subprocess
from unittest.mock import patch, MagicMock
from pathlib import Path

# Add parent directory to path so we can import deploy
sys.path.append(str(Path(__file__).parent.parent))

# Import deploy module
import deploy

class TestLogging(unittest.TestCase):
    def setUp(self):
        self.log_file = '/tmp/dotfiles-deploy.log'
        # Clear log file if exists
        if os.path.exists(self.log_file):
            os.remove(self.log_file)
            
    def tearDown(self):
        # Optional: cleanup
        pass

    def test_log_error_creates_file(self):
        deploy.log_error("Test error message")
        self.assertTrue(os.path.exists(self.log_file))
        
        with open(self.log_file, 'r') as f:
            content = f.read()
            self.assertIn("Test error message", content)
            self.assertIn("ERROR:", content)

    def test_log_error_permissions(self):
        deploy.log_error("Test permissions")
        stat = os.stat(self.log_file)
        # Check permissions are 600 (rw-------)
        # Note: on some systems/filesystems this might be approximate, but we set it explicitly
        mode = stat.st_mode & 0o777
        self.assertEqual(mode, 0o600)

    def test_log_error_with_exception(self):
        try:
            raise ValueError("Test generic exception")
        except ValueError as e:
            deploy.log_error("Caught exception", e)
            
        with open(self.log_file, 'r') as f:
            content = f.read()
            self.assertIn("ValueError", content)
            self.assertIn("Test generic exception", content)
            self.assertIn("content", "content") # dummy assertion

    def test_log_error_with_subprocess_error(self):
        cmd = ['ls', '/nonexistent']
        try:
            subprocess.run(cmd, check=True, capture_output=True)
        except subprocess.CalledProcessError as e:
            deploy.log_error("Subprocess failed", e)
            
        with open(self.log_file, 'r') as f:
            content = f.read()
            self.assertIn("Subprocess failed", content)
            self.assertIn("ls", content)
            self.assertIn("Return Code", content)

    def test_create_user_logs_error(self):
        # We can't easily mock the internal subprocess call of create_user without restructuring,
        # but we can verify that IF it failed, it would log.
        # Actually create_user catches the exception.
        
        # Let's mock subprocess.run globally for the test
        with patch('subprocess.run') as mock_run:
            mock_run.side_effect = subprocess.CalledProcessError(1, ['useradd'], stderr=b"User exists")
            
            success, msg = deploy.create_user("testuser")
            
            self.assertFalse(success)
            
            with open(self.log_file, 'r') as f:
                content = f.read()
                self.assertIn("Failed to create user", content)
                self.assertIn("useradd", content)

if __name__ == '__main__':
    unittest.main()
