
import unittest
import sys
import os
import shutil
import subprocess
import tempfile
from unittest.mock import patch, MagicMock
from pathlib import Path

# Add parent directory to path so we can import deploy
sys.path.append(str(Path(__file__).parent.parent))

# Import deploy module
import deploy

class TestLogging(unittest.TestCase):
    def setUp(self):
        # Create a temp file for logging
        self.temp_log = tempfile.NamedTemporaryFile(delete=False)
        self.temp_log.close()
        self.log_file = self.temp_log.name
        
        # Patch the LOG_FILE in deploy module
        self.patcher = patch('deploy.LOG_FILE', self.log_file)
        self.patcher.start()
            
    def tearDown(self):
        self.patcher.stop()
        if os.path.exists(self.log_file):
            os.remove(self.log_file)

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
            # self.assertIn("content", "content") # dummy assertion

    def test_log_error_with_subprocess_error(self):
        """
        Verify that subprocess errors log the command and return code.
        Note: We updated this to NOT include traceback/exception name for clean logging.
        So we assert that the COMMAND info is there, but we relax the check for "Exception Type" 
        or we check that it's NOT there if we want to be strict.
        For this existing test, I'll just check for the presence of command info.
        """
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
            # Verify NO traceback for subprocess errors
            self.assertNotIn("Traceback:", content)
            self.assertNotIn("Exception Type:", content)

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
