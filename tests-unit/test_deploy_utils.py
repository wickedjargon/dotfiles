import unittest
import sys
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy

class TestDeployUtils(unittest.TestCase):
    def test_is_valid_git_url(self):
        # Valid URLs
        self.assertTrue(deploy.is_valid_git_url("https://github.com/user/repo.git"))
        self.assertTrue(deploy.is_valid_git_url("git@github.com:user/repo.git"))
        self.assertTrue(deploy.is_valid_git_url("https://gitlab.com/user/repo"))
        
        # Invalid URLs
        self.assertFalse(deploy.is_valid_git_url("http://insecure.com/repo.git"))
        self.assertFalse(deploy.is_valid_git_url("ftp://server/repo.git"))
        self.assertFalse(deploy.is_valid_git_url("; rm -rf /"))  # command injection attempt
        self.assertFalse(deploy.is_valid_git_url("file:///etc/passwd")) # local file access

    def test_is_safe_dest_path(self):
        # We need to mock Path.home() to have a consistent test environment
        with patch('pathlib.Path.home') as mock_home:
            mock_home.return_value = Path('/home/testuser')
            
            # Safe paths
            self.assertTrue(deploy.is_safe_dest_path(".config/myapp"))
            self.assertTrue(deploy.is_safe_dest_path("projects/code"))
            
            # Unsafe paths
            self.assertFalse(deploy.is_safe_dest_path("../escape"))
            self.assertFalse(deploy.is_safe_dest_path("/etc/shadow"))
            self.assertFalse(deploy.is_safe_dest_path("/home/otheruser/.bashrc"))

class TestBackupDir(unittest.TestCase):
    @patch('deploy.Path')
    def test_rotation(self, mock_path_cls):
        # Test that get_backup_dir correctly rotates to .2 if .1 exists
        
        # Setup: Main backup dir exists
        mock_base = MagicMock()
        mock_base.exists.return_value = True
        mock_base.name = '.backup' # Fix: code uses .backup not .dotfiles_backup
        mock_base.__str__.return_value = '/home/u/.backup'
        
        # Setup: .backup2 exists (wait, code uses .backup{counter})
        # Code: backup_base = home / '.backup'
        # Loop starts at counter=2: backup_dir = home / f'.backup{counter}'
        # So it checks .backup2, .backup3...
        
        # Let's align with actual code:
        # 1. .backup exists? Yes.
        # 2. .backup2 exists? Yes.
        # 3. .backup3 exists? No -> return this.
        
        mock_backup2 = MagicMock()
        mock_backup2.exists.return_value = True
        
        mock_backup3 = MagicMock()
        mock_backup3.exists.return_value = False
        mock_backup3.__str__.return_value = '/home/u/.backup3'
        
        # Mock home directory input
        mock_home = MagicMock()
        
        # We need to simulate the division operator behavior on the mock_home object
        def home_div_side_effect(arg):
            if arg == '.backup':
                return mock_base
            if arg == '.backup2':
                return mock_backup2
            if arg == '.backup3':
                return mock_backup3
            return MagicMock()
            
        mock_home.__truediv__.side_effect = home_div_side_effect
        
        # Run - Pass the mock_home directly as the function expects a Path-like object
        result = deploy.get_backup_dir(mock_home)
        
        # Assert
        self.assertEqual(str(result), '/home/u/.backup3')

if __name__ == '__main__':
    unittest.main()
