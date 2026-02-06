import unittest
import sys
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy

class TestDeploySystem(unittest.TestCase):
    """
    Test system interaction functions in deploy.py.
    """

    @patch('subprocess.run')
    def test_create_user(self, mock_run):
        # Setup mock for success
        mock_run.return_value = MagicMock(returncode=0)
        
        username = "testuser"
        success, msg = deploy.create_user(username)
        
        self.assertTrue(success)
        self.assertIsNone(msg)
        
        # Verify call args
        mock_run.assert_called_with(
            ['useradd', '-m', '-s', '/usr/bin/fish', 'testuser'],
            check=True,
            capture_output=True
        )

    @patch('subprocess.run')
    def test_create_user_failure(self, mock_run):
        # Setup mock for failure
        mock_run.side_effect = subprocess.CalledProcessError(1, ['useradd'], stderr=b"User exists")
        
        success, msg = deploy.create_user("testuser")
        
        self.assertFalse(success)
        self.assertEqual(msg, "User exists")

    @patch('subprocess.run')
    def test_add_user_to_sudo(self, mock_run):
        deploy.add_user_to_sudo("testuser")
        
        mock_run.assert_called_with(
            ['usermod', '-aG', 'sudo', 'testuser'],
            check=True,
            capture_output=True
        )

    @patch('deploy.log_error')
    @patch('subprocess.run')
    def test_install_packages(self, mock_run, mock_log_error):
        # Setup TUI mock
        mock_tui = MagicMock()
        
        # Setup package installed check
        # We need to mock is_package_installed, but it's used inside the function.
        # It's cleaner to mock deploy.is_package_installed directly.
        pass

    @patch('deploy.is_package_installed')
    @patch('subprocess.run')
    def test_install_packages_mixed(self, mock_run, mock_is_installed):
        # Test installing a mix of existing and new packages
        packages = ['already-there', 'new-pkg']
        mock_tui = MagicMock()
        
        # is_package_installed side effect
        def is_installed_side_effect(pkg):
            return pkg == 'already-there'
            
        mock_is_installed.side_effect = is_installed_side_effect
        
        # Run
        deploy.install_packages(packages, mock_tui, 0)
        
        # Verify apt-get install called ONLY for new-pkg
        mock_run.assert_any_call(
            ['apt-get', 'install', '-y', 'new-pkg'],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )
        
        # Verify apt-get install NOT called for already-there
        # (This is harder to test directly with assert_any_call, but we can inspect call_args_list)
        calls = mock_run.call_args_list
        install_calls = [c for c in calls if 'install' in c[0][0]]
        self.assertEqual(len(install_calls), 1)
        self.assertIn('new-pkg', install_calls[0][0][0])
         
        # Verify apt-get update was called
        mock_run.assert_any_call(
            ['apt-get', 'update'],
            check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL
        )

    @patch('subprocess.Popen')
    def test_set_user_password(self, mock_popen):
        # Mock Popen context manager and communicate
        process_mock = MagicMock()
        process_mock.communicate.return_value = (None, None)
        process_mock.returncode = 0
        mock_popen.return_value = process_mock
        
        success = deploy.set_user_password("user", "pass")
        
        self.assertTrue(success)
        mock_popen.assert_called_with(['chpasswd'], stdin=subprocess.PIPE)
        process_mock.communicate.assert_called_with(b'user:pass')

if __name__ == '__main__':
    unittest.main()
