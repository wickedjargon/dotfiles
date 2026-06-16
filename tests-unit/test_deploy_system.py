import unittest
import sys
import subprocess
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy_lib

class TestDeploySystem(unittest.TestCase):
    """
    Test system interaction functions in deploy_lib.py.
    """

    @patch('subprocess.run')
    def test_create_user(self, mock_run):
        # Setup mock for success
        mock_run.return_value = MagicMock(returncode=0)
        
        username = "testuser"
        success, msg = deploy_lib.create_user(username)
        
        self.assertTrue(success)
        self.assertIsNone(msg)
        
        # Verify call args
        mock_run.assert_called_with(
            ['useradd', '-m', '-s', '/bin/bash', 'testuser'],
            check=True,
            capture_output=True
        )

    @patch('subprocess.run')
    def test_create_user_failure(self, mock_run):
        # Setup mock for failure
        mock_run.side_effect = subprocess.CalledProcessError(1, ['useradd'], stderr=b"User exists")
        
        success, msg = deploy_lib.create_user("testuser")
        
        self.assertFalse(success)
        self.assertEqual(msg, "User exists")

    @patch('subprocess.run')
    def test_add_user_to_sudo(self, mock_run):
        deploy_lib.add_user_to_sudo("testuser")
        
        mock_run.assert_called_with(
            ['usermod', '-aG', 'sudo', 'testuser'],
            check=True,
            capture_output=True
        )

    @patch('subprocess.run')
    def test_add_user_to_video(self, mock_run):
        deploy_lib.add_user_to_video("testuser")
        
        mock_run.assert_called_with(
            ['usermod', '-aG', 'video', 'testuser'],
            check=True,
            capture_output=True
        )

    @patch('deploy_lib.log_error')
    @patch('subprocess.run')
    def test_install_packages(self, mock_run, mock_log_error):
        # Setup TUI mock
        MagicMock()
        
        # Setup package installed check
        # We need to mock is_package_installed, but it's used inside the function.
        # It's cleaner to mock deploy_lib.is_package_installed directly.
        pass

    @patch('deploy_lib.is_package_installed')
    @patch('subprocess.run')
    def test_install_packages_mixed(self, mock_run, mock_is_installed):
        # Test installing a mix of existing and new packages
        packages = ['already-there', 'new-pkg']
        MagicMock()
        
        # is_package_installed side effect
        def is_installed_side_effect(pkg):
            return pkg == 'already-there'
            
        mock_is_installed.side_effect = is_installed_side_effect
        
        # apt-get update returns success so install proceeds
        mock_run.return_value = MagicMock(returncode=0)

        # Run
        deploy_lib.install_packages(packages, MagicMock(), 0)

        calls = mock_run.call_args_list

        # Verify apt-get install called ONLY for new-pkg (not already-there).
        # Commands now run non-interactively, so match on the command list
        # rather than the full call signature.
        install_calls = [
            c for c in calls if c[0][0][:2] == ['apt-get', 'install']
        ]
        self.assertEqual(len(install_calls), 1)
        install_cmd = install_calls[0][0][0]
        self.assertIn('new-pkg', install_cmd)
        self.assertNotIn('already-there', install_cmd)

        # Non-interactive hardening: noninteractive env, conffile policy, timeout
        install_kwargs = install_calls[0][1]
        self.assertEqual(
            install_kwargs['env'].get('DEBIAN_FRONTEND'), 'noninteractive'
        )
        self.assertIn('Dpkg::Options::=--force-confold', install_cmd)
        self.assertIn('timeout', install_kwargs)

        # Verify apt-get update was called
        update_calls = [c for c in calls if c[0][0][:2] == ['apt-get', 'update']]
        self.assertEqual(len(update_calls), 1)

    @patch('subprocess.Popen')
    def test_set_user_password(self, mock_popen):
        # Mock Popen context manager and communicate
        process_mock = MagicMock()
        process_mock.communicate.return_value = (None, None)
        process_mock.returncode = 0
        mock_popen.return_value = process_mock
        
        success = deploy_lib.set_user_password("user", "pass")
        
        self.assertTrue(success)
        mock_popen.assert_called_with(['chpasswd'], stdin=subprocess.PIPE)
        process_mock.communicate.assert_called_with(b'user:pass')

if __name__ == '__main__':
    unittest.main()
