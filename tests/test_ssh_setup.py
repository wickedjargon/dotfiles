import unittest
import sys
import importlib.util
from pathlib import Path

# setup-ssh-repos.py contains hyphens, so we need to import it using importlib
file_path = Path(__file__).parent.parent / 'setup-ssh-repos.py'
spec = importlib.util.spec_from_file_location("setup_ssh_repos", file_path)
ssh_setup = importlib.util.module_from_spec(spec)
sys.modules["setup_ssh_repos"] = ssh_setup
spec.loader.exec_module(ssh_setup)

class TestSSHSetup(unittest.TestCase):
    def test_convert_https_to_ssh(self):
        # Valid conversions
        self.assertEqual(
            ssh_setup.convert_https_to_ssh("https://github.com/user/repo"),
            "git@github.com:user/repo"
        )
        self.assertEqual(
            ssh_setup.convert_https_to_ssh("https://github.com/org/repo.git"),
            "git@github.com:org/repo.git"
        )
        
        # Should not change other URLs
        self.assertEqual(
            ssh_setup.convert_https_to_ssh("http://github.com/user/repo"),
            "http://github.com/user/repo"
        )
        self.assertEqual(
            ssh_setup.convert_https_to_ssh("https://gitlab.com/user/repo"),
            "https://gitlab.com/user/repo"
        )

    def test_is_user_repo(self):
        # Matching repos
        self.assertTrue(ssh_setup.is_user_repo("https://github.com/wickedjargon/dotfiles"))
        self.assertTrue(ssh_setup.is_user_repo("git@github.com:wickedjargon/project"))
        
        # Non-matching repos
        self.assertFalse(ssh_setup.is_user_repo("https://github.com/other/dotfiles"))
        self.assertFalse(ssh_setup.is_user_repo("https://google.com"))

from unittest.mock import patch, MagicMock

class TestConvertRepo(unittest.TestCase):
    def setUp(self):
        # Setup common mocks
        self.repo_path = Path('/home/user/.local/src/repo')
        self.mock_git_dir = self.repo_path / '.git'
    
    @patch('pathlib.Path.is_dir')
    def test_not_a_git_repo(self, mock_is_dir):
        # Setup: .git directory does not exist
        mock_is_dir.return_value = False
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertIsNone(result)
        
    @patch('setup_ssh_repos.get_remote_url')
    @patch('pathlib.Path.is_dir')
    def test_no_remote_url(self, mock_is_dir, mock_get_url):
        mock_is_dir.return_value = True
        mock_get_url.return_value = None
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertIsNone(result)

    @patch('setup_ssh_repos.get_remote_url')
    @patch('pathlib.Path.is_dir')
    def test_non_user_repo(self, mock_is_dir, mock_get_url):
        mock_is_dir.return_value = True
        mock_get_url.return_value = "https://github.com/h/repo.git"
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertIsNone(result)

    @patch('setup_ssh_repos.print_success')
    @patch('setup_ssh_repos.get_remote_url')
    @patch('pathlib.Path.is_dir')
    def test_already_ssh(self, mock_is_dir, mock_get_url, mock_print):
        mock_is_dir.return_value = True
        mock_get_url.return_value = "git@github.com:wickedjargon/repo.git"
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertEqual(result, 'already_ssh')
        mock_print.assert_called_with("Already SSH: repo")

    @patch('setup_ssh_repos.set_remote_url')
    @patch('setup_ssh_repos.get_remote_url')
    @patch('pathlib.Path.is_dir')
    def test_convert_success(self, mock_is_dir, mock_get_url, mock_set_url):
        mock_is_dir.return_value = True
        mock_get_url.return_value = "https://github.com/wickedjargon/repo.git"
        mock_set_url.return_value = True
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertEqual(result, 'converted')
        # Verify it tried to set the correct URL
        mock_set_url.assert_called_with(self.repo_path, "git@github.com:wickedjargon/repo.git")

    @patch('setup_ssh_repos.set_remote_url')
    @patch('setup_ssh_repos.get_remote_url')
    @patch('pathlib.Path.is_dir')
    def test_convert_failure(self, mock_is_dir, mock_get_url, mock_set_url):
        mock_is_dir.return_value = True
        mock_get_url.return_value = "https://github.com/wickedjargon/repo.git"
        mock_set_url.return_value = False
        
        result = ssh_setup.convert_repo(self.repo_path)
        
        self.assertEqual(result, 'failed')

if __name__ == '__main__':
    unittest.main()
