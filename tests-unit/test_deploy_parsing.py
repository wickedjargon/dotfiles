#!/usr/bin/env python

import unittest
import sys
from pathlib import Path
from unittest.mock import patch, mock_open

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy

class TestDeployParsing(unittest.TestCase):
    @patch('pathlib.Path.exists')
    def test_read_git_packages_src_file(self, mock_exists):
        mock_exists.return_value = True
        # Mock content of git-packages-src
        content = """
# This is a comment
https://github.com/user/repo1.git
  https://github.com/user/repo2.git  
		
"""
        with patch('builtins.open', mock_open(read_data=content)):
            # Warning: deploy.read_git_packages_src_file likely constructs Path(script_dir) / 'git-packages-src'
            # We need to make sure the open call catches that specific path or any path.
            
            repos = deploy.read_git_packages_src_file(Path("/tmp"))
            
            self.assertEqual(len(repos), 2)
            self.assertEqual(repos[0], "https://github.com/user/repo1.git")
            self.assertEqual(repos[1], "https://github.com/user/repo2.git")

    @patch('pathlib.Path.exists')
    def test_read_git_dotfiles_file(self, mock_exists):
        mock_exists.return_value = True
        # Mock content of git-dotfiles
        content = """
https://github.com/user/vim.git .vim
# Comment
https://github.com/user/zsh.git .zsh    
"""
        with patch('builtins.open', mock_open(read_data=content)):
            repos = deploy.read_git_dotfiles_file(Path("/tmp"))
            
            self.assertEqual(len(repos), 2)
            self.assertEqual(repos[0], ("https://github.com/user/vim.git", ".vim"))
            self.assertEqual(repos[1], ("https://github.com/user/zsh.git", ".zsh"))




    @patch('pathlib.Path.exists')
    def test_parsing_edge_cases(self, mock_exists):
        # Comprehensive test for comments, whitespace, and filtering
        mock_exists.return_value = True
        
        # 1. git-packages-src edge cases
        content_pkg = """
        https://github.com/valid/repo.git # inline comment
        
        # full line comment
           https://github.com/valid/repo2.git   
        http://insecure.com/bad.git
        """
        with patch('builtins.open', mock_open(read_data=content_pkg)):
            repos = deploy.read_git_packages_src_file(Path("/tmp"))
            self.assertEqual(len(repos), 2)
            self.assertEqual(repos[0], "https://github.com/valid/repo.git")
            self.assertEqual(repos[1], "https://github.com/valid/repo2.git")

        # 2. git-dotfiles edge cases
        content_dot = """
        https://github.com/v/r.git .config  # comment
        https://github.com/v/r2.git /unsafe/absolute
        https://github.com/v/r3.git ../unsafe/rel
        http://bad.com/r.git .valid
        """
        with patch('builtins.open', mock_open(read_data=content_dot)):
            repos = deploy.read_git_dotfiles_file(Path("/tmp"))
            # Should only get the first one
            self.assertEqual(len(repos), 1)
            self.assertEqual(repos[0], ("https://github.com/v/r.git", ".config"))

if __name__ == '__main__':
    unittest.main()
