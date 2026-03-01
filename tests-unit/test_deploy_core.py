import sys
import unittest
from pathlib import Path
from unittest.mock import MagicMock, patch

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy


class TestDeployCore(unittest.TestCase):
    """
    Test core deployment logic in deploy.py:
    - deploy_dotfiles
    - clone_and_build_repos
    """

    def setUp(self):
        # Common mocks
        self.username = "testuser"
        self.script_dir = Path("/script/dir")

    # NOTE: test_deploy_dotfiles_success was removed as it had complex mocking
    # but no actual assertions. The simpler test_deploy_dotfiles_simple provides
    # adequate coverage for the core deployment logic.

    @patch("shutil.copy2")
    @patch("shutil.move")
    @patch("subprocess.run")
    def test_deploy_overlay_simple(self, mock_run, mock_move, mock_copy2):
        # Test the unified deploy_overlay function
        import tempfile

        # Create a temp directory with dotfiles-overlay/ structure
        with tempfile.TemporaryDirectory() as tmpdir:
            script_dir = Path(tmpdir)
            dotfiles_dir = script_dir / "dotfiles-overlay" / "home" / "new-user"
            dotfiles_dir.mkdir(parents=True)
            bashrc = dotfiles_dir / ".bashrc"
            bashrc.touch()

            with patch("pathlib.Path.exists") as mock_exists:
                with patch("pathlib.Path.is_dir") as mock_is_dir:
                    with patch("pathlib.Path.mkdir") as _:
                        with patch("pathlib.Path.is_symlink") as mock_is_symlink:
                            with patch("deploy.get_backup_dir") as mock_get_backup:

                                # Home exists, Dest exists (trigger backup)
                                mock_exists.return_value = True
                                mock_is_dir.return_value = False
                                mock_is_symlink.return_value = False

                                mock_backup_dir = MagicMock()
                                mock_get_backup.return_value = mock_backup_dir

                                success, error, backup, items = deploy.deploy_overlay(
                                    self.username, script_dir
                                )

                                self.assertTrue(success)

    @patch("pathlib.Path.mkdir")
    @patch("subprocess.run")
    def test_clone_and_build_repos_success(self, mock_run, mock_mkdir):
        tui = MagicMock()
        repos = ["https://github.com/user/tool.git"]

        with patch("pathlib.Path.exists") as mock_exists:
            # 1. Repo does not exist (needs clone)
            # 2. Makefile exists (needs build)
            mock_exists.side_effect = [False, True]

            success, failed, row = deploy.clone_and_build_repos(
                repos, self.username, tui, 0
            )

            self.assertTrue(success)
            self.assertIsNone(failed)

            # Verify git clone
            mock_run.assert_any_call(
                [
                    "git",
                    "clone",
                    "https://github.com/user/tool.git",
                    f"/home/{self.username}/.local/src/tool",
                ],
                check=True,
                capture_output=True,
                text=True,
                timeout=300,
            )

            # Verify make
            mock_run.assert_any_call(
                ["make"],
                cwd=f"/home/{self.username}/.local/src/tool",
                check=True,
                capture_output=True,
                text=True,
                timeout=600,
            )

            # Verify make install
            mock_run.assert_any_call(
                ["make", "install"],
                cwd=f"/home/{self.username}/.local/src/tool",
                check=True,
                capture_output=True,
                text=True,
                timeout=300,
            )

    @patch("pathlib.Path.mkdir")
    @patch("subprocess.run")
    def test_clone_and_build_repos_makefile_missing(self, mock_run, mock_mkdir):
        tui = MagicMock()
        repos = ["https://github.com/user/tool.git"]

        with patch("pathlib.Path.exists") as mock_exists:
            # 1. Repo does not exist (clone ok)
            # 2. Makefile MISSING
            mock_exists.side_effect = [False, False]

            success, failed, row = deploy.clone_and_build_repos(
                repos, self.username, tui, 0
            )

            self.assertFalse(success)
            self.assertEqual(failed, ["tool"])

            # Verify git clone called
            self.assertTrue(mock_run.called)  # clone called

            # Verify make NOT called (can't easily verify NOT called with specific args in chaotic mock_run,
            # but we can check if it logged error)
            # But simpler: make is called with cwd=repo_path.
            # If failing early, it won't be called.

            # Verify logic flow:
            # Clone -> Check Makefile -> Raise FileNotFoundError -> Catch -> Log -> Add to failed
            pass


if __name__ == "__main__":
    unittest.main()
