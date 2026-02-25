import subprocess
import unittest
from unittest.mock import MagicMock, patch, mock_open
import sys
from pathlib import Path

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy


class TestReadDistroboxPackages(unittest.TestCase):
    """Test reading distrobox package list files."""

    def test_reads_packages(self):
        content = "base\nbase-devel\n\nsteam\n"
        with patch("builtins.open", mock_open(read_data=content)):
            with patch.object(Path, "exists", return_value=True):
                result = deploy.read_distrobox_packages(Path("/fake"), "pkglist.txt")
        self.assertEqual(result, ["base", "base-devel", "steam"])

    def test_skips_empty_lines(self):
        content = "\n\nfoo\n\nbar\n\n"
        with patch("builtins.open", mock_open(read_data=content)):
            with patch.object(Path, "exists", return_value=True):
                result = deploy.read_distrobox_packages(Path("/fake"), "pkglist.txt")
        self.assertEqual(result, ["foo", "bar"])

    def test_missing_file_returns_empty(self):
        with patch.object(Path, "exists", return_value=False):
            result = deploy.read_distrobox_packages(Path("/fake"), "missing.txt")
        self.assertEqual(result, [])


class TestDistroboxExists(unittest.TestCase):
    """Test container existence check."""

    @patch("deploy._run_as_user")
    def test_exists(self, mock_run):
        mock_run.return_value = (True, "abc123 | archbox | running | archlinux\n", "")
        self.assertTrue(deploy._distrobox_exists("testuser"))

    @patch("deploy._run_as_user")
    def test_not_exists(self, mock_run):
        mock_run.return_value = (True, "abc123 | fedora | running | fedora:latest\n", "")
        self.assertFalse(deploy._distrobox_exists("testuser"))

    @patch("deploy._run_as_user")
    def test_no_containers(self, mock_run):
        mock_run.return_value = (True, "", "")
        self.assertFalse(deploy._distrobox_exists("testuser"))

    @patch("deploy._run_as_user")
    def test_command_fails(self, mock_run):
        mock_run.return_value = (False, "", "not found")
        self.assertFalse(deploy._distrobox_exists("testuser"))


class TestRunAsUser(unittest.TestCase):
    """Test the _run_as_user wrapper."""

    @patch("subprocess.run")
    def test_success(self, mock_run):
        mock_run.return_value = MagicMock(stdout="output", stderr="")
        ok, stdout, stderr = deploy._run_as_user("testuser", "echo hi")
        self.assertTrue(ok)
        self.assertEqual(stdout, "output")
        cmd = mock_run.call_args[0][0]
        self.assertEqual(cmd, ['su', '-', 'testuser', '-c', 'echo hi'])

    @patch("subprocess.run", side_effect=subprocess.CalledProcessError(1, "cmd", stderr="err"))
    def test_failure(self, mock_run):
        ok, stdout, stderr = deploy._run_as_user("testuser", "false")
        self.assertFalse(ok)

    @patch("subprocess.run", side_effect=subprocess.TimeoutExpired("cmd", 5))
    def test_timeout(self, mock_run):
        ok, stdout, stderr = deploy._run_as_user("testuser", "sleep 999")
        self.assertFalse(ok)
        self.assertIn("timed out", stderr)


class TestRunInDistrobox(unittest.TestCase):
    """Test the _run_in_distrobox wrapper."""

    @patch("deploy._run_as_user")
    def test_wraps_command(self, mock_run):
        mock_run.return_value = (True, "output", "")
        ok, stdout, stderr = deploy._run_in_distrobox("testuser", "echo hi")
        self.assertTrue(ok)
        # Check that the command is wrapped with distrobox enter
        actual_cmd = mock_run.call_args[0][1]
        self.assertIn("distrobox enter archbox", actual_cmd)
        self.assertIn("echo hi", actual_cmd)


class TestSetupDistrobox(unittest.TestCase):
    """Test the main setup_distrobox orchestrator."""

    @patch("deploy.read_distrobox_packages")
    @patch("deploy._run_in_distrobox")
    @patch("deploy._run_as_user")
    @patch("deploy._distrobox_exists")
    def test_creates_new_container(self, mock_exists, mock_run_user, mock_run_box, mock_pkgs):
        tui = MagicMock()
        mock_exists.return_value = False
        # create succeeds, all distrobox commands succeed
        mock_run_user.return_value = (True, "", "")
        mock_run_box.return_value = (True, "", "")
        mock_pkgs.return_value = []

        success, error, row = deploy.setup_distrobox("testuser", Path("/fake"), tui, 0)
        self.assertTrue(success)
        self.assertIsNone(error)

    @patch("deploy.read_distrobox_packages")
    @patch("deploy._run_in_distrobox")
    @patch("deploy._run_as_user")
    @patch("deploy._distrobox_exists")
    def test_skips_existing_container(self, mock_exists, mock_run_user, mock_run_box, mock_pkgs):
        tui = MagicMock()
        mock_exists.return_value = True
        mock_run_box.return_value = (True, "", "")
        mock_pkgs.return_value = []

        success, error, row = deploy.setup_distrobox("testuser", Path("/fake"), tui, 0)
        self.assertTrue(success)
        # create should not have been called, but stop should have been
        self.assertEqual(mock_run_user.call_count, 1)
        self.assertIn("distrobox stop", mock_run_user.call_args[0][1])

    @patch("deploy._run_as_user")
    @patch("deploy._distrobox_exists")
    def test_create_failure(self, mock_exists, mock_run_user):
        tui = MagicMock()
        mock_exists.return_value = False
        mock_run_user.return_value = (False, "", "container error")

        success, error, row = deploy.setup_distrobox("testuser", Path("/fake"), tui, 0)
        self.assertFalse(success)
        self.assertIn("container", error.lower())

    @patch("deploy.read_distrobox_packages")
    @patch("deploy._run_in_distrobox")
    @patch("deploy._distrobox_exists")
    def test_sync_failure_is_fatal(self, mock_exists, mock_run_box, mock_pkgs):
        tui = MagicMock()
        mock_exists.return_value = True
        mock_pkgs.return_value = []
        # init ok, multilib check ok, keyring ok, sync fails
        mock_run_box.side_effect = [
            (True, "", ""),   # init container
            (True, "", ""),   # multilib check (already enabled)
            (True, "", ""),   # keyring init
            (False, "", "sync failed"),  # pacman -Syu fails
        ]

        success, error, row = deploy.setup_distrobox("testuser", Path("/fake"), tui, 0)
        self.assertFalse(success)
        self.assertIn("sync", error.lower())


class TestDeployCLIDistroboxFlag(unittest.TestCase):
    """Test CLI --distrobox flag."""

    def test_distrobox_flag_default(self):
        sys.path.append(str(Path(__file__).parent.parent))
        import deploy_cli
        args = deploy_cli.parse_args(["--username", "test"])
        self.assertFalse(args.distrobox)

    def test_distrobox_flag_set(self):
        import deploy_cli
        args = deploy_cli.parse_args(["--username", "test", "--distrobox"])
        self.assertTrue(args.distrobox)


if __name__ == "__main__":
    unittest.main()
