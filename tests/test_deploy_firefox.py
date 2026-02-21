import unittest
import sys
import subprocess
import tempfile
import shutil
from pathlib import Path
from unittest.mock import patch, MagicMock

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy


class TestConfigureFirefoxUserjs(unittest.TestCase):
    """Test configure_firefox_userjs function in deploy.py."""

    def setUp(self):
        self.username = "testuser"
        self.tmpdir = tempfile.mkdtemp()
        self.home_dir = Path(self.tmpdir)
        self.firefox_dir = self.home_dir / ".mozilla" / "firefox"

    def tearDown(self):
        shutil.rmtree(self.tmpdir)

    # --- Test: Firefox directory does not exist ---
    def test_no_firefox_dir(self):
        """Returns (True, None) when ~/.mozilla/firefox/ doesn't exist."""
        success, error = deploy.configure_firefox_userjs(
            self.username, _home_dir=self.home_dir
        )
        self.assertTrue(success)
        self.assertIsNone(error)

    # --- Test: No matching profile ---
    def test_no_matching_profile(self):
        """Returns (True, None) when no .default/.default-release dirs found."""
        self.firefox_dir.mkdir(parents=True)
        (self.firefox_dir / "abc123.other").mkdir()

        success, error = deploy.configure_firefox_userjs(
            self.username, _home_dir=self.home_dir
        )
        self.assertTrue(success)
        self.assertIsNone(error)

    # --- Test: Prefers .default-release over .default-esr and .default ---
    def test_prefers_default_release(self):
        """Selects .default-release profile when both exist."""
        self.firefox_dir.mkdir(parents=True)
        default_dir = self.firefox_dir / "abc123.default"
        default_dir.mkdir()
        esr_dir = self.firefox_dir / "def456.default-esr"
        esr_dir.mkdir()
        release_dir = self.firefox_dir / "xyz789.default-release"
        release_dir.mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// larbs\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run"):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)
        self.assertIsNone(error)
        self.assertTrue((release_dir / "user.js").exists())
        self.assertFalse((esr_dir / "user.js").exists())
        self.assertFalse((default_dir / "user.js").exists())


    # --- Test: Falls back to .default-esr when no .default-release ---
    def test_falls_back_to_default_esr(self):
        """Selects .default-esr profile when .default-release doesn't exist."""
        self.firefox_dir.mkdir(parents=True)
        default_dir = self.firefox_dir / "abc123.default"
        default_dir.mkdir()
        esr_dir = self.firefox_dir / "def456.default-esr"
        esr_dir.mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// larbs\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run"):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)
        self.assertTrue((esr_dir / "user.js").exists())
        self.assertFalse((default_dir / "user.js").exists())

    # --- Test: Falls back to .default when no .default-release or .default-esr ---
    def test_falls_back_to_default(self):
        """Selects .default profile when .default-release doesn't exist."""
        self.firefox_dir.mkdir(parents=True)
        default_dir = self.firefox_dir / "abc123.default"
        default_dir.mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// larbs\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run"):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)
        self.assertTrue((default_dir / "user.js").exists())

    # --- Test: Backup existing user.js ---
    def test_backup_existing_userjs(self):
        """Existing user.js gets backed up with timestamp before overwriting."""
        self.firefox_dir.mkdir(parents=True)
        profile_dir = self.firefox_dir / "abc123.default-release"
        profile_dir.mkdir()

        existing_userjs = profile_dir / "user.js"
        existing_userjs.write_text("// old content\n")

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// larbs\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run"):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)

        # Check backup was created
        backups = list(profile_dir.glob("user.js.backup.*"))
        self.assertEqual(len(backups), 1)
        self.assertEqual(backups[0].read_text(), "// old content\n")

    # --- Test: Downloads and combines in correct order ---
    def test_download_and_combine(self):
        """Arkenfox + larbs.js + override are written in correct order."""
        self.firefox_dir.mkdir(parents=True)
        profile_dir = self.firefox_dir / "abc123.default"
        profile_dir.mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// ARKENFOX CONTENT\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// LARBS CONTENT\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run"):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)

        content = (profile_dir / "user.js").read_text()
        ark_pos = content.index("// ARKENFOX CONTENT")
        larbs_pos = content.index("// LARBS CONTENT")
        override_pos = content.index('user_pref("dom.push.enabled", true)')

        self.assertLess(ark_pos, larbs_pos)
        self.assertLess(larbs_pos, override_pos)

    # --- Test: Curl failure (Arkenfox) ---
    def test_curl_failure_arkenfox(self):
        """Returns (False, error_msg) on Arkenfox download failure."""
        self.firefox_dir.mkdir(parents=True)
        (self.firefox_dir / "abc123.default-release").mkdir()

        with patch("deploy.run_command_with_retry",
                   side_effect=subprocess.CalledProcessError(1, ["curl"])):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertFalse(success)
        self.assertIn("Arkenfox", error)

    # --- Test: Curl failure (larbs.js) ---
    def test_curl_failure_larbs(self):
        """Returns (False, error_msg) on larbs.js download failure."""
        self.firefox_dir.mkdir(parents=True)
        (self.firefox_dir / "abc123.default").mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox,
                                subprocess.CalledProcessError(1, ["curl"])]):
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertFalse(success)
        self.assertIn("larbs", error)

    # --- Test: chown is called ---
    def test_chown_called(self):
        """chown is called with correct username on the user.js file."""
        self.firefox_dir.mkdir(parents=True)
        profile_dir = self.firefox_dir / "abc123.default"
        profile_dir.mkdir()

        mock_arkenfox = MagicMock()
        mock_arkenfox.stdout = "// arkenfox\n"
        mock_larbs = MagicMock()
        mock_larbs.stdout = "// larbs\n"

        with patch("deploy.run_command_with_retry",
                   side_effect=[mock_arkenfox, mock_larbs]), \
             patch("subprocess.run") as mock_run:
            success, error = deploy.configure_firefox_userjs(
                self.username, _home_dir=self.home_dir
            )

        self.assertTrue(success)

        # Find the chown call
        chown_calls = [c for c in mock_run.call_args_list
                       if c[0][0][0] == "chown"]
        self.assertEqual(len(chown_calls), 1)
        self.assertIn(f"{self.username}:{self.username}", chown_calls[0][0][0])


if __name__ == "__main__":
    unittest.main()
