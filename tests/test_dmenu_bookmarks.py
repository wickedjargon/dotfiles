"""Tests for dmenu-bookmarks script."""

import unittest
from unittest.mock import patch, MagicMock
import os
import tempfile

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-bookmarks')


class TestParseBookmarks(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def _parse(self, content):
        with tempfile.NamedTemporaryFile(mode='w', suffix='.md', delete=False) as f:
            f.write(content)
            path = f.name
        try:
            return self.script.parse_bookmarks(path)
        finally:
            os.unlink(path)

    def test_url_entry(self):
        entries = self._parse("# Title\nhttps://example.com\n\n")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['title'], 'Title')
        self.assertEqual(entries[0]['resource'], 'https://example.com')
        self.assertEqual(entries[0]['kind'], 'url')

    def test_isbn_entry(self):
        entries = self._parse("# Book\n- ISBN: 9780198575191\n\n")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['title'], 'Book')
        self.assertEqual(entries[0]['resource'], '9780198575191')
        self.assertEqual(entries[0]['kind'], 'isbn')

    def test_file_entry_home(self):
        entries = self._parse("# File\n/home/ff/test.pdf\n\n")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['resource'], '/home/ff/test.pdf')
        self.assertEqual(entries[0]['kind'], 'file')

    def test_file_entry_tilde(self):
        entries = self._parse("# File\n~/test.txt\n\n")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['resource'], os.path.expanduser('~/test.txt'))
        self.assertEqual(entries[0]['kind'], 'file')

    def test_file_entry_file_uri(self):
        entries = self._parse("# File\nfile:///home/ff/test.pdf\n\n")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['resource'], '/home/ff/test.pdf')
        self.assertEqual(entries[0]['kind'], 'file')

    def test_multiple_entries(self):
        content = (
            "# First\nhttps://example.com\n\n"
            "# Second\n- ISBN: 1234567890\n\n"
            "# Third\n/home/ff/doc.pdf\n\n"
        )
        entries = self._parse(content)
        self.assertEqual(len(entries), 3)
        self.assertEqual(entries[0]['title'], 'First')
        self.assertEqual(entries[1]['title'], 'Second')
        self.assertEqual(entries[2]['title'], 'Third')

    def test_entry_without_blank_separator(self):
        content = "# First\nhttps://example.com\n# Second\nhttps://other.com\n\n"
        entries = self._parse(content)
        self.assertEqual(len(entries), 2)
        self.assertEqual(entries[0]['title'], 'First')
        self.assertEqual(entries[1]['title'], 'Second')

    def test_entry_at_end_without_trailing_newline(self):
        entries = self._parse("# Title\nhttps://example.com")
        self.assertEqual(len(entries), 1)
        self.assertEqual(entries[0]['title'], 'Title')

    def test_empty_file(self):
        entries = self._parse("")
        self.assertEqual(entries, [])


class TestRewriteUrl(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_reddit(self):
        result = self.script.rewrite_url("https://reddit.com/r/linux")
        self.assertIn("old.reddit.com", result)
        self.assertIn("/r/linux", result)

    def test_www_reddit(self):
        result = self.script.rewrite_url("https://www.reddit.com/r/python")
        self.assertIn("old.reddit.com", result)
        self.assertIn("/r/python", result)

    def test_x_com(self):
        result = self.script.rewrite_url("https://x.com/user/status/123")
        self.assertIn("xcancel.com", result)
        self.assertIn("/user/status/123", result)

    def test_twitch(self):
        result = self.script.rewrite_url("https://twitch.tv/channel")
        self.assertIn("twitchls.com", result)
        self.assertIn("/channel", result)

    def test_non_matching_unchanged(self):
        url = "https://example.com/page"
        self.assertEqual(self.script.rewrite_url(url), url)

    def test_path_preserved(self):
        result = self.script.rewrite_url("https://reddit.com/r/linux/comments/abc")
        self.assertIn("/r/linux/comments/abc", result)


class TestGetHandler(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_isbn(self):
        entry = {"kind": "isbn", "resource": "9780198575191"}
        self.assertEqual(self.script.get_handler(entry), "isbn")

    def test_pdf_file(self):
        entry = {"kind": "file", "resource": "/home/ff/doc.pdf"}
        self.assertEqual(self.script.get_handler(entry), "zathura")

    def test_txt_file(self):
        entry = {"kind": "file", "resource": "/home/ff/notes.txt"}
        self.assertEqual(self.script.get_handler(entry), "emacs")

    def test_png_file(self):
        entry = {"kind": "file", "resource": "/home/ff/image.png"}
        self.assertEqual(self.script.get_handler(entry), "xdg")

    def test_youtube_url(self):
        entry = {"kind": "url", "resource": "https://youtube.com/watch?v=abc"}
        self.assertEqual(self.script.get_handler(entry), "youtube")

    def test_youtu_be_url(self):
        entry = {"kind": "url", "resource": "https://youtu.be/abc"}
        self.assertEqual(self.script.get_handler(entry), "youtube")

    def test_onion_url(self):
        entry = {"kind": "url", "resource": "http://example.onion/page"}
        self.assertEqual(self.script.get_handler(entry), "tor")

    def test_regular_url(self):
        entry = {"kind": "url", "resource": "https://example.com"}
        self.assertIsNone(self.script.get_handler(entry))


class TestOpenEntry(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    @patch('subprocess.Popen')
    def test_isbn_opens_firefox(self, mock_popen):
        entry = {"kind": "isbn", "resource": "9780198575191"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(
            ["firefox", "--new-window", "https://openlibrary.org/isbn/9780198575191"]
        )

    @patch('subprocess.Popen')
    def test_youtube_opens_mpv(self, mock_popen):
        entry = {"kind": "url", "resource": "https://youtube.com/watch?v=abc"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(
            ["mpv", "https://youtube.com/watch?v=abc"]
        )

    @patch('subprocess.Popen')
    def test_onion_opens_tor_browser(self, mock_popen):
        entry = {"kind": "url", "resource": "http://example.onion/page"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(
            ["tor-browser", "--new-window", "http://example.onion/page"]
        )

    @patch('subprocess.Popen')
    def test_regular_url_opens_firefox_with_rewrite(self, mock_popen):
        entry = {"kind": "url", "resource": "https://reddit.com/r/linux"}
        self.script.open_entry(entry)
        args = mock_popen.call_args[0][0]
        self.assertEqual(args[0], "firefox")
        self.assertEqual(args[1], "--new-window")
        self.assertIn("old.reddit.com", args[2])

    @patch('subprocess.Popen')
    def test_pdf_opens_zathura(self, mock_popen):
        entry = {"kind": "file", "resource": "/home/ff/doc.pdf"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(["zathura", "/home/ff/doc.pdf"])

    @patch('subprocess.Popen')
    def test_txt_opens_emacsclient(self, mock_popen):
        entry = {"kind": "file", "resource": "/home/ff/notes.txt"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(
            ["emacsclient", "-c", "/home/ff/notes.txt"]
        )

    @patch('subprocess.Popen')
    def test_other_file_opens_xdg(self, mock_popen):
        entry = {"kind": "file", "resource": "/home/ff/image.png"}
        self.script.open_entry(entry)
        mock_popen.assert_called_once_with(["xdg-open", "/home/ff/image.png"])


class TestLoadDmenuConfig(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_valid_config(self):
        config_content = (
            'DMENU_BASIC_ARGS="-nb #000000 -nf #ffffff"\n'
            'DMENU_FONT="monospace-10"\n'
        )
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name
        try:
            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path
            args, font = self.script.load_dmenu_config()
            self.script.os.path.expanduser = original
            self.assertEqual(font, "monospace-10")
            self.assertIn("-nb", args)
        finally:
            os.unlink(config_path)

    def test_missing_config(self):
        with patch.object(self.script.os.path, 'expanduser', return_value='/nonexistent'):
            with patch.object(self.script.os.path, 'exists', return_value=False):
                args, font = self.script.load_dmenu_config()
        self.assertEqual(args, [])
        self.assertEqual(font, "")


if __name__ == '__main__':
    unittest.main()
