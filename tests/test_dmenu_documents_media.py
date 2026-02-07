"""Tests for dmenu-documents-media script."""

import unittest
from unittest.mock import patch
import os
import tempfile

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-documents-media')


class TestDmenuDocumentsMedia(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_menu_items_contains_expected_keys(self):
        """Test MENU_ITEMS dict contains all expected keys."""
        expected_keys = {"Audio", "Books", "Images", "Notes", "Projects", "Videos"}
        self.assertEqual(set(self.script.MENU_ITEMS.keys()), expected_keys)

    def test_menu_items_values_are_dmenu_scripts(self):
        """Test all MENU_ITEMS values are valid dmenu script names."""
        for label, script_name in self.script.MENU_ITEMS.items():
            self.assertTrue(
                script_name.startswith("dmenu-"),
                f"Value for '{label}' should start with 'dmenu-', got '{script_name}'"
            )

    def test_load_dmenu_config_valid(self):
        """Test loading a valid dmenu config file."""
        config_content = '''DMENU_BASIC_ARGS="-nb #000000 -nf #ffffff -sb #222222 -sf #ffffff -l 12"
DMENU_FONT="monospace-10"
'''
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            original_expanduser = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path

            args, font = self.script.load_dmenu_config()

            self.script.os.path.expanduser = original_expanduser

            self.assertEqual(font, "monospace-10")
            self.assertIn("-nb", args)
            self.assertIn("#000000", args)
            self.assertIn("-l", args)
            self.assertIn("12", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        """Test behavior when config file doesn't exist."""
        original_expanduser = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/path'

        args, font = self.script.load_dmenu_config()

        self.script.os.path.expanduser = original_expanduser

        self.assertEqual(args, [])
        self.assertEqual(font, "")


if __name__ == '__main__':
    unittest.main()
