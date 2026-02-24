"""Tests for rofi-app-launcher script."""

import unittest
import os
import tempfile
import shutil
from pathlib import Path

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('rofi-app-launcher')


class TestRofiAppLauncher(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def _write_desktop_file(self, directory, filename, content):
        fpath = os.path.join(directory, filename)
        with open(fpath, 'w') as f:
            f.write(content)
        return fpath

    def _make_app_dir(self):
        tmpdir = tempfile.mkdtemp()
        appdir = os.path.join(tmpdir, "applications")
        os.makedirs(appdir)
        return tmpdir, appdir

    def test_get_apps_basic_parsing(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "firefox.desktop",
                "[Desktop Entry]\nName=Firefox\nExec=firefox\nIcon=firefox\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            self.assertEqual(len(apps), 1)
            self.assertEqual(apps[0]['name'], 'Firefox')
            self.assertEqual(apps[0]['exec'], 'firefox')
            self.assertEqual(apps[0]['icon'], 'firefox')
        finally:
            shutil.rmtree(tmpdir)

    def test_get_apps_no_display_excluded(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "hidden.desktop",
                "[Desktop Entry]\nName=Hidden\nExec=hidden\nNoDisplay=true\n")
            self._write_desktop_file(appdir, "visible.desktop",
                "[Desktop Entry]\nName=Visible\nExec=visible\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            names = [a['name'] for a in apps]
            self.assertNotIn('Hidden', names)
            self.assertIn('Visible', names)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_apps_missing_name_excluded(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "noname.desktop",
                "[Desktop Entry]\nExec=something\n")
            self._write_desktop_file(appdir, "valid.desktop",
                "[Desktop Entry]\nName=Valid\nExec=valid\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            self.assertEqual(len(apps), 1)
            self.assertEqual(apps[0]['name'], 'Valid')
        finally:
            shutil.rmtree(tmpdir)

    def test_get_apps_missing_exec_excluded(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "noexec.desktop",
                "[Desktop Entry]\nName=NoExec\n")
            self._write_desktop_file(appdir, "valid.desktop",
                "[Desktop Entry]\nName=Valid\nExec=valid\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            self.assertEqual(len(apps), 1)
            self.assertEqual(apps[0]['name'], 'Valid')
        finally:
            shutil.rmtree(tmpdir)

    def test_exec_command_cleaned(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "app.desktop",
                "[Desktop Entry]\nName=App\nExec=app %u --flag\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            self.assertEqual(apps[0]['exec'], 'app')
        finally:
            shutil.rmtree(tmpdir)

    def test_exec_command_cleaned_percent_F(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "editor.desktop",
                "[Desktop Entry]\nName=Editor\nExec=editor %F\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            self.assertEqual(apps[0]['exec'], 'editor')
        finally:
            shutil.rmtree(tmpdir)

    def test_apps_sorted_case_insensitive(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "z.desktop",
                "[Desktop Entry]\nName=Zeta\nExec=zeta\n")
            self._write_desktop_file(appdir, "a.desktop",
                "[Desktop Entry]\nName=alpha\nExec=alpha\n")
            self._write_desktop_file(appdir, "m.desktop",
                "[Desktop Entry]\nName=Middle\nExec=middle\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            names = [a['name'] for a in apps]
            self.assertEqual(names, ['alpha', 'Middle', 'Zeta'])
        finally:
            shutil.rmtree(tmpdir)

    def test_deduplication_by_name(self):
        tmpdir, appdir = self._make_app_dir()
        try:
            self._write_desktop_file(appdir, "app1.desktop",
                "[Desktop Entry]\nName=Firefox\nExec=firefox\n")
            self._write_desktop_file(appdir, "app2.desktop",
                "[Desktop Entry]\nName=Firefox\nExec=firefox-esr\n")

            original = self.script.DATA_DIRS
            self.script.DATA_DIRS = [Path(appdir)]

            apps = self.script.get_apps()

            self.script.DATA_DIRS = original

            names = [a['name'] for a in apps]
            self.assertEqual(names.count('Firefox'), 1)
        finally:
            shutil.rmtree(tmpdir)


if __name__ == '__main__':
    unittest.main()
