"""Tests for dmenu-scripts (the script launcher)."""

import unittest
import os
import stat
import tempfile
import shutil

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('dmenu-scripts')


class TestDmenuScripts(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def test_get_scripts_executable_only(self):
        tmpdir = tempfile.mkdtemp()
        try:
            exec_file = os.path.join(tmpdir, "my-script")
            with open(exec_file, 'w') as f:
                f.write("#!/bin/sh\n")
            os.chmod(exec_file, stat.S_IRWXU)

            noexec_file = os.path.join(tmpdir, "not-executable")
            with open(noexec_file, 'w') as f:
                f.write("data\n")
            os.chmod(noexec_file, stat.S_IRUSR | stat.S_IWUSR)

            subdir = os.path.join(tmpdir, "a-directory")
            os.mkdir(subdir)

            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: tmpdir

            results = self.script.get_scripts()

            self.script.os.path.expanduser = original

            self.assertIn("my-script", results)
            self.assertNotIn("not-executable", results)
            self.assertNotIn("a-directory", results)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_scripts_nonexistent_dir(self):
        original = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/bin/dir'

        results = self.script.get_scripts()

        self.script.os.path.expanduser = original

        self.assertEqual(results, [])

    def test_get_scripts_sorted(self):
        tmpdir = tempfile.mkdtemp()
        try:
            for name in ["zeta", "alpha", "middle"]:
                fpath = os.path.join(tmpdir, name)
                with open(fpath, 'w') as f:
                    f.write("#!/bin/sh\n")
                os.chmod(fpath, stat.S_IRWXU)

            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: tmpdir

            results = self.script.get_scripts()

            self.script.os.path.expanduser = original

            self.assertEqual(results, ["alpha", "middle", "zeta"])
        finally:
            shutil.rmtree(tmpdir)


if __name__ == '__main__':
    unittest.main()
