"""Tests for dmenu media scripts (read-book, view-image, watch-video, edit-note)."""

import unittest
from unittest.mock import patch
import os
import tempfile
import shutil
from pathlib import Path

from helpers import import_script, get_script_path

BOOK_SCRIPT_PATH = get_script_path('dmenu-read-book')
IMAGE_SCRIPT_PATH = get_script_path('dmenu-view-image')
VIDEO_SCRIPT_PATH = get_script_path('dmenu-watch-video')
NOTE_SCRIPT_PATH = get_script_path('dmenu-edit-note')


class TestDmenuReadBook(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(BOOK_SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {BOOK_SCRIPT_PATH}")
        cls.script = import_script(BOOK_SCRIPT_PATH)

    def test_get_files_matching_extensions(self):
        tmpdir = tempfile.mkdtemp()
        try:
            Path(tmpdir, "book.pdf").touch()
            Path(tmpdir, "book.epub").touch()
            Path(tmpdir, "readme.txt").touch()
            Path(tmpdir, "image.png").touch()

            results = self.script.get_files(Path(tmpdir), {'.pdf', '.epub'})

            names = [r[0] for r in results]
            self.assertIn("book.pdf", names)
            self.assertIn("book.epub", names)
            self.assertNotIn("readme.txt", names)
            self.assertNotIn("image.png", names)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_files_nonexistent_directory(self):
        results = self.script.get_files(Path("/nonexistent/dir/xyz"), {'.pdf'})
        self.assertEqual(results, [])

    def test_get_files_nested_subdirectories(self):
        tmpdir = tempfile.mkdtemp()
        try:
            subdir = Path(tmpdir, "sub", "deep")
            subdir.mkdir(parents=True)
            Path(tmpdir, "top.pdf").touch()
            Path(subdir, "nested.pdf").touch()
            Path(subdir, "skip.txt").touch()

            results = self.script.get_files(Path(tmpdir), {'.pdf'})

            names = [r[0] for r in results]
            self.assertIn("top.pdf", names)
            self.assertIn(os.path.join("sub", "deep", "nested.pdf"), names)
            self.assertEqual(len(results), 2)
        finally:
            shutil.rmtree(tmpdir)

    def test_load_dmenu_config_valid(self):
        config_content = 'DMENU_BASIC_ARGS="-nb #000000 -nf #ffffff"\nDMENU_FONT="monospace-12"\n'
        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.conf') as f:
            f.write(config_content)
            config_path = f.name

        try:
            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path

            args, font = self.script.load_dmenu_config()

            self.script.os.path.expanduser = original

            self.assertEqual(font, "monospace-12")
            self.assertIn("-nb", args)
            self.assertIn("#000000", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        original = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/config/path'

        args, font = self.script.load_dmenu_config()

        self.script.os.path.expanduser = original

        self.assertEqual(args, [])
        self.assertEqual(font, "")


class TestDmenuViewImage(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(IMAGE_SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {IMAGE_SCRIPT_PATH}")
        cls.script = import_script(IMAGE_SCRIPT_PATH)

    def test_get_files_matching_extensions(self):
        tmpdir = tempfile.mkdtemp()
        try:
            Path(tmpdir, "photo.jpg").touch()
            Path(tmpdir, "photo.png").touch()
            Path(tmpdir, "doc.pdf").touch()

            results = self.script.get_files(
                Path(tmpdir), {'.jpg', '.jpeg', '.png', '.gif', '.bmp', '.webp', '.tiff'}
            )

            names = [r[0] for r in results]
            self.assertIn("photo.jpg", names)
            self.assertIn("photo.png", names)
            self.assertNotIn("doc.pdf", names)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_files_nonexistent_directory(self):
        results = self.script.get_files(Path("/nonexistent/dir/xyz"), {'.jpg'})
        self.assertEqual(results, [])

    def test_get_files_nested_subdirectories(self):
        tmpdir = tempfile.mkdtemp()
        try:
            subdir = Path(tmpdir, "album")
            subdir.mkdir()
            Path(subdir, "deep.png").touch()
            Path(tmpdir, "top.jpg").touch()

            results = self.script.get_files(Path(tmpdir), {'.jpg', '.png'})

            names = [r[0] for r in results]
            self.assertIn("top.jpg", names)
            self.assertIn(os.path.join("album", "deep.png"), names)
        finally:
            shutil.rmtree(tmpdir)

    def test_load_dmenu_config_valid(self):
        config_content = 'DMENU_BASIC_ARGS="-l 10"\nDMENU_FONT="sans-11"\n'
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path
            args, font = self.script.load_dmenu_config()
            self.script.os.path.expanduser = original

            self.assertEqual(font, "sans-11")
            self.assertIn("-l", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        original = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/path'
        args, font = self.script.load_dmenu_config()
        self.script.os.path.expanduser = original

        self.assertEqual(args, [])
        self.assertEqual(font, "")


class TestDmenuWatchVideo(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(VIDEO_SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {VIDEO_SCRIPT_PATH}")
        cls.script = import_script(VIDEO_SCRIPT_PATH)

    def test_get_files_matching_extensions(self):
        tmpdir = tempfile.mkdtemp()
        try:
            Path(tmpdir, "movie.mp4").touch()
            Path(tmpdir, "clip.mkv").touch()
            Path(tmpdir, "song.mp3").touch()

            results = self.script.get_files(
                Path(tmpdir), {'.mp4', '.mkv', '.avi', '.mov', '.webm', '.flv', '.wmv'}
            )

            names = [r[0] for r in results]
            self.assertIn("movie.mp4", names)
            self.assertIn("clip.mkv", names)
            self.assertNotIn("song.mp3", names)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_files_nonexistent_directory(self):
        results = self.script.get_files(Path("/nonexistent/dir/xyz"), {'.mp4'})
        self.assertEqual(results, [])

    def test_get_files_nested_subdirectories(self):
        tmpdir = tempfile.mkdtemp()
        try:
            subdir = Path(tmpdir, "series", "s01")
            subdir.mkdir(parents=True)
            Path(subdir, "ep01.mkv").touch()
            Path(tmpdir, "trailer.mp4").touch()

            results = self.script.get_files(Path(tmpdir), {'.mp4', '.mkv'})

            names = [r[0] for r in results]
            self.assertIn("trailer.mp4", names)
            self.assertIn(os.path.join("series", "s01", "ep01.mkv"), names)
        finally:
            shutil.rmtree(tmpdir)

    def test_load_dmenu_config_valid(self):
        config_content = 'DMENU_BASIC_ARGS="-sb #333"\nDMENU_FONT="mono-9"\n'
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path
            args, font = self.script.load_dmenu_config()
            self.script.os.path.expanduser = original

            self.assertEqual(font, "mono-9")
            self.assertIn("-sb", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        original = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/path'
        args, font = self.script.load_dmenu_config()
        self.script.os.path.expanduser = original

        self.assertEqual(args, [])
        self.assertEqual(font, "")


class TestDmenuEditNote(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(NOTE_SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {NOTE_SCRIPT_PATH}")
        cls.script = import_script(NOTE_SCRIPT_PATH)

    def test_get_files_matching_extensions(self):
        tmpdir = tempfile.mkdtemp()
        try:
            Path(tmpdir, "notes.md").touch()
            Path(tmpdir, "todo.org").touch()
            Path(tmpdir, "readme.txt").touch()
            Path(tmpdir, "script.py").touch()

            results = self.script.get_files(Path(tmpdir), {'.md', '.org', '.txt'})

            names = [r[0] for r in results]
            self.assertIn("notes.md", names)
            self.assertIn("todo.org", names)
            self.assertIn("readme.txt", names)
            self.assertNotIn("script.py", names)
        finally:
            shutil.rmtree(tmpdir)

    def test_get_files_nonexistent_directory(self):
        results = self.script.get_files(Path("/nonexistent/dir/xyz"), {'.md'})
        self.assertEqual(results, [])

    def test_get_files_nested_subdirectories(self):
        tmpdir = tempfile.mkdtemp()
        try:
            subdir = Path(tmpdir, "journal", "2024")
            subdir.mkdir(parents=True)
            Path(subdir, "jan.md").touch()
            Path(tmpdir, "inbox.org").touch()

            results = self.script.get_files(Path(tmpdir), {'.md', '.org'})

            names = [r[0] for r in results]
            self.assertIn("inbox.org", names)
            self.assertIn(os.path.join("journal", "2024", "jan.md"), names)
        finally:
            shutil.rmtree(tmpdir)

    def test_load_dmenu_config_valid(self):
        config_content = 'DMENU_BASIC_ARGS="-l 20"\nDMENU_FONT="iosevka-10"\n'
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write(config_content)
            config_path = f.name

        try:
            original = self.script.os.path.expanduser
            self.script.os.path.expanduser = lambda x: config_path
            args, font = self.script.load_dmenu_config()
            self.script.os.path.expanduser = original

            self.assertEqual(font, "iosevka-10")
            self.assertIn("-l", args)
        finally:
            os.unlink(config_path)

    def test_load_dmenu_config_missing(self):
        original = self.script.os.path.expanduser
        self.script.os.path.expanduser = lambda x: '/nonexistent/path'
        args, font = self.script.load_dmenu_config()
        self.script.os.path.expanduser = original

        self.assertEqual(args, [])
        self.assertEqual(font, "")


if __name__ == '__main__':
    unittest.main()
