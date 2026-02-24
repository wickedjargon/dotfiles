"""Tests for poly-battery script."""

import unittest
from unittest.mock import patch, mock_open, MagicMock
import os
import sys
from io import StringIO

from helpers import import_script, get_script_path

SCRIPT_PATH = get_script_path('poly-battery')


class TestPolyBattery(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        if not os.path.exists(SCRIPT_PATH):
            raise unittest.SkipTest(f"Script not found: {SCRIPT_PATH}")
        cls.script = import_script(SCRIPT_PATH)

    def run_main_capture_output(self):
        """Run main() and capture stdout."""
        captured = StringIO()
        old_stdout = sys.stdout
        sys.stdout = captured
        try:
            self.script.main()
        finally:
            sys.stdout = old_stdout
        return captured.getvalue().strip()

    @patch('os.path.exists')
    def test_no_battery(self, mock_exists):
        """Test output when no battery is present."""
        mock_exists.return_value = False
        
        output = self.run_main_capture_output()
        
        self.assertEqual(output, "")

    @patch('builtins.open', mock_open())
    @patch('os.path.exists')
    def test_discharging(self, mock_exists):
        """Test output when battery is discharging."""
        mock_exists.return_value = True
        
        # Mock open to return different values for status and capacity
        def open_side_effect(path, *args, **kwargs):
            m = MagicMock()
            if 'status' in path:
                m.read.return_value = "Discharging\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            elif 'capacity' in path:
                m.read.return_value = "75\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            return m
        
        with patch('builtins.open', side_effect=open_side_effect):
            output = self.run_main_capture_output()
        
        self.assertEqual(output, "🔋 75%")

    @patch('builtins.open', mock_open())
    @patch('os.path.exists')
    def test_charging(self, mock_exists):
        """Test output when battery is charging."""
        mock_exists.return_value = True
        
        def open_side_effect(path, *args, **kwargs):
            m = MagicMock()
            if 'status' in path:
                m.read.return_value = "Charging\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            elif 'capacity' in path:
                m.read.return_value = "50\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            return m
        
        with patch('builtins.open', side_effect=open_side_effect):
            output = self.run_main_capture_output()
        
        self.assertEqual(output, "⚡ 50%")

    @patch('builtins.open', mock_open())
    @patch('os.path.exists')
    def test_full(self, mock_exists):
        """Test output when battery is full."""
        mock_exists.return_value = True
        
        def open_side_effect(path, *args, **kwargs):
            m = MagicMock()
            if 'status' in path:
                m.read.return_value = "Full\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            elif 'capacity' in path:
                m.read.return_value = "100\n"
                m.__enter__ = lambda s: m
                m.__exit__ = MagicMock(return_value=False)
            return m
        
        with patch('builtins.open', side_effect=open_side_effect):
            output = self.run_main_capture_output()
        
        self.assertEqual(output, "⚡ 100%")

    @patch('os.path.exists')
    def test_error_handling(self, mock_exists):
        """Test that exceptions result in empty output."""
        mock_exists.return_value = True
        
        # Make open raise an exception
        with patch('builtins.open', side_effect=IOError("Read error")):
            output = self.run_main_capture_output()
        
        self.assertEqual(output, "")


if __name__ == '__main__':
    unittest.main()
