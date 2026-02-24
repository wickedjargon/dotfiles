import unittest
from unittest.mock import patch
import sys
import json
from pathlib import Path
from io import StringIO

# Add parent directory to path
sys.path.append(str(Path(__file__).parent.parent))

import deploy_cli


class TestParseArgs(unittest.TestCase):
    """Test argument parsing."""

    def test_required_username(self):
        with self.assertRaises(SystemExit):
            deploy_cli.parse_args([])

    def test_basic_args(self):
        args = deploy_cli.parse_args(['--username', 'testuser', '--password', 'pass123'])
        self.assertEqual(args.username, 'testuser')
        self.assertEqual(args.password, 'pass123')
        self.assertFalse(args.yes)
        self.assertFalse(args.dry_run)
        self.assertFalse(args.json)

    def test_short_flags(self):
        args = deploy_cli.parse_args(['-u', 'myuser', '-p', 'pw', '-y', '-n'])
        self.assertEqual(args.username, 'myuser')
        self.assertEqual(args.password, 'pw')
        self.assertTrue(args.yes)
        self.assertTrue(args.dry_run)

    def test_json_flag(self):
        args = deploy_cli.parse_args(['-u', 'user', '--json'])
        self.assertTrue(args.json)

    def test_all_flags(self):
        args = deploy_cli.parse_args(['-u', 'user', '-p', 'pw', '-y', '-n', '--json'])
        self.assertTrue(args.yes)
        self.assertTrue(args.dry_run)
        self.assertTrue(args.json)


class TestCLIReporter(unittest.TestCase):
    """Test CLIReporter output."""

    def test_show_progress_ok(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_progress(0, "Testing...", success=True)
            output = mock_out.getvalue()
            self.assertIn("[OK]", output)
            self.assertIn("Testing...", output)

    def test_show_progress_failed(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_progress(0, "Testing...", success=False)
            output = mock_out.getvalue()
            self.assertIn("[FAILED]", output)

    def test_show_progress_pending(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_progress(0, "Testing...", success=None)
            output = mock_out.getvalue()
            self.assertIn("[..]", output)

    def test_show_message_skips_press_any_key(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_message(0, 0, "Press any key to exit...")
            self.assertEqual(mock_out.getvalue(), "")

    def test_show_message_skips_empty(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_message(0, 0, "")
            self.assertEqual(mock_out.getvalue(), "")

    def test_show_message_prints(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_message(0, 0, "Hello world")
            self.assertIn("Hello world", mock_out.getvalue())

    def test_draw_header(self):
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.draw_header("TEST HEADER")
            output = mock_out.getvalue()
            self.assertIn("TEST HEADER", output)
            self.assertIn("====", output)

    def test_stdscr_refresh_noop(self):
        cli = deploy_cli.CLIReporter()
        # Should not raise
        cli.stdscr.refresh()
        cli.stdscr.getch()


class TestCLIReporterJSON(unittest.TestCase):
    """Test CLIReporter in JSON mode."""

    def test_json_mode_collects_events(self):
        cli = deploy_cli.CLIReporter(json_mode=True)
        cli.show_progress(0, "Step 1", success=True)
        cli.show_progress(0, "Step 2", success=False)
        cli.show_message(0, 0, "Info message")

        self.assertEqual(len(cli.events), 3)
        self.assertEqual(cli.events[0]["status"], "ok")
        self.assertEqual(cli.events[1]["status"], "failed")
        self.assertEqual(cli.events[2]["type"], "message")

    def test_json_mode_suppresses_stdout(self):
        cli = deploy_cli.CLIReporter(json_mode=True)
        with patch('sys.stdout', new_callable=StringIO) as mock_out:
            cli.show_progress(0, "test", success=True)
            cli.show_message(0, 0, "test msg")
            cli.draw_header("TEST")
            # No stdout output in JSON mode
            self.assertEqual(mock_out.getvalue(), "")

    def test_json_pending_not_recorded(self):
        cli = deploy_cli.CLIReporter(json_mode=True)
        cli.show_progress(0, "Loading...", success=None)
        # Pending status [..] is not recorded as an event
        self.assertEqual(len(cli.events), 0)


class TestHandleError(unittest.TestCase):
    """Test the handle_error helper."""

    def test_fatal_returns_false(self):
        args = deploy_cli.parse_args(['-u', 'test', '-y'])
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO):
            result = deploy_cli.handle_error(args, cli, "Fatal error", is_fatal=True)
        self.assertFalse(result)

    def test_yes_flag_continues(self):
        args = deploy_cli.parse_args(['-u', 'test', '-y'])
        cli = deploy_cli.CLIReporter()
        with patch('sys.stdout', new_callable=StringIO):
            result = deploy_cli.handle_error(args, cli, "Non-fatal error")
        self.assertTrue(result)

    def test_json_mode_records_error(self):
        args = deploy_cli.parse_args(['-u', 'test', '-y', '--json'])
        cli = deploy_cli.CLIReporter(json_mode=True)
        deploy_cli.handle_error(args, cli, "Some error")
        self.assertEqual(len(cli.events), 1)
        self.assertEqual(cli.events[0]["type"], "error")
        self.assertEqual(cli.events[0]["message"], "Some error")


class TestMainValidation(unittest.TestCase):
    """Test main() validation checks."""

    def test_invalid_username_exits(self):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO):
                deploy_cli.main(['--username', 'INVALID_USER', '--password', 'test'])
        self.assertEqual(ctx.exception.code, 1)

    def test_invalid_username_json(self):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO) as mock_out:
                deploy_cli.main(['--username', 'INVALID!', '--password', 'test', '--json'])
        self.assertEqual(ctx.exception.code, 1)

    @patch('deploy.check_root', return_value=False)
    def test_non_root_exits(self, mock_root):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO):
                deploy_cli.main(['--username', 'testuser', '--password', 'test'])
        self.assertEqual(ctx.exception.code, 1)

    @patch('deploy.check_root', return_value=False)
    def test_non_root_json_outputs_error(self, mock_root):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO) as mock_out:
                deploy_cli.main(['--username', 'testuser', '--password', 'test', '--json'])
        output = mock_out.getvalue()
        result = json.loads(output)
        self.assertFalse(result["success"])
        self.assertIn("root", result["error"])

    @patch('deploy.user_exists', return_value=False)
    @patch('deploy.check_root', return_value=True)
    def test_new_user_without_password_exits(self, mock_root, mock_exists):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO):
                deploy_cli.main(['--username', 'newuser'])
        self.assertEqual(ctx.exception.code, 1)


class TestDryRun(unittest.TestCase):
    """Test dry-run mode."""

    @patch('deploy.user_exists', return_value=False)
    def test_dry_run_exits_zero(self, mock_exists):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO):
                deploy_cli.main(['--username', 'testuser', '--dry-run'])
        self.assertEqual(ctx.exception.code, 0)

    @patch('deploy.user_exists', return_value=True)
    def test_dry_run_existing_user(self, mock_exists):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO) as mock_out:
                deploy_cli.main(['--username', 'testuser', '--dry-run'])
        output = mock_out.getvalue()
        self.assertIn("already exists", output)
        self.assertEqual(ctx.exception.code, 0)

    @patch('deploy.user_exists', return_value=False)
    def test_dry_run_json_valid(self, mock_exists):
        with self.assertRaises(SystemExit) as ctx:
            with patch('sys.stdout', new_callable=StringIO) as mock_out:
                deploy_cli.main(['--username', 'testuser', '--dry-run', '--json'])
        output = mock_out.getvalue()
        result = json.loads(output)
        self.assertTrue(result["dry_run"])
        self.assertEqual(result["username"], "testuser")
        self.assertIsInstance(result["steps"], list)
        self.assertGreater(result["total_steps"], 0)
        self.assertEqual(ctx.exception.code, 0)


if __name__ == '__main__':
    unittest.main()
