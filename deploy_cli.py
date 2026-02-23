#!/usr/bin/env python3
"""
Command-line interface for dotfiles deployment.

This is the non-interactive counterpart to deploy.py's TUI.
All inputs that would be collected interactively are provided via
command-line arguments, making it suitable for automation and LLM testing.

Usage:
    sudo python3 deploy_cli.py --username myuser --password mypass
    sudo python3 deploy_cli.py --username existinguser --yes
    sudo python3 deploy_cli.py --username myuser --password mypass --yes --json
    python3 deploy_cli.py --username myuser --dry-run
"""

import argparse
import json as json_mod
import os
import re
import subprocess
import sys
from pathlib import Path

# Import all business logic from the existing deploy.py
import deploy


class CLIStdscr:
    """No-op stand-in for curses stdscr, needed because business logic
    calls tui.stdscr.refresh() in several places."""

    def refresh(self):
        pass

    def getch(self):
        return ord('y')


class CLIReporter:
    """Drop-in replacement for DeploymentTUI that prints to stdout.

    Implements the same interface that deploy.py business logic functions
    expect: show_progress(), show_message(), and stdscr.refresh().
    """

    def __init__(self, json_mode=False):
        self.stdscr = CLIStdscr()
        self._last_progress = None
        self._json_mode = json_mode
        # Accumulate structured events when in JSON mode
        self.events = []

    def show_progress(self, y, message, success=None):
        """Print a progress line with status indicator."""
        if self._json_mode:
            if success is not None:
                status = "ok" if success else "failed"
                self.events.append({"type": "progress", "message": message, "status": status})
            return

        if success is True:
            status = "\033[32m[OK]\033[0m"
        elif success is False:
            status = "\033[31m[FAILED]\033[0m"
        else:
            status = "\033[33m[..]\033[0m"

        line = f"  {status} {message}"

        if self._last_progress == message and success is not None:
            sys.stdout.write(f"\033[A\033[2K{line}\n")
        else:
            print(line)

        sys.stdout.flush()
        self._last_progress = message

    def show_message(self, y, x, message, color_pair=0, bold=False):
        """Print a message to stdout. Row/column positioning is ignored."""
        message = message.rstrip()
        if not message:
            return
        if "Press any key" in message:
            return

        if self._json_mode:
            self.events.append({"type": "message", "message": message})
            return

        print(f"  {message}")
        sys.stdout.flush()

    def draw_header(self, title):
        """Print a section header."""
        if self._json_mode:
            return
        width = 60
        print()
        print("=" * width)
        print(f"  {title}")
        print("=" * width)
        print()


def parse_args(argv=None):
    """Parse command-line arguments."""
    parser = argparse.ArgumentParser(
        description="Deploy dotfiles — command-line interface",
        epilog="Example: sudo python3 deploy_cli.py --username myuser --password mypass --yes",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument(
        "--username", "-u",
        required=True,
        help="Username to create or deploy to",
    )
    parser.add_argument(
        "--password", "-p",
        default=None,
        help="Password for new user (not needed if user already exists)",
    )
    parser.add_argument(
        "--yes", "-y",
        action="store_true",
        default=False,
        help="Auto-confirm all prompts (required for non-interactive use)",
    )
    parser.add_argument(
        "--dry-run", "-n",
        action="store_true",
        default=False,
        help="Preview what would happen without making any changes",
    )
    parser.add_argument(
        "--json",
        action="store_true",
        default=False,
        help="Output results as JSON (for LLM/automation consumption)",
    )

    return parser.parse_args(argv)


def dry_run_preview(args, script_dir):
    """Show what the deployment would do without making any changes.
    Does not require root."""
    username = args.username
    json_mode = args.json

    steps = []

    # User creation
    if deploy.user_exists(username):
        steps.append({"step": "user", "action": "skip", "detail": f"User '{username}' already exists"})
    else:
        steps.append({"step": "user", "action": "create", "detail": f"Create user '{username}' with home directory"})
        steps.append({"step": "password", "action": "set", "detail": f"Set password for '{username}'"})

    steps.append({"step": "sudo", "action": "add", "detail": f"Add '{username}' to sudo group"})

    # Dotfile repos
    dotfiles_repos = deploy.read_git_dotfiles_file(script_dir)
    if dotfiles_repos:
        for url, dest in dotfiles_repos:
            steps.append({"step": "clone_dotfile", "action": "clone", "detail": f"{url} → ~/{dest}"})

    # Prerequisites
    steps.append({"step": "prerequisites", "action": "install", "detail": "Install curl, gpg"})

    # Third-party repos
    third_party_repos = deploy.read_third_party_packages_file(script_dir)
    third_party_package_names = []
    if third_party_repos:
        for name, key_url, repo_line in third_party_repos:
            steps.append({"step": "third_party_repo", "action": "setup", "detail": f"Add repo for {name}"})
            third_party_package_names.append(name)

    # Packages
    packages = deploy.read_packages_file(script_dir)
    all_packages = packages + third_party_package_names
    already_installed = []
    to_install = []
    for pkg in all_packages:
        try:
            if deploy.is_package_installed(pkg):
                already_installed.append(pkg)
            else:
                to_install.append(pkg)
        except FileNotFoundError:
            # dpkg-query not available (non-Debian host) — assume not installed
            to_install.append(pkg)

    if to_install:
        steps.append({"step": "packages", "action": "install",
                       "detail": f"{len(to_install)} packages to install",
                       "packages": to_install})
    if already_installed:
        steps.append({"step": "packages", "action": "skip",
                       "detail": f"{len(already_installed)} packages already installed",
                       "packages": already_installed})

    # Root deployment
    root_dir = script_dir / 'root'
    if root_dir.exists():
        file_count = sum(1 for _ in root_dir.rglob('*') if _.is_file())
        steps.append({"step": "deploy_root", "action": "copy", "detail": f"Deploy {file_count} files from root/"})

    # Source repos
    src_repos = deploy.read_git_packages_src_file(script_dir)
    if src_repos:
        for url in src_repos:
            name = url.rstrip('/').split('/')[-1].replace('.git', '')
            steps.append({"step": "build_repo", "action": "clone+build", "detail": f"{name} ({url})"})

    # Tor Browser
    if (script_dir / 'install-tor-browser').exists():
        steps.append({"step": "tor_browser", "action": "install", "detail": "Download and install Tor Browser"})

    # Patches
    patches_dir = script_dir / 'patches'
    if patches_dir.exists():
        patch_count = sum(1 for _ in patches_dir.rglob('*') if _.is_file())
        steps.append({"step": "patches", "action": "apply", "detail": f"Apply {patch_count} patch files"})

    # Services
    steps.append({"step": "keyd", "action": "configure", "detail": "Enable and restart keyd service"})
    steps.append({"step": "kbdrate", "action": "configure", "detail": "Enable kbdrate service"})

    # Firefox
    if (script_dir / 'firefox-extensions.sh').exists():
        steps.append({"step": "firefox_extensions", "action": "install", "detail": "Install Firefox extensions via policy"})
    if (script_dir / 'firefox-user.js').exists():
        steps.append({"step": "firefox_userjs", "action": "install", "detail": "Deploy user.js to Firefox profiles"})

    # Cleanup
    steps.append({"step": "cleanup", "action": "run", "detail": "apt-get clean && autoremove"})

    # Output
    if json_mode:
        result = {
            "dry_run": True,
            "username": username,
            "steps": steps,
            "total_steps": len(steps),
        }
        print(json_mod.dumps(result, indent=2))
    else:
        print()
        print("=" * 60)
        print("  DRY RUN — no changes will be made")
        print("=" * 60)
        print()
        print(f"  Target user: {username}")
        print(f"  Total steps: {len(steps)}")
        print()

        for i, step in enumerate(steps, 1):
            action = step['action'].upper()
            detail = step['detail']
            print(f"  {i:2d}. [{action:>12s}]  {detail}")
            if 'packages' in step and step['action'] == 'install':
                # Show first few packages
                pkgs = step['packages']
                if len(pkgs) <= 10:
                    for pkg in pkgs:
                        print(f"                        - {pkg}")
                else:
                    for pkg in pkgs[:8]:
                        print(f"                        - {pkg}")
                    print(f"                        ... and {len(pkgs) - 8} more")

        print()
        print("  Run without --dry-run to execute.")
        print()


def handle_error(args, cli, message, is_fatal=False):
    """Handle an error: print, ask for confirmation or auto-continue.
    Returns True if should continue, False if should abort."""
    if cli._json_mode:
        cli.events.append({"type": "error", "message": message, "fatal": is_fatal})
    else:
        print(f"\033[31m  {message}\033[0m")

    if is_fatal:
        return False

    if args.yes:
        if not cli._json_mode:
            print("  --yes: continuing despite errors")
        return True
    else:
        response = input("  Continue anyway? [y/N]: ").strip().lower()
        return response == 'y'


def main(argv=None):
    """CLI entry point — mirrors the orchestration logic of main_tui()."""
    args = parse_args(argv)
    cli = CLIReporter(json_mode=args.json)
    script_dir = Path(__file__).parent.resolve()
    had_errors = False

    # JSON result accumulator
    json_result = {
        "success": True,
        "username": args.username,
        "errors": [],
        "warnings": [],
    }

    # ── Pre-flight checks ──────────────────────────────────────────

    # Validate username format (do this before root check so --dry-run works)
    if not re.match(r'^[a-z_][a-z0-9_-]*[$]?$', args.username):
        if args.json:
            print(json_mod.dumps({"success": False, "error": f"Invalid username '{args.username}'"}))
        else:
            print(f"\033[31mERROR: Invalid username '{args.username}'.\033[0m")
            print("Must start with lowercase letter or underscore.")
        sys.exit(1)

    # Dry-run mode — preview and exit (does not require root)
    if args.dry_run:
        dry_run_preview(args, script_dir)
        sys.exit(0)

    if not deploy.check_root():
        if args.json:
            print(json_mod.dumps({"success": False, "error": "Must be run as root (sudo)"}))
        else:
            print("\033[31mERROR: This script must be run as root (sudo).\033[0m")
        sys.exit(1)

    username = args.username
    password = args.password

    # ── User creation / verification ───────────────────────────────

    cli.draw_header("DOTFILES DEPLOYMENT (CLI)")
    row = 4

    user_created = False

    if deploy.user_exists(username):
        if not args.json:
            print(f"  User '{username}' already exists. Continuing with deployment.")
        if not args.yes:
            response = input("  Continue? [y/N]: ").strip().lower()
            if response != 'y':
                if not args.json:
                    print("Aborted.")
                sys.exit(0)
    else:
        if not password:
            if args.json:
                print(json_mod.dumps({"success": False, "error": "--password is required when creating a new user"}))
            else:
                print("\033[31mERROR: --password is required when creating a new user.\033[0m")
            sys.exit(1)

        user_created = True
        cli.show_progress(row, f"Creating user '{username}'...", success=None)
        success, error_msg = deploy.create_user(username)
        if success:
            cli.show_progress(row, f"Creating user '{username}'...", success=True)
            row += 1

            cli.show_progress(row, "Setting password...", success=None)
            if deploy.set_user_password(username, password):
                cli.show_progress(row, "Setting password...", success=True)
                row += 1
            else:
                cli.show_progress(row, "Setting password...", success=False)
                json_result["success"] = False
                json_result["errors"].append("Failed to set password")
                if args.json:
                    print(json_mod.dumps(json_result))
                sys.exit(1)
        else:
            cli.show_progress(row, f"Creating user '{username}'...", success=False)
            json_result["success"] = False
            json_result["errors"].append(f"Failed to create user: {error_msg}")
            if args.json:
                print(json_mod.dumps(json_result))
            elif error_msg:
                print(f"  Error: {error_msg}")
            sys.exit(1)

    # ── Add to sudo group ──────────────────────────────────────────

    cli.show_progress(row, "Adding to sudo group...", success=None)
    if deploy.add_user_to_sudo(username):
        cli.show_progress(row, "Adding to sudo group...", success=True)
        row += 1
    else:
        cli.show_progress(row, "Adding to sudo group...", success=False)
        json_result["success"] = False
        json_result["errors"].append("Failed to add user to sudo group")
        if args.json:
            print(json_mod.dumps(json_result))
        sys.exit(1)

    # ── Clone dotfile repos to home ────────────────────────────────

    dotfiles_repos = deploy.read_git_dotfiles_file(script_dir)
    if dotfiles_repos:
        if not args.json:
            print(f"\n  Cloning dotfile repositories ({len(dotfiles_repos)} total):")
        row += 1

        success, failed_repos, dotfiles_backup_dir, dotfiles_backed_up, row = \
            deploy.clone_dotfiles_home(dotfiles_repos, username, cli, row)

        if success:
            row += 1
            if dotfiles_backup_dir and dotfiles_backed_up:
                if not args.json:
                    print(f"  Backed up existing files to: {dotfiles_backup_dir.name}")
                row += 1
        else:
            row += 1
            err = f"Failed to clone: {', '.join(failed_repos)}"
            json_result["errors"].append(err)
            if not handle_error(args, cli, err):
                json_result["success"] = False
                if args.json:
                    print(json_mod.dumps(json_result))
                sys.exit(1)
            had_errors = True
            row += 3

    # ── Prerequisite packages (curl, gpg) ──────────────────────────

    cli.show_progress(row, "Checking prerequisite packages...", success=None)
    try:
        subprocess.run(['apt-get', 'install', '-y', 'curl', 'gpg'],
                      check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        cli.show_progress(row, "Checking prerequisite packages...", success=True)
        row += 1
    except subprocess.CalledProcessError:
        cli.show_progress(row, "Checking prerequisite packages...", success=False)
        json_result["warnings"].append("Failed to install curl/gpg. Third-party repos may fail.")
        if not args.json:
            print("  Failed to install curl/gpg. Third-party repos may fail.")
        row += 2

    # ── Setup third-party repositories ─────────────────────────────

    third_party_repos = deploy.read_third_party_packages_file(script_dir)
    third_party_package_names = []
    if third_party_repos:
        if not args.json:
            print(f"\n  Setting up third-party repositories ({len(third_party_repos)} total):")
        row += 1

        success, result, row = deploy.setup_third_party_repos(third_party_repos, cli, row)

        if success:
            third_party_package_names = [name for name, _, _ in third_party_repos]
            row += 1
        else:
            failed_repos = result
            row += 1
            err = f"Failed to setup repos: {', '.join(failed_repos)}"
            json_result["errors"].append(err)
            if not handle_error(args, cli, err):
                json_result["success"] = False
                if args.json:
                    print(json_mod.dumps(json_result))
                sys.exit(1)
            had_errors = True
            row += 3

    # ── Install packages ───────────────────────────────────────────

    packages = deploy.read_packages_file(script_dir)
    all_packages = packages + third_party_package_names
    if all_packages:
        if not args.json:
            print(f"\n  Installing packages ({len(all_packages)} total):")
        row += 1

        success, result, row = deploy.install_packages(all_packages, cli, row)

        if success:
            row += 1
        else:
            failed_packages = result
            row += 1
            err = f"Failed to install: {', '.join(failed_packages)}"
            json_result["errors"].append(err)
            if not handle_error(args, cli, err):
                json_result["success"] = False
                if args.json:
                    print(json_mod.dumps(json_result))
                sys.exit(1)
            had_errors = True
            row += 3

    # ── Deploy root/ files ─────────────────────────────────────────

    cli.show_progress(row, "Deploying from root/...", success=None)

    success, error, backup_dir, backed_up_items = deploy.deploy_root(username, script_dir)

    if not success:
        cli.show_progress(row, "Deploying from root/...", success=False)
        err = f"Failed to deploy from root/: {error}"
        json_result["success"] = False
        json_result["errors"].append(err)
        if args.json:
            print(json_mod.dumps(json_result))
        else:
            print(f"\033[31m  Error: {error}\033[0m")
        sys.exit(1)
    else:
        cli.show_progress(row, "Deploying from root/...", success=True)
        row += 2
        if backup_dir and backed_up_items:
            if not args.json:
                print(f"  Backed up existing files to: {backup_dir.name}")
            row += 1

    # ── Clone and build source repos ───────────────────────────────

    src_repos = deploy.read_git_packages_src_file(script_dir)
    if src_repos:
        if not args.json:
            print(f"\n  Cloning and building source repositories ({len(src_repos)} total):")
        row += 1

        success, failed_repos, row = deploy.clone_and_build_repos(src_repos, username, cli, row)

        if success:
            row += 1
        else:
            row += 1
            err = f"Failed to build: {', '.join(failed_repos)}"
            json_result["errors"].append(err)
            if not handle_error(args, cli, err):
                json_result["success"] = False
                if args.json:
                    print(json_mod.dumps(json_result))
                sys.exit(1)
            had_errors = True
            row += 3

    # ── Install Tor Browser ────────────────────────────────────────

    success, error, row = deploy.install_tor_browser(username, script_dir, cli, row)
    if not success:
        err = f"Tor Browser installation failed: {error[:80] if error else 'Unknown'}"
        json_result["errors"].append(err)
        if not handle_error(args, cli, err):
            json_result["success"] = False
            if args.json:
                print(json_mod.dumps(json_result))
            sys.exit(1)
        had_errors = True
        row += 3

    # ── Apply system patches ───────────────────────────────────────

    cli.show_progress(row, "Applying system patches...", success=None)

    success, error = deploy.deploy_patches(script_dir)

    if not success:
        cli.show_progress(row, "Applying system patches...", success=False)
        err = f"Failed to apply patches: {error}"
        json_result["errors"].append(err)
        if not handle_error(args, cli, err):
            json_result["success"] = False
            if args.json:
                print(json_mod.dumps(json_result))
            sys.exit(1)
        had_errors = True
        row += 4
    else:
        cli.show_progress(row, "Applying system patches...", success=True)
        row += 1

    # ── Configure keyd ─────────────────────────────────────────────

    success, error = deploy.configure_keyd()
    if not success:
        json_result["warnings"].append(f"keyd: {error}")
        if not args.json:
            print(f"\033[33m  Warning: {error}\033[0m")
        row += 1

    # ── Configure kbdrate ──────────────────────────────────────────

    success, error = deploy.configure_kbdrate()
    if not success:
        json_result["warnings"].append(f"kbdrate: {error}")
        if not args.json:
            print(f"\033[33m  Warning: {error}\033[0m")
        row += 1

    # ── Install Firefox extensions ─────────────────────────────────

    cli.show_progress(row, "Installing Firefox extensions...", success=None)

    success, error = deploy.install_firefox_extensions(script_dir)

    if not success:
        cli.show_progress(row, "Installing Firefox extensions...", success=False)
        err = f"Failed to install Firefox extensions: {error}"
        json_result["errors"].append(err)
        if not handle_error(args, cli, err):
            json_result["success"] = False
            if args.json:
                print(json_mod.dumps(json_result))
            sys.exit(1)
        had_errors = True
        row += 4
    else:
        cli.show_progress(row, "Installing Firefox extensions...", success=True)
        row += 1

    # ── Install Firefox user.js ────────────────────────────────────

    success, error, row = deploy.install_firefox_userjs(username, script_dir, cli, row)
    if not success:
        err = f"Failed to install Firefox user.js: {error[:80] if error else 'Unknown'}"
        json_result["errors"].append(err)
        if not handle_error(args, cli, err):
            json_result["success"] = False
            if args.json:
                print(json_mod.dumps(json_result))
            sys.exit(1)
        had_errors = True
        row += 3

    # ── Cleanup ────────────────────────────────────────────────────

    cli.show_progress(row, "Cleaning up...", success=None)

    try:
        subprocess.run(['apt-get', 'clean'], check=True, capture_output=True)
        subprocess.run(['apt-get', 'autoremove', '-y'], check=True, capture_output=True)
        cli.show_progress(row, "Cleaning up...", success=True)
    except subprocess.CalledProcessError as e:
        deploy.log_error("Cleanup failed", e)
        cli.show_progress(row, "Cleaning up...", success=False)
    row += 1

    # ── Done ───────────────────────────────────────────────────────

    if had_errors:
        json_result["success"] = True  # partial success — we continued
        json_result["partial"] = True

    if args.json:
        json_result["events"] = cli.events
        print(json_mod.dumps(json_result, indent=2))
    else:
        print()
        if had_errors:
            print("\033[33m  Deployment complete (with some errors).\033[0m")
            print("  Check /tmp/dotfiles-deploy.log for details.")
        else:
            print("\033[32m  Deployment complete!\033[0m")

    sys.exit(0)


if __name__ == '__main__':
    main()
