#!/usr/bin/env python3
"""
Auto-deploy script for my dotfiles
Creates user, sets up home directory, deploys dotfiles, installs packages
"""

import curses
import os
import sys
import subprocess
import shutil
import re
from pathlib import Path


class DeploymentTUI:
    def __init__(self, stdscr):
        self.stdscr = stdscr
        self.height, self.width = stdscr.getmaxyx()
        curses.curs_set(1)  # Show cursor
        curses.init_pair(1, curses.COLOR_CYAN, curses.COLOR_BLACK)
        curses.init_pair(2, curses.COLOR_GREEN, curses.COLOR_BLACK)
        curses.init_pair(3, curses.COLOR_RED, curses.COLOR_BLACK)
        curses.init_pair(4, curses.COLOR_YELLOW, curses.COLOR_BLACK)

    def draw_header(self, title):
        """Draw header with title"""
        self.stdscr.clear()
        self.stdscr.addstr(0, 0, "=" * self.width)
        self.stdscr.addstr(1, (self.width - len(title)) // 2, title,
                          curses.color_pair(1) | curses.A_BOLD)
        self.stdscr.addstr(2, 0, "=" * self.width)

    def show_message(self, y, x, message, color_pair=0, bold=False):
        """Display a message at given coordinates"""
        attr = curses.color_pair(color_pair)
        if bold:
            attr |= curses.A_BOLD
        self.stdscr.addstr(y, x, message, attr)

    def get_input(self, prompt, y, x):
        """Get user input with prompt"""
        self.show_message(y, x, prompt, color_pair=4, bold=True)
        self.stdscr.refresh()

        curses.echo()
        input_y = y + 1
        self.stdscr.move(input_y, x)
        user_input = self.stdscr.getstr(input_y, x, 30).decode('utf-8').strip()
        curses.noecho()

        return user_input

    def get_password(self, prompt, y, x):
        """Get password input without echoing"""
        self.show_message(y, x, prompt, color_pair=4, bold=True)
        self.stdscr.refresh()

        curses.noecho()
        input_y = y + 1
        self.stdscr.move(input_y, x)
        password = self.stdscr.getstr(input_y, x, 50).decode('utf-8').strip()

        return password

    def show_progress(self, y, message, success=None):
        """Show progress message with optional success/failure indicator"""
        self.show_message(y, 4, message)
        if success is True:
            self.show_message(y, self.width - 10, "[OK]", color_pair=2, bold=True)
        elif success is False:
            self.show_message(y, self.width - 12, "[FAILED]", color_pair=3, bold=True)
        self.stdscr.refresh()


def check_root():
    """Check if script is running as root"""
    return os.geteuid() == 0


def user_exists(username):
    """Check if user already exists"""
    try:
        subprocess.run(['id', username], capture_output=True, check=True)
        return True
    except subprocess.CalledProcessError:
        return False


def create_user(username):
    """Create a new user with home directory"""
    try:
        # Create user with home directory
        subprocess.run([
            'useradd',
            '-m',  # Create home directory
            '-s', '/bin/bash',  # Set default shell
            username
        ], check=True, capture_output=True)
        return True, None
    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.decode() if e.stderr else str(e)
        return False, error_msg


def set_user_password(username, password):
    """Set password for user using chpasswd"""
    try:
        # Use chpasswd to set password
        process = subprocess.Popen(['chpasswd'], stdin=subprocess.PIPE)
        process.communicate(f'{username}:{password}'.encode())
        return process.returncode == 0
    except Exception:
        return False


def add_user_to_sudo(username):
    """Add user to sudo group"""
    try:
        subprocess.run(['usermod', '-aG', 'sudo', username],
                      check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError:
        return False


def is_valid_git_url(url):
    """Validate that a URL is a safe git repository URL"""
    # Allow https://, git://, and git@ SSH URLs
    valid_patterns = [
        r'^https://[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9](/[-a-zA-Z0-9._]+)+(\.git)?$',
        r'^git://[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9](/[-a-zA-Z0-9._]+)+(\.git)?$',
        r'^git@[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9]:[-a-zA-Z0-9._/]+(\.git)?$',
    ]
    return any(re.match(pattern, url) for pattern in valid_patterns)


def read_git_packages_src_file(script_dir):
    """Read git repository URLs from git-packages-src file"""
    packages_file = script_dir / 'git-packages-src'
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith('#'):
                continue

            # Handle inline comments
            if '#' in line:
                line = line.split('#')[0].strip()

            if line:  # Validate and add URL
                if not is_valid_git_url(line):
                    print(f"Warning: Skipping invalid URL at line {line_num}: {line}", file=sys.stderr)
                    continue
                repos.append(line)

    return repos


def is_safe_dest_path(dest_path):
    """Validate that destination path doesn't escape home directory"""
    # Reject absolute paths and path traversal attempts
    if dest_path.startswith('/'):
        return False
    # Normalize and check for .. components
    normalized = os.path.normpath(dest_path)
    if normalized.startswith('..'):
        return False
    if '/..' in normalized or normalized == '..':
        return False
    return True


def read_git_dotfiles_file(script_dir):
    """Read git repository URLs from git-dotfiles file
    Format: repo-url destination-directory
    """
    packages_file = script_dir / 'git-dotfiles'
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith('#'):
                continue

            # Handle inline comments
            if '#' in line:
                line = line.split('#')[0].strip()

            if line:  # Parse and validate URL and destination
                parts = line.split()
                if len(parts) >= 2:
                    url, dest_dir = parts[0], parts[1]
                    # Validate URL
                    if not is_valid_git_url(url):
                        print(f"Warning: Skipping invalid URL at line {line_num}: {url}", file=sys.stderr)
                        continue
                    # Validate destination path
                    if not is_safe_dest_path(dest_dir):
                        print(f"Warning: Skipping unsafe destination at line {line_num}: {dest_dir}", file=sys.stderr)
                        continue
                    repos.append((url, dest_dir))

    return repos


def clone_and_build_repos(repos, username, tui, start_row):
    """Clone git repos to ~/.local/src and build them"""
    if not repos:
        return True, None, start_row

    home_dir = Path(f'/home/{username}')
    src_dir = home_dir / '.local' / 'src'

    # Create .local/src directory
    src_dir.mkdir(parents=True, exist_ok=True)

    # Fix ownership of entire .local directory to prevent root-owned parent dirs
    local_dir = home_dir / '.local'
    try:
        subprocess.run(['chown', '-R', f'{username}:{username}', str(local_dir)],
                      check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        # Non-fatal, but log it
        with open('/tmp/dotfiles-deploy.log', 'a') as f:
            f.write(f"\nWarning: Failed to set ownership of {local_dir}: {e}\n")

    failed_repos = []
    progress_row = start_row
    start_row += 1

    for i, repo_url in enumerate(repos, 1):
        # Extract repo name from URL
        repo_name = repo_url.rstrip('/').split('/')[-1].replace('.git', '')
        repo_path = src_dir / repo_name

        # Update progress line
        progress_msg = f"Repo {i}/{len(repos)}: {repo_name}"
        tui.show_message(progress_row, 4, progress_msg.ljust(60), color_pair=0)
        tui.stdscr.refresh()

        try:
            # Clone the repository
            if not repo_path.exists():
                subprocess.run(
                    ['git', 'clone', repo_url, str(repo_path)],
                    check=True,
                    capture_output=True,
                    text=True,
                    timeout=300  # 5 minute timeout for clone
                )
                # Set ownership
                try:
                    subprocess.run(['chown', '-R', f'{username}:{username}', str(repo_path)],
                                  check=True, capture_output=True)
                except subprocess.CalledProcessError:
                    pass  # Continue even if chown fails

            # Add safe.directory to allow root to access user-owned repo
            subprocess.run(
                ['git', 'config', '--global', '--add', 'safe.directory', str(repo_path)],
                check=True,
                capture_output=True
            )

            # Check if Makefile exists before building
            makefile_path = repo_path / 'Makefile'
            if not makefile_path.exists():
                raise FileNotFoundError(f"No Makefile found in {repo_path}")

            # Build with make
            subprocess.run(
                ['make'],
                cwd=str(repo_path),
                check=True,
                capture_output=True,
                text=True,
                timeout=600  # 10 minute timeout for build
            )

            # Install with sudo make install
            subprocess.run(
                ['make', 'install'],
                cwd=str(repo_path),
                check=True,
                capture_output=True,
                text=True,
                timeout=300  # 5 minute timeout for install
            )

        except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError) as e:
            # Log error to file with secure permissions
            try:
                log_file = '/tmp/dotfiles-deploy.log'
                # Create with restrictive permissions
                with open(log_file, 'a') as f:
                    os.chmod(log_file, 0o600)  # Owner read/write only
                    f.write(f"\n{'='*60}\n")
                    f.write(f"ERROR: Failed to build {repo_name}\n")
                    if hasattr(e, 'cmd'):
                        f.write(f"Command: {' '.join(e.cmd)}\n")
                    if hasattr(e, 'returncode'):
                        f.write(f"Return code: {e.returncode}\n")
                    f.write(f"Working directory: {repo_path}\n")
                    if hasattr(e, 'stdout') and e.stdout:
                        f.write(f"\nSTDOUT:\n{e.stdout}\n")
                    if hasattr(e, 'stderr') and e.stderr:
                        f.write(f"\nSTDERR:\n{e.stderr}\n")
                    f.write(f"Error type: {type(e).__name__}\n")
                    f.write(f"Error message: {str(e)}\n")
                    f.write(f"{'='*60}\n")
            except IOError:
                pass  # Can't write to log, continue anyway
            failed_repos.append(repo_name)

    # Summary
    start_row += 1
    if failed_repos:
        return False, failed_repos, start_row

    return True, None, start_row


def clone_dotfiles_home(repos, username, tui, start_row):
    """Clone git repos directly to home directory (no build)"""
    if not repos:
        return True, None, None, [], start_row

    home_dir = Path(f'/home/{username}')

    backup_dir = None
    backed_up_items = []
    failed_repos = []
    progress_row = start_row
    start_row += 1

    for i, (repo_url, dest_dir) in enumerate(repos, 1):
        # Extract repo name from URL for display
        repo_name = repo_url.rstrip('/').split('/')[-1].replace('.git', '')
        repo_path = home_dir / dest_dir

        # Update progress line
        progress_msg = f"Repo {i}/{len(repos)}: {dest_dir}"
        tui.show_message(progress_row, 4, progress_msg.ljust(60), color_pair=0)
        tui.stdscr.refresh()

        try:
            # Back up existing directory if it exists
            if repo_path.exists():
                if backup_dir is None:
                    backup_dir = get_backup_dir(home_dir)
                    backup_dir.mkdir(parents=True, exist_ok=True)

                # Back up to preserve structure
                backup_path = backup_dir / dest_dir
                backup_path.parent.mkdir(parents=True, exist_ok=True)

                # Move existing directory to backup
                shutil.move(str(repo_path), str(backup_path))
                backed_up_items.append(dest_dir)

            # Clone the repository
            subprocess.run(
                ['git', 'clone', repo_url, str(repo_path)],
                check=True,
                capture_output=True,
                text=True,
                timeout=300  # 5 minute timeout for clone
            )

            # Set ownership
            try:
                subprocess.run(['chown', '-R', f'{username}:{username}', str(repo_path)],
                              check=True, capture_output=True)
            except subprocess.CalledProcessError:
                pass  # Continue even if chown fails

        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            # Log error to file with secure permissions
            try:
                log_file = '/tmp/dotfiles-deploy.log'
                with open(log_file, 'a') as f:
                    os.chmod(log_file, 0o600)  # Owner read/write only
                    f.write(f"\n{'='*60}\n")
                    f.write(f"ERROR: Failed to clone {repo_name} to {dest_dir}\n")
                    if hasattr(e, 'cmd'):
                        f.write(f"Command: {' '.join(e.cmd)}\n")
                    if hasattr(e, 'returncode'):
                        f.write(f"Return code: {e.returncode}\n")
                    if hasattr(e, 'stdout') and e.stdout:
                        f.write(f"\nSTDOUT:\n{e.stdout}\n")
                    if hasattr(e, 'stderr') and e.stderr:
                        f.write(f"\nSTDERR:\n{e.stderr}\n")
                    f.write(f"Error type: {type(e).__name__}\n")
                    f.write(f"{'='*60}\n")
            except IOError:
                pass  # Can't write to log, continue anyway
            failed_repos.append(dest_dir)

    # Set ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(['chown', '-R', f'{username}:{username}', str(backup_dir)],
                          check=True, capture_output=True)
        except subprocess.CalledProcessError:
            pass  # Continue even if chown fails

    # Summary
    start_row += 1
    if failed_repos:
        return False, failed_repos, backup_dir, backed_up_items, start_row

    return True, None, backup_dir, backed_up_items, start_row


def read_packages_file(script_dir):
    """Read package names from apt-packages file"""
    packages_file = script_dir / 'apt-packages'
    if not packages_file.exists():
        return []

    packages = []
    with open(packages_file, 'r') as f:
        for line in f:
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith('#'):
                continue

            # Handle inline comments (e.g., "emacs  # text editor")
            if '#' in line:
                line = line.split('#')[0].strip()

            if line:  # Only add if there's a package name
                packages.append(line)

    return packages


def read_copy_these_file(script_dir):
    """Read dotfile paths from copy-these file
    Format: Files/directories to copy, one per line (e.g., /.bashrc, /.config/)
    Leading / is removed to get path relative to repo
    """
    copy_these_file = script_dir / 'copy-these'
    if not copy_these_file.exists():
        return []

    paths = []
    with open(copy_these_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith('#'):
                continue

            # Handle inline comments
            if '#' in line:
                line = line.split('#')[0].strip()

            if line:
                # Remove leading / to get relative path
                if line.startswith('/'):
                    line = line[1:]

                # Validate path safety
                if not is_safe_dest_path(line):
                    print(f"Warning: Skipping unsafe path at line {line_num}: {line}", file=sys.stderr)
                    continue

                paths.append(line)

    return paths


def is_package_installed(package_name):
    """Check if a package is already installed"""
    try:
        result = subprocess.run(
            ['dpkg-query', '-W', '-f=${Status}', package_name],
            capture_output=True,
            text=True,
            timeout=10
        )
        return 'install ok installed' in result.stdout
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return False


def read_third_party_packages_file(script_dir):
    """Read third-party package repository configurations
    Format: package_name | key_url | repo_line
    Returns list of tuples: (package_name, key_url, repo_line)
    """
    packages_file = script_dir / 'third-party-apt-packages'
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, 'r') as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and comments
            if not line or line.startswith('#'):
                continue

            # Parse the line
            parts = [part.strip() for part in line.split('|')]
            if len(parts) != 3:
                print(f"Warning: Skipping invalid line {line_num} in third-party-apt-packages", file=sys.stderr)
                continue

            package_name, key_url, repo_line = parts
            repos.append((package_name, key_url, repo_line))

    return repos


def setup_third_party_repos(repos, tui, start_row):
    """Setup third-party APT repositories"""
    if not repos:
        return True, None, start_row

    tui.show_progress(start_row, "Setting up third-party repositories...", success=None)
    tui.stdscr.refresh()

    keyrings_dir = Path('/etc/apt/keyrings')
    sources_dir = Path('/etc/apt/sources.list.d')

    # Create keyrings directory if it doesn't exist
    try:
        keyrings_dir.mkdir(parents=True, exist_ok=True)
    except (OSError, IOError) as e:
        tui.show_progress(start_row, "Setting up third-party repositories...", success=False)
        return False, f"Failed to create keyrings directory: {e}", start_row + 1

    failed_repos = []

    for package_name, key_url, repo_line in repos:
        try:
            # Download and install GPG key
            key_filename = f"{package_name}-repo-key.gpg"
            key_path = keyrings_dir / key_filename

            # Download key
            result = subprocess.run(
                ['curl', '-fsSL', key_url],
                capture_output=True,
                check=True,
                timeout=30
            )

            # Dearmor and save key
            subprocess.run(
                ['gpg', '--dearmor', '--yes', '-o', str(key_path)],
                input=result.stdout,
                check=True,
                timeout=10
            )

            # Add repository to sources.list.d
            sources_file = sources_dir / f"{package_name}.list"
            with open(sources_file, 'w') as f:
                f.write(f"{repo_line}\n")

        except (subprocess.CalledProcessError, subprocess.TimeoutExpired, IOError) as e:
            failed_repos.append(package_name)
            # Log error but continue with other repos
            with open('/tmp/dotfiles-deploy.log', 'a') as f:
                f.write(f"\nFailed to setup repository for {package_name}: {str(e)}\n")

    if failed_repos:
        tui.show_progress(start_row, "Setting up third-party repositories...", success=False)
        return False, failed_repos, start_row + 1

    tui.show_progress(start_row, "Setting up third-party repositories...", success=True)
    return True, None, start_row + 1


def install_packages(packages, tui, start_row):
    """Install packages using apt, showing progress for each one"""
    if not packages:
        return True, None, start_row

    # Update apt cache first
    tui.show_progress(start_row, "Updating package cache...", success=None)
    tui.stdscr.refresh()

    try:
        subprocess.run(['apt-get', 'update'],
                      check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        tui.show_progress(start_row, "Updating package cache...", success=True)
        start_row += 1
    except subprocess.CalledProcessError:
        tui.show_progress(start_row, "Updating package cache...", success=False)
        return False, "Failed to update package cache", start_row + 1

    # Install packages one by one
    failed_packages = []

    progress_row = start_row
    start_row += 1

    for i, package in enumerate(packages, 1):
        # Update progress line
        progress_msg = f"Package {i}/{len(packages)}: {package[:30]}"
        tui.show_message(progress_row, 4, progress_msg.ljust(60), color_pair=0)
        tui.stdscr.refresh()

        # Check if already installed
        if is_package_installed(package):
            continue

        # Install the package
        try:
            subprocess.run(['apt-get', 'install', '-y', package],
                         check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        except subprocess.CalledProcessError:
            failed_packages.append(package)

    # Summary
    start_row += 1
    if failed_packages:
        return False, failed_packages, start_row

    return True, None, start_row



def get_backup_dir(home_dir):
    """Find next available backup directory name"""
    backup_base = home_dir / '.backup'
    if not backup_base.exists():
        return backup_base

    counter = 2
    while True:
        backup_dir = home_dir / f'.backup{counter}'
        if not backup_dir.exists():
            return backup_dir
        counter += 1


def deploy_dotfiles(username, script_dir):
    """Deploy dotfiles to user's home directory"""
    home_dir = Path(f'/home/{username}')

    if not home_dir.exists():
        return False, "Home directory doesn't exist", None, []

    deployments = []

    # Read dotfiles to deploy from copy-these file
    dotfiles_to_deploy = read_copy_these_file(script_dir)

    # Build deployment list from copy-these entries
    for relative_path in dotfiles_to_deploy:
        src = script_dir / relative_path
        dst = home_dir / relative_path

        # Skip if source doesn't exist
        if not src.exists():
            print(f"Warning: Source path doesn't exist: {relative_path}", file=sys.stderr)
            continue

        # Create display name (with ~/ for home directory paths)
        if relative_path.startswith('.'):
            display_name = f'~/{relative_path}'
        else:
            display_name = relative_path

        deployments.append((display_name, src, dst))

    backup_dir = None
    backed_up_items = []

    for name, src, dst in deployments:
        try:
            # If destination exists, back it up first
            if dst.exists():
                if backup_dir is None:
                    backup_dir = get_backup_dir(home_dir)
                    backup_dir.mkdir(parents=True, exist_ok=True)

                # Determine backup path (preserve directory structure)
                relative_path = dst.relative_to(home_dir)
                backup_path = backup_dir / relative_path
                backup_path.parent.mkdir(parents=True, exist_ok=True)

                # Move existing file/directory to backup
                shutil.move(str(dst), str(backup_path))
                backed_up_items.append(name)

            # Create parent directory if it doesn't exist
            dst.parent.mkdir(parents=True, exist_ok=True)

            # Copy directory or file
            if src.is_dir():
                shutil.copytree(src, dst)
            else:
                shutil.copy2(src, dst)

            # Change ownership to the new user
            try:
                subprocess.run(['chown', '-R', f'{username}:{username}', str(dst)],
                              check=True, capture_output=True)
            except subprocess.CalledProcessError:
                pass  # Continue even if chown fails
        except (OSError, IOError, subprocess.CalledProcessError) as e:
            return False, str(e), None, []

    # Change ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(['chown', '-R', f'{username}:{username}', str(backup_dir)],
                          check=True, capture_output=True)
        except subprocess.CalledProcessError:
            pass  # Continue even if chown fails

    return True, None, backup_dir, backed_up_items


def deploy_system_configs(script_dir):
    """Deploy system configuration files to their appropriate system locations

    Returns: (success, error_message)
    """
    system_configs_dir = script_dir / 'system-configs'

    if not system_configs_dir.exists():
        return True, None  # No system configs to deploy, not an error

    deployed_files = []
    failed_files = []

    # Walk through system-configs directory
    for root, dirs, files in os.walk(system_configs_dir):
        for file in files:
            src_file = Path(root) / file

            # Calculate destination path by removing 'system-configs' prefix
            relative_path = src_file.relative_to(system_configs_dir)

            # Special handling for .patch files
            if file.endswith('.patch'):
                # Handle patching existing config files
                dest_file = Path('/') / relative_path.parent / file.replace('.patch', '')

                try:
                    # Read patch content
                    with open(src_file, 'r') as f:
                        patch_lines = [line.strip() for line in f if line.strip() and not line.strip().startswith('#')]

                    if dest_file.exists():
                        # Read existing config
                        with open(dest_file, 'r') as f:
                            existing_content = f.read()

                        # Check if patch lines already exist
                        needs_update = False
                        for patch_line in patch_lines:
                            if patch_line not in existing_content:
                                needs_update = True
                                break

                        if needs_update:
                            # Backup original
                            backup_path = Path(str(dest_file) + '.bak')
                            shutil.copy2(dest_file, backup_path)

                            # Append patch content
                            with open(dest_file, 'a') as f:
                                f.write('\n# Added by dotfiles deployment\n')
                                for patch_line in patch_lines:
                                    f.write(patch_line + '\n')

                            deployed_files.append(str(relative_path))
                    else:
                        # Create new file with patch content
                        dest_file.parent.mkdir(parents=True, exist_ok=True)
                        with open(dest_file, 'w') as f:
                            for patch_line in patch_lines:
                                f.write(patch_line + '\n')
                        deployed_files.append(str(relative_path))

                except (IOError, OSError) as e:
                    failed_files.append((str(relative_path), str(e)))
                    continue
            else:
                # Regular file - direct copy
                dest_file = Path('/') / relative_path

                try:
                    # Create parent directories if needed
                    dest_file.parent.mkdir(parents=True, exist_ok=True)

                    # Copy file
                    shutil.copy2(src_file, dest_file)

                    # Make scripts executable
                    if dest_file.suffix == '.sh' or 'bin' in dest_file.parts or 'dispatcher.d' in dest_file.parts or 'system-sleep' in dest_file.parts:
                        os.chmod(dest_file, 0o755)

                    deployed_files.append(str(relative_path))

                except (IOError, OSError) as e:
                    failed_files.append((str(relative_path), str(e)))
                    continue

    if failed_files:
        error_msg = f"Failed to deploy: {', '.join([f[0] for f in failed_files])}"
        return False, error_msg

    return True, None


def install_firefox_extensions(script_dir):
    """Install Firefox extensions via Enterprise Policies

    Returns: (success, error_message)
    """
    firefox_script = script_dir / 'firefox-extensions.sh'

    if not firefox_script.exists():
        return True, None  # No script to run, not an error

    try:
        # Make script executable
        os.chmod(firefox_script, 0o755)

        # Run the script
        subprocess.run(
            [str(firefox_script)],
            check=True,
            capture_output=True,
            text=True,
            timeout=30
        )

        return True, None

    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, OSError) as e:
        error_msg = f"Failed to install Firefox extensions: {str(e)}"
        return False, error_msg


def main_tui(stdscr):
    """Main TUI application"""
    tui = DeploymentTUI(stdscr)
    script_dir = Path(__file__).parent.resolve()

    # Check if running as root
    if not check_root():
        tui.draw_header("ERROR: ROOT REQUIRED")
        tui.show_message(4, 4, "This script must be run as root (sudo).", color_pair=3, bold=True)
        tui.show_message(6, 4, "Press any key to exit...")
        stdscr.getch()
        return

    # Welcome screen
    tui.draw_header("DOTFILES DEPLOYMENT")
    username = tui.get_input("Enter username to create:", 4, 4)

    if not username:
        tui.show_message(8, 4, "No username provided. Exiting.", color_pair=3)
        tui.show_message(9, 4, "Press any key to exit...")
        stdscr.getch()
        return

    # Validate username (lowercase letters, digits, underscore, hyphen; must start with letter or underscore)
    if not re.match(r'^[a-z_][a-z0-9_-]*[$]?$', username):
        tui.show_message(8, 4, "Invalid username format.", color_pair=3)
        tui.show_message(9, 4, "Must start with lowercase letter or underscore.", color_pair=3)
        tui.show_message(10, 4, "Press any key to exit...")
        stdscr.getch()
        return

    # Check if user exists and handle accordingly
    password = None
    user_created = False

    if user_exists(username):
        tui.show_message(8, 4, f"User '{username}' already exists.", color_pair=4)
        tui.show_message(9, 4, "Continue with deployment? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        user_created = False
    else:
        # Get password for new user
        password = tui.get_password("Enter password for new user:", 7, 4)
        if not password:
            tui.show_message(10, 4, "No password provided. Exiting.", color_pair=3)
            tui.show_message(11, 4, "Press any key to exit...")
            stdscr.getch()
            return

        password_confirm = tui.get_password("Confirm password:", 10, 4)
        if password != password_confirm:
            tui.show_message(13, 4, "Passwords do not match. Exiting.", color_pair=3)
            tui.show_message(14, 4, "Press any key to exit...")
            stdscr.getch()
            return
        user_created = True

    # Start deployment process
    tui.draw_header("DEPLOYMENT IN PROGRESS")
    row = 4

    # Create user if needed
    if user_created:
        tui.show_progress(row, f"Creating user '{username}'...", success=None)
        tui.stdscr.refresh()

        success, error_msg = create_user(username)
        if success:
            tui.show_progress(row, f"Creating user '{username}'...", success=True)
            row += 1

            # Set password
            tui.show_progress(row, "Setting password...", success=None)
            tui.stdscr.refresh()
            if set_user_password(username, password):
                tui.show_progress(row, "Setting password...", success=True)
                row += 1
            else:
                tui.show_progress(row, "Setting password...", success=False)
                tui.show_message(row + 1, 4, "Press any key to exit...", color_pair=3)
                stdscr.getch()
                return
        else:
            tui.show_progress(row, f"Creating user '{username}'...", success=False)
            if error_msg:
                tui.show_message(row + 1, 4, f"Error: {error_msg[:60]}", color_pair=3)
                row += 1
            tui.show_message(row + 1, 4, "Press any key to exit...", color_pair=3)
            stdscr.getch()
            return

    # Add to sudo group (for both new and existing users)
    tui.show_progress(row, "Adding to sudo group...", success=None)
    tui.stdscr.refresh()
    if add_user_to_sudo(username):
        tui.show_progress(row, "Adding to sudo group...", success=True)
        row += 1
    else:
        tui.show_progress(row, "Adding to sudo group...", success=False)
        tui.show_message(row + 1, 4, "Press any key to exit...", color_pair=3)
        stdscr.getch()
        return

    # Clone dotfile repos to home directory (before package installation)
    dotfiles_repos = read_git_dotfiles_file(script_dir)
    if dotfiles_repos:
        tui.show_message(row, 4, f"Cloning dotfile repositories ({len(dotfiles_repos)} total):",
                        color_pair=1, bold=True)
        row += 1
        tui.stdscr.refresh()

        success, failed_repos, dotfiles_backup_dir, dotfiles_backed_up, row = clone_dotfiles_home(dotfiles_repos, username, tui, row)

        if success:
            # Success - show backup info if any
            row += 1
            if dotfiles_backup_dir and dotfiles_backed_up:
                tui.show_message(row, 4, f"Backed up existing files to: {dotfiles_backup_dir.name}",
                               color_pair=4, bold=True)
                row += 1
        else:
            row += 1
            tui.show_message(row, 4, f"Failed to clone: {', '.join(failed_repos)}",
                           color_pair=3)
            row += 1
            tui.show_message(row, 4, "Error details: /tmp/dotfiles-deploy.log",
                           color_pair=4, bold=True)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Setup third-party repositories
    third_party_repos = read_third_party_packages_file(script_dir)
    third_party_package_names = []
    if third_party_repos:
        tui.show_message(row, 4, f"Setting up third-party repositories ({len(third_party_repos)} total):",
                        color_pair=1, bold=True)
        row += 1
        tui.stdscr.refresh()

        success, result, row = setup_third_party_repos(third_party_repos, tui, row)

        if success:
            # Extract package names to install later
            third_party_package_names = [name for name, _, _ in third_party_repos]
            row += 1
        else:
            failed_repos = result
            row += 1
            tui.show_message(row, 4, f"Failed to setup repos: {', '.join(failed_repos)}",
                           color_pair=3)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Install packages
    packages = read_packages_file(script_dir)
    # Add third-party package names to the installation list
    all_packages = packages + third_party_package_names
    if all_packages:
        tui.show_message(row, 4, f"Installing packages ({len(all_packages)} total):",
                        color_pair=1, bold=True)
        row += 1
        tui.stdscr.refresh()

        success, result, row = install_packages(all_packages, tui, row)

        if success:
            # Success - continue to dotfiles
            row += 1
        else:
            failed_packages = result
            row += 1
            tui.show_message(row, 4, f"Failed to install: {', '.join(failed_packages)}",
                           color_pair=3)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Deploy dotfiles
    tui.show_progress(row, "Deploying dotfiles...", success=None)
    tui.stdscr.refresh()

    success, error, backup_dir, backed_up_items = deploy_dotfiles(username, script_dir)

    if not success:
        tui.show_progress(row, "Deploying dotfiles...", success=False)
        tui.show_message(row + 2, 4, f"Error: {error}", color_pair=3)
        tui.show_message(row + 3, 4, "Press any key to exit...", color_pair=3)
        stdscr.getch()
        return
    else:
        tui.show_progress(row, "Deploying dotfiles...", success=True)
        row += 2

        # Show backup info if files were backed up
        if backup_dir and backed_up_items:
            tui.show_message(row, 4, f"Backed up existing files to: {backup_dir.name}",
                           color_pair=4, bold=True)
            row += 1

    # Clone and build source repositories
    src_repos = read_git_packages_src_file(script_dir)
    if src_repos:
        tui.show_message(row, 4, f"Cloning and building source repositories ({len(src_repos)} total):",
                        color_pair=1, bold=True)
        row += 1
        tui.stdscr.refresh()

        success, failed_repos, row = clone_and_build_repos(src_repos, username, tui, row)

        if success:
            # Success - continue
            row += 1
        else:
            row += 1
            tui.show_message(row, 4, f"Failed to build: {', '.join(failed_repos)}",
                           color_pair=3)
            row += 1
            tui.show_message(row, 4, "Error details: /tmp/dotfiles-deploy.log",
                           color_pair=4, bold=True)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Deploy system configurations
    tui.show_progress(row, "Deploying system configurations...", success=None)
    tui.stdscr.refresh()

    success, error = deploy_system_configs(script_dir)

    if not success:
        tui.show_progress(row, "Deploying system configurations...", success=False)
        tui.show_message(row + 1, 4, f"Error: {error}", color_pair=3)
        tui.show_message(row + 2, 4, "Continue anyway? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        row += 4
    else:
        tui.show_progress(row, "Deploying system configurations...", success=True)
        row += 1

    # Install Firefox extensions
    tui.show_progress(row, "Installing Firefox extensions...", success=None)
    tui.stdscr.refresh()

    success, error = install_firefox_extensions(script_dir)

    if not success:
        tui.show_progress(row, "Installing Firefox extensions...", success=False)
        tui.show_message(row + 1, 4, f"Error: {error}", color_pair=3)
        tui.show_message(row + 2, 4, "Continue anyway? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        row += 4
    else:
        tui.show_progress(row, "Installing Firefox extensions...", success=True)
        row += 1

    # Final message
    row += 2
    tui.show_message(row, 4, "Deployment complete!", color_pair=2, bold=True)
    tui.show_message(row + 1, 4, "Press any key to exit...")
    stdscr.getch()


def main():
    """Entry point"""
    try:
        curses.wrapper(main_tui)
    except KeyboardInterrupt:
        print("\nDeployment cancelled.")
        sys.exit(1)


if __name__ == '__main__':
    main()
