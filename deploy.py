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
import shlex
import re
import datetime
import traceback
from pathlib import Path



import time

def run_command_with_retry(cmd, max_retries=3, delay=2, **kwargs):
    """Run subprocess.run with retries"""
    for attempt in range(max_retries):
        try:
            return subprocess.run(cmd, **kwargs)
        except subprocess.CalledProcessError as e:
            if attempt == max_retries - 1:
                raise  # Re-raise on last attempt
            
            # Log retry attempt
            log_error(f"Command failed (attempt {attempt + 1}/{max_retries}): {cmd}", e)
            time.sleep(delay)
    return None  # Should not be reached due to raise

def get_log_file_path():
    """Create a timestamped log file path for this run.

    Each invocation gets its own file, e.g.
    /tmp/dotfiles-deploy-2026-02-24_214718.log
    Lexicographic ordering == chronological ordering.
    """
    stamp = datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S')
    return f'/tmp/dotfiles-deploy-{stamp}.log'


LOG_FILE = get_log_file_path()


def log_error(message, exception=None, context=None):
    """Log error to secure temporary file"""
    
    try:
        # Create or append to log file
        with open(LOG_FILE, 'a') as f:
            # Ensure secure permissions (only owner can read/write)
            try:
                os.chmod(LOG_FILE, 0o600)
            except OSError:
                pass  # Best effort
                
            timestamp = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            f.write(f"\n{'-'*60}\n")
            f.write(f"[{timestamp}] ERROR: {message}\n")
            
            if context:
                f.write(f"Context: {context}\n")
                
            if isinstance(exception, subprocess.CalledProcessError):
                # Clean logging for subprocess errors - NO TRACEBACK
                f.write(f"Command: {exception.cmd}\n")
                f.write(f"Return Code: {exception.returncode}\n")
                if exception.stdout:
                    f.write(f"STDOUT:\n{exception.stdout}\n")
                if exception.stderr:
                    f.write(f"STDERR:\n{exception.stderr}\n")
            elif exception:
                # Standard logging for other exceptions
                f.write(f"Exception Type: {type(exception).__name__}\n")
                f.write(f"Exception Message: {str(exception)}\n")
                
                # Write traceback for unexpected python exceptions
                f.write("\nTraceback:\n")
                traceback.print_tb(exception.__traceback__, file=f)
                
            f.write(f"{'-'*60}\n")
    except Exception:
        # Failsafe: if logging fails, try to print to stderr (though curses might hide it)
        # but don't crash the program
        pass


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
        self.stdscr.addstr(1, (self.width - len(title)) // 2, title,
                          curses.color_pair(1) | curses.A_BOLD)

    def show_message(self, y, x, message, color_pair=0, bold=False):
        """Display a message at given coordinates"""
        # Bounds checking to prevent curses errors
        if y >= self.height - 1 or y < 0 or x >= self.width or x < 0:
            return
        # Truncate message if it would extend past the screen
        max_len = self.width - x - 1
        if len(message) > max_len:
            message = message[:max_len]
        attr = curses.color_pair(color_pair)
        if bold:
            attr |= curses.A_BOLD
        try:
            self.stdscr.addstr(y, x, message, attr)
        except curses.error:
            pass  # Silently ignore any remaining curses errors

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
        log_error(f"Failed to create user '{username}'", e)
        return False, error_msg


def set_user_password(username, password):
    """Set password for user using chpasswd"""
    try:
        # Use chpasswd to set password
        process = subprocess.Popen(['chpasswd'], stdin=subprocess.PIPE)
        process.communicate(f'{username}:{password}'.encode())
        return process.returncode == 0
    except Exception as e:
        log_error(f"Failed to set password for user '{username}'", e)
        return False


def add_user_to_sudo(username):
    """Add user to sudo group"""
    try:
        subprocess.run(['usermod', '-aG', 'sudo', username],
                      check=True, capture_output=True)
        return True
    except subprocess.CalledProcessError as e:
        log_error(f"Failed to add user '{username}' to sudo group", e)
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
    packages_file = script_dir / 'packages/git-packages-src'
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
    packages_file = script_dir / 'packages/git-dotfiles'
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
        log_error(f"Warning: Failed to set ownership of {local_dir}", e)

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
            # Log error
            log_error(f"Failed to build {repo_name}", e, context=f"Working directory: {repo_path}")
            failed_repos.append(repo_name)

    # Summary
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
            # Log error
            log_error(f"Failed to clone {repo_name} to {dest_dir}", e)
            failed_repos.append(dest_dir)

    # Set ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(['chown', '-R', f'{username}:{username}', str(backup_dir)],
                          check=True, capture_output=True)
        except subprocess.CalledProcessError:
            pass  # Continue even if chown fails

    # Summary
    if failed_repos:
        return False, failed_repos, backup_dir, backed_up_items, start_row

    return True, None, backup_dir, backed_up_items, start_row


def read_packages_file(script_dir):
    """Read package names from apt-packages file"""
    packages_file = script_dir / 'packages/apt-packages'
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
    packages_file = script_dir / 'packages/third-party-apt-packages'
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
        log_error("Failed to create keyrings directory", e)
        tui.show_progress(start_row, "Setting up third-party repositories...", success=False)
        return False, f"Failed to create keyrings directory: {e}", start_row + 1

    failed_repos = []

    for package_name, key_url, repo_line in repos:
        try:
            # Download and install GPG key
            key_filename = f"{package_name}-repo-key.gpg"
            key_path = keyrings_dir / key_filename

            # Download key
            result = run_command_with_retry(
                ['curl', '-fsSL', key_url],
                max_retries=3,
                check=True,
                capture_output=True,
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
            log_error(f"Failed to setup repository for {package_name}", e)

    if failed_repos:
        tui.show_progress(start_row, "Setting up third-party repositories...", success=False)
        return False, failed_repos, start_row + 1

    tui.show_progress(start_row, "Setting up third-party repositories...", success=True)
    return True, None, start_row + 1


def install_packages(packages, tui, start_row):
    """Install packages using apt, showing progress for each one"""
    if not packages:
        return True, None, start_row

    # Enable non-free repositories
    tui.show_progress(start_row, "Enabling non-free repositories...", success=None)
    tui.stdscr.refresh()
    
    try:
        # DEB822 format (Debian 12+)
        sources_path = Path('/etc/apt/sources.list.d/debian.sources')
        if sources_path.exists():
            content = sources_path.read_text()
            new_content = re.sub(r'^Components:\s*main\s*$', 'Components: main contrib non-free non-free-firmware', content, flags=re.MULTILINE)
            if new_content != content:
                sources_path.write_text(new_content)
                
        # Traditional format (Debian 11 and older)
        list_path = Path('/etc/apt/sources.list')
        if list_path.exists():
            content = list_path.read_text()
            lines = content.splitlines()
            new_lines = []
            for line in lines:
                if line.strip().startswith('deb ') or line.strip().startswith('deb-src '):
                    if ' main' in line and 'contrib' not in line:
                        line = line + ' contrib non-free non-free-firmware'
                new_lines.append(line)
            new_content = '\n'.join(new_lines) + '\n'
            if new_content != content:
                list_path.write_text(new_content)
                
        tui.show_progress(start_row, "Enabling non-free repositories...", success=True)
        start_row += 1
    except Exception as e:
        tui.show_progress(start_row, "Enabling non-free repositories...", success=False)
        log_error("Failed to enable non-free repositories", e)

    # Update apt cache first
    tui.show_progress(start_row, "Updating package cache...", success=None)
    tui.stdscr.refresh()

    try:
        subprocess.run(['apt-get', 'update'],
                      check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        tui.show_progress(start_row, "Updating package cache...", success=True)
        start_row += 1
    except subprocess.CalledProcessError as e:
        tui.show_progress(start_row, "Updating package cache...", success=False)
        log_error("Failed to update package cache", e)
        return False, "Failed to update package cache", start_row + 1

    # Upgrade existing packages
    tui.show_progress(start_row, "Upgrading system packages...", success=None)
    tui.stdscr.refresh()

    try:
        subprocess.run(['apt-get', 'upgrade', '-y'],
                      check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        tui.show_progress(start_row, "Upgrading system packages...", success=True)
        start_row += 1
    except subprocess.CalledProcessError as e:
        tui.show_progress(start_row, "Upgrading system packages...", success=False)
        log_error("Failed to upgrade system packages", e)
        return False, "Failed to upgrade system packages", start_row + 1

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
        except subprocess.CalledProcessError as e:
            log_error(f"Failed to install package: {package}", e)
            failed_packages.append(package)

    # Summary
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


def deploy_root(username, script_dir):
    """Deploy all files from root/ to their corresponding system locations

    The root/ directory is a filesystem mirror:
      root/home/new-user/.bashrc  -> /home/<username>/.bashrc
      root/etc/keyd/default.conf  -> /etc/keyd/default.conf
      root/usr/local/bin/myscript -> /usr/local/bin/myscript

    Home directory files (root/home/new-user/) get backup + chown treatment.
    All other files are copied directly.

    Returns: (success, error_message, backup_dir, backed_up_items)
    """
    root_dir = script_dir / 'root'

    if not root_dir.exists():
        return False, "root/ directory doesn't exist", None, []

    home_dir = Path(f'/home/{username}')

    backup_dir = None
    backed_up_items = []
    failed_files = []

    for src_root, dirs, files in os.walk(root_dir):
        for file in files:
            src_file = Path(src_root) / file

            # Calculate relative path from root/
            relative_path = src_file.relative_to(root_dir)
            relative_parts = relative_path.parts

            # Determine if this is a home directory file
            is_home_file = (
                len(relative_parts) > 2
                and relative_parts[0] == 'home'
                and relative_parts[1] == 'new-user'
            )

            if is_home_file:
                # Map root/home/new-user/X -> /home/<username>/X
                home_relative = Path(*relative_parts[2:])  # strip home/new-user/
                dest_file = home_dir / home_relative
            else:
                # Map root/X -> /X
                dest_file = Path('/') / relative_path

            try:
                if is_home_file:
                    # Back up existing home directory files
                    if dest_file.exists() or dest_file.is_symlink():
                        if backup_dir is None:
                            backup_dir = get_backup_dir(home_dir)
                            backup_dir.mkdir(parents=True, exist_ok=True)

                        backup_path = backup_dir / home_relative
                        backup_path.parent.mkdir(parents=True, exist_ok=True)
                        shutil.move(str(dest_file), str(backup_path))

                        display = f'~/{home_relative}'
                        backed_up_items.append(display)

                # Create parent directories
                dest_file.parent.mkdir(parents=True, exist_ok=True)

                # Copy file
                shutil.copy2(src_file, dest_file)

                # Make scripts executable for system files
                if not is_home_file:
                    if (dest_file.suffix == '.sh'
                            or 'bin' in dest_file.parts
                            or 'dispatcher.d' in dest_file.parts
                            or 'system-sleep' in dest_file.parts
                            or 'acpi' in dest_file.parts):
                        os.chmod(dest_file, 0o755)

                # Set ownership for home directory files
                # (Removed per-file chown: we now do a blanket chown of the entire
                # home directory at the end of deploy_root to ensure parent directories
                # like ~/.config correctly get user ownership)

            except (OSError, IOError, subprocess.CalledProcessError) as e:
                log_error(f"Failed to deploy: {src_file} -> {dest_file}", e)
                failed_files.append((str(relative_path), str(e)))
                continue



    # Change ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(['chown', '-R', f'{username}:{username}', str(backup_dir)],
                          check=True, capture_output=True)
        except subprocess.CalledProcessError:
            pass

    # Change ownership of the entire home directory to ensure all newly created
    # parent directories (like ~/.config) are owned by the user, not root.
    try:
        subprocess.run(['chown', '-R', f'{username}:{username}', str(home_dir)],
                      check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        log_error(f"Failed to chown home directory {home_dir}", e)

    # Update font cache
    try:
        subprocess.run(['fc-cache', '-f'], check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        log_error("Failed to update font cache", e)

    # Ensure fontconfig user configuration is enabled (50-user.conf)
    user_conf_link = Path('/etc/fonts/conf.d/50-user.conf')
    user_conf_target = Path('/usr/share/fontconfig/conf.avail/50-user.conf')

    if not user_conf_link.exists() and user_conf_target.exists():
        try:
            user_conf_link.symlink_to(user_conf_target)
        except (OSError, IOError) as e:
            log_error("Failed to enable fontconfig user configuration (50-user.conf)", e)

    if failed_files:
        error_msg = f"Failed to deploy: {', '.join([f[0] for f in failed_files])}"
        return False, error_msg, backup_dir, backed_up_items

    return True, None, backup_dir, backed_up_items


def deploy_patches(script_dir):
    """Apply patch files from patches/ to their corresponding system locations

    Files under patches/ contain key=value lines (and comments) that are
    merged into existing system files. The directory structure mirrors the
    filesystem: patches/etc/systemd/logind.conf -> /etc/systemd/logind.conf

    Returns: (success, error_message)
    """
    patches_dir = script_dir / 'patches'

    if not patches_dir.exists():
        return True, None  # No patches to apply, not an error

    deployed_files = []
    failed_files = []

    # Walk through patches/ directory
    for root, dirs, files in os.walk(patches_dir):
        for file in files:
            src_file = Path(root) / file

            # Calculate destination path: patches/etc/foo/bar -> /etc/foo/bar
            relative_path = src_file.relative_to(patches_dir)
            dest_file = Path('/') / relative_path

            try:
                # Read patch content (non-empty, non-comment lines)
                with open(src_file, 'r') as f:
                    patch_lines = [line.strip() for line in f
                                   if line.strip() and not line.strip().startswith('#')]

                if dest_file.exists():
                    # Read existing config
                    with open(dest_file, 'r') as f:
                        existing_lines = [l.strip() for l in f.readlines()]

                    # Check if any patch lines are missing
                    needs_update = any(line not in existing_lines for line in patch_lines)

                    if needs_update:
                        # Backup original
                        backup_path = Path(str(dest_file) + '.bak')
                        shutil.copy2(dest_file, backup_path)

                        # Append missing lines
                        with open(dest_file, 'a') as f:
                            f.write('\n# Added by dotfiles deployment\n')
                            for patch_line in patch_lines:
                                if patch_line not in existing_lines:
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
                log_error(f"Failed to apply patch: {relative_path}", e)
                failed_files.append((str(relative_path), str(e)))
                continue

    if failed_files:
        error_msg = f"Failed to apply patches: {', '.join([f[0] for f in failed_files])}"
        return False, error_msg

    return True, None


def configure_keyd():
    """Enable and restart keyd service to apply configuration.
    
    Called after deploy_system_configs to apply the new keyd configuration.
    Returns: (success, error_message)
    """
    # Check if systemctl is available
    if not shutil.which('systemctl'):
         return True, None # Not a systemd system

    # Check if keyd package is installed (Debian-specific)
    # Note: The binary may be named differently (e.g., keyd.rvaiya on Debian)
    # so we check for the package instead of the binary
    try:
        result = subprocess.run(
            ['dpkg-query', '-W', '-f=${Status}', 'keyd'],
            capture_output=True,
            text=True,
            timeout=5
        )
        if 'install ok installed' not in result.stdout:
            return True, None  # keyd not installed
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError):
        return True, None  # dpkg not available or keyd not installed

    try:
        # Enable keyd service
        subprocess.run(
            ['systemctl', 'enable', 'keyd'],
            check=True,
            capture_output=True,
            timeout=10
        )
        
        # Restart keyd to apply new config
        subprocess.run(
            ['systemctl', 'restart', 'keyd'],
            check=True,
            capture_output=True,
            timeout=10
        )
        return True, None
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        error_msg = f"Failed to configure keyd: {str(e)}"
        log_error("Failed to configure keyd service", e)
        # Don't fail the whole deployment if just the service restart fails, 
        # but return False so it can be reported
        return False, error_msg


def configure_kbdrate():
    """Enable and start kbdrate service for TTY keyboard repeat configuration.
    
    Called after deploy_system_configs to enable the kbdrate service.
    Returns: (success, error_message)
    """
    # Check if systemctl is available
    if not shutil.which('systemctl'):
         return True, None # Not a systemd system

    # Check if kbd package is installed (provides kbdrate)
    try:
        result = subprocess.run(
            ['dpkg-query', '-W', '-f=${Status}', 'kbd'],
            capture_output=True,
            text=True,
            timeout=5
        )
        if 'install ok installed' not in result.stdout:
            return True, None  # kbd not installed
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired, FileNotFoundError):
        return True, None  # dpkg not available or kbd not installed

    try:
        # Enable kbdrate service
        subprocess.run(
            ['systemctl', 'enable', 'kbdrate'],
            check=True,
            capture_output=True,
            timeout=10
        )
        
        # Start kbdrate service
        subprocess.run(
            ['systemctl', 'start', 'kbdrate'],
            check=True,
            capture_output=True,
            timeout=10
        )
        return True, None
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        error_msg = f"Failed to configure kbdrate: {str(e)}"
        log_error("Failed to configure kbdrate service", e)
        return False, error_msg



def install_firefox_extensions(script_dir):
    """Install Firefox extensions via Enterprise Policies

    Returns: (success, error_message)
    """
    firefox_script = script_dir / 'firefox/firefox-extensions.sh'

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
        log_error("Failed to install Firefox extensions", e)
        return False, error_msg




def install_firefox_userjs(username, script_dir, tui, row):
    """Installs the custom user.js to all Firefox profiles.
    Creates a default profile if none exist.
    """
    userjs_src = script_dir / 'firefox/firefox-user.js'
    
    if not userjs_src.exists():
        return True, None, row

    tui.show_progress(row, "Installing Firefox user.js...", success=None)
    tui.stdscr.refresh()

    try:
        home_dir = Path(f'/home/{username}')
        firefox_dir = home_dir / '.mozilla' / 'firefox'
        # We learned that Firefox ESR on Debian ignores hardcoded profiles.ini setups 
        # and instead creates hash-based locks (like Install3B6073811A6ABF12).
        # We must let Firefox generate its profile naturally first, then inject.
        
        # 1. Ensure mozilla/firefox directory exists with correct skeletal ownership
        firefox_dir.mkdir(parents=True, exist_ok=True)
        subprocess.run(['chown', '-R', f'{username}:{username}', str(home_dir / '.mozilla')], check=True)
        
        # 2. Run Headless Firefox briefly
        # This forces Firefox-ESR to generate its true default profile and its installs.ini hashes
        try:
            # We use Popen so that we don't block. We sleep to let it write profiles.ini.
            # This exactly replicates the LARBS librewolf deployment method.
            cmd = "firefox-esr --headless || firefox --headless"
            p = subprocess.Popen(['su', '-', username, '-c', cmd], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            time.sleep(4)
        except Exception as e:
            log_error(f"Failed to bootstrap firefox profile: {e}")
            
        # 3. Find whatever profiles it generated
        import glob
        profiles = glob.glob(str(firefox_dir / '*.default*'))
        
        installed = False
        for profile in profiles:
            dest = Path(profile) / 'user.js'
            
            # Backup existing if present
            if dest.exists():
                backup = dest.with_name(f'user.js.backup.{int(time.time())}')
                shutil.copy2(dest, backup)
                subprocess.run(['chown', f'{username}:{username}', str(backup)], check=True)
                
            shutil.copy2(userjs_src, dest)
            subprocess.run(['chown', f'{username}:{username}', str(dest)], check=True)
            installed = True

        # 4. Kill the headless instance
        subprocess.run(['pkill', '-u', username, '-f', 'firefox'], capture_output=True)
        try:
            p.terminate()
            p.wait(timeout=2)
        except Exception:
            pass

        if not installed:
            tui.show_progress(row, "Installing Firefox user.js...", success=False)
            return False, "Failed to find any Firefox profile directories.", row + 1

        tui.show_progress(row, "Installing Firefox user.js...", success=True)
        return True, None, row + 1

    except Exception as e:
        log_error("Failed to install Firefox user.js", e)
        tui.show_progress(row, "Installing Firefox user.js...", success=False)
        return False, str(e), row + 1



# ── Distrobox (Arch Linux) provisioning ────────────────────────────────

DISTROBOX_NAME = "archbox"
DISTROBOX_IMAGE = "archlinux"


def _run_as_user(username, cmd, timeout=600):
    """Run a shell command as the target user via su.

    Returns (success, stdout, stderr).
    """
    try:
        result = subprocess.run(
            ['su', '-', username, '-c', cmd],
            capture_output=True,
            text=True,
            check=True,
            timeout=timeout,
        )
        return True, result.stdout, result.stderr
    except subprocess.CalledProcessError as e:
        return False, e.stdout or "", e.stderr or ""
    except subprocess.TimeoutExpired:
        return False, "", "Command timed out"


def _run_in_distrobox(username, cmd, timeout=600):
    """Run a command inside the archbox distrobox container as the target user.

    Returns (success, stdout, stderr).
    """
    full_cmd = f'distrobox enter {DISTROBOX_NAME} -- bash -c {shlex.quote(cmd)}'
    return _run_as_user(username, full_cmd, timeout=timeout)


def _distrobox_exists(username):
    """Check if the archbox distrobox container exists for the given user."""
    ok, stdout, _ = _run_as_user(username, 'distrobox list --no-color', timeout=30)
    if not ok:
        return False
    for line in stdout.splitlines():
        parts = [p.strip() for p in line.split('|')]
        if len(parts) >= 2 and parts[1] == DISTROBOX_NAME:
            return True
    return False


def read_distrobox_packages(script_dir, filename):
    """Read a distrobox package list file."""
    filepath = script_dir / 'distrobox' / filename
    if not filepath.exists():
        return []
    with open(filepath) as f:
        return [line.strip() for line in f if line.strip()]


def setup_distrobox(username, script_dir, tui, row):
    """Provision the Arch Linux distrobox container.

    Creates the container, enables multilib (for Steam), initializes the
    keyring, syncs repos, installs pacman packages, bootstraps yay, and
    installs AUR packages.

    Returns (success, error_message, row).
    """
    # 1. Stop existing container if running, to prevent corruption
    tui.show_progress(row, f"Stopping existing {DISTROBOX_NAME} (if any)...", success=None)
    tui.stdscr.refresh()
    # It might fail if the container doesn't exist, so we just ignore errors
    _run_as_user(username, f"distrobox stop --yes {DISTROBOX_NAME}", timeout=60)
    tui.show_progress(row, f"Stopping existing {DISTROBOX_NAME} (if any)...", success=True)
    row += 1

    # 2. Create container
    tui.show_progress(row, "Creating archbox container...", success=None)
    tui.stdscr.refresh()

    if _distrobox_exists(username):
        tui.show_progress(row, "Creating archbox container... (already exists)", success=True)
    else:
        distrobox_home = f'/home/{username}/.local/share/distrobox/{DISTROBOX_NAME}'
        cmd = (f'distrobox create --name {DISTROBOX_NAME} --image {DISTROBOX_IMAGE}'
               f' --home {distrobox_home} --yes')
        ok, _, stderr = _run_as_user(username, cmd, timeout=300)
        if not ok:
            tui.show_progress(row, "Creating archbox container...", success=False)
            log_error(f"Failed to create distrobox container", context=f"stderr: {stderr}")
            return False, f"Could not create container: {stderr[:80]}", row + 1
        tui.show_progress(row, "Creating archbox container...", success=True)
    row += 1

    # 1.5 Initialize container
    # The first time 'distrobox enter' runs, it installs base packages like sudo.
    # This can take 1-2 minutes. We must do this before running setup commands.
    tui.show_progress(row, "Initializing container (this takes a minute)...", success=None)
    tui.stdscr.refresh()
    ok, _, stderr = _run_in_distrobox(username, "true", timeout=600)
    if not ok:
        tui.show_progress(row, "Initializing container (this takes a minute)...", success=False)
        log_error(f"Failed to initialize distrobox container", context=f"stderr: {stderr}")
        return False, f"Could not initialize container: {stderr[:80]}", row + 1
    tui.show_progress(row, "Initializing container (this takes a minute)...", success=True)
    row += 1

    # 2. Enable multilib
    tui.show_progress(row, "Enabling multilib (for Steam)...", success=None)
    tui.stdscr.refresh()

    ok, _, _ = _run_in_distrobox(username, "grep -q '^\\[multilib\\]' /etc/pacman.conf", timeout=30)
    if ok:
        tui.show_progress(row, "Enabling multilib (for Steam)... (already enabled)", success=True)
    else:
        enable_cmd = (
            "if grep -q '#\\[multilib\\]' /etc/pacman.conf; then "
            "  sudo sed -i '/^#\\[multilib\\]/,/^#Include/ s/^#//' /etc/pacman.conf; "
            "else "
            "  printf '\\n[multilib]\\nInclude = /etc/pacman.d/mirrorlist\\n' "
            "| sudo tee -a /etc/pacman.conf > /dev/null; "
            "fi"
        )
        ok, _, stderr = _run_in_distrobox(username, enable_cmd, timeout=30)
        if not ok:
            tui.show_progress(row, "Enabling multilib (for Steam)...", success=False)
            log_error(f"Failed to enable multilib: {stderr}")
            # Non-fatal — continue
        else:
            tui.show_progress(row, "Enabling multilib (for Steam)...", success=True)
    row += 1

    # 3. Init keyring
    tui.show_progress(row, "Initializing pacman keyring...", success=None)
    tui.stdscr.refresh()

    ok, _, stderr = _run_in_distrobox(
        username, "sudo pacman-key --init && sudo pacman-key --populate archlinux", timeout=120
    )
    if ok:
        tui.show_progress(row, "Initializing pacman keyring...", success=True)
    else:
        tui.show_progress(row, "Initializing pacman keyring...", success=False)
        log_error(f"Keyring init failed: {stderr}")
    row += 1

    # 4. Sync repos
    tui.show_progress(row, "Syncing package databases (pacman -Syu)...", success=None)
    tui.stdscr.refresh()

    ok, _, stderr = _run_in_distrobox(
        username, "sudo pacman -Syu --noconfirm", timeout=600
    )
    if not ok:
        tui.show_progress(row, "Syncing package databases (pacman -Syu)...", success=False)
        return False, f"Could not sync repos: {stderr[:80]}", row + 1
    tui.show_progress(row, "Syncing package databases (pacman -Syu)...", success=True)
    row += 1

    # 5. Install pacman packages
    pacman_pkgs = read_distrobox_packages(script_dir, 'distrobox-arch-pacman-pkglist.txt')
    if pacman_pkgs:
        tui.show_progress(row, f"Installing {len(pacman_pkgs)} pacman packages...", success=None)
        tui.stdscr.refresh()

        pkg_str = " ".join(pacman_pkgs)
        ok, _, stderr = _run_in_distrobox(
            username, f"sudo pacman -S --needed --noconfirm {pkg_str}", timeout=1200
        )
        if ok:
            tui.show_progress(row, f"Installing {len(pacman_pkgs)} pacman packages...", success=True)
        else:
            tui.show_progress(row, f"Installing {len(pacman_pkgs)} pacman packages...", success=False)
            log_error(f"Some pacman packages failed: {stderr}")
        row += 1

    # 6. Bootstrap yay
    tui.show_progress(row, "Bootstrapping yay (AUR helper)...", success=None)
    tui.stdscr.refresh()

    ok, _, _ = _run_in_distrobox(username, "command -v yay", timeout=30)
    if ok:
        tui.show_progress(row, "Bootstrapping yay (AUR helper)... (already installed)", success=True)
    else:
        yay_cmd = (
            "sudo pacman -S --needed --noconfirm git base-devel && "
            "cd /tmp && rm -rf yay-bin && "
            "git clone https://aur.archlinux.org/yay-bin.git && "
            "cd yay-bin && makepkg -si --noconfirm"
        )
        ok, _, stderr = _run_in_distrobox(username, yay_cmd, timeout=300)
        if not ok:
            tui.show_progress(row, "Bootstrapping yay (AUR helper)...", success=False)
            log_error(f"yay bootstrap failed: {stderr}")
            return False, f"Could not bootstrap yay: {stderr[:80]}", row + 1
        tui.show_progress(row, "Bootstrapping yay (AUR helper)...", success=True)
    row += 1

    # 7. Install AUR packages
    aur_pkgs = read_distrobox_packages(script_dir, 'distrobox-arch-aur-pkglist.txt')
    # Filter out yay-bin since we just bootstrapped it
    aur_pkgs = [p for p in aur_pkgs if p not in ('yay-bin', 'yay-bin-debug')]
    if aur_pkgs:
        tui.show_progress(row, f"Installing {len(aur_pkgs)} AUR packages...", success=None)
        tui.stdscr.refresh()

        aur_failed = []
        for pkg in aur_pkgs:
            ok, _, stderr = _run_in_distrobox(
                username, f"yay -S --needed --noconfirm {pkg}", timeout=600
            )
            if not ok:
                aur_failed.append(pkg)
                log_error(f"AUR package failed: {pkg}: {stderr}")

        if aur_failed:
            tui.show_progress(row, f"Installing {len(aur_pkgs)} AUR packages...", success=False)
            log_error(f"Failed AUR packages: {', '.join(aur_failed)}")
        else:
            tui.show_progress(row, f"Installing {len(aur_pkgs)} AUR packages...", success=True)
        row += 1

    return True, None, row


def install_tor_browser(username, script_dir, tui, row):
    """Install Tor Browser for the target user

    Runs the install-tor-browser script as the target user to ensure
    correct ownership of ~/.local/src/tor-browser and symlink.
    """
    install_script = script_dir / 'scripts/install-tor-browser'

    if not install_script.exists():
        return True, None, row  # Script not present, skip silently

    tui.show_progress(row, "Installing Tor Browser...", success=None)
    tui.stdscr.refresh()

    try:
        # Read script content as root (who can access the dotfiles dir)
        # and pipe it to bash running as the target user
        with open(install_script, 'r') as f:
            script_content = f.read()

        # Run as target user, passing script via stdin
        subprocess.run(
            ['su', '-c', 'bash -s', username],
            input=script_content.encode(),
            check=True,
            capture_output=True,
            timeout=600  # 10 minute timeout for download
        )
        tui.show_progress(row, "Installing Tor Browser...", success=True)
        return True, None, row + 1
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        error_msg = e.stderr.decode() if hasattr(e, 'stderr') and e.stderr else str(e)
        
        log_error(f"Tor Browser Installation Error for user {username}", e, context=f"Script: {install_script}")
        tui.show_progress(row, "Installing Tor Browser...", success=False)
        return False, error_msg, row + 1


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
        tui.show_message(row, 4, "Cloning dotfile repositories:",
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
            tui.show_message(row, 4, f"Error details: {LOG_FILE}",
                           color_pair=4, bold=True)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Ensure dependencies for adding repositories (curl, gpg) are installed
    # These are needed for setup_third_party_repos below
    tui.show_progress(row, "Checking prerequisite packages...", success=None)
    tui.stdscr.refresh()
    try:
        subprocess.run(['apt-get', 'install', '-y', 'curl', 'gpg'],
                      check=True, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        tui.show_progress(row, "Checking prerequisite packages...", success=True)
        row += 1
    except subprocess.CalledProcessError:
        tui.show_progress(row, "Checking prerequisite packages...", success=False)
        tui.show_message(row + 1, 4, "Failed to install curl/gpg. Third-party repos may fail.", color_pair=3)
        row += 2

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

    # Deploy all files from root/ (dotfiles + system configs)
    tui.show_progress(row, "Deploying from root/...", success=None)
    tui.stdscr.refresh()

    success, error, backup_dir, backed_up_items = deploy_root(username, script_dir)

    if not success:
        tui.show_progress(row, "Deploying from root/...", success=False)
        tui.show_message(row + 2, 4, f"Error: {error}", color_pair=3)
        tui.show_message(row + 3, 4, "Press any key to exit...", color_pair=3)
        stdscr.getch()
        return
    else:
        tui.show_progress(row, "Deploying from root/...", success=True)
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
            tui.show_message(row, 4, f"Error details: {LOG_FILE}",
                           color_pair=4, bold=True)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3

    # Install Tor Browser
    success, error, row = install_tor_browser(username, script_dir, tui, row)
    if not success:
        tui.show_message(row, 4, f"Error: {error[:50] if error else 'Unknown'}...", color_pair=3)
        tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        row += 3

    # Apply system patches
    tui.show_progress(row, "Applying system patches...", success=None)
    tui.stdscr.refresh()

    success, error = deploy_patches(script_dir)

    if not success:
        tui.show_progress(row, "Applying system patches...", success=False)
        tui.show_message(row + 1, 4, f"Error: {error}", color_pair=3)
        tui.show_message(row + 2, 4, "Continue anyway? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        row += 4
    else:
        tui.show_progress(row, "Applying system patches...", success=True)
        row += 1

    # Configure keyd to apply new configuration
    success, error = configure_keyd()
    if not success:
        tui.show_message(row, 4, f"Warning: {error}", color_pair=3)
        row += 1
    
    # Configure kbdrate service for TTY keyboard repeat
    success, error = configure_kbdrate()
    if not success:
        tui.show_message(row, 4, f"Warning: {error}", color_pair=3)
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

    # Install Firefox user.js
    success, error, row = install_firefox_userjs(username, script_dir, tui, row)
    if not success:
        tui.show_message(row, 4, f"Error: {error[:50] if error else 'Unknown'}...", color_pair=3)
        tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
        tui.stdscr.refresh()
        response = stdscr.getch()
        if chr(response).lower() != 'y':
            return
        row += 3

    # Optional: Set up Arch Linux distrobox
    tui.draw_header("ARCH LINUX DISTROBOX")
    row = 4
    tui.show_message(row, 4, "Set up Arch Linux distrobox (archbox)? (y/n): ", color_pair=4)
    tui.stdscr.refresh()
    curses.flushinp()
    response = stdscr.getch()
    if chr(response).lower() == 'y':
        row += 1
        tui.show_message(row, 4, "Setting up archbox:", color_pair=1, bold=True)
        row += 1

        success, error, row = setup_distrobox(username, script_dir, tui, row)
        if not success:
            row += 1
            tui.show_message(row, 4, f"Error: {error[:60] if error else 'Unknown'}", color_pair=3)
            tui.show_message(row + 1, 4, "Continue anyway? (y/n): ", color_pair=4)
            tui.stdscr.refresh()
            curses.flushinp()
            response = stdscr.getch()
            if chr(response).lower() != 'y':
                return
            row += 3
        else:
            row += 1
    else:
        row += 1

    # Cleanup - remove unnecessary packages and clean apt cache
    tui.show_progress(row, "Cleaning up...", success=None)
    tui.stdscr.refresh()
    
    try:
        subprocess.run(['apt-get', 'clean'], check=True, capture_output=True)
        subprocess.run(['apt-get', 'autoremove', '-y'], check=True, capture_output=True)
        tui.show_progress(row, "Cleaning up...", success=True)
    except subprocess.CalledProcessError as e:
        log_error("Cleanup failed", e)
        tui.show_progress(row, "Cleaning up...", success=False)
    row += 1

    # Final message - ensure it's visible even if row exceeds terminal height
    row += 2
    final_row = min(row, tui.height - 3)  # Leave room for both messages
    tui.show_message(final_row, 4, "Deployment complete!", color_pair=2, bold=True)
    tui.show_message(final_row + 1, 4, "Press any key to exit...")
    stdscr.refresh()
    stdscr.getch()


def main():
    """Entry point"""
    try:
        curses.wrapper(main_tui)
    except KeyboardInterrupt:
        print("\nDeployment cancelled.")
        sys.exit(1)
    except Exception as e:
        log_error("Unhandled exception in main application", e)
        # Try to restore terminal state if curses was active
        try:
            curses.endwin()
        except:
            pass
        print(f"CRITICAL ERROR: {e}")
        print(f"See {LOG_FILE} for details.")
        sys.exit(1)


if __name__ == '__main__':
    main()
