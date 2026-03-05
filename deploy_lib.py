#!/usr/bin/env python3
"""
Business-logic library for dotfiles deployment.

Provides helper functions for user creation, package installation,
dotfile overlay deployment, patching, and service configuration.

This module is imported by deploy.py, which is the sole entry point.
"""

import datetime
import os
import re
import shutil
import subprocess
import sys
import time
import traceback
from pathlib import Path


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
    stamp = datetime.datetime.now().strftime("%Y-%m-%d_%H%M%S")
    return f"/tmp/dotfiles-deploy-{stamp}.log"


LOG_FILE = get_log_file_path()


def log_error(message, exception=None, context=None):
    """Log error to secure temporary file"""

    try:
        # Create or append to log file
        with open(LOG_FILE, "a") as f:
            # Ensure secure permissions (only owner can read/write)
            try:
                os.chmod(LOG_FILE, 0o600)
            except OSError:
                pass  # Best effort

            timestamp = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
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





def check_root():
    """Check if script is running as root"""
    return os.geteuid() == 0


def user_exists(username):
    """Check if user already exists"""
    try:
        subprocess.run(["id", username], capture_output=True, check=True)
        return True
    except subprocess.CalledProcessError:
        return False


def create_user(username):
    """Create a new user with home directory"""
    try:
        # Create user with home directory
        subprocess.run(
            [
                "useradd",
                "-m",  # Create home directory
                "-s",
                "/bin/bash",  # Set default shell
                username,
            ],
            check=True,
            capture_output=True,
        )
        return True, None
    except subprocess.CalledProcessError as e:
        error_msg = e.stderr.decode() if e.stderr else str(e)
        log_error(f"Failed to create user '{username}'", e)
        return False, error_msg


def set_user_password(username, password):
    """Set password for user using chpasswd"""
    try:
        # Use chpasswd to set password
        process = subprocess.Popen(["chpasswd"], stdin=subprocess.PIPE)
        process.communicate(f"{username}:{password}".encode())
        return process.returncode == 0
    except Exception as e:
        log_error(f"Failed to set password for user '{username}'", e)
        return False


def add_user_to_sudo(username):
    """Add user to sudo group"""
    try:
        subprocess.run(
            ["usermod", "-aG", "sudo", username], check=True, capture_output=True
        )
        return True
    except subprocess.CalledProcessError as e:
        log_error(f"Failed to add user '{username}' to sudo group", e)
        return False


def add_user_to_video(username):
    """Add user to video group for backlight control (brightnessctl)"""
    try:
        subprocess.run(
            ["usermod", "-aG", "video", username], check=True, capture_output=True
        )
        return True
    except subprocess.CalledProcessError as e:
        log_error(f"Failed to add user '{username}' to video group", e)
        return False


def is_valid_git_url(url):
    """Validate that a URL is a safe git repository URL"""
    # Allow https://, git://, and git@ SSH URLs
    valid_patterns = [
        r"^https://[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9](/[-a-zA-Z0-9._]+)+(\.git)?$",
        r"^git://[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9](/[-a-zA-Z0-9._]+)+(\.git)?$",
        r"^git@[a-zA-Z0-9][-a-zA-Z0-9.]*[a-zA-Z0-9]:[-a-zA-Z0-9._/]+(\.git)?$",
    ]
    return any(re.match(pattern, url) for pattern in valid_patterns)


def read_git_packages_src_file(script_dir):
    """Read git repository URLs from git-packages-src file"""
    packages_file = script_dir / "packages/debian-git-packages-src.txt"
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, "r") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith("#"):
                continue

            # Handle inline comments
            if "#" in line:
                line = line.split("#")[0].strip()

            if line:  # Validate and add URL
                if not is_valid_git_url(line):
                    log_error(
                        f"Skipping invalid URL at line {line_num}",
                        context=f"File: {packages_file}, URL: {line}",
                    )
                    continue
                repos.append(line)

    return repos


def is_safe_dest_path(dest_path):
    """Validate that destination path doesn't escape home directory"""
    # Reject absolute paths and path traversal attempts
    if dest_path.startswith("/"):
        return False
    # Normalize and check for .. components
    normalized = os.path.normpath(dest_path)
    if normalized.startswith(".."):
        return False
    if "/.." in normalized or normalized == "..":
        return False
    return True


def read_git_dotfiles_file(script_dir):
    """Read git repository URLs from git-dotfiles file
    Format: repo-url destination-directory
    """
    packages_file = script_dir / "packages/debian-git-dotfiles.txt"
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, "r") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith("#"):
                continue

            # Handle inline comments
            if "#" in line:
                line = line.split("#")[0].strip()

            if line:  # Parse and validate URL and destination
                parts = line.split()
                if len(parts) >= 2:
                    url, dest_dir = parts[0], parts[1]
                    # Validate URL
                    if not is_valid_git_url(url):
                        log_error(
                            f"Skipping invalid URL at line {line_num}",
                            context=f"File: {packages_file}, URL: {url}",
                        )
                        continue
                    # Validate destination path
                    if not is_safe_dest_path(dest_dir):
                        log_error(
                            f"Skipping unsafe destination at line {line_num}",
                            context=f"File: {packages_file}, dest: {dest_dir}",
                        )
                        continue
                    repos.append((url, dest_dir))

    return repos


def clone_and_build_repos(repos, username, tui, start_row):
    """Clone git repos to ~/.local/src and build them"""
    if not repos:
        return True, None, start_row

    home_dir = Path(f"/home/{username}")
    src_dir = home_dir / ".local" / "src"

    # Create .local/src directory
    src_dir.mkdir(parents=True, exist_ok=True)

    # Fix ownership of entire .local directory to prevent root-owned parent dirs
    local_dir = home_dir / ".local"
    try:
        subprocess.run(
            ["chown", "-R", f"{username}:{username}", str(local_dir)],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        # Non-fatal, but log it
        log_error(f"Warning: Failed to set ownership of {local_dir}", e)

    failed_repos = []
    progress_row = start_row
    start_row += 1

    for i, repo_url in enumerate(repos, 1):
        # Extract repo name from URL
        repo_name = repo_url.rstrip("/").split("/")[-1].replace(".git", "")
        repo_path = src_dir / repo_name

        # Update progress line
        progress_msg = f"Cloning Repo {i}/{len(repos)}: {repo_name}"
        tui.show_message(progress_row, 4, progress_msg.ljust(60), color_pair=0)
        tui.stdscr.refresh()

        try:
            # Clone the repository
            if not repo_path.exists():
                subprocess.run(
                    ["git", "clone", repo_url, str(repo_path)],
                    check=True,
                    capture_output=True,
                    text=True,
                    timeout=300,  # 5 minute timeout for clone
                )
                # Set ownership
                try:
                    subprocess.run(
                        ["chown", "-R", f"{username}:{username}", str(repo_path)],
                        check=True,
                        capture_output=True,
                    )
                except subprocess.CalledProcessError:
                    pass  # Continue even if chown fails

            # Add safe.directory to allow root to access user-owned repo
            subprocess.run(
                [
                    "git",
                    "config",
                    "--global",
                    "--add",
                    "safe.directory",
                    str(repo_path),
                ],
                check=True,
                capture_output=True,
            )

            # Check if Makefile exists before building
            makefile_path = repo_path / "Makefile"
            if not makefile_path.exists():
                raise FileNotFoundError(f"No Makefile found in {repo_path}")

            # Build with make
            subprocess.run(
                ["make"],
                cwd=str(repo_path),
                check=True,
                capture_output=True,
                text=True,
                timeout=600,  # 10 minute timeout for build
            )

            # Install with sudo make install
            subprocess.run(
                ["make", "install"],
                cwd=str(repo_path),
                check=True,
                capture_output=True,
                text=True,
                timeout=300,  # 5 minute timeout for install
            )

        except (
            subprocess.CalledProcessError,
            subprocess.TimeoutExpired,
            FileNotFoundError,
        ) as e:
            # Log error
            log_error(
                f"Failed to build {repo_name}",
                e,
                context=f"Working directory: {repo_path}",
            )
            failed_repos.append(repo_name)

    # Summary
    if failed_repos:
        return False, failed_repos, start_row

    return True, None, start_row


def clone_dotfiles_home(repos, username, tui, start_row):
    """Clone git repos directly to home directory (no build)"""
    if not repos:
        return True, None, None, [], start_row

    home_dir = Path(f"/home/{username}")

    backup_dir = None
    backed_up_items = []
    failed_repos = []
    progress_row = start_row
    start_row += 1

    for i, (repo_url, dest_dir) in enumerate(repos, 1):
        # Extract repo name from URL for display
        repo_name = repo_url.rstrip("/").split("/")[-1].replace(".git", "")
        repo_path = home_dir / dest_dir

        # Update progress line
        progress_msg = f"Cloning Repo {i}/{len(repos)}: {dest_dir}"
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
                ["git", "clone", repo_url, str(repo_path)],
                check=True,
                capture_output=True,
                text=True,
                timeout=300,  # 5 minute timeout for clone
            )

            # Set ownership
            try:
                subprocess.run(
                    ["chown", "-R", f"{username}:{username}", str(repo_path)],
                    check=True,
                    capture_output=True,
                )
            except subprocess.CalledProcessError:
                pass  # Continue even if chown fails

        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            # Log error
            log_error(f"Failed to clone {repo_name} to {dest_dir}", e)
            failed_repos.append(dest_dir)

    # Set ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(
                ["chown", "-R", f"{username}:{username}", str(backup_dir)],
                check=True,
                capture_output=True,
            )
        except subprocess.CalledProcessError:
            pass  # Continue even if chown fails

    # Summary
    if failed_repos:
        return False, failed_repos, backup_dir, backed_up_items, start_row

    return True, None, backup_dir, backed_up_items, start_row


def read_packages_file(script_dir):
    """Read package names from apt-packages file"""
    packages_file = script_dir / "packages/debian-apt-packages.txt"
    if not packages_file.exists():
        return []

    packages = []
    with open(packages_file, "r") as f:
        for line in f:
            line = line.strip()
            # Skip empty lines and full-line comments
            if not line or line.startswith("#"):
                continue

            # Handle inline comments (e.g., "emacs  # text editor")
            if "#" in line:
                line = line.split("#")[0].strip()

            if line:  # Only add if there's a package name
                packages.append(line)

    return packages


def is_package_installed(package_name):
    """Check if a package is already installed"""
    try:
        result = subprocess.run(
            ["dpkg-query", "-W", "-f=${Status}", package_name],
            capture_output=True,
            text=True,
            timeout=10,
        )
        return "install ok installed" in result.stdout
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
        return False


def read_third_party_packages_file(script_dir):
    """Read third-party package repository configurations
    Format: package_name | key_url | repo_line
    Returns list of tuples: (package_name, key_url, repo_line)
    """
    packages_file = script_dir / "packages/debian-third-party-apt-packages.txt"
    if not packages_file.exists():
        return []

    repos = []
    with open(packages_file, "r") as f:
        for line_num, line in enumerate(f, 1):
            line = line.strip()
            # Skip empty lines and comments
            if not line or line.startswith("#"):
                continue

            # Parse the line
            parts = [part.strip() for part in line.split("|")]
            if len(parts) != 3:
                log_error(
                    f"Skipping invalid line {line_num} in third-party-apt-packages",
                    context=f"File: {packages_file}, content: {line}",
                )
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

    keyrings_dir = Path("/etc/apt/keyrings")
    sources_dir = Path("/etc/apt/sources.list.d")

    # Create keyrings directory if it doesn't exist
    try:
        keyrings_dir.mkdir(parents=True, exist_ok=True)
    except (OSError, IOError) as e:
        log_error("Failed to create keyrings directory", e)
        tui.show_progress(
            start_row, "Setting up third-party repositories...", success=False
        )
        return False, f"Failed to create keyrings directory: {e}", start_row + 1

    failed_repos = []

    for package_name, key_url, repo_line in repos:
        try:
            # Download and install GPG key
            key_filename = f"{package_name}-repo-key.gpg"
            key_path = keyrings_dir / key_filename

            # Download key
            result = run_command_with_retry(
                ["curl", "-fsSL", key_url],
                max_retries=3,
                check=True,
                capture_output=True,
                timeout=30,
            )

            # Dearmor and save key
            subprocess.run(
                ["gpg", "--dearmor", "--yes", "-o", str(key_path)],
                input=result.stdout,
                check=True,
                timeout=10,
            )

            # Add repository to sources.list.d
            sources_file = sources_dir / f"{package_name}.list"
            with open(sources_file, "w") as f:
                f.write(f"{repo_line}\n")

        except (subprocess.CalledProcessError, subprocess.TimeoutExpired, IOError) as e:
            failed_repos.append(package_name)
            # Log error but continue with other repos
            log_error(f"Failed to setup repository for {package_name}", e)

    if failed_repos:
        tui.show_progress(
            start_row, "Setting up third-party repositories...", success=False
        )
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
        sources_path = Path("/etc/apt/sources.list.d/debian.sources")
        if sources_path.exists():
            content = sources_path.read_text()
            new_content = re.sub(
                r"^Components:\s*main\s*$",
                "Components: main contrib non-free non-free-firmware",
                content,
                flags=re.MULTILINE,
            )
            if new_content != content:
                sources_path.write_text(new_content)

        # Traditional format (Debian 11 and older)
        list_path = Path("/etc/apt/sources.list")
        if list_path.exists():
            content = list_path.read_text()
            lines = content.splitlines()
            new_lines = []
            for line in lines:
                if line.strip().startswith("deb ") or line.strip().startswith(
                    "deb-src "
                ):
                    if " main" in line and "contrib" not in line:
                        line = line + " contrib non-free non-free-firmware"
                new_lines.append(line)
            new_content = "\n".join(new_lines) + "\n"
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
        subprocess.run(
            ["apt-get", "update"],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
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
        subprocess.run(
            ["apt-get", "upgrade", "-y"],
            check=True,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        )
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
        progress_msg = f"Installing Package {i}/{len(packages)}: {package[:30]}"
        tui.show_message(progress_row, 4, progress_msg.ljust(60), color_pair=0)
        tui.stdscr.refresh()

        # Check if already installed
        if is_package_installed(package):
            continue

        # Install the package
        try:
            subprocess.run(
                ["apt-get", "install", "-y", package],
                check=True,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
        except subprocess.CalledProcessError as e:
            log_error(f"Failed to install package: {package}", e)
            failed_packages.append(package)

    # Summary
    if failed_packages:
        return False, failed_packages, start_row

    return True, None, start_row


def get_backup_dir(home_dir):
    """Find next available backup directory name"""
    backup_base = home_dir / ".backup"
    if not backup_base.exists():
        return backup_base

    counter = 2
    while True:
        backup_dir = home_dir / f".backup{counter}"
        if not backup_dir.exists():
            return backup_dir
        counter += 1


def deploy_overlay(username, script_dir):
    """Deploy all files from dotfiles-overlay/ to their corresponding system locations

    The dotfiles-overlay/ directory is a filesystem mirror:
      dotfiles-overlay/home/new-user/.bashrc  -> /home/<username>/.bashrc
      dotfiles-overlay/etc/keyd/default.conf  -> /etc/keyd/default.conf
      dotfiles-overlay/usr/local/bin/myscript -> /usr/local/bin/myscript

    Home directory files (dotfiles-overlay/home/new-user/) get backup + chown treatment.
    All other files are copied directly.

    Returns: (success, error_message, backup_dir, backed_up_items)
    """
    overlay_dir = script_dir / "dotfiles-overlay"

    if not overlay_dir.exists():
        return False, "dotfiles-overlay/ directory doesn't exist", None, []

    home_dir = Path(f"/home/{username}")

    backup_dir = None
    backed_up_items = []
    failed_files = []

    for src_root, dirs, files in os.walk(overlay_dir):
        for file in files:
            src_file = Path(src_root) / file

            # Calculate relative path from dotfiles-overlay/
            relative_path = src_file.relative_to(overlay_dir)
            relative_parts = relative_path.parts

            # Determine if this is a home directory file
            is_home_file = (
                len(relative_parts) > 2
                and relative_parts[0] == "home"
                and relative_parts[1] == "new-user"
            )

            if is_home_file:
                # Map dotfiles-overlay/home/new-user/X -> /home/<username>/X
                home_relative = Path(*relative_parts[2:])  # strip home/new-user/
                dest_file = home_dir / home_relative
            else:
                # Map dotfiles-overlay/X -> /X
                dest_file = Path("/") / relative_path

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

                        display = f"~/{home_relative}"
                        backed_up_items.append(display)

                # Create parent directories
                dest_file.parent.mkdir(parents=True, exist_ok=True)

                # Copy file
                shutil.copy2(src_file, dest_file)

                # Make scripts executable for system files
                if not is_home_file:
                    if (
                        dest_file.suffix == ".sh"
                        or "bin" in dest_file.parts
                        or "dispatcher.d" in dest_file.parts
                        or "system-sleep" in dest_file.parts
                        or "acpi" in dest_file.parts
                    ):
                        os.chmod(dest_file, 0o755)

                # Set ownership for home directory files
                # (Removed per-file chown: we now do a blanket chown of the entire
                # home directory at the end of deploy_overlay to ensure parent directories
                # like ~/.config correctly get user ownership)

            except (OSError, IOError, subprocess.CalledProcessError) as e:
                log_error(f"Failed to deploy: {src_file} -> {dest_file}", e)
                failed_files.append((str(relative_path), str(e)))
                continue

    # Change ownership of backup directory if created
    if backup_dir and backup_dir.exists():
        try:
            subprocess.run(
                ["chown", "-R", f"{username}:{username}", str(backup_dir)],
                check=True,
                capture_output=True,
            )
        except subprocess.CalledProcessError:
            pass

    # Change ownership of the entire home directory to ensure all newly created
    # parent directories (like ~/.config) are owned by the user, not root.
    try:
        subprocess.run(
            ["chown", "-R", f"{username}:{username}", str(home_dir)],
            check=True,
            capture_output=True,
        )
    except subprocess.CalledProcessError as e:
        log_error(f"Failed to chown home directory {home_dir}", e)

    # Update font cache
    try:
        subprocess.run(["fc-cache", "-f"], check=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        log_error("Failed to update font cache", e)

    # Ensure fontconfig user configuration is enabled (50-user.conf)
    user_conf_link = Path("/etc/fonts/conf.d/50-user.conf")
    user_conf_target = Path("/usr/share/fontconfig/conf.avail/50-user.conf")

    if not user_conf_link.exists() and user_conf_target.exists():
        try:
            user_conf_link.symlink_to(user_conf_target)
        except (OSError, IOError) as e:
            log_error(
                "Failed to enable fontconfig user configuration (50-user.conf)", e
            )

    if failed_files:
        error_msg = f"Failed to deploy: {', '.join([f[0] for f in failed_files])}"
        return False, error_msg, backup_dir, backed_up_items

    return True, None, backup_dir, backed_up_items


def deploy_patches_to_system(script_dir):
    """Apply patch files from dotfiles-patches/ to their corresponding system locations

    Files under dotfiles-patches/ contain key=value lines (and comments) that are
    merged into existing system files. The directory structure mirrors the
    filesystem: dotfiles-patches/etc/systemd/logind.conf -> /etc/systemd/logind.conf

    Returns: (success, error_message)
    """
    patches_dir = script_dir / "dotfiles-patches"

    if not patches_dir.exists():
        return True, None  # No patches to apply, not an error

    deployed_files = []
    failed_files = []

    # Walk through dotfiles-patches/ directory
    for root, dirs, files in os.walk(patches_dir):
        for file in files:
            src_file = Path(root) / file

            # Calculate destination path: dotfiles-patches/etc/foo/bar -> /etc/foo/bar
            relative_path = src_file.relative_to(patches_dir)
            dest_file = Path("/") / relative_path

            try:
                # Read patch content (non-empty, non-comment lines)
                with open(src_file, "r") as f:
                    patch_lines = [
                        line.strip()
                        for line in f
                        if line.strip() and not line.strip().startswith("#")
                    ]

                if dest_file.exists():
                    # Read existing config
                    with open(dest_file, "r") as f:
                        existing_lines = [l.strip() for l in f.readlines()]

                    # Check if any patch lines are missing
                    needs_update = any(
                        line not in existing_lines for line in patch_lines
                    )

                    if needs_update:
                        # Backup original
                        backup_path = Path(str(dest_file) + ".bak")
                        shutil.copy2(dest_file, backup_path)

                        # Append missing lines
                        with open(dest_file, "a") as f:
                            f.write("\n# Added by dotfiles deployment\n")
                            for patch_line in patch_lines:
                                if patch_line not in existing_lines:
                                    f.write(patch_line + "\n")

                        deployed_files.append(str(relative_path))
                else:
                    # Create new file with patch content
                    dest_file.parent.mkdir(parents=True, exist_ok=True)
                    with open(dest_file, "w") as f:
                        for patch_line in patch_lines:
                            f.write(patch_line + "\n")
                    deployed_files.append(str(relative_path))

            except (IOError, OSError) as e:
                log_error(f"Failed to apply patch: {relative_path}", e)
                failed_files.append((str(relative_path), str(e)))
                continue

    if failed_files:
        error_msg = (
            f"Failed to apply patches: {', '.join([f[0] for f in failed_files])}"
        )
        return False, error_msg

    return True, None


def configure_keyd():
    """Enable and restart keyd service to apply configuration.

    Called after deploy_system_configs to apply the new keyd configuration.
    Returns: (success, error_message)
    """
    # Check if systemctl is available
    if not shutil.which("systemctl"):
        return True, None  # Not a systemd system

    # Check if keyd package is installed (Debian-specific)
    # Note: The binary may be named differently (e.g., keyd.rvaiya on Debian)
    # so we check for the package instead of the binary
    try:
        result = subprocess.run(
            ["dpkg-query", "-W", "-f=${Status}", "keyd"],
            capture_output=True,
            text=True,
            timeout=5,
        )
        if "install ok installed" not in result.stdout:
            return True, None  # keyd not installed
    except (
        subprocess.CalledProcessError,
        subprocess.TimeoutExpired,
        FileNotFoundError,
    ):
        return True, None  # dpkg not available or keyd not installed

    try:
        # Enable keyd service
        subprocess.run(
            ["systemctl", "enable", "keyd"], check=True, capture_output=True, timeout=10
        )

        # Restart keyd to apply new config
        subprocess.run(
            ["systemctl", "restart", "keyd"],
            check=True,
            capture_output=True,
            timeout=10,
        )
        return True, None
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        error_msg = f"Failed to configure keyd: {str(e)}"
        log_error("Failed to configure keyd service", e)
        # Don't fail the whole deployment if just the service restart fails,
        # but return False so it can be reported
        return False, error_msg




def install_firefox_extensions(script_dir):
    """Install Firefox extensions via Enterprise Policies

    Returns: (success, error_message)
    """
    firefox_script = script_dir / "firefox/firefox-extensions.sh"

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
            timeout=30,
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
    userjs_src = script_dir / "firefox/firefox-user.js"

    if not userjs_src.exists():
        return True, None, row

    tui.show_progress(row, "Installing Firefox user.js...", success=None)
    tui.stdscr.refresh()

    try:
        home_dir = Path(f"/home/{username}")
        firefox_dir = home_dir / ".mozilla" / "firefox"
        # We learned that Firefox ESR on Debian ignores hardcoded profiles.ini setups
        # and instead creates hash-based locks (like Install3B6073811A6ABF12).
        # We must let Firefox generate its profile naturally first, then inject.

        # 1. Ensure mozilla/firefox directory exists with correct skeletal ownership
        firefox_dir.mkdir(parents=True, exist_ok=True)
        subprocess.run(
            ["chown", "-R", f"{username}:{username}", str(home_dir / ".mozilla")],
            check=True,
        )

        # 2. Run Headless Firefox briefly
        # This forces Firefox-ESR to generate its true default profile and its installs.ini hashes
        try:
            # We use Popen so that we don't block. We sleep to let it write profiles.ini.
            # This exactly replicates the LARBS librewolf deployment method.
            cmd = "firefox-esr --headless || firefox --headless"
            p = subprocess.Popen(
                ["su", "-", username, "-c", cmd],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            time.sleep(4)
        except Exception as e:
            log_error(f"Failed to bootstrap firefox profile: {e}")

        # 3. Find whatever profiles it generated
        import glob

        profiles = glob.glob(str(firefox_dir / "*.default*"))

        installed = False
        for profile in profiles:
            dest = Path(profile) / "user.js"

            # Backup existing if present
            if dest.exists():
                backup = dest.with_name(f"user.js.backup.{int(time.time())}")
                shutil.copy2(dest, backup)
                subprocess.run(
                    ["chown", f"{username}:{username}", str(backup)], check=True
                )

            shutil.copy2(userjs_src, dest)
            subprocess.run(["chown", f"{username}:{username}", str(dest)], check=True)
            installed = True

        # 4. Kill the headless instance
        subprocess.run(["pkill", "-u", username, "-f", "firefox"], capture_output=True)
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


def install_tor_browser(username, script_dir, tui, row):
    """Install Tor Browser for the target user

    Runs the install-debian-tor-browser.sh script as the target user to ensure
    correct ownership of ~/.local/src/tor-browser and symlink.
    """
    install_script = script_dir / "scripts/install-debian-tor-browser.sh"

    if not install_script.exists():
        return True, None, row  # Script not present, skip silently

    tui.show_progress(row, "Installing Tor Browser...", success=None)
    tui.stdscr.refresh()

    try:
        # Read script content as root (who can access the dotfiles dir)
        # and pipe it to bash running as the target user
        with open(install_script, "r") as f:
            script_content = f.read()

        # Run as target user, passing script via stdin
        subprocess.run(
            ["su", "-c", "bash -s", username],
            input=script_content.encode(),
            check=True,
            capture_output=True,
            timeout=600,  # 10 minute timeout for download
        )
        tui.show_progress(row, "Installing Tor Browser...", success=True)
        return True, None, row + 1
    except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
        error_msg = e.stderr.decode() if hasattr(e, "stderr") and e.stderr else str(e)

        log_error(
            f"Tor Browser Installation Error for user {username}",
            e,
            context=f"Script: {install_script}",
        )
        tui.show_progress(row, "Installing Tor Browser...", success=False)
        return False, error_msg, row + 1


