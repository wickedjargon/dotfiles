"""Shared test utilities for dotfiles test suite."""

import os
import importlib.util
import importlib.machinery


def import_script(path):
    """Import a Python script that doesn't have a .py extension.
    
    This is useful for scripts in .local/bin that are executable
    but don't have file extensions.
    
    Args:
        path: Absolute path to the script file
        
    Returns:
        The imported module
        
    Raises:
        FileNotFoundError: If the script doesn't exist
    """
    if not os.path.exists(path):
        raise FileNotFoundError(f"Script not found: {path}")
        
    name = os.path.basename(path).replace('-', '_')
    loader = importlib.machinery.SourceFileLoader(name, path)
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


def get_script_path(script_name):
    """Get the absolute path to a script in root/home/new-user/.local/bin.
    
    Args:
        script_name: Name of the script (e.g., 'battery-warnings')
        
    Returns:
        Absolute path to the script
    """
    tests_dir = os.path.dirname(os.path.abspath(__file__))
    return os.path.join(tests_dir, '..', 'root', 'home', 'new-user', '.local', 'bin', script_name)
