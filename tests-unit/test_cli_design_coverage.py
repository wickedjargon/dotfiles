"""Enforce the CLI_DESIGN.md contract for `.local/bin` scripts.

Every script in `.local/bin` must have:
  1. A tldr page at `.local/share/tldr/custom/pages/<name>.page.md`
  2. A bash completion file at `.local/share/bash-completion/completions/<name>`
"""

import os
import unittest

HOME_OVERLAY = os.path.abspath(
    os.path.join(os.path.dirname(__file__), '../dotfiles-overlay/home/new-user')
)
BIN_DIR = os.path.join(HOME_OVERLAY, '.local', 'bin')
TLDR_DIR = os.path.join(HOME_OVERLAY, '.local', 'share', 'tldr', 'custom', 'pages')
COMPLETION_DIR = os.path.join(
    HOME_OVERLAY, '.local', 'share', 'bash-completion', 'completions'
)


def bin_scripts():
    """All CLI utilities in .local/bin (skips directories like __pycache__)."""
    return sorted(
        name for name in os.listdir(BIN_DIR)
        if os.path.isfile(os.path.join(BIN_DIR, name))
    )


class TestCliDesignCoverage(unittest.TestCase):
    def test_bin_dir_exists_and_is_not_empty(self):
        self.assertTrue(bin_scripts(), f"No scripts found in {BIN_DIR}")

    def test_every_bin_script_has_a_tldr_page(self):
        missing = [
            name for name in bin_scripts()
            if not os.path.isfile(os.path.join(TLDR_DIR, f"{name}.page.md"))
        ]
        self.assertEqual(
            missing, [],
            "Scripts missing a tldr page (required by CLI_DESIGN.md); "
            f"add .local/share/tldr/custom/pages/<name>.page.md for: {missing}"
        )

    def test_every_bin_script_has_a_bash_completion(self):
        missing = [
            name for name in bin_scripts()
            if not os.path.isfile(os.path.join(COMPLETION_DIR, name))
        ]
        self.assertEqual(
            missing, [],
            "Scripts missing a bash completion (required by CLI_DESIGN.md); "
            f"add .local/share/bash-completion/completions/<name> for: {missing}"
        )

    def test_completions_register_their_command(self):
        """Each completion file must actually call `complete ... <name>`."""
        broken = []
        for name in bin_scripts():
            path = os.path.join(COMPLETION_DIR, name)
            if not os.path.isfile(path):
                continue  # reported by the test above
            with open(path) as f:
                content = f.read()
            if not any(
                line.strip().startswith("complete ") and line.strip().endswith(f" {name}")
                for line in content.splitlines()
            ):
                broken.append(name)
        self.assertEqual(
            broken, [],
            f"Completion files that never register their command via 'complete': {broken}"
        )


if __name__ == '__main__':
    unittest.main()
