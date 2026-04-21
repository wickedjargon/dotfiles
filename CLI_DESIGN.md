# CLI Utility Design Philosophy (`.local/bin`)

The majority of scripts in `.local/bin` are inspired by the design philosophy of **`pass`** (the Unix password manager). This document is the authoritative style guide for creating new CLI utilities in this project.

## Rules for ALL `.local/bin` Scripts

Regardless of whether a script follows the `pass`-inspired pattern, **every** script in `.local/bin` must have:

1. **A tldr page** at `~/.local/share/tldr/custom/pages/<name>.page.md`
2. **A bash completion file** at `~/.local/share/bash-completion/completions/<name>`

### tldr Format

```markdown
# name

> One-line description.
> Additional context lines.

- Description of usage:

`name subcommand`

- Another usage:

`name subcommand {{argument}}`
```

### Bash Completion Format

```bash
# bash completion for name
_name() {
    local cur prev
    COMPREPLY=()
    cur="${COMP_WORDS[COMP_CWORD]}"
    prev="${COMP_WORDS[COMP_CWORD-1]}"

    if [ "$COMP_CWORD" -eq 1 ]; then
        COMPREPLY=( $(compgen -W "subcommand1 subcommand2 help --help" -- "$cur") )
        return
    fi

    local cmd="${COMP_WORDS[1]}"
    case "$cmd" in
        subcommand1)
            # context-aware completions
            ;;
    esac
}

complete -F _name name
```

---

## Pass-Inspired Scripts

The following scripts follow the `pass` design philosophy: **`drives`**, **`wifi`**, **`bt`**, **`displays`**, **`psync`**, **`upload`**, **`sched`**, **`rss`**, **`svc`**, **`vpn`**, **`mac`**.

### Core Principles (from `pass`)

1. **Simple, verb-based subcommands** — `drives mount`, `wifi connect`, `bt pair`. No flags for primary actions. The bare command (no subcommand) shows status/listing (mirrors `pass` defaulting to `show`).
2. **Smart defaults & auto-selection** — If there's only one candidate (e.g., one unmounted drive), act on it automatically without prompting.
3. **Minimal dependencies** — Each script is a single Python file with no pip dependencies. Only stdlib + system tools (nmcli, udisksctl, etc.).
4. **Unix philosophy** — Do one thing well. Each utility manages one domain (drives, wifi, bluetooth, displays, etc.).
5. **Human-readable output by default** — Colored, styled terminal output for interactive use. Machine-parseable output via `list` subcommands when needed.

### Structural Conventions

Every pass-inspired script follows this skeleton:

```python
#!/usr/bin/env python3
"""name — one-line description.

Requires: system-tool (package-name)
"""

import subprocess
import sys

# ── Colors ──────────────────────────────────────────────────────────
RED    = "\033[0;31m"
GREEN  = "\033[0;32m"
YELLOW = "\033[0;33m"
CYAN   = "\033[0;36m"
BOLD   = "\033[1m"
DIM    = "\033[2m"
RESET  = "\033[0m"

# ── Helpers ─────────────────────────────────────────────────────────
def info(msg):  print(f"{CYAN}▸{RESET} {msg}")
def ok(msg):    print(f"{GREEN}✓{RESET} {msg}")
def warn(msg):  print(f"{YELLOW}!{RESET} {msg}")
def die(msg):
    print(f"{RED}✗{RESET} {msg}", file=sys.stderr)
    sys.exit(1)

# ── Domain-specific parsing ────────────────────────────────────────
# ...

# ── Commands ────────────────────────────────────────────────────────
def cmd_status():
    """Bare command — show current state."""
    ...

def cmd_list():
    """List items in the domain."""
    ...

# ── Help + Main ─────────────────────────────────────────────────────
HELP_TEXT = """\
usage: name [command] [options]

Description.

commands:
  (none)                        show status
  list                          list items
  ...

examples:
  name                          show status
  name list                     list items
  ..."""

def main():
    if len(sys.argv) >= 2 and sys.argv[1] in ("-h", "--help", "help"):
        print(HELP_TEXT)
        sys.exit(0)

    from shutil import which
    if not which("required-tool"):
        die("required-tool not installed")

    if len(sys.argv) < 2:
        cmd_status()  # bare command = status
        return

    command = sys.argv[1]
    args = sys.argv[2:]

    commands = {
        "list": lambda: cmd_list(),
        # ...
    }

    if command not in commands:
        print(f"Error: unknown command '{command}'", file=sys.stderr)
        print("Run 'name --help' for usage.", file=sys.stderr)
        sys.exit(1)

    commands[command]()

if __name__ == "__main__":
    main()
```

### Status Indicators

Use consistent Unicode symbols for state:
- `●` (green) — active/connected/mounted
- `○` (dim) — inactive/disconnected/unmounted
- `▸` (cyan) — informational action in progress
- `✓` (green) — success
- `✗` (red) — fatal error
- `!` (yellow) — warning

### UX Patterns

#### Auto-selection
When exactly one candidate matches, act on it without prompting:
```
$ drives mount
▸ Mounting sdb1...
✓ Mounted sdb1 "BACKUP"  16G  → /run/media/user/BACKUP
```

#### Disambiguation
When multiple candidates exist, list them and ask the user to specify:
```
$ drives mount
Multiple unmounted drives found:
  ○ sdb1  "BACKUP"  16G  ext4  (unmounted)
  ○ sdc1  "DATA"    32G  ntfs  (unmounted)

Specify one, e.g.: drives mount sdb1
```

#### Help text
Follows `pass` conventions: `usage:` line, `commands:` section with aligned descriptions, `examples:` section.

### Section Separators

Use decorated comment headers to separate logical sections:
```python
# ── Section Name ────────────────────────────────────────────────────
```

---

## Non-Pass Scripts

The following scripts do **not** follow the `pass` pattern (they use `argparse` with flags instead of verb subcommands): **`theme`**, **`shorten-url`**.

They still require tldr pages and bash completions.

---

## Existing Utilities

| Script       | Pass-inspired | System tool        | tldr | completion |
|--------------|---------------|--------------------|------|------------|
| `drives`     | ✓             | udisksctl, lsblk   | ✓    | ✓          |
| `wifi`       | ✓             | nmcli              | ✓    | ✓          |
| `bt`         | ✓             | bluetoothctl       | ✓    | ✓          |
| `displays`   | ✓             | xrandr/wlr-randr   | ✓    | ✓          |
| `psync`      | ✓             | rsync, tailscale   | ✓    | ✓          |
| `upload`     | ✓             | curl               | ✓    | ✓          |
| `sched`      | ✓             | at, aplay          | ✓    | ✓          |
| `rss`        | ✓             | (stdlib only)      | ✓    | ✓          |
| `svc`        | ✓             | systemctl          | ✓    | ✓          |
| `vpn`        | ✓             | wg-quick, curl     | ✓    | ✓          |
| `mac`        | ✓             | ip (iproute2)      | ✓    | ✓          |
| `theme`      | ✗             | various            | ✓    | ✓          |
| `shorten-url`| ✗             | curl               | ✗    | ✗          |
