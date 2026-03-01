import importlib.util
import os
import tempfile
import unittest

# Import the script source directly since it has no extension
SCRIPT_PATH = os.path.abspath(
    os.path.join(
        os.path.dirname(__file__),
        "../dotfiles-overlay/home/new-user/.local/bin/rofi-sxhkd-help",
    )
)

if not os.path.exists(SCRIPT_PATH):
    raise FileNotFoundError(f"Script not found at {SCRIPT_PATH}")

import importlib.machinery

loader = importlib.machinery.SourceFileLoader("rofi_sxhkd_help", SCRIPT_PATH)
spec = importlib.util.spec_from_loader("rofi_sxhkd_help", loader)
rofi_sxhkd_help = importlib.util.module_from_spec(spec)
loader.exec_module(rofi_sxhkd_help)


class TestRofiSxhkdHelp(unittest.TestCase):

    def test_get_braces_content(self):
        # Basic case
        start, end, content = rofi_sxhkd_help.get_braces_content("foo{a,b}bar")
        self.assertEqual(content, "a,b")
        self.assertEqual(start, 3)
        self.assertEqual(end, 7)

        # No braces
        start, end, content = rofi_sxhkd_help.get_braces_content("foobar")
        self.assertEqual(content, None)

        # Nested braces (should take outer)
        # Note: Current implementation finds first closing brace, so {a{b}c} might behave simply.
        # The script's logic is meant for simple sxhkd braces.
        # Let's verify current behavior (first matching set).
        start, end, content = rofi_sxhkd_help.get_braces_content("foo{a,b}bar{c,d}")
        self.assertEqual(content, "a,b")
        self.assertEqual(start, 3)
        self.assertEqual(end, 7)

    def test_expand_braces(self):
        # Simple expansion
        res = rofi_sxhkd_help.expand_braces("foo{a,b}bar")
        # Expect list of tuples (text, variants)
        # Expansion of foo{a,b}bar -> fooabar (var a), foobbar (var b)
        expected = [("fooabar", ["a"]), ("foobbar", ["b"])]
        self.assertEqual(res, expected)

        # No braces
        res = rofi_sxhkd_help.expand_braces("foobar")
        self.assertEqual(res, [("foobar", [])])

        # Multiple comma-separated values
        res = rofi_sxhkd_help.expand_braces("{a,b,c}")
        self.assertEqual(res, [("a", ["a"]), ("b", ["b"]), ("c", ["c"])])

        # Recursive/Nested (if supported by script, though simple split used)
        # Script does recursive call using prefix + part + suffix.
        # test: a{b,c}d{e,f}
        # Step 1: a + b + d{e,f}, a + c + d{e,f}
        # Next expansion: abdef (e), abdf (f) ...
        # Result variants should accumulate?
        # Script logic: new_variants = variants + [part_clean]

        res = rofi_sxhkd_help.expand_braces("a{b,c}d{e,f}")
        # Expected: abde, abdf, acde, acdf
        # Variants: [b,e], [b,f], [c,e], [c,f]

        # Checking just texts for brevity
        texts = [x[0] for x in res]
        self.assertEqual(texts, ["abde", "abdf", "acde", "acdf"])

        # Checking variants for first item
        self.assertEqual(res[0][1], ["b", "e"])

    def test_parse_config(self):
        # Create a temporary sxhkdrc
        content = """
# Description 1
super + a
    command 1

# Description 2
super + {b,c}
    command {2,3}

# Header that should be ignored
# ========================

# Description 3
super + d
    command 4
"""
        with tempfile.NamedTemporaryFile(mode="w+", delete=False) as tmp:
            tmp.write(content)
            tmp_path = tmp.name

        try:
            bindings = rofi_sxhkd_help.parse_config(tmp_path)

            # Verify bindings count
            # 1 (a) + 2 (b,c) + 1 (d) = 4
            self.assertEqual(len(bindings), 4)

            # Check Description 1
            self.assertEqual(bindings[0]["desc"], "Description 1")
            self.assertEqual(bindings[0]["key"], "super + a")
            self.assertEqual(bindings[0]["cmd"], "command 1")

            # Check Description 2 variants
            # Should have suffixes "2" and "3" based on command variants
            self.assertEqual(bindings[1]["desc"], "Description 2 - 2")
            self.assertEqual(bindings[1]["key"], "super + b")
            self.assertEqual(bindings[1]["cmd"], "command 2")

            self.assertEqual(bindings[2]["desc"], "Description 2 - 3")
            self.assertEqual(bindings[2]["key"], "super + c")
            self.assertEqual(bindings[2]["cmd"], "command 3")

            # Check Description 3 handling of headers
            self.assertEqual(bindings[3]["desc"], "Description 3")
            self.assertEqual(bindings[3]["key"], "super + d")
            self.assertEqual(bindings[3]["cmd"], "command 4")

        finally:
            os.remove(tmp_path)


if __name__ == "__main__":
    unittest.main()
