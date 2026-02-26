"""Tests for the theme toggle script."""

import configparser
import json
import os
import subprocess

import pytest
from helpers import get_script_path, import_script

# Import the theme script as a module
theme_mod = import_script(get_script_path("theme"))


@pytest.fixture
def fake_home(tmp_path, monkeypatch):
    """Create a fake HOME with all theme config files pre-populated (dark defaults)."""
    home = tmp_path / "home"
    home.mkdir()

    # .gtkrc-2.0
    (home / ".gtkrc-2.0").write_text('gtk-theme-name = "Arc-Dark"\n')

    # gtk-3.0
    gtk3 = home / ".config" / "gtk-3.0"
    gtk3.mkdir(parents=True)
    (gtk3 / "settings.ini").write_text(
        "[Settings]\ngtk-theme-name=Arc-Dark\ngtk-application-prefer-dark-theme=true\n"
    )

    # gtk-4.0
    gtk4 = home / ".config" / "gtk-4.0"
    gtk4.mkdir(parents=True)
    (gtk4 / "settings.ini").write_text(
        "[Settings]\ngtk-theme-name=Arc-Dark\ngtk-application-prefer-dark-theme=true\n"
    )

    # qt5ct
    qt5 = home / ".config" / "qt5ct"
    qt5.mkdir(parents=True)
    (qt5 / "qt5ct.conf").write_text("[Appearance]\nstyle=kvantum-dark\n")

    # qt6ct
    qt6 = home / ".config" / "qt6ct"
    qt6.mkdir(parents=True)
    (qt6 / "qt6ct.conf").write_text("[Appearance]\nstyle=kvantum-dark\n")

    # Kvantum
    kv = home / ".config" / "Kvantum"
    kv.mkdir(parents=True)
    (kv / "kvantum.kvconfig").write_text("[General]\ntheme=KvArcDark\n")

    # Polybar
    polybar = home / ".config" / "polybar"
    polybar.mkdir(parents=True)
    (polybar / "config.ini").write_text(
        "[colors]\nbackground = #01010A\nforeground = #ffffff\n\n"
        "[bar/mybar]\nbackground = ${colors.background}\nforeground = ${colors.foreground}\n"
    )
    (polybar / "bspwm_custom.sh").write_text(
        "#!/bin/sh\n"
        'COLOR_FOCUSED_UNDERLINE="#FFFFFF"\n'
        'COLOR_FOREGROUND="#B3B3B3"\n'
        'COLOR_EMPTY="#4D4D4D"\n'
        'COLOR_URGENT="#A5492F"\n'
    )

    # BSPWM
    bspwm = home / ".config" / "bspwm"
    bspwm.mkdir(parents=True)
    (bspwm / "bspwmrc").write_text(
        "#!/bin/sh\n"
        'bspc config normal_border_color  "#222222"\n'
        'bspc config active_border_color  "#222222"\n'
        'bspc config focused_border_color "#8b1a1a"\n'
        'bspc config presel_feedback_color  "#8b1a1a"\n'
    )

    # XOB
    xob = home / ".config" / "xob"
    xob.mkdir(parents=True)
    (xob / "styles.cfg").write_text(
        "default = {\n"
        "    color = {\n"
        "        normal = {\n"
        '            fg = "#ffffff";\n'
        '            bg = "#000000";\n'
        '            border = "#222222";\n'
        "        };\n"
        "        alt = {\n"
        '            fg = "#555555";\n'
        '            bg = "#000000";\n'
        '            border = "#222222";\n'
        "        };\n"
        "        overflow = {\n"
        '            fg = "#8b1a1a";\n'
        '            bg = "#000000";\n'
        '            border = "#8b1a1a";\n'
        "        };\n"
        "        altoverflow = {\n"
        '            fg = "#550000";\n'
        '            bg = "#000000";\n'
        '            border = "#550000";\n'
        "        };\n"
        "    };\n"
        "};\n"
    )

    # Dunst
    dunst = home / ".config" / "dunst"
    dunst.mkdir(parents=True)
    (dunst / "dunstrc").write_text(
        "[global]\n"
        "    font = Monospace 10\n\n"
        "[urgency_low]\n"
        '    background = "#000000"\n'
        '    frame_color = "#000000"\n'
        '    foreground = "#ffffff"\n'
        "    timeout = 5\n\n"
        "[urgency_normal]\n"
        '    background = "#000000"\n'
        '    frame_color = "#000000"\n'
        '    foreground = "#ffffff"\n'
        "    timeout = 5\n\n"
        "[urgency_critical]\n"
        '    background = "#660000"\n'
        '    frame_color = "#660000"\n'
        '    foreground = "#ffffff"\n'
    )

    # Dmenu
    dmenu = home / ".config" / "dmenu"
    dmenu.mkdir(parents=True)
    (dmenu / "config").write_text(
        '\nDMENU_BASIC_ARGS="-nb #000000 -nf #ffffff -sb #222222 -sf #ffffff -l 12"\n'
        'DMENU_FONT="monospace-10"\n'
    )

    # Antigravity (VS Code)
    antigravity = home / ".config" / "Antigravity" / "User"
    antigravity.mkdir(parents=True)
    (antigravity / "settings.json").write_text(
        json.dumps(
            {
                "editor.minimap.enabled": False,
                "workbench.colorTheme": "GitHub Dark Colorblind (Beta)",
                "window.restoreWindows": "none",
            },
            indent=4,
        )
        + "\n"
    )

    # Alacritty
    alacritty = home / ".config" / "alacritty"
    alacritty.mkdir(parents=True)
    (alacritty / "alacritty.toml").write_text(
        "[colors]\n"
        "[colors.primary]\n"
        'background = "#0d0e1c"\n'
        'foreground = "#ebdbb2"\n\n'
        "[colors.normal]\n"
        'black   = "#1d2021"\n'
        'red     = "#cc241d"\n'
        'green   = "#98971a"\n'
        'yellow  = "#d79921"\n'
        'blue    = "#458588"\n'
        'magenta = "#b16286"\n'
        'cyan    = "#689d6a"\n'
        'white   = "#a89984"\n\n'
        "[colors.bright]\n"
        'black   = "#928374"\n'
        'red     = "#fb4934"\n'
        'green   = "#b8bb26"\n'
        'yellow  = "#fabd2f"\n'
        'blue    = "#83a598"\n'
        'magenta = "#d3869b"\n'
        'cyan    = "#8ec07c"\n'
        'white   = "#ebdbb2"\n'
    )

    # Rofi
    rofi = home / ".config" / "rofi"
    rofi.mkdir(parents=True)
    (rofi / "config.rasi").write_text(
        "* {\n"
        "    background:                  #000000;\n"
        "    foreground:                  #ffffff;\n"
        "    selected-background:         #222222;\n"
        "    selected-foreground:         #ffffff;\n"
        "    border-color:                #333333;\n"
        "    separator-color:             #333333;\n"
        "}\n"
    )

    # Calibre
    calibre = home / ".config" / "calibre"
    calibre.mkdir(parents=True)
    (calibre / "viewer-webengine.json").write_text(
        "{\n"
        '  "session_data": {\n'
        '    "current_color_scheme": "sepia-dark"\n'
        "  }\n"
        "}\n"
    )

    # Zathura
    zathura = home / ".config" / "zathura"
    zathura.mkdir(parents=True)
    (zathura / "zathurarc").write_text(
        "set recolor true\n"
        "set default-bg \\#0d0e1c\n"
        "set recolor-lightcolor \\#0d0e1c\n"
    )

    # Patch HOME in the module
    monkeypatch.setattr(theme_mod, "HOME", str(home))
    monkeypatch.setattr(theme_mod, "STATE_FILE", str(home / ".config" / "theme-mode"))

    return home


# ── Milestone 1 Tests ────────────────────────────────────────────────────────


class TestReadWriteState:
    def test_read_state_missing_file(self, tmp_path, monkeypatch):
        monkeypatch.setattr(theme_mod, "STATE_FILE", str(tmp_path / "nonexistent"))
        assert theme_mod.read_state() == "dark"

    def test_read_state_dark(self, tmp_path, monkeypatch):
        state_file = tmp_path / "theme-mode"
        state_file.write_text("dark\n")
        monkeypatch.setattr(theme_mod, "STATE_FILE", str(state_file))
        assert theme_mod.read_state() == "dark"

    def test_read_state_light(self, tmp_path, monkeypatch):
        state_file = tmp_path / "theme-mode"
        state_file.write_text("light\n")
        monkeypatch.setattr(theme_mod, "STATE_FILE", str(state_file))
        assert theme_mod.read_state() == "light"

    def test_read_state_invalid_defaults_dark(self, tmp_path, monkeypatch):
        state_file = tmp_path / "theme-mode"
        state_file.write_text("garbage\n")
        monkeypatch.setattr(theme_mod, "STATE_FILE", str(state_file))
        assert theme_mod.read_state() == "dark"

    def test_write_state(self, tmp_path, monkeypatch):
        state_file = tmp_path / "config" / "theme-mode"
        monkeypatch.setattr(theme_mod, "STATE_FILE", str(state_file))
        theme_mod.write_state("light")
        assert state_file.read_text().strip() == "light"


class TestGtkrc2:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_gtkrc2(theme_mod.THEMES["light"])
        content = (fake_home / ".gtkrc-2.0").read_text()
        assert 'gtk-theme-name = "Arc"' in content
        assert "Arc-Dark" not in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_gtkrc2(theme_mod.THEMES["light"])
        theme_mod.switch_gtkrc2(theme_mod.THEMES["dark"])
        content = (fake_home / ".gtkrc-2.0").read_text()
        assert 'gtk-theme-name = "Arc-Dark"' in content


class TestGtk3:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_gtk3(theme_mod.THEMES["light"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "gtk-3.0" / "settings.ini"))
        assert config.get("Settings", "gtk-theme-name") == "Arc"
        assert config.get("Settings", "gtk-application-prefer-dark-theme") == "false"

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_gtk3(theme_mod.THEMES["light"])
        theme_mod.switch_gtk3(theme_mod.THEMES["dark"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "gtk-3.0" / "settings.ini"))
        assert config.get("Settings", "gtk-theme-name") == "Arc-Dark"
        assert config.get("Settings", "gtk-application-prefer-dark-theme") == "true"


class TestGtk4:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_gtk4(theme_mod.THEMES["light"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "gtk-4.0" / "settings.ini"))
        assert config.get("Settings", "gtk-theme-name") == "Arc"

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_gtk4(theme_mod.THEMES["light"])
        theme_mod.switch_gtk4(theme_mod.THEMES["dark"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "gtk-4.0" / "settings.ini"))
        assert config.get("Settings", "gtk-theme-name") == "Arc-Dark"


class TestQt:
    def test_switch_qt5_to_light(self, fake_home):
        theme_mod.switch_qt(theme_mod.THEMES["light"], 5)
        config = configparser.ConfigParser()
        config.optionxform = str
        config.read(str(fake_home / ".config" / "qt5ct" / "qt5ct.conf"))
        assert config.get("Appearance", "style") == "kvantum"

    def test_switch_qt6_to_light(self, fake_home):
        theme_mod.switch_qt(theme_mod.THEMES["light"], 6)
        config = configparser.ConfigParser()
        config.optionxform = str
        config.read(str(fake_home / ".config" / "qt6ct" / "qt6ct.conf"))
        assert config.get("Appearance", "style") == "kvantum"

    def test_switch_qt5_to_dark(self, fake_home):
        theme_mod.switch_qt(theme_mod.THEMES["light"], 5)
        theme_mod.switch_qt(theme_mod.THEMES["dark"], 5)
        config = configparser.ConfigParser()
        config.optionxform = str
        config.read(str(fake_home / ".config" / "qt5ct" / "qt5ct.conf"))
        assert config.get("Appearance", "style") == "kvantum-dark"

    def test_switch_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "qt5ct" / "qt5ct.conf"))
        theme_mod.switch_qt(theme_mod.THEMES["light"], 5)


class TestKvantum:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_kvantum(theme_mod.THEMES["light"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "Kvantum" / "kvantum.kvconfig"))
        assert config.get("General", "theme") == "KvArc"

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_kvantum(theme_mod.THEMES["light"])
        theme_mod.switch_kvantum(theme_mod.THEMES["dark"])
        config = configparser.ConfigParser()
        config.read(str(fake_home / ".config" / "Kvantum" / "kvantum.kvconfig"))
        assert config.get("General", "theme") == "KvArcDark"

    def test_switch_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "Kvantum" / "kvantum.kvconfig"))
        theme_mod.switch_kvantum(theme_mod.THEMES["light"])


# ── Milestone 2 Tests ────────────────────────────────────────────────────────


class TestPolybar:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_polybar(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "polybar" / "config.ini").read_text()
        assert "#E8E8EE" in content
        assert "#333333" in content
        assert "${colors.background}" in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_polybar(theme_mod.THEMES["light"])
        theme_mod.switch_polybar(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "polybar" / "config.ini").read_text()
        assert "#01010A" in content
        assert "#ffffff" in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "polybar" / "config.ini"))
        theme_mod.switch_polybar(theme_mod.THEMES["light"])


class TestPolybarBspwm:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_polybar_bspwm(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "polybar" / "bspwm_custom.sh").read_text()
        assert 'COLOR_FOCUSED_UNDERLINE="#333333"' in content
        assert 'COLOR_FOREGROUND="#555555"' in content
        assert 'COLOR_EMPTY="#BBBBBB"' in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_polybar_bspwm(theme_mod.THEMES["light"])
        theme_mod.switch_polybar_bspwm(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "polybar" / "bspwm_custom.sh").read_text()
        assert 'COLOR_FOCUSED_UNDERLINE="#FFFFFF"' in content
        assert 'COLOR_FOREGROUND="#B3B3B3"' in content
        assert 'COLOR_EMPTY="#4D4D4D"' in content


class TestBspwm:
    def test_switch_to_light(self, fake_home, monkeypatch):
        monkeypatch.setattr(theme_mod, "run_quiet", lambda cmd: None)
        theme_mod.switch_bspwm(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "bspwm" / "bspwmrc").read_text()
        assert '"#d0d0d0"' in content  # normal/active border
        assert '"#5294e2"' in content  # focused border (Arc blue)

    def test_switch_to_dark(self, fake_home, monkeypatch):
        monkeypatch.setattr(theme_mod, "run_quiet", lambda cmd: None)
        theme_mod.switch_bspwm(theme_mod.THEMES["light"])
        theme_mod.switch_bspwm(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "bspwm" / "bspwmrc").read_text()
        assert '"#222222"' in content


class TestXob:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_xob(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "xob" / "styles.cfg").read_text()
        assert '"#5294e2"' in content  # xob_fg light (Arc blue)
        assert '"#ffffff"' in content  # xob_bg light
        assert '"#d0d0d0"' in content  # xob_border light

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_xob(theme_mod.THEMES["light"])
        theme_mod.switch_xob(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "xob" / "styles.cfg").read_text()
        assert '"#ffffff"' in content
        assert '"#000000"' in content
        assert '"#222222"' in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "xob" / "styles.cfg"))
        theme_mod.switch_xob(theme_mod.THEMES["light"])


class TestDunst:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_dunst(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "dunst" / "dunstrc").read_text()
        assert '"#E8E8EE"' in content
        assert '"#CCCCCC"' in content
        assert '"#333333"' in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_dunst(theme_mod.THEMES["light"])
        theme_mod.switch_dunst(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "dunst" / "dunstrc").read_text()
        assert '"#000000"' in content
        assert '"#ffffff"' in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "dunst" / "dunstrc"))
        theme_mod.switch_dunst(theme_mod.THEMES["light"])


class TestDmenu:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_dmenu(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "dmenu" / "config").read_text()
        assert "-nb #E8E8EE" in content
        assert "-nf #333333" in content
        assert "-sb #CCCCCC" in content
        assert "-sf #333333" in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_dmenu(theme_mod.THEMES["light"])
        theme_mod.switch_dmenu(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "dmenu" / "config").read_text()
        assert "-nb #000000" in content
        assert "-nf #ffffff" in content
        assert "-sb #222222" in content
        assert "-sf #ffffff" in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "dmenu" / "config"))
        theme_mod.switch_dmenu(theme_mod.THEMES["light"])


class TestAlacritty:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_alacritty(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "alacritty" / "alacritty.toml").read_text()
        assert '"#fbf1c7"' in content  # light bg
        assert '"#3c3836"' in content  # light fg

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_alacritty(theme_mod.THEMES["light"])
        theme_mod.switch_alacritty(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "alacritty" / "alacritty.toml").read_text()
        assert '"#0d0e1c"' in content  # dark bg
        assert '"#ebdbb2"' in content  # dark fg

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "alacritty" / "alacritty.toml"))
        theme_mod.switch_alacritty(theme_mod.THEMES["light"])


class TestRofi:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_rofi(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "rofi" / "config.rasi").read_text()
        assert "background:                  #E8E8EE" in content
        assert "foreground:                  #333333" in content
        assert "selected-background:         #CCCCCC" in content
        assert "border-color:                #AAAAAA" in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_rofi(theme_mod.THEMES["light"])
        theme_mod.switch_rofi(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "rofi" / "config.rasi").read_text()
        assert "background:                  #000000" in content
        assert "foreground:                  #ffffff" in content
        assert "selected-background:         #222222" in content
        assert "border-color:                #333333" in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "rofi" / "config.rasi"))
        theme_mod.switch_rofi(theme_mod.THEMES["light"])


class TestWallpaper:
    def test_switch_calls_convert_and_feh_light(self, fake_home, monkeypatch):
        calls = []

        def mock_run(cmd, **kwargs):
            calls.append(cmd)
            return subprocess.CompletedProcess(cmd, 0)

        monkeypatch.setattr(theme_mod.subprocess, "run", mock_run)
        theme_mod.switch_wallpaper(theme_mod.THEMES["light"])
        assert any("convert" in c and "#CCCCCC" in c[3] for c in calls)
        assert any("feh" in c for c in calls)

    def test_switch_calls_convert_and_feh_dark(self, fake_home, monkeypatch):
        calls = []

        def mock_run(cmd, **kwargs):
            calls.append(cmd)
            return subprocess.CompletedProcess(cmd, 0)

        monkeypatch.setattr(theme_mod.subprocess, "run", mock_run)
        theme_mod.switch_wallpaper(theme_mod.THEMES["dark"])
        assert any("convert" in c and "#000000" in c[3] for c in calls)
        assert any("feh" in c for c in calls)


class TestCalibre:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_calibre(theme_mod.THEMES["light"])
        path = fake_home / ".config" / "calibre" / "viewer-webengine.json"
        settings = json.loads(path.read_text())
        assert settings["session_data"]["current_color_scheme"] == "sepia-light"

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_calibre(theme_mod.THEMES["light"])
        theme_mod.switch_calibre(theme_mod.THEMES["dark"])
        path = fake_home / ".config" / "calibre" / "viewer-webengine.json"
        settings = json.loads(path.read_text())
        assert settings["session_data"]["current_color_scheme"] == "sepia-dark"

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "calibre" / "viewer-webengine.json"))
        theme_mod.switch_calibre(theme_mod.THEMES["light"])


class TestZathura:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_zathura(theme_mod.THEMES["light"])
        content = (fake_home / ".config" / "zathura" / "zathurarc").read_text()
        assert "set default-bg \\#fbf1c7" in content
        assert "set recolor-lightcolor \\#fbf1c7" in content

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_zathura(theme_mod.THEMES["light"])
        theme_mod.switch_zathura(theme_mod.THEMES["dark"])
        content = (fake_home / ".config" / "zathura" / "zathurarc").read_text()
        assert "set default-bg \\#0d0e1c" in content
        assert "set recolor-lightcolor \\#0d0e1c" in content

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "zathura" / "zathurarc"))
        theme_mod.switch_zathura(theme_mod.THEMES["light"])


# ── Milestone 3 Tests ────────────────────────────────────────────────────────


class TestAntigravity:
    def test_switch_to_light(self, fake_home):
        theme_mod.switch_antigravity(theme_mod.THEMES["light"])
        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        settings = json.loads(path.read_text())
        assert settings["workbench.colorTheme"] == "GitHub Light Colorblind (Beta)"

    def test_switch_to_dark(self, fake_home):
        theme_mod.switch_antigravity(theme_mod.THEMES["light"])
        theme_mod.switch_antigravity(theme_mod.THEMES["dark"])
        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        settings = json.loads(path.read_text())
        assert settings["workbench.colorTheme"] == "GitHub Dark Colorblind (Beta)"

    def test_preserves_other_settings(self, fake_home):
        theme_mod.switch_antigravity(theme_mod.THEMES["light"])
        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        settings = json.loads(path.read_text())
        assert settings["editor.minimap.enabled"] is False
        assert settings["window.restoreWindows"] == "none"

    def test_handles_comments(self, fake_home):
        """Verify the script can handle // comments in settings.json."""
        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        path.write_text(
            "{\n"
            '    "workbench.colorTheme": "GitHub Dark Colorblind (Beta)"\n'
            '    // "some.commented.setting": true\n'
            "}\n"
        )
        theme_mod.switch_antigravity(theme_mod.THEMES["light"])
        settings = json.loads(path.read_text())
        assert settings["workbench.colorTheme"] == "GitHub Light Colorblind (Beta)"

    def test_missing_config(self, fake_home):
        os.remove(str(fake_home / ".config" / "Antigravity" / "User" / "settings.json"))
        theme_mod.switch_antigravity(theme_mod.THEMES["light"])


# ── Integration Tests ────────────────────────────────────────────────────────


def _mock_system(monkeypatch):
    """Mock all system-dependent calls for integration tests."""
    monkeypatch.setattr(theme_mod, "switch_gsettings", lambda t: None)
    monkeypatch.setattr(theme_mod, "switch_wallpaper", lambda t: None)
    monkeypatch.setattr(theme_mod, "run_quiet", lambda cmd: None)
    monkeypatch.setattr(theme_mod, "restart_services", lambda: None)
    monkeypatch.setattr(theme_mod, "restart_xdg_portal", lambda: None)


class TestApplyTheme:
    def test_apply_light(self, fake_home, monkeypatch):
        _mock_system(monkeypatch)
        theme_mod.apply_theme("light")

        assert (fake_home / ".config" / "theme-mode").read_text().strip() == "light"

        content = (fake_home / ".gtkrc-2.0").read_text()
        assert "Arc" in content
        assert "Arc-Dark" not in content

        content = (fake_home / ".config" / "polybar" / "config.ini").read_text()
        assert "#E8E8EE" in content

        content = (fake_home / ".config" / "dmenu" / "config").read_text()
        assert "-nb #E8E8EE" in content

        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        settings = json.loads(path.read_text())
        assert settings["workbench.colorTheme"] == "GitHub Light Colorblind (Beta)"

    def test_apply_dark(self, fake_home, monkeypatch):
        _mock_system(monkeypatch)
        theme_mod.apply_theme("light")
        theme_mod.apply_theme("dark")

        assert (fake_home / ".config" / "theme-mode").read_text().strip() == "dark"
        content = (fake_home / ".gtkrc-2.0").read_text()
        assert "Arc-Dark" in content

        content = (fake_home / ".config" / "polybar" / "config.ini").read_text()
        assert "#01010A" in content

        path = fake_home / ".config" / "Antigravity" / "User" / "settings.json"
        settings = json.loads(path.read_text())
        assert settings["workbench.colorTheme"] == "GitHub Dark Colorblind (Beta)"

    def test_toggle_from_dark(self, fake_home, monkeypatch):
        _mock_system(monkeypatch)
        theme_mod.write_state("dark")
        current = theme_mod.read_state()
        mode = "light" if current == "dark" else "dark"
        theme_mod.apply_theme(mode)
        assert (fake_home / ".config" / "theme-mode").read_text().strip() == "light"

    def test_toggle_from_light(self, fake_home, monkeypatch):
        _mock_system(monkeypatch)
        theme_mod.write_state("light")
        current = theme_mod.read_state()
        mode = "light" if current == "dark" else "dark"
        theme_mod.apply_theme(mode)
        assert (fake_home / ".config" / "theme-mode").read_text().strip() == "dark"
