"""Tests for the audio CLI (parsing and matching logic)."""

from helpers import get_bin_path, import_script

audio = import_script(get_bin_path("audio"))


class TestParsePercent:
    def test_bare_number(self):
        assert audio.parse_percent("60") == 60

    def test_percent_suffix(self):
        assert audio.parse_percent("115%") == 115

    def test_whitespace(self):
        assert audio.parse_percent(" 50 ") == 50

    def test_garbage_rejected(self):
        assert audio.parse_percent("loud") is None
        assert audio.parse_percent("-5") is None
        assert audio.parse_percent("5%%") is None
        assert audio.parse_percent("") is None
        assert audio.parse_percent(None) is None


class TestValidVolumeSpec:
    def test_normal_range(self):
        assert audio.valid_volume_spec("0") == 0
        assert audio.valid_volume_spec("100") == 100

    def test_boost_allowed_up_to_cap(self):
        assert audio.valid_volume_spec("150") == 150

    def test_above_cap_rejected(self):
        assert audio.valid_volume_spec("151") is None
        assert audio.valid_volume_spec("999") is None


class TestValidStepSpec:
    def test_typical_steps(self):
        assert audio.valid_step_spec("5") == 5
        assert audio.valid_step_spec("10%") == 10

    def test_bounds(self):
        assert audio.valid_step_spec("1") == 1
        assert audio.valid_step_spec("50") == 50
        assert audio.valid_step_spec("0") is None
        assert audio.valid_step_spec("51") is None


class TestParseWpctlVolume:
    def test_plain(self):
        assert audio.parse_wpctl_volume("Volume: 1.15") == (115, False)

    def test_muted(self):
        assert audio.parse_wpctl_volume("Volume: 0.50 [MUTED]") == (50, True)

    def test_zero(self):
        assert audio.parse_wpctl_volume("Volume: 0.00") == (0, False)

    def test_unparseable(self):
        assert audio.parse_wpctl_volume("") is None
        assert audio.parse_wpctl_volume("wpctl: error") is None


class TestVolumePct:
    def test_max_of_channels(self):
        volume = {
            "front-left": {"value": 38000, "value_percent": "58%"},
            "front-right": {"value": 39000, "value_percent": "60%"},
        }
        assert audio.volume_pct(volume) == 60

    def test_empty(self):
        assert audio.volume_pct({}) == 0

    def test_missing_percent_field(self):
        assert audio.volume_pct({"mono": {"value": 38000}}) == 0


class TestAppLabel:
    def test_application_name_wins(self):
        props = {"application.name": "Firefox", "media.name": "song.mp3"}
        assert audio.app_label(props) == "Firefox"

    def test_media_name_fallback(self):
        assert audio.app_label({"media.name": "song.mp3"}) == "song.mp3"

    def test_binary_fallback(self):
        props = {"application.process.binary": "mpv"}
        assert audio.app_label(props) == "mpv"

    def test_unknown(self):
        assert audio.app_label({}) == "unknown"


class TestFindMatches:
    DEVICES = [
        {"name": "alsa_output.pci-0000_00_1f.3.analog-stereo",
         "desc": "Built-in Audio Analog Stereo"},
        {"name": "alsa_output.pci-0000_00_1f.3.hdmi-stereo",
         "desc": "Built-in Audio Digital Stereo (HDMI)"},
        {"name": "bluez_output.AA_BB.1", "desc": "Nothing Ear"},
    ]

    def test_partial_desc_match(self):
        matches = audio.find_matches(self.DEVICES, "hdmi")
        assert len(matches) == 1
        assert matches[0]["desc"] == "Built-in Audio Digital Stereo (HDMI)"

    def test_case_insensitive(self):
        matches = audio.find_matches(self.DEVICES, "NOTHING")
        assert len(matches) == 1
        assert matches[0]["desc"] == "Nothing Ear"

    def test_multiple_matches(self):
        assert len(audio.find_matches(self.DEVICES, "built-in")) == 2

    def test_exact_desc_beats_partial(self):
        devices = [
            {"name": "a", "desc": "Speakers"},
            {"name": "b", "desc": "Speakers (rear)"},
        ]
        matches = audio.find_matches(devices, "speakers")
        assert len(matches) == 1
        assert matches[0]["name"] == "a"

    def test_no_match(self):
        assert audio.find_matches(self.DEVICES, "usb") == []
