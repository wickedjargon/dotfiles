"""Tests for the sched alarm CLI."""

import datetime
import json
import os
from unittest.mock import MagicMock, patch

import pytest

from helpers import get_bin_path, import_script

cal_mod = import_script(get_bin_path("sched"))


# ── parse_time_str ──────────────────────────────────────────────────


class TestParseTimeStr:
    """Tests for parse_time_str (individual time token parsing)."""

    def test_simple_12h_pm(self):
        assert cal_mod.parse_time_str("2pm") == datetime.time(14, 0)

    def test_simple_12h_am(self):
        assert cal_mod.parse_time_str("9am") == datetime.time(9, 0)

    def test_12h_with_minutes(self):
        assert cal_mod.parse_time_str("2:30pm") == datetime.time(14, 30)

    def test_12h_with_minutes_am(self):
        assert cal_mod.parse_time_str("11:45am") == datetime.time(11, 45)

    def test_12h_with_space_before_ampm(self):
        assert cal_mod.parse_time_str("2:30 pm") == datetime.time(14, 30)

    def test_12pm_is_noon(self):
        assert cal_mod.parse_time_str("12pm") == datetime.time(12, 0)

    def test_12am_is_midnight(self):
        assert cal_mod.parse_time_str("12am") == datetime.time(0, 0)

    def test_24h_format(self):
        assert cal_mod.parse_time_str("14:00") == datetime.time(14, 0)

    def test_24h_format_single_digit_hour(self):
        assert cal_mod.parse_time_str("9:30") == datetime.time(9, 30)

    def test_24h_midnight(self):
        assert cal_mod.parse_time_str("0:00") == datetime.time(0, 0)

    def test_24h_end_of_day(self):
        assert cal_mod.parse_time_str("23:59") == datetime.time(23, 59)

    def test_invalid_hour_24h(self):
        assert cal_mod.parse_time_str("25:00") is None

    def test_invalid_minute_24h(self):
        assert cal_mod.parse_time_str("12:60") is None

    def test_invalid_hour_12h(self):
        assert cal_mod.parse_time_str("13pm") is None

    def test_invalid_minute_12h(self):
        assert cal_mod.parse_time_str("2:60pm") is None

    def test_garbage_input(self):
        assert cal_mod.parse_time_str("hello") is None

    def test_empty_string(self):
        assert cal_mod.parse_time_str("") is None

    def test_uppercase_ampm(self):
        assert cal_mod.parse_time_str("2PM") == datetime.time(14, 0)

    def test_mixed_case_ampm(self):
        assert cal_mod.parse_time_str("2:30Pm") == datetime.time(14, 30)


# ── format_eta ──────────────────────────────────────────────────────


class TestFormatEta:
    """Tests for format_eta (timedelta → human-readable string)."""

    def test_less_than_one_minute(self):
        assert cal_mod.format_eta(datetime.timedelta(seconds=30)) == "<1m"

    def test_exactly_one_minute(self):
        assert cal_mod.format_eta(datetime.timedelta(minutes=1)) == "1m"

    def test_minutes_only(self):
        assert cal_mod.format_eta(datetime.timedelta(minutes=45)) == "45m"

    def test_hours_only(self):
        assert cal_mod.format_eta(datetime.timedelta(hours=2)) == "2h"

    def test_hours_and_minutes(self):
        assert cal_mod.format_eta(datetime.timedelta(hours=1, minutes=30)) == "1h 30m"

    def test_zero_seconds(self):
        assert cal_mod.format_eta(datetime.timedelta(seconds=0)) == "<1m"

    def test_large_delta(self):
        assert cal_mod.format_eta(datetime.timedelta(hours=25, minutes=5)) == "25h 5m"


# ── parse_time ──────────────────────────────────────────────────────


class TestParseTime:
    """Tests for parse_time (full argument parsing with date logic)."""

    @pytest.fixture(autouse=True)
    def freeze_now(self):
        """Freeze 'now' to 2025-06-15 10:00:00 for deterministic tests."""
        _real_datetime = datetime.datetime
        self.frozen_now = _real_datetime(2025, 6, 15, 10, 0, 0)
        with patch.object(cal_mod.datetime, "datetime", wraps=_real_datetime) as mock_dt:
            mock_dt.now.return_value = self.frozen_now
            yield

    def test_future_time_today(self):
        # 2pm is after 10am, so it should be today
        result = cal_mod.parse_time(["2pm"])
        assert result == datetime.datetime(2025, 6, 15, 14, 0)

    def test_past_time_rolls_to_tomorrow(self):
        # 9am is before 10am, so it rolls to tomorrow
        result = cal_mod.parse_time(["9am"])
        assert result == datetime.datetime(2025, 6, 16, 9, 0)

    def test_24h_future_today(self):
        result = cal_mod.parse_time(["14:00"])
        assert result == datetime.datetime(2025, 6, 15, 14, 0)

    def test_tomorrow_prefix(self):
        result = cal_mod.parse_time(["tomorrow", "9am"])
        assert result == datetime.datetime(2025, 6, 16, 9, 0)

    def test_iso_date_prefix(self):
        result = cal_mod.parse_time(["2025-07-04", "2pm"])
        assert result == datetime.datetime(2025, 7, 4, 14, 0)

    def test_relative_minutes(self):
        result = cal_mod.parse_time(["30min"])
        assert result == self.frozen_now + datetime.timedelta(minutes=30)

    def test_relative_hours(self):
        result = cal_mod.parse_time(["2hr"])
        assert result == self.frozen_now + datetime.timedelta(hours=2)

    def test_relative_hours_and_minutes(self):
        result = cal_mod.parse_time(["1h30m"])
        assert result == self.frozen_now + datetime.timedelta(hours=1, minutes=30)

    def test_relative_with_plus_prefix(self):
        result = cal_mod.parse_time(["+1h30m"])
        assert result == self.frozen_now + datetime.timedelta(hours=1, minutes=30)

    def test_empty_args(self):
        assert cal_mod.parse_time([]) is None

    def test_tomorrow_without_time_returns_none(self):
        assert cal_mod.parse_time(["tomorrow"]) is None

    def test_invalid_time_exits(self):
        with pytest.raises(SystemExit):
            cal_mod.parse_time(["gobbledygook"])

    def test_invalid_iso_date_exits(self):
        with pytest.raises(SystemExit):
            cal_mod.parse_time(["2025-13-40", "2pm"])


# ── fire_alarm ──────────────────────────────────────────────────────


class TestFireAlarm:
    """Tests for fire_alarm (alarm execution path)."""

    def test_fire_reads_label_and_notifies(self, tmp_path):
        alarm_file = tmp_path / "abc123.json"
        alarm_file.write_text(json.dumps({
            "id": "abc123",
            "time": "2025-06-15T14:00:00",
            "label": "Take medicine",
        }))

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch.object(cal_mod, "ALARM_SOUND", "/nonexistent"), \
             patch("subprocess.run") as mock_run:
            cal_mod.fire_alarm("abc123")

        # notify-send was called with the label
        mock_run.assert_called_once()
        call_args = mock_run.call_args[0][0]
        assert "notify-send" in call_args
        assert "Take medicine" in call_args

        # Metadata file was cleaned up
        assert not alarm_file.exists()

    def test_fire_without_label_uses_time(self, tmp_path):
        alarm_file = tmp_path / "xyz789.json"
        alarm_file.write_text(json.dumps({
            "id": "xyz789",
            "time": "2025-06-15T14:00:00",
            "label": "",
        }))

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch.object(cal_mod, "ALARM_SOUND", "/nonexistent"), \
             patch("subprocess.run") as mock_run:
            cal_mod.fire_alarm("xyz789")

        call_args = mock_run.call_args[0][0]
        assert "notify-send" in call_args
        assert not alarm_file.exists()

    def test_fire_plays_sound_when_present(self, tmp_path):
        alarm_file = tmp_path / "snd001.json"
        alarm_file.write_text(json.dumps({
            "id": "snd001",
            "time": "2025-06-15T14:00:00",
            "label": "Ring",
        }))
        sound_file = tmp_path / "alarm.oga"
        sound_file.write_text("")  # just needs to exist

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch.object(cal_mod, "ALARM_SOUND", str(sound_file)), \
             patch("subprocess.run") as mock_run:
            cal_mod.fire_alarm("snd001")

        # Should have been called twice: notify-send + paplay
        assert mock_run.call_count == 2
        paplay_call = mock_run.call_args_list[1][0][0]
        assert "paplay" in paplay_call

    def test_fire_missing_alarm_file_still_notifies(self, tmp_path):
        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch.object(cal_mod, "ALARM_SOUND", "/nonexistent"), \
             patch("subprocess.run") as mock_run:
            cal_mod.fire_alarm("missing")

        mock_run.assert_called_once()


# ── cancel_alarm ────────────────────────────────────────────────────


class TestCancelAlarm:
    """Tests for cancel_alarm (alarm cancellation)."""

    def test_cancel_removes_file_and_stops_timer(self, tmp_path):
        alarm_file = tmp_path / "abc123.json"
        alarm_file.write_text(json.dumps({
            "id": "abc123",
            "time": "2025-06-15T14:00:00",
            "label": "Test",
        }))

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run"):
            cal_mod.cancel_alarm("abc123")

        assert not alarm_file.exists()

    def test_cancel_nonexistent_exits(self, tmp_path):
        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run"):
            with pytest.raises(SystemExit):
                cal_mod.cancel_alarm("nope")


# ── list_alarms ─────────────────────────────────────────────────────


class TestListAlarms:
    """Tests for list_alarms (alarm listing)."""

    def test_list_empty_dir(self, tmp_path, capsys):
        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)):
            cal_mod.list_alarms()
        assert "No active alarms" in capsys.readouterr().out

    def test_list_shows_active_alarms(self, tmp_path, capsys):
        future = (datetime.datetime.now() + datetime.timedelta(hours=2)).isoformat()
        alarm_file = tmp_path / "abc123.json"
        alarm_file.write_text(json.dumps({
            "id": "abc123",
            "time": future,
            "label": "Dentist",
        }))

        mock_result = MagicMock()
        mock_result.stdout = "active"

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run", return_value=mock_result):
            cal_mod.list_alarms()

        output = capsys.readouterr().out
        assert "abc123" in output
        assert "Dentist" in output

    def test_list_cleans_stale_alarms(self, tmp_path, capsys):
        alarm_file = tmp_path / "stale1.json"
        alarm_file.write_text(json.dumps({
            "id": "stale1",
            "time": "2025-01-01T00:00:00",
            "label": "Old",
        }))

        mock_result = MagicMock()
        mock_result.stdout = "inactive"

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run", return_value=mock_result):
            cal_mod.list_alarms()

        # Stale file should be cleaned up
        assert not alarm_file.exists()
        assert "No active alarms" in capsys.readouterr().out


# ── set_alarm ───────────────────────────────────────────────────────


class TestSetAlarm:
    """Tests for set_alarm (alarm scheduling)."""

    def test_set_alarm_creates_metadata_and_calls_systemd(self, tmp_path, capsys):
        target = datetime.datetime.now() + datetime.timedelta(hours=1)

        mock_result = MagicMock()
        mock_result.returncode = 0

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run", return_value=mock_result):
            cal_mod.set_alarm(target, label="Test alarm")

        # A metadata JSON was created
        json_files = list(tmp_path.glob("*.json"))
        assert len(json_files) == 1

        data = json.loads(json_files[0].read_text())
        assert data["label"] == "Test alarm"
        assert data["time"] == target.isoformat()

        output = capsys.readouterr().out
        assert "Test alarm" in output

    def test_set_alarm_cleans_up_on_systemd_failure(self, tmp_path):
        target = datetime.datetime.now() + datetime.timedelta(hours=1)

        mock_result = MagicMock()
        mock_result.returncode = 1
        mock_result.stderr = "unit failed"

        with patch.object(cal_mod, "ALARM_DIR", str(tmp_path)), \
             patch("subprocess.run", return_value=mock_result):
            with pytest.raises(SystemExit):
                cal_mod.set_alarm(target, label="Fail")

        # Metadata file should have been removed on failure
        assert len(list(tmp_path.glob("*.json"))) == 0


# ── generate_id ─────────────────────────────────────────────────────


class TestGenerateId:
    """Tests for generate_id."""

    def test_length(self):
        assert len(cal_mod.generate_id()) == 6

    def test_alphanumeric(self):
        for _ in range(20):
            assert cal_mod.generate_id().isalnum()

    def test_uniqueness(self):
        ids = {cal_mod.generate_id() for _ in range(100)}
        # With 36^6 possibilities, 100 samples should all be unique
        assert len(ids) == 100
