"""Tests for the power CLI (battery status + caffeine)."""

import os
from pathlib import Path

from helpers import get_bin_path, import_script

power = import_script(get_bin_path("power"))


def _make_battery(root, name="BAT0", **files):
    bat = root / name
    bat.mkdir(parents=True)
    (bat / "type").write_text("Battery\n")
    for fname, value in files.items():
        (bat / fname).write_text(f"{value}\n")
    return bat


# ── Battery parsing ─────────────────────────────────────────────────


class TestGetBatteries:
    def test_energy_battery_discharging(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT", Path(tmp_path))
        _make_battery(
            tmp_path,
            capacity=85, status="Discharging",
            energy_now=40_000_000, power_now=12_000_000,
            energy_full=44_000_000, energy_full_design=48_000_000,
        )
        (b,) = power.get_batteries()
        assert b["capacity"] == 85
        assert b["status"] == "Discharging"
        assert b["remaining_min"] == 200  # 40Wh / 12W = 3h20m
        assert b["health"] == 92
        assert b["unit"] == "Wh"

    def test_charge_battery_charging(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT", Path(tmp_path))
        _make_battery(
            tmp_path,
            capacity=50, status="Charging",
            charge_now=2_000_000, current_now=1_000_000,
            charge_full=4_000_000, charge_full_design=4_000_000,
        )
        (b,) = power.get_batteries()
        # (4Ah - 2Ah) / 1A = 2h until full
        assert b["remaining_min"] == 120
        assert b["health"] == 100
        assert b["unit"] == "Ah"

    def test_zero_rate_gives_no_estimate(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT", Path(tmp_path))
        _make_battery(
            tmp_path,
            capacity=100, status="Full",
            energy_now=44_000_000, power_now=0,
        )
        (b,) = power.get_batteries()
        assert b["remaining_min"] is None

    def test_non_battery_supplies_skipped(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT", Path(tmp_path))
        ac = tmp_path / "AC"
        ac.mkdir()
        (ac / "type").write_text("Mains\n")
        (ac / "online").write_text("1\n")
        assert power.get_batteries() == []
        assert power.ac_online() is True

    def test_missing_root_is_empty(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT",
                            Path(tmp_path) / "nope")
        assert power.get_batteries() == []
        assert power.ac_online() is None


# ── format_duration ─────────────────────────────────────────────────


class TestFormatDuration:
    def test_hours_and_minutes(self):
        assert power.format_duration(200) == "3h 20m"

    def test_minutes_only(self):
        assert power.format_duration(45) == "45m"

    def test_zero_padded_minutes_with_hours(self):
        assert power.format_duration(61) == "1h 01m"


# ── Caffeine pidfile ────────────────────────────────────────────────


class TestCaffeinePid:
    def test_no_pidfile_means_off(self, tmp_path, monkeypatch):
        monkeypatch.setattr(power, "PIDFILE", tmp_path / "caffeine.pid")
        assert power.caffeine_pid() is None

    def test_live_pid_is_reported(self, tmp_path, monkeypatch):
        pidfile = tmp_path / "caffeine.pid"
        pidfile.write_text(str(os.getpid()))
        monkeypatch.setattr(power, "PIDFILE", pidfile)
        assert power.caffeine_pid() == os.getpid()

    def test_stale_pid_is_cleaned_up(self, tmp_path, monkeypatch):
        pidfile = tmp_path / "caffeine.pid"
        # Find a pid that certainly isn't running
        pid = 99999
        while True:
            try:
                os.kill(pid, 0)
                pid -= 1
            except ProcessLookupError:
                break
            except PermissionError:
                pid -= 1
        pidfile.write_text(str(pid))
        monkeypatch.setattr(power, "PIDFILE", pidfile)
        assert power.caffeine_pid() is None
        assert not pidfile.exists()


# ── list output ─────────────────────────────────────────────────────


class TestCmdList:
    def test_tab_separated_fields(self, tmp_path, monkeypatch, capsys):
        monkeypatch.setattr(power, "POWER_SUPPLY_ROOT", Path(tmp_path))
        _make_battery(
            tmp_path,
            capacity=85, status="Discharging",
            energy_now=40_000_000, power_now=12_000_000,
            energy_full=44_000_000, energy_full_design=48_000_000,
        )
        power.cmd_list()
        out = capsys.readouterr().out.strip()
        assert out == "BAT0\t85\tDischarging\t200\t92"
