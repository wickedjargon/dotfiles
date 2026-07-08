"""Tests for the vm CLI (filename convention, qemu argv, ISO catalog)."""

import json
import os

import pytest

from helpers import get_bin_path, import_script

vm = import_script(get_bin_path("vm"))


class TestParseVmFilename:
    def test_standard_name(self):
        assert vm.parse_vm_filename("debian.x86_64.1G.qcow2") == {
            "name": "debian", "arch": "x86_64", "ram": "1G"}

    def test_gb_suffix_normalized(self):
        # Older images use '2GB'; qemu -m only accepts '2G'.
        assert vm.parse_vm_filename("debian.x86_64.2GB.qcow2") == {
            "name": "debian", "arch": "x86_64", "ram": "2G"}

    def test_aarch64_and_megabytes(self):
        assert vm.parse_vm_filename("pi.aarch64.512M.qcow2") == {
            "name": "pi", "arch": "aarch64", "ram": "512M"}

    def test_mb_suffix_normalized(self):
        assert vm.parse_vm_filename("pi.aarch64.512MB.qcow2")["ram"] == \
            "512M"

    def test_dotted_name(self):
        parsed = vm.parse_vm_filename("test.box.x86_64.2G.qcow2")
        assert parsed["name"] == "test.box"

    def test_non_vm_files_rejected(self):
        assert vm.parse_vm_filename("notes.txt") is None
        assert vm.parse_vm_filename("debian.qcow2") is None


class TestLatestMatch:
    def test_picks_highest_version(self):
        html = ('href="debian-12.9.0-amd64-netinst.iso"\n'
                'href="debian-12.11.0-amd64-netinst.iso"\n'
                'href="debian-12.10.0-amd64-netinst.iso"\n')
        assert vm.latest_match(
            html, r"debian-(\d+\.\d+\.\d+)-amd64-netinst\.iso") == \
            "debian-12.11.0-amd64-netinst.iso"

    def test_no_match_is_none(self):
        assert vm.latest_match("nothing here", r"x-(\d+\.\d+)") is None


class TestChecksumFromText:
    def test_sums_file(self):
        text = ("abc123  debian-12.11.0-amd64-netinst.iso\n"
                "def456  debian-12.11.0-amd64-DVD-1.iso\n")
        assert vm.checksum_from_text(
            text, "debian-12.11.0-amd64-netinst.iso") == "abc123"

    def test_binary_marker_stripped(self):
        text = "abc123 *archlinux-x86_64.iso\n"
        assert vm.checksum_from_text(text, "archlinux-x86_64.iso") == \
            "abc123"

    def test_bare_digest(self):
        digest = "a" * 64
        assert vm.checksum_from_text(digest + "\n", "any.iso") == digest

    def test_missing_is_none(self):
        assert vm.checksum_from_text("abc  other.iso", "mine.iso") is None


class TestParseCreateOpts:
    def test_defaults(self):
        positional, opts = vm.parse_create_opts(["debian"])
        assert positional == ["debian"]
        assert opts["arch"] == "x86_64"
        assert opts["name"] is None

    def test_all_options(self):
        positional, opts = vm.parse_create_opts(
            ["alpine", "--name", "test", "--ram", "512M",
             "--disk", "4G", "--arch", "aarch64"])
        assert positional == ["alpine"]
        assert opts == {"name": "test", "ram": "512M",
                        "disk": "4G", "arch": "aarch64"}

    def test_bad_arch_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["debian", "--arch", "sparc"])

    def test_missing_value_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["debian", "--name"])


class TestBuildCreateCommand:
    def test_passes_options_through(self):
        cmd = vm.build_create_command(
            "/tmp/d.iso",
            {"name": "test", "ram": "2G", "disk": "20G",
             "arch": "x86_64"})
        assert "/tmp/d.iso" in cmd
        assert cmd[cmd.index("--name") + 1] == "test"
        assert cmd[cmd.index("--ram") + 1] == "2G"
        assert cmd[cmd.index("--disk-size") + 1] == "20G"

    def test_omits_unset_options(self):
        cmd = vm.build_create_command(
            "/tmp/d.iso",
            {"name": None, "ram": None, "disk": None, "arch": "x86_64"})
        assert "--name" not in cmd
        assert "--ram" not in cmd


class TestBuildQemuCommand:
    def _vm(self, arch="x86_64"):
        return {"name": "debian", "arch": arch, "ram": "2G",
                "path": "/vms/debian.x86_64.2G.qcow2"}

    def test_x86_ssh_forward_and_ram(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, "/run/mon")
        assert cmd[0] == "qemu-system-x86_64"
        assert "2G" in cmd
        assert any("hostfwd=tcp::2222-:22" in a for a in cmd)

    def test_headless_uses_display_none(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, "/run/mon",
                                    headless=True)
        assert "none" in cmd[cmd.index("-display") + 1]

    def test_gui_uses_gtk(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, "/run/mon")
        assert "gtk" in cmd[cmd.index("-display") + 1]

    def test_monitor_socket_configured(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, "/run/mon")
        assert any("unix:/run/mon" in a for a in cmd)

    def test_aarch64_uses_efi_and_virt_machine(self):
        cmd = vm.build_qemu_command(self._vm("aarch64"), 2223, "/run/mon")
        assert cmd[0] == "qemu-system-aarch64"
        assert "virt" in cmd
        assert any("QEMU_EFI.fd" in a for a in cmd)


class TestReadState:
    def test_live_state(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        state = {"pid": os.getpid(), "port": 2222, "monitor": "/m"}
        (tmp_path / "vm-debian.json").write_text(json.dumps(state))
        assert vm.read_state("debian") == state

    def test_stale_pid_cleans_up(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        pid = 99999
        while True:
            try:
                os.kill(pid, 0)
                pid -= 1
            except ProcessLookupError:
                break
            except PermissionError:
                pid -= 1
        f = tmp_path / "vm-debian.json"
        f.write_text(json.dumps({"pid": pid, "port": 2222}))
        assert vm.read_state("debian") is None
        assert not f.exists()

    def test_missing_state_is_none(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        assert vm.read_state("debian") is None


class TestVmFromPath:
    def test_parses_path_outside_vm_dir(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        monkeypatch.setattr(vm, "qemu_pid_for", lambda p: None)
        img = tmp_path / "debian.x86_64.2GB.qcow2"
        img.touch()
        result = vm.vm_from_path(str(img))
        assert result["name"] == "debian"
        assert result["arch"] == "x86_64"
        assert result["ram"] == "2G"
        assert result["path"] == str(img)
        assert result["port"] == 2222

    def test_unconventional_filename_dies(self, tmp_path):
        with pytest.raises(SystemExit):
            vm.vm_from_path(str(tmp_path / "image.qcow2"))

    def test_missing_file_dies(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        with pytest.raises(SystemExit):
            vm.vm_from_path(str(tmp_path / "gone.x86_64.1G.qcow2"))


class TestResolveVmByPath:
    def test_path_bypasses_vm_dir(self, tmp_path, monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        monkeypatch.setattr(vm, "qemu_pid_for", lambda p: None)
        img = tmp_path / "debian.x86_64.2GB.qcow2"
        img.touch()
        result = vm.resolve_vm(str(img), want_running=False)
        assert result["name"] == "debian"

    def test_bare_qcow2_filename_resolves_in_cwd(self, tmp_path,
                                                 monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        monkeypatch.setattr(vm, "qemu_pid_for", lambda p: None)
        monkeypatch.chdir(tmp_path)
        (tmp_path / "debian.x86_64.2GB.qcow2").touch()
        result = vm.resolve_vm("debian.x86_64.2GB.qcow2",
                               want_running=False)
        assert result["path"] == str(tmp_path / "debian.x86_64.2GB.qcow2")

    def test_already_running_path_dies_for_start(self, tmp_path,
                                                 monkeypatch):
        monkeypatch.setattr(vm, "RUNTIME", str(tmp_path))
        monkeypatch.setattr(vm, "qemu_pid_for", lambda p: 12345)
        img = tmp_path / "debian.x86_64.2GB.qcow2"
        img.touch()
        with pytest.raises(SystemExit):
            vm.resolve_vm(str(img), want_running=False)
