"""Tests for the vm CLI (filename convention and qemu argv)."""

import json
import os

from helpers import get_bin_path, import_script

vm = import_script(get_bin_path("vm"))


class TestParseVmFilename:
    def test_standard_name(self):
        assert vm.parse_vm_filename("debian.x86_64.1G.qcow2") == {
            "name": "debian", "arch": "x86_64", "ram": "1G"}

    def test_aarch64_and_megabytes(self):
        assert vm.parse_vm_filename("pi.aarch64.512M.qcow2") == {
            "name": "pi", "arch": "aarch64", "ram": "512M"}

    def test_dotted_name(self):
        parsed = vm.parse_vm_filename("test.box.x86_64.2G.qcow2")
        assert parsed["name"] == "test.box"

    def test_non_vm_files_rejected(self):
        assert vm.parse_vm_filename("notes.txt") is None
        assert vm.parse_vm_filename("debian.qcow2") is None


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
