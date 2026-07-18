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
        assert opts == {"name": "test", "ram": "512M", "disk": "4G",
                        "arch": "aarch64", "os": None, "cpus": None}

    def test_bad_arch_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["debian", "--arch", "sparc"])

    def test_missing_value_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["debian", "--name"])

    def test_windows_alias_maps_to_win11(self):
        _, opts = vm.parse_create_opts(["x.iso", "--os", "windows"])
        assert opts["os"] == "win11"

    def test_unknown_os_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["x.iso", "--os", "beos"])

    def test_non_numeric_cpus_dies(self):
        with pytest.raises(SystemExit):
            vm.parse_create_opts(["x.iso", "--cpus", "many"])


class TestDetectOs:
    def test_win11_release_iso(self):
        assert vm.detect_os("Win11_24H2_English_x64.iso") == "win11"

    def test_win10_release_iso(self):
        assert vm.detect_os("Win10_22H2_English_x64.iso") == "win10"

    def test_techbench_style_name(self):
        assert vm.detect_os(
            "en-us_windows_11_consumer_editions_x64.iso") == "win11"

    def test_unversioned_windows_assumes_win11(self):
        assert vm.detect_os("windows_custom.iso") == "win11"

    def test_linux_isos(self):
        assert vm.detect_os("debian-13.0.0-amd64-netinst.iso") == "linux"
        assert vm.detect_os("archlinux-x86_64.iso") == "linux"


class TestDefaultName:
    def test_windows_uses_os_name(self):
        assert vm.default_name("/tmp/Win11_24H2.iso", "win11") == "win11"

    def test_linux_uses_leading_word(self):
        assert vm.default_name(
            "/tmp/debian-13.0.0-amd64-netinst.iso", "linux") == "debian"


class TestReadConf:
    def test_missing_sidecar_is_empty(self, tmp_path):
        assert vm.read_conf(str(tmp_path / "a.qcow2")) == {}

    def test_sidecar_read(self, tmp_path):
        img = tmp_path / "w.x86_64.4G.qcow2"
        (tmp_path / "w.x86_64.4G.qcow2.json").write_text(
            json.dumps({"os": "win11", "cpus": 8}))
        assert vm.read_conf(str(img)) == {"os": "win11", "cpus": 8}

    def test_unknown_os_falls_back_to_linux(self, tmp_path):
        img = tmp_path / "w.x86_64.4G.qcow2"
        (tmp_path / "w.x86_64.4G.qcow2.json").write_text(
            json.dumps({"os": "beos"}))
        assert vm.read_conf(str(img))["os"] == "linux"


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

    def test_linux_uses_virtio_nic(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, "/run/mon")
        assert "virtio-net-pci,netdev=net0" in cmd

    def _win(self):
        return {"name": "win11", "arch": "x86_64", "ram": "4G",
                "path": "/vms/win11.x86_64.4G.qcow2",
                "os": "win11", "cpus": 4}

    def test_windows_gets_uefi_tpm_and_inbox_devices(self):
        cmd = vm.build_qemu_command(
            self._win(), 2222, "/run/mon",
            uefi=("/fw/CODE.ms.fd", "/vms/vars.fd", True),
            tpm_sock="/run/tpm.sock")
        assert cmd[cmd.index("-machine") + 1] == "q35,smm=on"
        assert any("if=pflash" in a and "CODE.ms.fd" in a for a in cmd)
        assert any("if=pflash" in a and "vars.fd" in a for a in cmd)
        assert "e1000e,netdev=net0" in cmd
        assert "virtio-net-pci,netdev=net0" not in cmd
        assert "usb-tablet" in cmd
        assert "tpm-tis,tpmdev=tpm0" in cmd
        assert any("path=/run/tpm.sock" in a for a in cmd)

    def test_smp_from_conf(self):
        cmd = vm.build_qemu_command(self._win(), 2222, "/run/mon",
                                    uefi=("/c", "/v", False))
        assert cmd[cmd.index("-smp") + 1] == "4"
        assert cmd[cmd.index("-machine") + 1] == "q35"

    def test_installer_iso_bios_forces_cd_boot(self):
        cmd = vm.build_qemu_command(self._vm(), 2222, None,
                                    iso="/tmp/d.iso")
        assert cmd[cmd.index("-cdrom") + 1] == "/tmp/d.iso"
        assert "-boot" in cmd
        assert "-monitor" not in cmd

    def test_installer_iso_uefi_skips_boot_flag(self):
        cmd = vm.build_qemu_command(self._win(), 2222, None,
                                    iso="/tmp/w.iso",
                                    uefi=("/c", "/v", True))
        assert "-cdrom" in cmd
        assert "-boot" not in cmd


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
