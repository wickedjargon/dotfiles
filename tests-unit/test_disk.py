"""Tests for the disk CLI (mount parsing and formatting)."""

from helpers import get_bin_path, import_script

disk = import_script(get_bin_path("disk"))


class TestParseMounts:
    def test_filters_virtual_filesystems(self):
        text = ("proc /proc proc rw 0 0\n"
                "/dev/nvme0n1p2 / ext4 rw 0 0\n"
                "tmpfs /tmp tmpfs rw 0 0\n"
                "/dev/nvme0n1p1 /boot/efi vfat rw 0 0\n")
        assert disk.parse_mounts(text) == [
            ("/dev/nvme0n1p2", "/"),
            ("/dev/nvme0n1p1", "/boot/efi"),
        ]

    def test_dedups_bind_mounts_keeping_shortest(self):
        text = ("/dev/sda1 /home ext4 rw 0 0\n"
                "/dev/sda1 /home/user/jail ext4 rw 0 0\n")
        assert disk.parse_mounts(text) == [("/dev/sda1", "/home")]

    def test_unescapes_spaces(self):
        text = "/dev/sdb1 /mnt/my\\040drive ext4 rw 0 0\n"
        assert disk.parse_mounts(text) == [("/dev/sdb1", "/mnt/my drive")]

    def test_empty(self):
        assert disk.parse_mounts("") == []


class TestUsageBar:
    def test_full_and_empty_extremes(self):
        assert "█" * 12 in disk.usage_bar(100)
        assert "░" * 12 in disk.usage_bar(0)

    def test_color_thresholds(self):
        assert disk.GREEN in disk.usage_bar(50)
        assert disk.YELLOW in disk.usage_bar(75)
        assert disk.RED in disk.usage_bar(95)


class TestHuman:
    def test_bytes(self):
        assert disk.human(512) == "512 B"

    def test_gigabytes(self):
        assert disk.human(50 * 1024**3) == "50.0 GB"
