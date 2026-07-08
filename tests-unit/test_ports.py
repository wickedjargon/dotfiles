"""Tests for the ports CLI (ss output parsing)."""

from helpers import get_bin_path, import_script

ports = import_script(get_bin_path("ports"))


class TestParseSsLine:
    def test_tcp_with_process(self):
        line = ('tcp   LISTEN 0      4096       127.0.0.1:3000  0.0.0.0:*  '
                'users:(("node",pid=1234,fd=23))')
        e = ports.parse_ss_line(line)
        assert e["proto"] == "tcp"
        assert e["addr"] == "127.0.0.1"
        assert e["port"] == 3000
        assert e["procs"] == [("node", 1234)]

    def test_udp_without_process(self):
        line = "udp   UNCONN 0      0          0.0.0.0:68      0.0.0.0:*"
        e = ports.parse_ss_line(line)
        assert e["proto"] == "udp"
        assert e["port"] == 68
        assert e["procs"] == []

    def test_ipv6_address(self):
        line = ('tcp   LISTEN 0      511        [::]:8080  [::]:*  '
                'users:(("python3",pid=99,fd=3))')
        e = ports.parse_ss_line(line)
        assert e["addr"] == "[::]"
        assert e["port"] == 8080

    def test_multiple_owners(self):
        line = ('tcp   LISTEN 0      511        0.0.0.0:80  0.0.0.0:*  '
                'users:(("nginx",pid=10,fd=6),("nginx",pid=11,fd=6))')
        e = ports.parse_ss_line(line)
        assert e["procs"] == [("nginx", 10), ("nginx", 11)]

    def test_garbage_line_is_none(self):
        assert ports.parse_ss_line("") is None
        assert ports.parse_ss_line("some random text") is None


class TestPidsOnPort:
    def test_dedup_across_protocols(self, monkeypatch):
        entries = [
            {"proto": "tcp", "addr": "0.0.0.0", "port": 53,
             "procs": [("dnsmasq", 42)]},
            {"proto": "udp", "addr": "0.0.0.0", "port": 53,
             "procs": [("dnsmasq", 42)]},
            {"proto": "tcp", "addr": "127.0.0.1", "port": 631,
             "procs": [("cupsd", 7)]},
        ]
        monkeypatch.setattr(ports, "get_listening", lambda: entries)
        assert ports.pids_on_port(53) == [42]
        assert ports.pids_on_port(631) == [7]
        assert ports.pids_on_port(9999) == []
