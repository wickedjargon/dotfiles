"""Tests for the net CLI (connectivity ladder parsing)."""

from helpers import get_bin_path, import_script

net = import_script(get_bin_path("net"))


class TestParseDefaultRoute:
    def test_typical_route(self):
        out = "default via 192.168.1.1 dev wlan0 proto dhcp metric 600\n"
        assert net.parse_default_route(out) == ("192.168.1.1", "wlan0")

    def test_no_route(self):
        assert net.parse_default_route("") is None

    def test_multiple_routes_takes_first(self):
        out = ("default via 10.0.0.1 dev eth0 metric 100\n"
               "default via 192.168.1.1 dev wlan0 metric 600\n")
        assert net.parse_default_route(out) == ("10.0.0.1", "eth0")


class TestParseRouteDev:
    def test_direct_route(self):
        out = "1.1.1.1 via 192.168.1.1 dev wlan0 src 192.168.1.42 uid 1000\n"
        assert net.parse_route_dev(out) == "wlan0"

    def test_vpn_route(self):
        out = "1.1.1.1 dev mullvad table 51820 src 10.66.1.2 uid 1000\n"
        assert net.parse_route_dev(out) == "mullvad"

    def test_no_dev(self):
        assert net.parse_route_dev("unreachable 1.1.1.1") is None
