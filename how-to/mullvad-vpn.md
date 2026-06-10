# How to Set Up Mullvad VPN (vpn)

The `vpn` script manages WireGuard connections to Mullvad servers and allows you to change locations dynamically. Because it uses WireGuard directly instead of the Mullvad app, there is a specific setup process required on a fresh install.

Connections are managed through **NetworkManager** (`nmcli`), so no `sudo` or sudoers configuration is needed — importing and activating the tunnel goes through polkit for your active desktop session. The script never writes to `/etc/wireguard` and never calls `sudo`.

When you first deploy your dotfiles, `vpn` will fail until the identity file (step 2) is created.

## 1. Generate WireGuard Configuration

First, you need a valid WireGuard private key and an assigned internal IP address from Mullvad.

1. Log in to your Mullvad account on their website.
2. Go to **WireGuard Configuration**.
3. Generate a new key (or use an existing one).
4. Download any WireGuard configuration file (e.g., `mlv-se-got-wg-001.conf`).

## 2. Set Up the Identity File

`vpn` needs to know your PrivateKey and your assigned Address. It reads these from a local identity file.

1. Open the `.conf` file you downloaded in a text editor.
2. Create the identity file at `~/.config/mullvad/identity.conf`:

```ini
# ~/.config/mullvad/identity.conf
PrivateKey = <Copy PrivateKey from downloaded config>
Address = <Copy Address from downloaded config>
```

> **Note:** Do not include the `[Interface]` or `[Peer]` headers in this file. It is just a storage location for these two values.

## 3. Test the Connection

Once the identity file is created, the setup is complete — there is no sudoers step.

1. Run `vpn connect` to connect to a random relay.
2. Or run `vpn location` to see available locations, then `vpn location sweden` to connect to Sweden.
3. The script generates the corresponding WireGuard configuration, imports it into NetworkManager as a connection named `mullvad`, and activates it.

You can verify your connection and location by running `my-location` in the terminal.

## Notes

- The connection appears in NetworkManager as `mullvad` (`nmcli connection show`), and the live interface is `mullvad` (`/sys/class/net/mullvad`). The polybar VPN indicator keys off that interface.
- To remove the VPN entirely: `vpn disconnect` then `nmcli connection delete mullvad`.

### Migrating from the old sudoers-based setup

Earlier versions used `wg-quick` + `/etc/wireguard` and required two sudoers drop-ins. If you set those up previously, you can remove them now (they are no longer used):

```bash
sudo rm -f /etc/sudoers.d/mullvad-vpn /etc/sudoers.d/wireguard
sudo rm -f /etc/wireguard/mullvad.conf
```
