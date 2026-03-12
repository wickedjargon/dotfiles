# How to Set Up Mullvad VPN (dmenu-vpn)

The `dmenu-vpn` script manages WireGuard connections to Mullvad servers and allows you to change locations dynamically. Because it uses WireGuard directly instead of the Mullvad app, there is a specific setup process required on a fresh install.

When you first deploy your dotfiles, `dmenu-vpn` will fail until these steps are completed.

## 1. Generate WireGuard Configuration

First, you need a valid WireGuard private key and an assigned internal IP address from Mullvad.

1. Log in to your Mullvad account on their website.
2. Go to **WireGuard Configuration**.
3. Generate a new key (or use an existing one).
4. Download any WireGuard configuration file (e.g., `mlv-se-got-wg-001.conf`).

## 2. Set Up the Identity File

`dmenu-vpn` needs to know your PrivateKey and your assigned Address. It reads these from a local identity file.

1. Open the `.conf` file you downloaded in a text editor.
2. Create the identity file at `~/.config/mullvad/identity.conf`:

```ini
# ~/.config/mullvad/identity.conf
PrivateKey = <Copy PrivateKey from downloaded config>
Address = <Copy Address from downloaded config>
```

> **Note:** Do not include the `[Interface]` or `[Peer]` headers in this file. It is just a storage location for these two values.

## 3. Configure Sudoers Permissions

`dmenu-vpn` requires passwordless `sudo` access to perform three specific actions:
1. Bring the WireGuard interface up (`wg-quick up`)
2. Bring the WireGuard interface down (`wg-quick down`)
3. Copy the dynamically generated configuration file into `/etc/wireguard/` (`cp`)

Because `dmenu` runs without a terminal, it cannot prompt you for a password.

### Applying the Sudoers Rules

Your dotfiles include a pre-configured sudoers drop-in file that explicitly grants these permissions. You must copy it to the system's sudoers directory and set the correct permissions.

Run the following commands in your terminal (you will be prompted for your password once):

```bash
sudo cp ~/.config/mullvad/sudoers-mullvad /etc/sudoers.d/mullvad-vpn
sudo chmod 0440 /etc/sudoers.d/mullvad-vpn
```

*(If you are running these commands from within a distrobox container, prefix the commands with `distrobox-host-exec`)*:

```bash
distrobox-host-exec sudo cp ~/.config/mullvad/sudoers-mullvad /etc/sudoers.d/mullvad-vpn
distrobox-host-exec sudo chmod 0440 /etc/sudoers.d/mullvad-vpn
```

## 4. Test the Connection

Once the identity file is created and the sudoers permissions are applied, the setup is complete.

1. Trigger your `dmenu-vpn` keyboard shortcut.
2. Select **Change Location**.
3. Choose a country and city.
4. The script will automatically generate the corresponding WireGuard configuration, place it in `/etc/wireguard/mullvad.conf`, and connect.

You can verify your connection and location by running `my-location` in the terminal.
