# System Configuration Files

This directory contains system-level configuration files that need to be deployed to various system directories.

## Files Included:

### `/etc/acpi/power-adapter.sh`
ACPI event handler that refreshes the dwmblocks battery status indicator when the power adapter is plugged/unplugged.

### `/etc/NetworkManager/dispatcher.d/99-dwmblocks-network`
NetworkManager dispatcher script that refreshes the dwmblocks network status indicator when network state changes (connect, disconnect, DHCP changes, etc).

### `/usr/local/bin/refresh-dwmblocks`
Utility script to manually refresh all dwmblocks status bar modules.

### `/usr/lib/systemd/system-sleep/dwmblocks-refresh`
Systemd sleep hook that automatically refreshes dwmblocks after the system wakes from sleep/hibernate.

### `/etc/systemd/logind.conf.patch`
Configuration patch for systemd-logind to set laptop lid behavior to hibernate.

## Deployment

These files are automatically deployed when running `deploy.py` as root.

The deployment script:
- Copies all files to their appropriate system locations
- Sets proper permissions (755 for scripts)
- Handles `.patch` files by merging them into existing config files
- Creates backups of modified system files (`.bak` extension)

## Manual Deployment

If you need to manually deploy these files:

```bash
sudo cp system-configs/etc/acpi/power-adapter.sh /etc/acpi/power-adapter.sh
sudo chmod 755 /etc/acpi/power-adapter.sh

sudo cp system-configs/etc/NetworkManager/dispatcher.d/99-dwmblocks-network /etc/NetworkManager/dispatcher.d/99-dwmblocks-network
sudo chmod 755 /etc/NetworkManager/dispatcher.d/99-dwmblocks-network

sudo cp system-configs/usr/local/bin/refresh-dwmblocks /usr/local/bin/refresh-dwmblocks
sudo chmod 755 /usr/local/bin/refresh-dwmblocks

sudo cp system-configs/usr/lib/systemd/system-sleep/dwmblocks-refresh /usr/lib/systemd/system-sleep/dwmblocks-refresh
sudo chmod 755 /usr/lib/systemd/system-sleep/dwmblocks-refresh

# For logind.conf, add the following line:
# HandleLidSwitch=hibernate
sudo nano /etc/systemd/logind.conf
```

## Post-Deployment

After deploying system configurations, you may need to:

1. **Reload systemd-logind** to apply lid switch changes:
   ```bash
   sudo systemctl restart systemd-logind
   ```

2. **Restart NetworkManager** to activate the dispatcher script:
   ```bash
   sudo systemctl restart NetworkManager
   ```

3. **Test ACPI events** by plugging/unplugging power adapter
4. **Test network events** by connecting/disconnecting from WiFi
