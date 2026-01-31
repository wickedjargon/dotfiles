# System Configuration Files

This directory contains system-level configuration files that need to be deployed to various system directories.

## Files Included:

### `/etc/systemd/logind.conf.patch`
Configuration patch for systemd-logind to set laptop lid behavior to hibernate.

## Deployment

These files are automatically deployed when running `deploy.py` as root.

The deployment script:
- Copies all files to their appropriate system locations
- Sets proper permissions
- Handles `.patch` files by merging them into existing config files
- Creates backups of modified system files (`.bak` extension)

## Manual Deployment

If you need to manually deploy these files:

```bash
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
