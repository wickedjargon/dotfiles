# create bootable USB using dd
run `dd` command to create bootable USB:
```
dd bs=4M if=path/to/archlinux-version-x86_64.iso of=/dev/sdx conv=fsync oflag=direct status=progress
```

supporting articles: https://wiki.archlinux.org/title/USB_flash_installation_medium

# set system clock
```
timedatectl set-ntp true
```

# partition setup
run fdisk on the sdx of your internal drive where arch will be installed:
```
fdisk /dev/sdx
```

delete all partitions by hitting `d` until all are deleted:
```
d
```

# create new partions

hit `n` to create a new partition:
```
n
```

select 
```
n > p > [default part. number] > [default first sector] > [space for partition]
```
for each partition

## (1) boot partition
```
+500M
```

## (2) SWAP partition
I choose
```
+12G
```
for 4GB even with no hibernation

## SWAP recommendations

Here is a table some find useful when determining an appropriate swap:

### table of recommended swap space

| Amount of RAM Installed 	| Min SWAP space (no hib) 	| Min SWAP space (w/ hib)                  	|
|-------------------------	|-------------------------	|------------------------------------------	|
| ≤ 2GB                   	| 2X RAM                  	| 3X RAM                                   	|
| 2GB – 8GB               	| 1X RAM                  	| 2X RAM                                   	|
| 8GB – 64GB              	| 4G to 0.5X RAM          	| 1.5X RAM                                 	|
| >64GB                   	| Minimum 4GB             	| Hibernation not recommended (disable it) 	|

I have 4 GB of RAM and have a swap size of 12 GB

## (3) root partition:
I choose
```
+70G
```
for a 128 GB drive

## (4) home partition:
choose default values to occupy the rest of the disk


# format the partitions

I only have legacy BIOS

Do:
```
mkfs.ext4 /dev/root_partition
```

for all partitions except swap.

# make swap

make swap
```
mkswap /dev/swap_partition
```

turn swap on:
```
swapon /dev/swap_partition
```

# Mount the file systems

make the required directories
```
mkdir /mnt/
mkdir /mnt/boot/
mkdir /mont/home/
```

mount the partitions to the directories
```
mount /dev/root /mnt
mount /dev/boot /mnt/boot
mount /dev/home /mnt/home
```

# install arch via pacstrap
```
pacstrap /mnt base base-devel linux linux-firmware vim
```

# fstab
In order for partitions to be synced after reboot, run:
```
genfstab -U /mnt >> /mnt/etc/fstab
```

# chroot into system
login to system root:
```
arch-chroot /mnt
```

# time zone

set time zone
```
ln -sf /usr/share/zoneinfo/Region/City /etc/localtime
```

update hardware clock
```
hwclock --systohc
```

# localization
use:
```
vim /etc/locale.gen
```

and uncomment
```
en_US.UTF-8 UTF-8
```

make a new file
```
vim /etc/locale.conf
```

and put in it:
```
LANG=en_US.UTF-8
```

# network
```
vim /etc/hostname
```

and put in it your desired hostname
```
myhostname
```

install network manager
```
pacman -S networkmanager
```

tell systemd to start network manager
```
systemctl enable NetworkManager
```

# root password
set a root password with
```
passwd
```


# boot loader: grub

install grub from pacman
```
pacman -S grub
```

install grub onto partition
```
grub-install --target=i386-pc /dev/sda
```

make config file
```
grub-mkconfig -o /boot/grub/grub.cfg
```

# reboot

exit charoot with
```
exit
```

unmount with
```
umount -R /mnt
```

reboot with
```
reboot
```
