# check device / partition names
lsblk

# delete partition, add new one
fdisk /dev/partition
d, d, d, n, w

# create new partition
mkfs.fat -F 32 /dev/device


