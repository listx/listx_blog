---
title: Installing Arch Linux and Enabling System Encryption
tags: linux
---

The follow list of commands is what you need to execute in order to set up Arch Linux with system-level encryption.
The setup is 1 hard drive with 1 100MiB `/boot` partition, with the rest as a single LVM partition that will contain the `/` root, `/home`, and `swap` partitions.
This LVM partition will be encrypted so that when the system boots, it will require a password to unlock the `/`, `/home`, and `swap` partitions to boot up the system.
The `/boot` partition will remain unencrypted for the sake of simplicity.

If you are wondering why the `/home` partition is mounted on `/mnt/home` in some of the commands, this is because the Arch Linux live CD environment will load up a temporary Linux system onto your RAM, and we use the `/mnt` directory (again all files/folders are temporarily in your RAM) to mount the hard drive partitions.
You can always use a live CD anywhere to load up a barebone Linux system to your RAM and mount the hard drives on your computer for inspection, and in such a case you will again use the `/mnt` directory to mount your hard drives here.

If you want to look up the manpages for these commands, use `ALT+F2`, `ALT+F3`, etc. to log into and switch between independent TTY screens.

Be sure to wipe the drive with
```
dd if=/dev/urandom of=/dev/sdX
```
before doing anything.
This way, your new Linux install will blend into the random noise data resulting from the command above, making it virtually impossible to detect which portions of the drive contain actual data.
If the disk you want to wipe is a secondary device to an existing Linux installation, you can try using [`frandom`][frandom] to generate a faster stream of pseudorandom bytes.
If you get `frandom` working, you can just do
```
dd if=/dev/frandom of=/dev/sdX
```
and it will be roughly 10 to 50 times faster than using `/dev/urandom` --- which is a real time saver if your disk is larger than 16 GiB.

```
# Put in the Arch Linux live CD and run the following commands...

# Set up 2 partitions; the first one is the boot 100MB partition (be sure
# to toggle the BOOT flag on it), and the second will be the LVM partition.
# Be sure to select 8E for the LVM partition! You can use
# /dev/disk/by-uuid/XXXX for more fine-grained control; use "blkid -o list"
# to find out which drives are given which UUIDs.

cfdisk /dev/sda

# Now set up encryption on the LVM partition (the 2nd partition, so it's
# /dev/sda2). It is recommended to use aes-xts-plain64 if the partition is
# larger than 2 TB, I think.

cryptsetup -c aes-xts-plain -y -s 512 luksFormat /dev/sda2

# Decrypt the encrypted partition with LUKS. We need to decrypt it first
# before setting up LVM volumes and filesystems on it. This will create
# an entry under /dev/mapper/luks.

cryptsetup luksOpen /dev/sda2 luks

# Set up LVM. We call our volume group "vg0". To be honest I am not sure if
# the "--contiguous y" option for swap matters because we are encrypting it
# (doesn't block encryption encrypt data randomly on the disk?), but that's
# what I found online and that's what I'll use here. The "+100%FREE" option
# is a very handy option that tells LVM to fill up all remaining free space,
# in this case, for the "home" logical volume.
# Read more about LVM on your favorite online wiki.

pvcreate /dev/mapper/luks
vgcreate vg0 /dev/mapper/luks
lvcreate --size 40G vg0 --name root
lvcreate --size 2G --contiguous y vg0 --name swap
lvcreate -l +100%FREE vg0 --name home

# Initialize filesystems. Btrfs is still experimental, so I choose ext4
# here. The vg0-root, vg0-home, and vg0-swap volumes should
# appear under /dev/mapper as you create them with the lvcreate command.

mkfs.ext4 /dev/sda1 # the boot partition
mkfs.ext4 /dev/mapper/vg0-root
mkfs.ext4 /dev/mapper/vg0-home
mkswap /dev/mapper/vg0-swap

# Mount the logical volumes. Why? Because we need to install Arch Linux onto
# it! You probably don't need to mount the "home" volume but it couldn't
# hurt. Besides, it's nice to test that all volumes can be mounted OK anyway.
#
# NOTE: Mount order is important! YOU MUST FIRST MOUNT THE ROOT PARTITION into
# /mnt before creating more directories such as /mnt/boot, /mnt/home, etc. to
# mount the /boot, /home, and any other partitions.

mount /dev/mapper/vg0-root /mnt # /mnt is our system's "/" root   directory

mkdir /mnt/boot
mount /dev/sda1 /mnt/boot

mkdir /mnt/home
mount /dev/mapper/vg0-home /mnt/home

swapon /dev/mapper/vg0-swap

# Set up package download mirrors. The kernel mirror is generally a good one
# if you live in the US. Google "arch linux mirror status" to find the
# fastest mirrors.

vi /etc/pacman.d/mirrorlist

# Download and install the core minimum packages to get a working Linux
# terminal (console) environment. Ah, simplicity!

pacstrap -i /mnt base base-devel

# Generate and inspect the filesystem tables needed when the system first
# starts. The file should contain four "partitions" (I put them in quotes
# because remember, we are using only 2 real partitions; 1st one is /boot and
# the 2nd one is our LVM containing /, /home, and swap).

genfstab -U -p /mnt >> /mnt/etc/fstab
vi /mnt/etc/fstab

# Make the system pretend that /mnt is /. This is called "chrooting". We need
# to do this because some of the commands like locale-gen, systemctl, etc.
# read configuration files from /etc, for example, and right now our /etc is
# the /etc used by the Arch Linux live CD, not our newly-installed system's
# root directory, /mnt.

arch-chroot /mnt /bin/bash

# Choose and generate locale; programs supporting internationalization (aka
# "i18n") will use the language you pick to generate the proper text used in
# menus, error messages, etc. Just choose "en_US.UTF-8 UTF-8" if you live in
# the US.

vi /etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 > /etc/locale.conf

# Choose default font used by the virtual console (the terminal screen you
# will see when you first boot up your new Arch Linux system).

setfont Lat2-Terminus16
echo FONT=Lat2-Terminus16 > /etc/vconsole.conf

# Choose timezone. Here, we tell hwclock that we want the hardware clock on
# the motherboard to be set to UTC, which will then be interpreted as UTC
# time by the kernel and re-adjusted to the timezone you chose when you
# actually use the system. This is nice because UTC is the "master" time the
# world uses anyway, so it makes sense to tell your motherboard to use it.

ln -s /usr/share/zoneinfo/America/Los_Angeles /etc/localtime
hwclock --systohc --utc

# Choose your computer's name. Keep it simple, without spaces. I use "k0".

echo MYHOSTNAME > /etc/hostname

# Enable DHCPCD... aka "acquire dynamic ip address from your router/switch".  Be
# sure to choose the right device name (typically eth0 or eth1 (eth2, if you
# have 3 ethernet ports)); you can find out the correct device name with `ip
# link` (look for the device under the first entry, `lo`.). If you choose the
# wrong "eth" number, just go to /etc/systemd/system/multi-user.target.wants/
# and rename the symlink; e.g., rename
# /etc/systemd/system/multi-user.target.wants/dhcpcd@eth0.service to
# /etc/systemd/system/multi-user.target.wants/dhcpcd@eth1.service.

systemctl enable dhcpcd@DEVICENAME.service

# Disable unused package repos. You will want to disable the [testing] repo
# unless you want to test unstable packages and engage in the package
# debugging/development process. For Haskellers, be sure to add the
# [haskell-core] repo (see
# https://wiki.archlinux.org/index.php/Haskell_Package_Guidelines).

vi /etc/pacman.conf

# Set up the root user ("administrator" for you Windows people) password.

passwd

# Install zsh, because you will like it better any other shell out there.

pacman -S zsh

# Add your regular user, and set up your password. This is your normal
# username. We use the "-s /bin/zsh" option to tell useradd that we want to
# log in with zsh.
#
# If you want, you can add a new group that will only have your username as its
# only "member", with `groupadd -g 1000 MYGROUPNAME`, and then you can use this
# group name for the `-g` flag's argument in the `useradd` command below. I like
# to have 1-character usernames for my machines to keep things simple and short,
# as I will be the exclusive user of the system anyway, so I also use the same
# character for my group as well, so that, in particular, the `ls` command's
# full listing of my files looks very succinct, as I like to live in the
# terminal and use `ls` daily.

useradd -m -g users -G wheel,storage,power -s /bin/zsh MYUSERNAME
passwd MYUSERNAME

# VERY IMPORTANT: Add in the filesystem type that /root partition (LVM) is
# using (for this tutorial, it is "ext4") into the MODULES variable and also
# add 'encrypt' and 'lvm2' into HOOKS.

vi /etc/mkinitcpio.conf

# Re-generate the linux image to take into account the LVM and encrypt
# flags we added into /etc/mkinitcpio.conf. I say "re-generate" because this
# is our second time doing this (the first time was when we installed the
# linux package with the pacstrap command above).

mkinitcpio -p linux

# Install boot loader. I like SYSLINUX because of the saner/easier-looking
# boot configuration file, compared to GRUB 2. Be sure to add in
#     "APPEND cryptdevice=/dev/disk/by-uuid/xxxxxxxxxx:luks root=/dev/mapper/vg0-root resume=/dev/mapper/vg0-swap ro"
# under the "LABEL arch" entries, so that SYSLINUX tells the kernel to look for
# the encrypted LVM partition. Otherwise, your system won't boot! Don't forget
# to actually look up the UUID of your LVM partition (/dev/sda2 in this
# tutorial). To easily add in the long UUID of the disk, just concatenate it
# into the syslinux.cfg file with `blkid -o list >>
# /boot/syslinux/syslinux.cfg`, and edit as needed.
#
# You don't need to tell the kernel about your vg0-home volume because that will
# get mounted by /etc/fstab when it is read later in the boot process.

pacman -S syslinux
syslinux-install_update -i -a -m
vi /boot/syslinux/syslinux.cfg

# Exit chroot environment.

exit

# Unmount volumes, and reboot. Remove your Arch Linux live CD when your
# computer powers on again.

umount /mnt/boot
umount /mnt/home
umount /mnt
swapoff
reboot
```
[frandom]: http://www.billauer.co.il/frandom.html
