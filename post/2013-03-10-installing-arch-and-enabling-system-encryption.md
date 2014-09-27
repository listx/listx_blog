---
title: Installing Arch Linux and Enabling System Encryption
tags: linux, arch
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
If the disk you want to wipe is a secondary device to an existing Linux installation, you can try using [`floop`][floop] to generate a faster stream of pseudorandom bytes.
With `floop`, you can just do
```
floop --threads 4 --thread-buf 0x200000 --count 0 | dd of=/dev/sdX
```
and it will be significantly faster than using `/dev/urandom`.

- i arch-setup.sh

[floop]: /code.html#floop
