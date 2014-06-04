---
title: Solving the "NVRM: API mismatch" Problem on Arch Linux
tags: arch, linux, nvidia
---

Sometimes when you do a system upgrade on Arch, Xorg will die on a reboot and you'll get this kernel error (available with `dmesg`):

```{.numberLines}
NVRM: API mismatch: the client has the version XXX.XX, but
NVRM: this kernel module has the version YYY.YY.  Please
NVRM: make sure that this kernel module and all NVIDIA driver
NVRM: components have the same version.
```

.
This is usually the case when you upgrade your `linux` package, followed by upgrading your `nvidia` package.
The problem is that the new kernel image references your older `nvidia` package (when it executed `mkinitcpio -p linux` before `nvidia` got upgraded).
There are two solutions:

  1) Simply remake your kernel image against your newly upgraded `nvidia` package.
  Run `mkinitcpio -p linux` again, and reboot.
  2) Downgrade to your previous set of `nvidia` drivers, located in `/var/cache/pacman/pkg/`.

Hope this helps someone out there. =)
