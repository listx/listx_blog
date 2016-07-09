---
title: "Brother MFC-9840CDW: Setup on Arch Linux"
tags: arch, linux, hardware, printer
---

After more than a year of struggling with my Brother printer, I finally got it to print flawlessly.
I will kindly refrain from getting into the drama, and just write a short tutorial to set it up with CUPS in Arch Linux.

First, install the `cups` package, as well as the Brother driver packages in the AUR:

- `brother-cups-wrapper-ac`
- `brother-cups-wrapper-common`
- `brother-lpr-drivers-ac`
- `brother-lpr-drivers-common`

Then, enable the `cups` daemon with `systemd`, and then start it:

```{.numberLines}
sudo systemctl enable cups.service
sudo systemctl start cups.service
```

Now go into your favorite web browser, and go to `http://localhost:631`, to find the CUPS web admin interface.
From here, just add a new printer using the web form buttons.
When adding the printer, use the LPR/LPD protocol, as follows:

```{.numberLines}
lpd://IP_ADDRESS_OF_PRINTER/BINARY_P1
```

and use the appropriate IP address that your printer has on the network.
For the driver, select the CUPS driver for the MFC-9840CDW model (the various Brother drivers will only appear after installing the Brother driver packages from the AUR).

Done!
