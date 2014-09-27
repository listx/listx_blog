---
title: Quad Monitor Setup
tags: linux, hardware, nvidia, xmonad
---

I got my two QNIX QX2710 LED Evolution II SE Matte 27" 2560x1440 Samsung PLS WQHD monitors and incorporated them into my existing dual Acer X222W 22" 1680x1050 setup.
I put the two QNIXes in the middle, and put the Acers on the left and right edges.
All four are in "portrait" orientation, because I primarily code on my computer.
A single Gigabyte GeForce GTX 750 Ti OC 2GB GDDR5 2DVI/2HDMI card powers all four monitors; the QNIXes use the DVI ports, while the Acers use the HDMI ports via two HDMI-to-DVI(female) adapters.
I bought this card because of the simplicity of the four ports --- just DVI and HDMI.

As for the stands, I just bought another dual monitor stand and put it next to my existing dual monitor stand; each stand holds a QNIX and an Acer on each arm.
The fact that the stands are subject to differently weighted monitors (27" monitor on one arm and a 22" on another) does not really affect them negatively.
The desk in the image below is 140mm wide, in case you want to have a sense of how tight two dual-monitor stands would fit on your own desk.

<img src="../img/quad-monitor.jpg" alt="Quad monitor setup" title="Quad monitor setup" width="100%">

Using four monitors sounds difficult, but it's a breeze with Xinerama and Xmonad.
The hardest part was figuring out the correct `xorg.conf` configuration file.
It was difficult because the QNIX monitors, being cheap, do not work out of the box, because the latest Nvidia driver (version 337.12) does not detect the correct resolution.
What's worse, the QNIXes do not support anything other than their native 2560x1440 resolution, so you'll get blank screens if you try to just use these monitors without tweaking some `xorg.conf` settings.
I actually ended up googling to find the correct `Modeline` value for `xorg.conf` for my QNIX, at 96hz refresh rate.[^overclock]
You could try using the `cvt` program included with `xorg-server`, but when I tried it out on my own it generated different numbers and my QNIXes couldn't handle those numbers.

Here is my `xorg.conf`:

- i quad-monitor-portrait.xorg.conf

[^overclock]: I tried using the higher refresh rate modelines, but they resulted in my monitors going crazy (extreme artifacting, lines shrieking irregularly with different colors, etc.).
