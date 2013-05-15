---
title: Using the Total Bithead with ALSA on Arch Linux
tags: linux, sound, alsa, arch, hardware
---

So I got HeadRoom's "Total Bithead" USB DAC (digital-to-analog converter) and micro amplifier.
The problem is, when I plug it into my Linux desktop, mplayer/firefox still recognizes the onboard sound on my motherboard as the default device.
After some searching, I found some resources to (1) use it as the default sound device and (2) enable mixing on it (different applications can simultaneously use the Total Bithead).

The Setup
---------

We need to manually call a script every time the Total Bithead is connected.
The script, called `switch_audio.sh`, looks like this:

```{.bash .numberLines}
#!/bin/zsh

if [[ -e /proc/asound/card1 ]]; then
	ln -sf ~/syscfg/alsa/cfg-total-bithead ~/.asoundrc
else
	case $HOST in
	k0) ln -sf ~/syscfg/alsa/cfg-k0 ~/.asoundrc ;;
	k1) ln -sf ~/syscfg/alsa/cfg-k1 ~/.asoundrc ;;
	*) echo "Unknown host \`$HOST'" ;;
	esac
fi
```

where k0 is my desktop and k1 is my laptop.
`cfg-k0` looks like this:

	# use `aplay -l | awk '/^card/{print$3}' | sort | uniq' for card names
	defaults.pcm.!card NVidia
	defaults.ctl.!card NVidia
	defaults.pcm.!device 0
	defaults.ctl.!device 0

and `cfg-total-bithead` looks like this (yes, the Linux kernel recognizes the Total Bithead as CODEC):

	defaults.pcm.!card CODEC
	defaults.ctl.!card CODEC
	defaults.pcm.!device 0
	defaults.ctl.!device 0

That's all there is to it.
Now every time you plug in/out the Total Bithead, just run this script.
You could do some fiddling with udev or some other low-level mechanism to automatically run the script when it detects the Total Bithead plugged into a USB port, but I'd rather just call it from a keyboard shortcut manually each time for explicit control.
The `~/.asoundrc` configurations are taken from [this helpful blog post][asoundrc].
It's pretty sad that the 4-liner `~/.asoundrc` solution offered from that blog remains unknown to all the mplayer/alsa/linux wikis out there on the web.
The setup here takes care of multiple applications playing sound simultaneously.

Other Thoughts
--------------

I got the Total Bithead used for around $50.
From what I can tell, it first appeared in the market in 2004 (there's a [review][review] of it dating back to that time).
I do recommend it at this price point when comparing it against the newer Realtek HD audio chips found in most modern motherboards.
Don't buy a new one for over $100; it's probably much better to just get a better pair of headphones with that amount of money.

[review]: http://www.6moons.com/audioreviews/bithead/bithead.html
[asoundrc]: http://ptspts.blogspot.com/2009/03/how-to-select-alsa-sound-card-and-have.html
