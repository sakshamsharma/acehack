------
title: Mini Gentoo installation log
technical: true
summary: Super condensed installation experience
tags: gentoo, install
category: linux
author: Saksham Sharma
------


This article is more of a note to self. Would help me install Gentoo a little bit faster
whenever I choose to do that again (not anytime soon :) ). More probably, I'll use this
as a reference to remember what's in my system etc.

And what about *you*? So I've often had to tell people how to do stuff. I like to keep all my instructions
online. Saves time ;) . Feel free to refer to this in case you plan to jump on the Gentoo ship.

**NOTE:** This article is not meant as a replacement for the *awesome* Gentoo wiki. Infact this is a super
condensed version of the steps written there, and may not work for everyone. But would certainly help if
you're having some trouble getting something to work (for example: the bootloader).

# Testing
* Make bootable with dd command.
* Ping internet

# Make partition
* `parted -a optimal /dev/sda`
* unit mib
* Exact same steps as on [https://wiki.gentoo.org/wiki/Handbook:AMD64/Installation/Disks]
* Created swap space of 9.5 GB
* Mounted into directories.

# Stage 3
* Downloaded tar.gz from iitk mirror.
* Unpacked it.
* Wrote all my USE flags (copied from laptop. See section on *other notes*)

# Chroot
* Mounted all proc,sys,dev etc as written on Gentoo Wiki.
* Did chroot and sourced etc/profile
* Did `emerge-webrsync`
* Selected plain systemd profile

# Systemd
* Symlinked: ln -sf /proc/self/mounts /etc/mtab
* Set root password

# Other
* echo "Asia/Kolkata" > /etc/timezone
* emerge --config sys-libs/timezone-data
* Added locales in /etc/locale.gen
* locale-gen
* Set locale to en_US.utf8 using eselect
* Installing vim

# Kernel
* emerge gentoo-sources
* Did genkernel for now.
* genkernel --menuconfig all to add inkernel support for ext4
* Required kernel parameters: root=/dev/sda4, rootfstype=ext4
* Realized that I should've let my boot partition be ext2
* Did that. Recreated the boot partition and regenerated the kernel.
* Realized I should've just made a backup of the previous build of the kernel.

# Config
* Edited /etc/fstab file and mounted root with noatime
* Set root passwd
* emerged linux-firmware
* emerged net-misc/netifrc
* Removed udev
* Installed systemd
* Did emerge -avDN @world
* Enabled systemd-networkd service using systemd
* Installed ssh, dhcpcd
* Installed eix to be able to query packages
* SUDO!
* useradd
* EDITOR=vim visudo and give myself permissions

# Bootloader
* Installed grub and os-prober
* BIOS: grub2-install /dev/sda
* Set this line in /etc/default/grub: `GRUB_CMDLINE_LINUX="init=/usr/lib/systemd/systemd"`
* grub2-mkconfig -o /boot/grub/grub.cfg
* DIDN'T WORK!!
* Change of plans, go with UEFI.
* Used my Arch bootable (which had a dual, UEFI and Legacy boot) to boot into a UEFI environment which had the EFI variables. Gentoo bootable wasn't UEFI for some reason.
* Did the regular gentoo liveUSB style chroot into my gentoo installation.
* Add entry in make.conf to install efivars: `GRUB_PLATFORMS="efi-64 efi-32 pc emu"`
* Mount /dev/sda2 to /boot
* Mount /dev/sda1 to /boot/efi and format it as vfat
* Now do the grub UEFI install steps
* Enable systemd-networkd on boot

# Graphics
* Installed x11-base/xorg-x11
* Installed ghc and xmonad
* Installed i3 and dmenu
* Copied xinitrc from /etc/X11/xinit/xinitrc to ~/.xinitrc
* Startx runs i3 (added exec i3 to .xinitrc)!

# Audio
* Installed pulseaudio (fixed unmet use flags)
* alsa-utils
* Relogin

# Packages
* rxvt-unicode and urxvt-perls
* networkmanager
* Emacs and started its daemon to download packages
* Vim and installed neobundle and vim-plug, followed by installation of my vim packages
* elinks
* firefox-bin-43.0
* google-chrome-unstable
* x11-misc/xclip
* zsh fortune-mod cowsay
* cabal
* taffybar
* i3lock
* setxkbmap and xmodmap for keyboard layouts
* arandr and xrandr
* unrar, pcmanfm
* gentoolkit, genlop

# Set up ssh key
* `ssh-keygen -t rsa -C -b 4096 "your_email"`
* Set strong password
* `eval "$(ssh-agent -s)"`
* `ssh-add ~/.ssh/id_rsa`
* Preferably add this to your xinitrc as follows: `exec ssh-agent i3` (or whatever DE you use)

# Haskell
* Installed ghc and xmonad
* Now `sudo layman -a haskell`
* Installed taffybar
* xmonad-contrib
* Installed taffybar again
* Installed ghc-mod (hell) using: `emerge -av --oneshot --keep-going ghc-mod --backtrack=30`
* Failed. Did haskell-updater

# Virtualbox
* Installed virtualbox-bin

# Other notes:
Here are my USE flags:
```
USE="bindist mmx sse sse2 udisks systemd X acl alsa gtk bindist mp3 mp4 unicode jpg xft 256-color unicode3 fading-colors jpeg bluetooth science truetype mtp png xvmc imagemagick libass fontconfig pulseaudio x264 gd gtk3 dbus"
```
