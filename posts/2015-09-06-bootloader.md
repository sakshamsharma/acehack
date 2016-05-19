-------
title: Bootloaders: Basics
technical: true
summary: Installing Syslinux/GRUB2
-------

First things first, bootloader, what? For those who don't know, bootloader is the piece of code which loads up your Operating System. Remember that pink screen on boot which allows you to choose between Ubuntu and Windows? Yes, that one. Its named *grub*. Remember when you booted from a pen-drive perhaps to install Ubuntu? You got a black screen allowing you to choose whether to *test* Ubuntu or to install it directly? Yes, that one. Why just Linux? The blue screen on Windows 8 (and above) PCs which appears if you want to open safe mode or something?

Enough examples, the job of the bootloader is to load up the 'core' of your OS (techies, read: Kernel), and some more things (like initial RAM disk, no don't get confused already) which allow the operating system to start.

There are 2 types of booting, BIOS and UEFI, the latter being the newer one. They are considerably different, and thus, the bootloaders available vary. Most newer PCs boot via UEFI.

Arch Linux, like all the 'techie' linux distributions, requires you to manually install your bootloader. This can be very daunting, because you can run into either of the following horrible situations (yes, to scare you):

1. Instead of Windows or your default GRUB, your PC now loads up to a weird screen a command line you cannot type anything sane into.
2. You cannot boot onto your Windows installation.
3. You get a menu, but instead of opening your OS, the menu entries lead to errors.

I myself have had a lot of those experiences, and I recall running to a senior once to help me 'save' my laptop. So it might be tricky, strap on your seatbelts.

**Syslinux**

Why is this before GRUB, you ask? You don't need to install it on a new partition, unlike GRUB; that's why. I had wanted to install GRUB but ending up with Syslinux during the 'save my laptop' time. Syslinux is a simple bootloader, which supports both BIOS and UEFI. If you have a single OS (mind this), go ahead, you won't have any trouble. I installed it on BIOS mode, to boot my Arch, and configured my laptop to boot with Legacy mode (BIOS mode) in priority. When I wanted to boot Windows, I'd mash F12 on my keyboard (might be different for you) and selected it from the boot menu. A weird way, but had been using this since 4-5 months. Doesn't really matter when you open Windows once a week. Again mind it, Syslinux is not for people wnating to boot multiple OS from a single bootloader.

First do an `lsblk`, note your boot and root partition and make sure they are mounted.

How to install:

    sudo pacman -S syslinux gptfdisk
    sudo syslinux-install_update -i -a -m

You need to verify (and most probably edit) the file `/boot/syslinux/syslinux.cfg`. Check to see that the root partition being mounted is correct. It defaults to /dev/sda2 but it might be different for you, correct it or else you'll be struck at the 3rd 'horrible situations' as mentioned above.

If your root is /dev/sda3, the file should contain something like this:

    LABEL arch
            LINUX ../vmlinuz-linux
            APPEND root=/dev/sda5 rw
            INITRD ../initramfs-linux.img

You're good to go now, if luck is on your side.

**GRUB2**

The universal 'go-to' bootloader, GRUB2 is the newer version of the iconic 'GRUB', now renamed 'GRUB Legacy'. It took me long enough to figure out that you cannot install GRUB2 on a ext-x partition. So if you want to install GRUB2, make a new partition and format it as vfat with `mkfs.vfat /dev/sdaX`, where 'sdaX' is the drive which you want to format. Don't play around with X, identify it with `lsblk`. You've been warned :) 

Once done, you need to configure your OS to mount the boot partition at it's proper place during boots. If you are in the process of installing your OS, then you'd probably not have booted even once till now. In any case, you need to mount this /dev/sdaX at /boot of your Arch installation first.
After that, in Arch, you edit the file `/etc/fstab` and write something like this there (not recommended, *really*):

    /dev/sdaX   	/boot     	vfat    	rw,relatime,fmask=0022,dmask=0022,codepage=437,iocharset=iso8859-1,shortname=mixed,errors=remount-ro	0 2

The recommended way is to use `genfstab` while out of chroot (which you would have had been using if you're currently installing your arch). That would automatically create your fstab according to the currently used mount pattern. Double check it.

Installing the required packages is the next step:

    sudo pacman -S grub efibootmgr os-prober

Now following the Arch Wiki, we do this:

    grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=grub --recheck
    grub-mkconfig -o /boot/grub/grub.cfg

And there! You're probably done! If you run a normal PC (that is, no archaic stuff or some really non-obvious choice of partitions or configurations which might require some more work). Luckily, the Arch wiki is there to help in such cases.

Feel free to comment if you run into a mountain.
