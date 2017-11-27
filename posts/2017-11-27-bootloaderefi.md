------
title: Things no one told me about EFI
summary: Fixing up my laptop's bootloader and partitions
tags: bootloader, grub, EFI
category: linux
id: bootloader-efi
author: Saksham Sharma
------

About one and a half years later, I finally thought it is time to reinstall Windows for completeness sake (don't judge me please). Little did I know that I would be opening a pandora's box.

So I never really understood the concept of EFI, or the difference between BIOS/UEFI; and yet, I have played around with bootloaders for quite a while now. I somehow managed to survive <!--more--> by trying all possible variants of grub commands, every time. This caused more havoc on my hard disk than I knew.

So, here's what I learnt later, once everything was fixed:

## EFI vs Legacy
Legacy bootloaders reside in some bytes at the beginning of each partition. During legacy boot, the BIOS reads data off the start of each partition.

Instead, in the case of EFI, there's supposed to be a single partition on your hard disk, which contains all the bootloaders. The BIOS is configured to read data from this partition.

Now, it is recommended to have only one EFI partition. I had 3 at some point, somehow, and it caused quite a few troubles while installing Windows.

## How does an EFI installation work?
We first need to see how Linux boot works. The bootloader's job is to load up the Linux kernel. Grub usually has a configuration which stores some preset ways to load into your well-settled installations, but here's what actually happens. You can boot using these steps to boot using grub a grub shell:
```
> insmod ext2
 > set root=(hd0,gpt5)
 > linux /vmlinuz-4.10.4-generic root=/dev/sda6 init=/usr/lib/systemd/systemd
 > boot
```

Let's see some step by step explanations:

1. We need to load the ext2 module for this (don't ask me why).
2. This *root* that we mention here, that's the partition where `/boot` is often mounted. The strict requirement is that this partition should have the linux kernel (`vmlinuz-<something>`). It may be your root partition, or the one where you mount `/boot`.
3. We specify the kernel we want to use. There are a couple of arguments:
   1. Notice how the path used is `/vmlinuz-...`. The path is relative to the root mentioned in step 2. It would've been `/boot/vmlinuz-...` if you don't have a separate boot partition.
   2. The root variable (a different one, you say?) is now set to the location of your **root** partition.
   3. I chose to set `init` to systemd, since I use Gentoo and it defaults to OpenRC. You may not need this.
4. Boot. Profit.

The take-away is that the booting pretty much involves providing a kernel, and a root partition.

Now how did the bootloader start up? Inside your EFI partition, there shall be a binary of the type: `/EFI/<your-os-name>/grub64x.efi`. That's how BIOS finds the bootloader.

## Things I learned
Well, so ideally, for a EFI installation, here's some things you need to keep in mind.

* Make sure there's only one EFI partition. Open up gparted, and look at the partitions with the `esp` flag set. **ESP** stands for **EFI System Partition**. If there are other partitions than the one you'd want to be your EFI partition, you should unset the `esp` flag on those.

* Grub configuration is created using `grub-mkconfig -o /boot/grub/grub.cfg`. Now, your kernel, grub configuration, and any other bootloader-specific things should stay in the same partition.

* Now about installation of the bootloader into the EFI partition. Grub defaults to the assumption that you have mounted the ESP at `/boot/efi`. So if that's done, you can simply use `grub-install` and it should automatically copy the `.efi` file to the correct location. Otherwise, you can point to the installation location (it has `/EFI/<bootloader-names>` directories). The way to do it is `grub-install --efi-directory=/place/where/you/mounted/EFI`.

* I was under the impression that my hard disk was using a GPT partition table. Well, it was using MBR. I first converted it to GPT (no data loss usually), after which the remaining pieces fell into place, and I could finally proceed with my Windows installation without much issue.

* If you already have a few partitions, don't create the partition for the windows installation from the windows installer. It creates lots of wasteful extra partitions (I had most of them already, due to this being a reinstallation). I created an NTFS partition manually for Windows using parted on Linux.
