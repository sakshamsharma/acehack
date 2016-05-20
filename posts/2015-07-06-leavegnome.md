-------
title: Shifting away from GNOME
technical: true
summary: Bloatware goodbye
author: Saksham Sharma
tags: gnome, technical
-------

Let me get it straight, I love the GNOME project and DE. It's great looking, has great support, very stable, and includes almost all the basic essestials. It's great for high-performance PCs. Then why 'shifting away' you ask? Here's why I shifted:

* It's slow. Slow to boot up, and slow to use (it still depends heavily on mouse usage, which is slow). Mind you, I have a i7 4710HQ with 8GB RAM.

* It's over-cluttered. I'm sure it looks great because of that, but that's not a programmer loves.

* I'm a Linux enthusiast and installing GNOME on my Arch basically defeates the purpose of installing Arch in the first place, which was to use a self-built system.

* Even keeping a single GNOME package means having to install countless dependencies with it.

I left GNOME about 5-6 months back. Came back to it after 4 months. Left it again 3 months ago, and systematically pushed all its dependencies away from my laptop.

### GNOME Shell

Yes it looks great and all, but it simply isn't for a pure keyboard user. So I shifted to i3, in my opinion the best Window Manager for programmers atleast. Read more about it here: [Hopping over to i3wm](http://acehack.org/technical?aname=i3)

### Nautilus

I still miss it sometimes, but it was exceptionally slow to start on the first launch, and yes, it required quite a few dependencies. I initially used Nemo file manager which is the best looking one I found (of course not better than Nautilus). For people who use i3, or people who don't want it to manage their desktop because feh or some other package is doing that for them, alias nemo with this `nemo --no-desktop`

### GNOME Screenshot

Yes it is awesome. But here's the alternative I use: [screengrab](http://screengrab.doomer.org/). Many more are listed at [arch wiki](https://wiki.archlinux.org/index.php/Taking_a_screenshot)


### GNOME Terminal

This stuck around for the longest time. It allows for easy configuration, and is great overall. But everything has to go. One of the main reasons for removing it was the menu bar at the top. When you keep 6 terminals open on a single screen at a time (courtesy i3wm, yes that's possible and practical), you want to keep as much space as possible for the Terminal and no space for the extras. So here is the solution, urxvt (or rxvt-unicode). I configured it to look exactly like my GNOME Terminal, and its much faster, lighter, cleaner, and with no 1000 dependencies. If you want, have a look at my .Xresources [here](https://github.com/sakshamsharma/my-rc-files/blob/master/.Xresources) or read about urxvt in my upcoming article here on acehack.
I hope this article helps some of you go ahead and take that leap. It's not *that* dark and murky world without GNOME, you'll come to love it too.
