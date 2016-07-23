-----
title: Dealing with the Emacs finger strain
technical: true
summary: How I got about with the excessive stress on my fingers due to Emacs
tags: emacs, short
category: technical
author: Saksham Sharma
image: /images/articles/emacs.jpg
-----

This is something every Emacs user would relate to. Keeping the left control pressed half of the time is quite a habit for most users. After all, it's the 'C' !

I had just shifted from Vim, and my hands hurt A LOT after even slight usage of Emacs. Not one to give up (or shift to the evil-mode, which might've been a  good option too), I went ahead and looked for some details online.

What I found is that the pain is mostly because the 'pinky finger' isn't meant to take up that much of a strain for such long intervals. Okay. Also, I came across quite a few solutions for it, most of which weren't very conventional or pleasing to the mind.

I tried 2 solutions:

## The simple and mild solution:
Swap the caps lock and left control keys. It's really easy in Linux. Infact, I chose to keep the original left control key too (that is, no caps key, it's a waste anyway).
Here's what you need to add into the ```~/.xinitrc``` file:
```
    setxkbmap -option ctrl:nocaps
```
Worked out pretty well for me. *Initially*, that is.

In this case you're basically reducing the strain on your little finger. It doesn't have to be pressed all the time, it's much easier to press the caps lock than the left control. Pain still set in after some time. That brings me to the second solution.

## The awesome solution:
Swap the right control with the right alt (meta) button. Yes, RIGHT. It really breaks your instincts, I know. But try it for once.

* No pain (You're using your right thumb, it's quite strong for this)
* Going back in the browser is now a breeze (alt-left key is now control-left, 2 ajacent keys)
* Doesn't break common instinct for others using your laptop. So they can still use the left control and alt keys the way they wanted to.
* No more finger acrobatics needed (both hands are separate to press their keys). For instance, try doing a ```C-x C-s``` with this.

Here's how I did it.

* Install the ```xmodmap``` package (Google how to do it).
* Create a new file ```~/.Xmodmap```. Contents later.
* Add this line to your ```.xinitrc``` (Or whatever file you use to control your startup scripts): ```xmodmap ~/.Xmodmap```

Here are the contents for the ```Xmodmap``` file:
```
clear control
clear mod1
keycode 105 = Alt_R Meta_R
keycode 108 = Control_R
add control = Control_L Control_R
add mod1 = Alt_R Meta_R
```

This should get you going. Now either logout-login, or run ```xmodmap ~/.Xmodmap``` to get this into effect.

**NOTE** Remember, everytime you connect a new keyboard, you'd have to run the command again, whether it's the setxkbmap command or the xmodmap command.

I hope this helps some of you. I personally have been using the second one since more than a month. Has worked out really well. Takes a bit of getting used to, but in the end it's really worth it.
