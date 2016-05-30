AceHack
=======

[![Build Status](https://travis-ci.org/sakshamsharma/acehack.svg?branch=master)](https://travis-ci.org/sakshamsharma/acehack)

Source code for my static blog cum website generated using Hakyll (Haskell flavored Jekyll ;) ).

Has a nice search feature for blog posts, and implements tags and categories as well.

Is basically a static-site-gen aware port of the `portfolio` material design theme from Google.


### Build and Run

I personally prefer to install Hakyll via stack, since it does not mess with your system / cabal installations.

```
stack setup
stack build
stack exec site watch
```
