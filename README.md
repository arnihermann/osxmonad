# osxmonad

This is a library which allows XMonad to manage Mac OS X windows.

## Status

* Only attached hook is `layoutHook`
* No workspaces
* No borders
* No `focusFollowsMouse`

## Installation

We need XMonad's compilation step to include the `-framework Cocoa`
flag to GHC. This repository includes a `xmonad.patch` (1 line diff)
that you must apply to the XMonad source:

    git clone git://github.com/pufuwozu/osxmonad.git
    darcs get http://code.haskell.org/xmonad
    cd xmonad
    darcs apply ../osxmonad/xmonad.patch
    cabal configure
    cabal install
    cd ../osxmonad
    cabal configure
    cabal install

**Note**: Mountain Lion users will have to download and install
[XQuartx](http://xquartz.macosforge.org/landing/).

## Configuration

Create `~/.xmonad/xmonad.hs`:

    import XMonad
    import OSXMonad.Core

    main = osxmonad defaultConfig {
             modMask = mod1Mask .|. mod4Mask,
             keys = osxKeys
           }

Now we can run `xmonad` to have our windows managed.

## License

BSD-3
