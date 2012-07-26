# osxmonad

This is a library which allows XMonad to manage Mac OS X windows.

## Status

* Only pure layouts work (i.e. `Tall`)
* Only attached hook is `layoutHook`
* No moving windows around
* No keyboard shortcuts
* No workspaces
* No borders
* No `focusFollowsMouse`
* Crashes

## Installation

We need XMonad's compilation step to include the `-framework Cocoa`
flag to GHC. This repository includes a `xmonad.patch` (1 line diff)
that you must apply to the XMonad source:

    darcs get http://code.haskell.org/xmonad
    cd xmonad
    darcs apply ../osxmonad/xmonad.patch
    cabal configure
    cabal install

## Configuration

Create `~/.xmonad/xmonad.hs`:

    import XMonad
    import OSXMonad.Core

    main = osxmonad defaultConfig {
             layoutHook = Tall 1 (3/100) (1/2)
           }

Now we can run `xmonad` to have our windows managed.

## License

BSD-3
