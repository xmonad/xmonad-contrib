-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc.Configuring
-- Copyright   :  (C) 2007 Don Stewart and Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This is a brief tutorial that will teach you how to create a
-- basic xmonad configuration.
--
-- For more detailed instructions on extending xmonad with the
-- xmonad-contrib library, see "XMonad.Doc.Extending".
--
-----------------------------------------------------------------------------

module XMonad.Doc.Configuring
    (
    -- * Configuring xmonad
    -- $configure

    -- * A simple example
    -- $example

    -- * Checking whether your xmonad.hs is correct
    -- $check

    -- * Loading your configuration
    -- $load

    ) where

--------------------------------------------------------------------------------
--
--  Configuring Xmonad
--
--------------------------------------------------------------------------------

{- $configure
#Configuring_xmonad#
xmonad can be configured by creating and editing the Haskell file:

>    ~/.xmonad/xmonad.hs

If this file does not exist, xmonad will simply use default settings;
if it does exist, xmonad will use whatever settings you specify.  Note
that this file can contain arbitrary Haskell code, which means that
you have quite a lot of flexibility in configuring xmonad.

NOTE for users of previous versions (< 0.5) of xmonad: this is a major
change in the way xmonad is configured.  Prior to version 0.5,
configuring xmonad required editing an xmonad source file called
Config.hs, recompiling xmonad, and then restarting.  From version 0.5
onwards, however, all you have to do is edit xmonad.hs and restart
with @mod-q@; xmonad does the recompiling itself.  The format of the
configuration file has also changed; it is now simpler and much
shorter, only requiring you to list those settings which are different
from the defaults.

-}

{- $example
#A_simple_example#

Here is a basic example, which starts with the default xmonad
configuration and overrides the border width, default terminal, and
some colours:

>    --
>    -- An example, simple ~/.xmonad/xmonad.hs file.
>    -- It overrides a few basic settings, reusing all the other defaults.
>    --
>
>    import XMonad
>
>    main = xmonad $ defaultConfig
>        { borderWidth        = 2
>        , terminal           = "urxvt"
>        , normalBorderColor  = "#cccccc"
>        , focusedBorderColor = "#cd8b00" }

This will run \'xmonad\', the window manager, with your settings
passed as arguments.

Overriding default settings like this (using \"record update
syntax\"), will yield the shortest config file, as you only have to
describe values that differ from the defaults.

An alternative is to inline the entire default config file from
xmonad, and edit values you wish to change. This is requires more
work, but some users may find this easier. You can find the defaults
in the "XMonad.Config" module of the core xmonad library.

However, note that (unlike previous versions of xmonad) you should not
edit Config.hs itself.

To see what fields can be customized beyond the ones in the example
above, the definition of the 'XMonad.Core.XConfig' data structure can
be found in "XMonad.Core".

-}

{- $check
#Checking_whether_your_xmonad.hs_is_correct#

After changing your configuration, it is a good idea to check that it
is syntactically and type correct.  You can do this easily by loading
your configuration file in the Haskell interpreter:

>    $ ghci ~/.xmonad/xmonad.hs
>    GHCi, version 6.8.1: http://www.haskell.org/ghc/  :? for help
>    Loading package base ... linking ... done.
>    Ok, modules loaded: Main.
>
>    Prelude Main> :t main
>    main :: IO ()

Ok, looks good.

Note, however, that if you skip this step and try restarting xmonad
with errors in your xmonad.hs, it's not the end of the world; xmonad
will simply display a window showing the errors and continue with the
previous configuration settings.

-}

{- $load
#Loading_your_configuration#

To get xmonad to use your new settings, type @mod-q@. xmonad will
attempt to compile this file, and run it.  If everything goes well,
xmonad will seamlessly restart itself with the new settings, keeping
all your windows, layouts, etc. intact.  (If you change anything
related to your layouts, you may need to hit @mod-shift-space@ after
restarting to see the changes take effect.)  If something goes wrong,
the previous (default) settings will be used.  Note this requires that
GHC and xmonad are in your @$PATH@. If GHC isn't in your path, you can
still compile @xmonad.hs@ yourself:

>    $ cd ~/.xmonad
>    $ /path/to/ghc --make xmonad.hs
>    $ ls
>    xmonad    xmonad.hi xmonad.hs xmonad.o

When you hit @mod-q@, this newly compiled xmonad will be used.

-}

