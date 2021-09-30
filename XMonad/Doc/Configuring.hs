-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Doc.Configuring
-- Description :  Brief xmonad tutorial.
-- Copyright   :  (C) 2007 Don Stewart and Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  portable
--
-- This is a brief tutorial that will teach you how to create a basic
-- xmonad configuration.  For a more comprehensive tutorial, see the
-- <https://xmonad.org/TUTORIAL.html xmonad website>.
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

HISTORICAL NOTE regarding upgrading from versions (< 0.5) of xmonad
or using old documentation:

xmonad-0.5 delivered a major change in the way xmonad is configured.  Prior
to version 0.5, configuring xmonad required editing a source file called
Config.hs, manually recompiling xmonad, and then restarting.  From
version 0.5 onwards, however, you should NOT edit this file or manually
compile with ghc --make.  All you have to do is edit xmonad.hs and restart
with @mod-q@; xmonad does the recompiling itself. The format of the
configuration file also changed with version 0.5; enabling simpler and
much shorter xmonad.hs files that only require listing those settings which
are different from the defaults.

While the complicated template.hs (man/xmonad.hs) files listing all default
settings are still provided for reference, once you wish to make substantial
changes to your configuration, the template.hs style configuration is not
recommended. It is fine to use top-level definitions to organize your
xmonad.hs, but wherever possible it is better to leave out settings that
simply duplicate defaults.
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
>    main = xmonad $ def
>        { borderWidth        = 2
>        , terminal           = "urxvt"
>        , normalBorderColor  = "#cccccc"
>        , focusedBorderColor = "#cd8b00" }

This will run \'xmonad\', the window manager, with your settings
passed as arguments.

Overriding default settings like this (using \"record update
syntax\"), will yield the shortest config file, as you only have to
describe values that differ from the defaults.

As an alternative, you can copy the template @xmonad.hs@ file (found
either in the @man@ directory, if you have the xmonad source, or on
the xmonad wiki config archive at
<http://haskell.org/haskellwiki/Xmonad/Config_archive>)
into your @~\/.xmonad\/@ directory.  This template file contains all
the default settings spelled out, and you should be able to simply
change the ones you would like to change.

To see what fields can be customized beyond the ones in the example
above, the definition of the 'XMonad.Core.XConfig' data structure can
be found in "XMonad.Core".

-}

{- $check
#Checking_whether_your_xmonad.hs_is_correct#

After changing your configuration, it is a good idea to check that it
is syntactically and type correct.  You can do this easily by using an xmonad
flag:

>    $ xmonad --recompile
>    $

If there is no output, your xmonad.hs has no errors.  If there are errors, they
will be printed to the console.  Patch them up and try again.

Note, however, that if you skip this step and try restarting xmonad
with errors in your xmonad.hs, it's not the end of the world; xmonad
will simply display a window showing the errors and continue with the
previous configuration settings. (This assumes that you have the
\'xmessage\' utility installed; you probably do.)

-}

{- $load
#Loading_your_configuration#

To get xmonad to use your new settings, type @mod-q@. (Remember, the
mod key is \'alt\' by default, but you can configure it to be
something else, such as your Windows key if you have one.) xmonad will
attempt to compile this file, and run it.  If everything goes well,
xmonad will seamlessly restart itself with the new settings, keeping
all your windows, layouts, etc. intact.  (If you change anything
related to your layouts, you may need to hit @mod-shift-space@ after
restarting to see the changes take effect.)  If something goes wrong,
the previous (default) settings will be used.  Note this requires that
GHC and xmonad are in the @$PATH@ in the environment from which xmonad
is started.

-}
