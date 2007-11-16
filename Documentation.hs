-----------------------------------------------------------------------------
-- |
-- Module      :  Documentation
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a module for documenting the xmonad-contrib library
--
-----------------------------------------------------------------------------

module Documentation
    ( 
    -- * Configuring XMonad
    -- $configure
      
    -- ** A simple example
    -- $example
    
    -- ** Checking your xmonad.hs is correct
    -- $check
    
    -- ** Loading your configuration
    -- $load
    
    -- ** Where are the defaults?
    -- $where
    
    -- * The XmonadContrib Library
    -- $library
    
    -- * Extending XMonad
    -- $extending
    
    -- ** Editing Key Bindings
    -- $keys

    -- *** Adding Key Bindings
    -- $keyAdding

    -- *** Removing Key Bindings
    -- $keyDel

    -- *** Adding and Removing Key Bindings
    -- $keyAddDel

    -- ** Adding\/Removing Layouts
    -- $layout

    -- ** Hooks Management
    -- $hooks

    -- * Writing Other Extensions
    -- $writing
    ) where

--------------------------------------------------------------------------------
--
--  Configuring Xmonad
--
--------------------------------------------------------------------------------

{- $configure

xmonad is configured by creating and editing the Haskell file:

>    ~/.xmonad/xmonad.hs

xmonad then uses default settings from this file as arguments to the
window manager.

-}

{- $example

Here is a basic example, which takes defaults from xmonad, and
overrides the border width, default terminal, and some colours:

>    --
>    -- An example, simple ~/.xmonad/xmonad.hs file.
>    -- It overrides a few basic settings, reusing all the other defaults,
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
in the file:

>    XMonad/Config.hs

-}

{- $check

Place this text in @~/.xmonad/xmonad.hs@, and then check that it is
syntactically and type correct, by loading it in the Haskell
interpreter:

>    $ ghci ~/.xmonad/xmonad.hs  
>    GHCi, version 6.8.1: http://www.haskell.org/ghc/  :? for help
>    Loading package base ... linking ... done.
>    Ok, modules loaded: Main.
>
>    Prelude Main> :t main
>    main :: IO ()

Ok, looks good.  

-}

{- $load

To have xmonad start using your settings, try @mod-q@. xmonad will
attempt to compile this file, and run it. If it is unable to, the
defaults are used. This requires GHC and xmonad are in your @$PATH@
settings. If GHC isn't in your path, you can still compile the
@xmonad.hs@ file yourself:

>    $ cd ~/.xmonad
>    $ ghc --make xmonad.hs
>    $ ls
>    xmonad    xmonad.hi xmonad.hs xmonad.o

When you hit @mod-q@, this newly compiled xmonad will be used.

-}

{- $where

The default configuration values are defined in the source file:

>   XMonad/Config.hs 

the 'XMonad.Core.XConfig' data structure itself is defined in:

>    XMonad/Core.hs

See "XMonad.Core".

-}

--------------------------------------------------------------------------------
--
--  The XmonadContrib Library
--
--------------------------------------------------------------------------------

{- $library

The xmonad-contrib (xmc) library is a set of modules contributed by
xmonad hackers and users. Examples include an ion3-like tabbed layout,
a prompt\/program launcher, and various other useful modules.

Some of these modules provide libraries and other useful functions to
write other modules and extensions.

Here is a short overview of the xmc content:
 
   [@Actions@] The content of Action

   [@Config@] The content of Config

   [@Hooks@] The content of Hooks

   [@Layout@] The content of Layout

   [@Prompt@] The content of Prompt

   [@Util@] The content of Util

-}

--------------------------------------------------------------------------------
--
--  Extending Xmonad
--
--------------------------------------------------------------------------------

{- $extending

Since the @xmonad.hs@ file is just another Haskell module, you may
import and use any Haskell code or libraries you wish, such as
extensions from the xmonad-contrib library, or other code you write
yourself.

-}

{- $keys

Editing key bindings means changing the 'XMonad.Core.XConfig.keys'
record of the 'XMonad.Core.XConfig' data type, like:

>    main = xmonad defaultConfig { keys = myKeys }

and providing a proper definition of @myKeys@ such as:

>       myKeys x   = 
>          [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>          , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>          ]

Remember that this definition requires importing "Graphics.X11.Xlib",
"XMonad.Prompt", "XMonad.Prompt.Shell", and "XMonad.Prompt.XMonad"

-}

{- $keyAdding

Adding key bindings can be done in different ways. The type signature
of "XMonad.Core.XConfig.keys" is:

> keys :: XConfig Layout -> M.Map (ButtonMask,KeySym) (X ())

which means you need to create a 'Data.Map.Map' from the list of your
bindings, with 'Data.Map.fromList', and join it with the exist one
with 'Data.Map.union'.

For instance, if you have defined some additional key bindings like
these:

>       myKeys x   = 
>          [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>          , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>          ]


then you create a new map by joining the default one with yours:

>    newKeys x  = M.union (keys defaultConfig x) (M.fromList (myKeys x))

Finally you need to update accordingly the default configuration
'XMonad.Core.XConfig.keys' record:

>    main = xmonad defaultConfig { keys = newKeys }


And that's it.

At the end your @~\/.xmonad\/xmonad.hs@ would look like this:


> module Main (main) where
> 
> import XMonad
> 
> import qualified Data.Map as M
> import Graphics.X11.Xlib
> import XMonad.Prompt
> import XMonad.Prompt.Shell
> import XMonad.Prompt.XMonad
>
> main :: IO ()
> main = xmonad defaultConfig { keys = newKeys }
>
> newKeys x = M.union (keys defaultConfig x) (M.fromList (myKeys x))
>
> myKeys x =
>          [ ((modMask x, xK_F12), xmonadPrompt defaultXPConfig)
>          , ((modMask x, xK_F3 ), shellPrompt  defaultXPConfig)
>          ]


Obviously there are other ways of defining @newKeys@. For instance,
you could define it like this:

> newKeys x = foldr (uncurry M.insert) (keys defaultConfig x) (myKeys x)

A simpler way to add new keybindings is the use of some of the
utilities provided by the xmonad-contrib library. For instance,
"XMonad.Util.EZConfig" and "XMonad.Util.CustomKeys" both provide
useful functions for editing your key bindings. Look, for instance, at
'XMonad.Util.EZConfig.additionalKeys'.

 -}

{- $keyDel

Removing key bindings requires modifying the binding 'Data.Map.Map'.
This can be done with 'Data.Map.difference' or with 'Data.Map.delete'.

Suppose you wan to get rid of @mod-q@ and @mod-shift-q@. To do this
you just need to define a @newKeys@ as a 'Data.Map.difference' between
the default map and the map of the key bindings you want to remove.

> newKeys x = M.difference (keys defaultConfig x) (M.fromList $ keysToRemove x)
> 
> keysToRemove :: XConfig Layout -> [((KeyMask, KeySym),X ())]
> keysToRemove x =
>          [ ((modMask x              , xK_q ), return ())
>          , ((modMask x .|. shiftMask, xK_q ), return ())
>          ]                                              

As you may see we do not need to define an action for the key bindings
we want to get rid of. We just build a map of keys to remove.

It is also possible to define a list of key bindings and then use
'Data.Map.delete' to remove them from the default key bindings, in
which case we should write something like:

> newKeys x = foldr M.delete (keys defaultConfig x) (keysToRemove x)
> 
> keysToRemove :: XConfig Layout -> [(KeyMask, KeySym)]
> keysToRemove x =
>          [ (modMask x              , xK_q )
>          , (modMask x .|. shiftMask, xK_q )
>          ]                                              

Another even simpler possibility is the use of some of the utilities
provided by the xmonad-contrib library. Look, for instance, at
'XMonad.Util.EZConfig.removeKeys'.

-}

{- $keyAddDel

keyAddDel

-}

{- $layout

Layouts
-}

{- $hooks

Hooks
-}

--------------------------------------------------------------------------------
--
--  Writing Extensions
--
--------------------------------------------------------------------------------

{- $writing

Writing Other Extensions

-}