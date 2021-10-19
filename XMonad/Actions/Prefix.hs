{-# LANGUAGE FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Prefix
-- Description :  Use an Emacs-style prefix argument for commands.
-- Copyright   :  (c) Matus Goljer <matus.goljer@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Matus Goljer <matus.goljer@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module that allows the user to use a prefix argument (raw or numeric).
--
-----------------------------------------------------------------------------

module XMonad.Actions.Prefix
       (
      -- * Usage
      -- $usage

      -- * Installation
      -- $installation

         PrefixArgument(..)
       , usePrefixArgument
       , useDefaultPrefixArgument
       , withPrefixArgument
       , isPrefixRaw
       , isPrefixNumeric
       , ppFormatPrefix
       ) where

import qualified Data.Map as M

import XMonad.Prelude
import XMonad
import XMonad.Util.ExtensibleState as XS
import XMonad.Util.Paste (sendKey)
import XMonad.Actions.Submap (submapDefaultWithKey)
import XMonad.Util.EZConfig (readKeySequence)

{- $usage

This module implements Emacs-style prefix argument.  The argument
comes in two flavours, "Raw" and "Numeric".

To initiate the "prefix mode" you hit the prefix keybinding (default
C-u).  This sets the Raw argument value to 1.  Repeatedly hitting this
key increments the raw value by 1.  The Raw argument is usually used
as a toggle, changing the behaviour of the function called in some way.

An example might be calling "mpc add" to add new song to the playlist,
but with C-u we also clean up the playlist beforehand.

When in the "Raw mode", you can hit numeric keys 0..9 (with no
modifier) to enter a "Numeric argument".  Numeric argument represents
a natural number.  Hitting numeric keys in sequence produces the
decimal number that would result from typing them.  That is, the
sequence C-u 4 2 sets the Numeric argument value to the number 42.

If you have a function which understands the prefix argument, for example:

>    addMaybeClean :: PrefixArgument -> X ()
>    addMaybeClean (Raw _) = spawn "mpc clear" >> spawn "mpc add <file>"
>    addMaybeClean _ = spawn "mpc add <file>"

you can turn it into an X action with the function 'withPrefixArgument'.

Binding it in your config

>    ((modm, xK_a), withPrefixArgument addMaybeClean)

Hitting MOD-a will add the <file> to the playlist while C-u MOD-a will
clear the playlist and then add the file.

You can of course use an anonymous action, like so:

>    ((modm, xK_a), withPrefixArgument $ \prefix -> do
>        case prefix of ...
>    )

If the prefix key is followed by a binding which is unknown to XMonad,
the prefix along with that binding is sent to the active window.

There is one caveat: when you use an application which has a nested
C-u binding, for example C-c C-u in Emacs org-mode, you have to hit
C-g (or any other non-recognized key really) to get out of the "xmonad
grab" and let the C-c C-u be sent to the application.

-}

{- $installation

The simplest way to enable this is to use 'useDefaultPrefixArgument'

>    xmonad $ useDefaultPrefixArgument $ def { .. }

The default prefix argument is C-u.  If you want to customize the
prefix argument, 'usePrefixArgument' can be used:

>    xmonad $ usePrefixArgument "M-u" $ def { .. }

where the key is entered in Emacs style (or "XMonad.Util.EZConfig"
style) notation.  The letter `M` stands for your chosen modifier.  The
function defaults to C-u if the argument could not be parsed.
-}

data PrefixArgument = Raw Int | Numeric Int | None
                      deriving (Read, Show)
instance ExtensionClass PrefixArgument where
  initialValue = None
  extensionType = PersistentExtension

-- | Run 'job' in the 'X' monad and then execute 'cleanup'.  In case
-- of exception, 'cleanup' is executed anyway.
--
-- Return the return value of 'job'.
finallyX :: X a -> X a -> X a
finallyX job cleanup = catchX (job >>= \r -> cleanup >> return r) cleanup

-- | Set up Prefix.  Defaults to C-u when given an invalid key.
--
-- See usage section.
usePrefixArgument :: LayoutClass l Window
                  => String
                  -> XConfig l
                  -> XConfig l
usePrefixArgument prefix conf =
  conf{ keys = M.insert binding (handlePrefixArg [binding]) . keys conf }
 where
  binding = case readKeySequence conf prefix of
    Just [key] -> key
    _          -> (controlMask, xK_u)

-- | Set Prefix up with default prefix key (C-u).
useDefaultPrefixArgument :: LayoutClass l Window
                         => XConfig l
                         -> XConfig l
useDefaultPrefixArgument = usePrefixArgument "C-u"

handlePrefixArg :: [(KeyMask, KeySym)] -> X ()
handlePrefixArg events = do
  ks <- asks keyActions
  logger <- asks (logHook . config)
  flip finallyX (XS.put None >> logger) $ do
    prefix <- XS.get
    case prefix of
      Raw a -> XS.put $ Raw (a + 1)
      None -> XS.put $ Raw 1
      _ -> return ()
    logger
    submapDefaultWithKey defaultKey ks
  where defaultKey key@(m, k) =
          if k `elem` (xK_0 : [xK_1 .. xK_9]) && m == noModMask
          then do
            prefix <- XS.get
            let x = fromJust (Prelude.lookup k keyToNum)
            case prefix of
              Raw _ -> XS.put $ Numeric x
              Numeric a -> XS.put $ Numeric $ a * 10 + x
              None -> return () -- should never happen
            handlePrefixArg (key:events)
          else do
            prefix <- XS.get
            mapM_ (uncurry sendKey) $ case prefix of
              Raw a -> replicate a (head events) ++ [key]
              _ -> reverse (key:events)
        keyToNum = (xK_0, 0) : zip [xK_1 .. xK_9] [1..9]

-- | Turn a prefix-aware X action into an X-action.
--
-- First, fetch the current prefix, then pass it as argument to the
-- original function.  You should use this to "run" your commands.
withPrefixArgument :: (PrefixArgument -> X ()) -> X ()
withPrefixArgument = (>>=) XS.get

-- | Test if 'PrefixArgument' is 'Raw' or not.
isPrefixRaw :: PrefixArgument -> Bool
isPrefixRaw (Raw _) = True
isPrefixRaw _ = False

-- | Test if 'PrefixArgument' is 'Numeric' or not.
isPrefixNumeric :: PrefixArgument -> Bool
isPrefixNumeric (Numeric _) = True
isPrefixNumeric _ = False

-- | Format the prefix using the Emacs convetion for use in a
-- statusbar, like xmobar.
--
-- To add this formatted prefix to printer output, you can set it up
-- like so
--
-- > myPrinter :: PP
-- > myPrinter = def { ppExtras = [ppFormatPrefix] }
--
-- And then add to your status bar using "XMonad.Hooks.StatusBar":
--
-- > mySB = statusBarProp "xmobar" myPrinter
-- > main = xmonad $ withEasySB mySB defToggleStrutsKey def
--
-- Or, directly in your 'logHook' configuration
--
-- > logHook = dynamicLogWithPP myPrinter
ppFormatPrefix :: X (Maybe String)
ppFormatPrefix = do
  prefix <- XS.get
  return $ case prefix of
    Raw n -> Just $ foldr1 (\a b -> a ++ " " ++ b) $ replicate n "C-u"
    Numeric n -> Just $ "C-u " ++ show n
    None -> Nothing
