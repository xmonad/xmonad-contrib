{-# LANGUAGE CPP #-}
{- |
Module      :  XMonad.Util.XSelection
Description :  A module for accessing and manipulating the primary selection.
Copyright   :  (C) 2007 Andrea Rossato, Matthew Sackman
License     :  BSD3

Maintainer  : Gwern Branwen <gwern0@gmail.com>
Stability   :  unstable
Portability :  unportable

A module for accessing and manipulating X Window's mouse selection (the buffer used in copy and pasting).
'getSelection' is an adaptation of Hxsel.hs and Hxput.hs from the XMonad-utils, available:

> $ darcs get <http://gorgias.mine.nu/repos/xmonad-utils>
-}

module XMonad.Util.XSelection (  -- * Usage
                                 -- $usage
                                 getSelection,
                                 promptSelection,
                                 safePromptSelection,
                                 transformPromptSelection,
                                 transformSafePromptSelection) where

import Control.Exception as E (catch,SomeException(..))
import XMonad
import XMonad.Util.Run (safeSpawn, unsafeSpawn)

import Codec.Binary.UTF8.String (decode)

{- $usage
   Add @import XMonad.Util.XSelection@ to the top of Config.hs
   Then make use of getSelection or promptSelection as needed; if
   one wanted to run Firefox with the selection as an argument (perhaps
   the selection string is an URL you just highlighted), then one could add
   to the xmonad.hs a line like thus:

   > , ((modm .|. shiftMask, xK_b), promptSelection "firefox")

   Future improvements for XSelection:

   * More elaborate functionality: Emacs' registers are nice; if you
      don't know what they are, see <http://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html#Registers> -}

-- | Returns a String corresponding to the current mouse selection in X;
--   if there is none, an empty string is returned.
--
-- WARNING: this function is fundamentally implemented incorrectly and may, among other possible failure modes,
-- deadlock or crash. For details, see <http://code.google.com/p/xmonad/issues/detail?id=573>.
-- (These errors are generally very rare in practice, but still exist.)
getSelection :: MonadIO m => m String
getSelection = io $ do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  p <- internAtom dpy "PRIMARY" True
  ty <- E.catch
               (E.catch
                     (internAtom dpy "UTF8_STRING" False)
                     (\(E.SomeException _) -> internAtom dpy "COMPOUND_TEXT" False))
             (\(E.SomeException _) -> internAtom dpy "sTring" False)
  clp <- internAtom dpy "BLITZ_SEL_STRING" False
  xConvertSelection dpy p ty clp win currentTime
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    result <- if ev_event_type ev == selectionNotify
                 then do res <- getWindowProperty8 dpy clp win
                         return $ decode . maybe [] (map fromIntegral) $ res
                 else destroyWindow dpy win >> return ""
    closeDisplay dpy
    return result

{- | A wrapper around 'getSelection'. Makes it convenient to run a program with the current selection as an argument.
  This is convenient for handling URLs, in particular. For example, in your Config.hs you could bind a key to
         @promptSelection \"firefox\"@;
  this would allow you to highlight a URL string and then immediately open it up in Firefox.

  'promptSelection' passes strings through the system shell, \/bin\/sh; if you do not wish your selected text
  to be interpreted or mangled by the shell, use 'safePromptSelection'. safePromptSelection will bypass the
  shell using 'safeSpawn' from "XMonad.Util.Run"; see its documentation for more
  details on the advantages and disadvantages of using safeSpawn. -}
promptSelection, safePromptSelection, unsafePromptSelection :: String -> X ()
promptSelection = unsafePromptSelection
safePromptSelection app = safeSpawn app . return =<< getSelection
unsafePromptSelection app = unsafeSpawn . (\x -> app ++ " " ++ x) =<< getSelection

{- | A wrapper around 'promptSelection' and its safe variant. They take two parameters, the
     first is a function that transforms strings, and the second is the application to run.
     The transformer essentially transforms the selection in X.
     One example is to wrap code, such as a command line action copied out of the browser
     to be run as @"sudo" ++ cmd@ or @"su - -c \""++ cmd ++"\""@. -}
transformPromptSelection, transformSafePromptSelection :: (String -> String) -> String -> X ()
transformPromptSelection f app = (safeSpawn app . return . f) =<< getSelection
transformSafePromptSelection f app = unsafeSpawn . (\x -> app ++ " " ++ x) . f =<< getSelection
