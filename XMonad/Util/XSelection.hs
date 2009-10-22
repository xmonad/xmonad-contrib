{-# LANGUAGE CPP #-}
{- |
Module      :  XMonad.Util.XSelection
Copyright   :  (C) 2007 Andrea Rossato, Matthew Sackman
License     :  BSD3

Maintainer  : Gwern Branwen <gwern0@gmail.com>
Stability   :  unstable
Portability :  unportable

A module for accessing and manipulating X Window's mouse selection (the buffer used in copy and pasting).
'getSelection' and 'putSelection' are adaptations of Hxsel.hs and Hxput.hs from the XMonad-utils, available:

> $ darcs get <http://gorgias.mine.nu/repos/xmonad-utils>
-}

module XMonad.Util.XSelection (  -- * Usage
                                 -- $usage
                                 getSelection,
                                 promptSelection,
                                 safePromptSelection,
                                 transformPromptSelection,
                                 transformSafePromptSelection,
                                 putSelection) where

import Control.Concurrent (forkIO)
import Control.Exception as E (catch)
import Control.Monad(Monad (return, (>>)), Functor(..), liftM, join)
import Data.Char (ord)
import Data.Maybe (fromMaybe)
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

   There are a number of known problems with XSelection:

    * Unicode handling is busted. But it's still better than calling
      'chr' to translate to ASCII, at least.
      As near as I can tell, the mangling happens when the String is
      outputted somewhere, such as via promptSelection's passing through
      the shell, or GHCi printing to the terminal. utf-string has IO functions
      which can fix this, though I do not know have to use them here. It's
      a complex issue; see
      <http://www.haskell.org/pipermail/xmonad/2007-September/001967.html>
      and <http://www.haskell.org/pipermail/xmonad/2007-September/001966.html>.

    * Needs more elaborate functionality: Emacs' registers are nice; if you
      don't know what they are, see <http://www.gnu.org/software/emacs/manual/html_node/emacs/Registers.html#Registers> -}

-- | Returns a String corresponding to the current mouse selection in X; if there is none, an empty string is returned. Note that this is
-- really only reliable for ASCII text and currently escapes or otherwise mangles more complex UTF-8 characters.
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
                     (\_ -> internAtom dpy "COMPOUND_TEXT" False))
             (\_ -> internAtom dpy "sTring" False)
  clp <- internAtom dpy "BLITZ_SEL_STRING" False
  xConvertSelection dpy p ty clp win currentTime
  allocaXEvent $ \e -> do
    nextEvent dpy e
    ev <- getEvent e
    if ev_event_type ev == selectionNotify
       then do res <- getWindowProperty8 dpy clp win
               return $ decode . map fromIntegral . fromMaybe [] $ res
       else destroyWindow dpy win >> return ""

-- | Set the current X Selection to a specified string.
putSelection :: MonadIO m => String -> m ()
putSelection text = io $ do
  dpy <- openDisplay ""
  let dflt = defaultScreen dpy
  rootw  <- rootWindow dpy dflt
  win <- createSimpleWindow dpy rootw 0 0 1 1 0 0 0
  p <- internAtom dpy "PRIMARY" True
  ty <- internAtom dpy "UTF8_STRING" False
  xSetSelectionOwner dpy p win currentTime
  winOwn <- xGetSelectionOwner dpy p
  if winOwn == win
    then do forkIO ((allocaXEvent $ processEvent dpy ty text) >> destroyWindow dpy win) >> return ()
    else do putStrLn "Unable to obtain ownership of the selection" >> destroyWindow dpy win
  return ()
                     where
                       processEvent :: Display -> Atom -> [Char] -> XEventPtr -> IO ()
                       processEvent dpy ty txt e = do
                                                      nextEvent dpy e
                                                      ev <- getEvent e
                                                      if ev_event_type ev == selectionRequest
                                                       then do print ev
                                                               allocaXEvent $ \replyPtr -> do
                                                                 changeProperty8 (ev_event_display ev)
                                                                                 (ev_requestor ev)
                                                                                 (ev_property ev)
                                                                                 ty
                                                                                 propModeReplace
                                                                                 (map (fromIntegral . ord) txt)
                                                                 setSelectionNotify replyPtr (ev_requestor ev) (ev_selection ev)
                                                                                        (ev_target ev) (ev_property ev) (ev_time ev)
                                                                 sendEvent dpy (ev_requestor ev) False noEventMask replyPtr
                                                                 sync dpy False
                                                       else do putStrLn "Unexpected Message Received"
                                                               print ev
                                                      processEvent dpy ty text e

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
safePromptSelection app = join $ io $ liftM (safeSpawn app . return) getSelection
unsafePromptSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

{- | A wrapper around 'promptSelection' and its safe variant. They take two parameters, the first is a function that transforms strings, and the second is the application to run. The transformer essentially transforms the selection in X.
One example is to wrap code, such as a command line action copied out of the browser to be run as @"sudo" ++ cmd@ or @"su - -c \""++ cmd ++"\""@.
-}
transformPromptSelection, transformSafePromptSelection :: (String -> String) -> String -> X ()
transformPromptSelection f app = join $ io $ liftM (safeSpawn app . return) (fmap f getSelection)
transformSafePromptSelection f app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) (fmap f getSelection)
