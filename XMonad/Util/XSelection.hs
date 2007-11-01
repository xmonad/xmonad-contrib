-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.XSelection
-- Copyright   :  (C) 2007 Andrea Rossato, Matthew Sackman
-- License     :  BSD3
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unibz.it>,
--                Matthew Sackman <matthew@wellquite.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for accessing and manipulating the X Window mouse selection (used in copy and pasting).
-- getSelection and putSelection are adaptations of Hxsel.hs and Hxput.hs from XMonad-utils, available:
--
-- $ darcs get "http:\/\/gorgias.mine.nu\/repos\/xmonad-utils"
-----------------------------------------------------------------------------

module XMonad.Util.XSelection (
                                 -- * Usage
                                 -- $usage
                                 getSelection,
                                 promptSelection,
                                 safePromptSelection,
                                 putSelection) where

import Graphics.X11.Xlib.Extras (Graphics.X11.Xlib.Extras.Event(ev_event_display,
							       ev_time, ev_property, ev_target, ev_selection,
							       ev_requestor, ev_event_type),
				xConvertSelection, xGetSelectionOwner, xSetSelectionOwner, getEvent,
				currentTime, setSelectionNotify, getWindowProperty8, changeProperty8,
				propModeReplace)
import Graphics.X11.Xlib (destroyWindow, createSimpleWindow, Display, XEventPtr,
			 sendEvent, nextEvent, sync, allocaXEvent, openDisplay, rootWindow,
			 defaultScreen, internAtom, Atom, selectionNotify, selectionRequest, noEventMask)
import Control.Concurrent (forkIO)
import Control.Exception as E (catch)
import Control.Monad(Monad (return, (>>)), Functor(..), liftM, join)
import Data.Char (chr, ord)
import Data.Maybe (fromMaybe)
import Foreign(Word8, Data.Bits.Bits (shiftL, (.&.), (.|.)))
import XMonad.Util.Run (safeSpawn, unsafeSpawn)
import XMonad (X, io)

{- $usage
    Add 'import XMonadContrib.XSelection' to the top of Config.hs
    Then make use of getSelection or promptSelection as needed; if
    one wanted to run Firefox with the selection as an argument (say,
    the selection is an URL you just highlighted), then one could add
    to the Config.hs a line like thus:

>  , ((modMask .|. shiftMask, xK_b     ), promptSelection "firefox")

    TODO:

             * Fix Unicode handling. Currently it's still better than calling
               'chr' to translate to ASCII, though.
               As near as I can tell, the mangling happens when the String is
               outputted somewhere, such as via promptSelection's passing through
               the shell, or GHCi printing to the terminal. utf-string has IO functions
               which can fix this, though I do not know have to use them here. It's
               a complex issue; see
               <http://www.haskell.org/pipermail/xmonad/2007-September/001967.html>
               and <http://www.haskell.org/pipermail/xmonad/2007-September/001966.html>.

             * Possibly add some more elaborate functionality: Emacs' registers are nice.
-}

-- | Returns a String corresponding to the current mouse selection in X; if there is none, an empty string is returned. Note that this is
-- really only reliable for ASCII text and currently escapes or otherwise mangles more complex UTF-8 characters.
getSelection :: IO String
getSelection = do
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
               return $ decode  . fromMaybe [] $ res
       else destroyWindow dpy win >> return ""

-- | Set the current X Selection to a given String.
putSelection :: String -> IO ()
putSelection text = do
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
                                                    -- selection == eg PRIMARY
                                                    -- target == type eg UTF8
                                                    -- property == property name or None
                                                               allocaXEvent $ \replyPtr -> do
                                                                 changeProperty8 (ev_event_display ev)
                                                                                 (ev_requestor ev)
                                                                                 (ev_property ev)
                                                                                 ty
                                                                                 propModeReplace
                                                                                 (map (fromIntegral . ord) txt)
                                                                 setSelectionNotify replyPtr (ev_requestor ev) (ev_selection ev) (ev_target ev) (ev_property ev) (ev_time ev)
                                                                 sendEvent dpy (ev_requestor ev) False noEventMask replyPtr
                                                                 sync dpy False
                                                       else do putStrLn "Unexpected Message Received"
                                                               print ev
                                                      processEvent dpy ty text e

{- | A wrapper around getSelection. Makes it convenient to run a program with the current selection as an argument.
This is convenient for handling URLs, in particular. For example, in your Config.hs you could bind a key to
         @promptSelection \"firefox\"@;
this would allow you to highlight a URL string and then immediately open it up in Firefox.

promptSelection passes strings through the shell; if you do not wish your selected text to be interpreted/mangled
by the shell, use safePromptSelection which will bypass the shell using safeSpawn from Run.hs; see Run.hs for more
details on the advantages/disadvantages of this. -}
promptSelection, safePromptSelection, unsafePromptSelection :: String -> X ()
promptSelection = unsafePromptSelection
safePromptSelection app = join $ io $ liftM (safeSpawn app) (getSelection)
unsafePromptSelection app = join $ io $ liftM unsafeSpawn $ fmap (\x -> app ++ " " ++ x) getSelection

{- UTF-8 decoding for internal use in getSelection. This code is copied from Eric Mertens's utf-string library
   <http://code.haskell.org/utf8-string/> (version 0.1), which is BSD-3 licensed, as is this module.
   It'd be better to just import Codec.Binary.UTF8.String (decode), but then users of this would need to install it; Xmonad has enough
   dependencies already. -}
decode :: [Word8] -> String
decode [    ] = ""
decode (c:cs)
  | c < 0x80  = chr (fromEnum c) : decode cs
  | c < 0xc0  = replacement_character : decode cs
  | c < 0xe0  = multi_byte 1 0x1f 0x80
  | c < 0xf0  = multi_byte 2 0xf  0x800
  | c < 0xf8  = multi_byte 3 0x7  0x10000
  | c < 0xfc  = multi_byte 4 0x3  0x200000
  | c < 0xfe  = multi_byte 5 0x1  0x4000000
  | otherwise = replacement_character : decode cs
  where
    replacement_character :: Char
    replacement_character = '\xfffd'

    multi_byte :: Int -> Word8 -> Int -> [Char]
    multi_byte i mask overlong = aux i cs (fromEnum (c .&. mask))
      where
        aux :: Int -> [Word8] -> Int -> [Char]
        aux 0 rs acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = chr acc : decode rs
          | otherwise = replacement_character : decode rs
        aux n (r:rs) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs
                               $ shiftL acc 6 .|. fromEnum (r .&. 0x3f)
        aux _ rs     _ = replacement_character : decode rs
