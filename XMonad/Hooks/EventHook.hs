{-# OPTIONS_GHC -fglasgow-exts #-} -- for deriving Typeable
{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.EventHook
-- Copyright   :  (c) 2007 Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier that implements an event hook at the layout level.
--
-- Since it operates at the 'Workspace' level, it will install itself
-- on the first current 'Workspace' and will broadcast a 'Message' to
-- all other 'Workspace's not to handle events.
-----------------------------------------------------------------------------

module XMonad.Hooks.EventHook
    ( -- * Usage
      -- $usage

      -- * Writing a hook
      -- $hook
      EventHook (..)
    , eventHook
    , HandleEvent
    ) where

import Data.Maybe

import XMonad
import XMonad.StackSet (Workspace (..), currentTag)

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.EventHook
--
-- Then edit your @layoutHook@ by adding the 'eventHook':
--
-- > layoutHook = eventHook EventHookExample $ avoidStruts $ simpleTabbed ||| Full ||| etc..
--
-- and then:
--
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- $hook
-- Writing a hook is very simple.
--
-- This is a basic example to log all events:
--
-- > data EventHookExample = EventHookExample deriving ( Show, Read )
-- > instance EventHook EventHookExample where
-- >     handleEvent _ e = io $ hPutStrLn stderr . show $ e --return ()
--
-- This is an 'EventHook' to log mouse button events:
--
-- > data EventHookButton = EventHookButton deriving ( Show, Read )
-- > instance EventHook EventHookButton where
-- >    handleEvent _ (ButtonEvent {ev_window = w}) = do
-- >         io $ hPutStrLn stderr $ "This is a button event on window " ++ (show w)
-- >    handleEvent _ _ = return ()
--
-- Obviously you can compose event hooks:
--
-- > layoutHook = eventHook EventHookButton $ eventHook EventHookExample $ avoidStruts $ simpleTabbed ||| Full ||| etc..

eventHook :: EventHook eh => eh -> l a -> (HandleEvent eh l) a
eventHook = HandleEvent Nothing True

class (Read eh, Show eh) => EventHook eh where
    handleEvent :: eh -> Event -> X ()
    handleEvent _ _ = return ()

data HandleEvent eh l a = HandleEvent (Maybe WorkspaceId) Bool eh (l a) deriving ( Show, Read )

data EventHandleMsg = HandlerOff deriving ( Typeable )
instance Message EventHandleMsg

instance (EventHook eh, LayoutClass l a) => LayoutClass (HandleEvent eh l) a where
    runLayout (Workspace i (HandleEvent Nothing True eh l) ms) r = do
      broadcastMessage HandlerOff
      iws       <- gets (currentTag . windowset)
      (wrs, ml) <- runLayout (Workspace i l ms) r
      return (wrs, Just $ HandleEvent (Just iws) True eh (fromMaybe l ml))

    runLayout (Workspace i (HandleEvent mi b eh l) ms) r = do
      (wrs, ml) <- runLayout (Workspace i l ms) r
      return (wrs, Just $ HandleEvent mi b eh (fromMaybe l ml))

    handleMessage (HandleEvent i True eh l) m
        | Just HandlerOff <- fromMessage m = return . Just $ HandleEvent i False eh l
        | Just e          <- fromMessage m = handleMessage l (SomeMessage e) >>= \ml ->
                                             handleEvent eh e >>
                                             maybe (return Nothing) (\l' -> return . Just $ HandleEvent i True eh l') ml
    handleMessage (HandleEvent i b eh l) m = handleMessage l m >>=
                                             maybe (return Nothing) (\l' -> return . Just $ HandleEvent i b    eh l')

    description (HandleEvent _ _ _ l) = description l
