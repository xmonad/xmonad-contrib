{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Decoration
-- Copyright   :  (c) 2010 Audun Skaugen
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  audunskaugen@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Hooks for sending messages about fullscreen windows to layouts, and
-- a few example layout modifier that implement fullscreen windows.
-----------------------------------------------------------------------------
module XMonad.Layout.Fullscreen 
    ( -- * Usage:
      -- $usage
     fullscreenFull
    ,fullscreenFocus
    ,fullscreenFullRect
    ,fullscreenFocusRect
    ,fullscreenFloat
    ,fullscreenFloatRect
    ,fullscreenEventHook
    ,fullscreenManageHook
    ,fullscreenManageHookWith
    ,FullscreenMessage(..)
    ) where

import XMonad
import XMonad.Layout.LayoutModifier
import XMonad.Util.WindowProperties
import XMonad.Hooks.ManageHelpers (isFullscreen)
import qualified XMonad.StackSet as W
import Data.List
import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Control.Monad
import Control.Arrow (second)

-- $usage
-- Provides a ManageHook and an EventHook that sends layout messages
-- with information about fullscreening windows. This allows layouts
-- to make their own decisions about what they should to with a
-- window that requests fullscreen.
--
-- The module also includes a few layout modifiers as an illustration
-- of how such layouts should behave.
-- 
-- To use this module, add 'fullscreenEventHook' and 'fullscreenManageHook'
-- to your config, i.e.
--
-- > xmonad defaultconfig { eventHook = fullscreenEventHook,
-- >                        manageHook = fullscreenManageHook,
-- >                        layoutHook = myLayouts }
--
-- Now you can use layouts that respect fullscreen, for example the
-- provided 'fullscreenFull':
--
-- > myLayouts = fullscreenFull someLayout
--

-- | Messages that control the fullscreen state of the window.
-- AddFullscreen and RemoveFullscreen are sent to all layouts 
-- when a window wants or no longer wants to be fullscreen. 
-- FullscreenChanged is sent to the current layout after one
-- of the above have been sent. 
data FullscreenMessage = AddFullscreen Window
                       | RemoveFullscreen Window
                       | FullscreenChanged
     deriving (Typeable)

instance Message FullscreenMessage

data FullscreenFull a = FullscreenFull W.RationalRect [a]
     deriving (Read, Show)

data FullscreenFocus a = FullscreenFocus W.RationalRect [a]
     deriving (Read, Show)

data FullscreenFloat a = FullscreenFloat W.RationalRect (M.Map a (W.RationalRect, Bool))
     deriving (Read, Show)

instance LayoutModifier FullscreenFull Window where
  pureMess (FullscreenFull frect fulls) m = case fromMessage m of
    Just (AddFullscreen win) -> Just $ FullscreenFull frect $ nub $ win:fulls 
    Just (RemoveFullscreen win) -> Just $ FullscreenFull frect $ delete win $ fulls
    _ -> Nothing
  
  pureModifier (FullscreenFull frect fulls) rect _ list = 
    (map (flip (,) rect') visfulls ++ rest, Nothing)
    where visfulls = intersect fulls $ map fst list
          rest = filter (flip notElem visfulls . fst) list 
          rect' = scaleRationalRect rect frect

instance LayoutModifier FullscreenFocus Window where
  pureMess (FullscreenFocus frect fulls) m = case fromMessage m of
    Just (AddFullscreen win) -> Just $ FullscreenFocus frect $ nub $ win:fulls 
    Just (RemoveFullscreen win) -> Just $ FullscreenFocus frect $ delete win $ fulls 
    _ -> Nothing

  pureModifier (FullscreenFocus frect fulls) rect (Just (W.Stack {W.focus = f})) list 
     | f `elem` fulls = ((f, rect') : rest, Nothing)
     | otherwise = (list, Nothing)
     where rest = filter ((/= f) . fst) list
           rect' = scaleRationalRect rect frect
  pureModifier _ _ Nothing list = (list, Nothing)

instance LayoutModifier FullscreenFloat Window where
  handleMess (FullscreenFloat frect fulls) m = case fromMessage m of
    Just (AddFullscreen win) -> do
      mrect <- (M.lookup win . W.floating) `fmap` gets windowset
      return $ case mrect of
        Just rect -> Just $ FullscreenFloat frect $ M.insert win (rect,True) fulls
        Nothing -> Nothing

    Just (RemoveFullscreen win) ->
      return $ Just $ FullscreenFloat frect $ M.adjust (second $ const False) win fulls

    -- Modify the floating member of the stack set directly; this is the hackish part.
    Just FullscreenChanged -> do
      state <- get
      let ws = windowset state
          flt = W.floating ws
          flt' = M.intersectionWith doFull fulls flt
      put state {windowset = ws {W.floating = M.union flt' flt}}
      return $ Just $ FullscreenFloat frect $ M.filter snd fulls
      where doFull (_, True) _ = frect 
            doFull (rect, False) _ = rect
   
    Nothing -> return Nothing

-- | Layout modifier that makes fullscreened window fill the
-- entire screen.
fullscreenFull :: LayoutClass l a => 
  l a -> ModifiedLayout FullscreenFull l a
fullscreenFull = fullscreenFullRect $ W.RationalRect 0 0 1 1

-- | As above, but the fullscreened window will fill the 
-- specified rectangle instead of the entire screen.
fullscreenFullRect :: LayoutClass l a => 
  W.RationalRect -> l a -> ModifiedLayout FullscreenFull l a
fullscreenFullRect r = ModifiedLayout $ FullscreenFull r []

-- | Layout modifier that makes the fullscreened window fill
-- the entire screen only if it is currently focused.
fullscreenFocus :: LayoutClass l a => 
  l a -> ModifiedLayout FullscreenFocus l a
fullscreenFocus = fullscreenFocusRect $ W.RationalRect 0 0 1 1

-- | As above, but the fullscreened window will fill the 
-- specified rectangle instead of the entire screen.
fullscreenFocusRect :: LayoutClass l a => 
  W.RationalRect -> l a -> ModifiedLayout FullscreenFocus l a
fullscreenFocusRect r = ModifiedLayout $ FullscreenFocus r []

-- | Hackish layout modifier that makes floating fullscreened
-- windows fill the entire screen.
fullscreenFloat :: LayoutClass l a =>
  l a -> ModifiedLayout FullscreenFloat l a
fullscreenFloat = fullscreenFloatRect $ W.RationalRect 0 0 1 1

-- | As above, but the fullscreened window will fill the 
-- specified rectangle instead of the entire screen.
fullscreenFloatRect :: LayoutClass l a =>
  W.RationalRect -> l a -> ModifiedLayout FullscreenFloat l a
fullscreenFloatRect r = ModifiedLayout $ FullscreenFloat r M.empty

-- | The event hook required for the layout modifiers to work
fullscreenEventHook :: Event -> X All
fullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  state <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 state win
  let fi :: (Integral i, Num n) => i -> n
      fi = fromIntegral
      isFull = fi fullsc `elem` wstate
      remove = 0
      add = 1
      toggle = 2
      ptype = 4
      chWState f = io $ changeProperty32 dpy win state ptype propModeReplace (f wstate)
  when (typ == state && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWState (fi fullsc:)
      broadcastMessage $ AddFullscreen win
      sendMessage FullscreenChanged
    when (action == remove || (action == toggle && isFull)) $ do
      chWState $ delete (fi fullsc)
      broadcastMessage $ RemoveFullscreen win
      sendMessage FullscreenChanged
  return $ All True

fullscreenEventHook (DestroyWindowEvent {ev_window = w}) = do
  -- When a window is destroyed, the layouts should remove that window
  -- from their states. 
  broadcastMessage $ RemoveFullscreen w
  cw <- (W.workspace . W.current) `fmap` gets windowset
  sendMessageWithNoRefresh FullscreenChanged cw
  return $ All True

fullscreenEventHook _ = return $ All True

-- | Manage hook that sets the fullscreen property for
-- windows that are initially fullscreen
fullscreenManageHook :: ManageHook
fullscreenManageHook = fullscreenManageHook' isFullscreen

-- | A version of fullscreenManageHook that lets you specify
-- your own query to decide whether a window should be fullscreen.
fullscreenManageHookWith :: Query Bool -> ManageHook
fullscreenManageHookWith h = fullscreenManageHook' $ isFullscreen <||> h

fullscreenManageHook' :: Query Bool -> ManageHook
fullscreenManageHook' isFull = isFull --> do
  w <- ask 
  liftX $ do
    broadcastMessage $ AddFullscreen w
    cw <- (W.workspace . W.current) `fmap` gets windowset
    sendMessageWithNoRefresh FullscreenChanged cw
  idHook
    
