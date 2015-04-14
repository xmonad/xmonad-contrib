{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Hidden
-- Copyright   :  (c) Peter Jones 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  pjones@devalot.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Similar to "XMonad.Layout.Minimize" but completely removes windows
-- from the window set so "XMonad.Layout.BoringWindows" isn't
-- necessary.  Perfect companion to
-- "XMonad.Layout.BinarySpacePartition" since it can be used to move
-- windows to another part of the BSP tree.
--
-----------------------------------------------------------------------------
module XMonad.Layout.Hidden
       ( -- * Usage
         -- $usage
         HiddenMsg (..)
       , hiddenWindows
       , hideWindow
       , popOldestHiddenWindow
       , popNewestHiddenWindow
       ) where

--------------------------------------------------------------------------------
import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

--------------------------------------------------------------------------------
-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Hidden
--
-- Then edit your @layoutHook@ by adding the @HiddenWindows@ layout modifier:
--
-- > myLayout = hiddenWindows (Tall 1 (3/100) (1/2)) ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In the key bindings, do something like:
--
-- >        , ((modMask, xK_backslash), withFocused hideWindow)
-- >        , ((modMask .|. shiftMask, xK_backslash), popOldestHiddenWindow)
-- >        ...
--
-- For detailed instruction on editing the key bindings see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

--------------------------------------------------------------------------------
data HiddenWindows a = HiddenWindows [Window] deriving (Show, Read)

--------------------------------------------------------------------------------
-- | Messages for the @HiddenWindows@ layout modifier.
data HiddenMsg = HideWindow Window       -- ^ Hide a window.
               | PopNewestHiddenWindow   -- ^ Restore window (FILO).
               | PopOldestHiddenWindow   -- ^ Restore window (FIFO).
               deriving (Typeable, Eq)

instance Message HiddenMsg

--------------------------------------------------------------------------------
instance LayoutModifier HiddenWindows Window where
  handleMess h@(HiddenWindows hidden) mess
    | Just (HideWindow win)        <- fromMessage mess = hideWindowMsg h win
    | Just (PopNewestHiddenWindow) <- fromMessage mess = popNewestMsg h
    | Just (PopOldestHiddenWindow) <- fromMessage mess = popOldestMsg h
    | Just ReleaseResources        <- fromMessage mess = doUnhook
    | otherwise                                        = return Nothing
    where doUnhook = do mapM_ restoreWindow hidden
                        return Nothing

  modifierDescription _ = "Hidden"

--------------------------------------------------------------------------------
-- | Apply the @HiddenWindows@ layout modifier.
hiddenWindows :: LayoutClass l Window => l Window -> ModifiedLayout HiddenWindows l Window
hiddenWindows = ModifiedLayout $ HiddenWindows []

--------------------------------------------------------------------------------
-- | Remove the given window from the current layout.  It is placed in
-- list of hidden windows so it can be restored later.
hideWindow :: Window -> X ()
hideWindow = sendMessage . HideWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FIFO queue.  That is, the
-- first window hidden will be restored.
popOldestHiddenWindow :: X ()
popOldestHiddenWindow = sendMessage PopOldestHiddenWindow

--------------------------------------------------------------------------------
-- | Restore a previously hidden window.  Using this function will
-- treat the list of hidden windows as a FILO queue.  That is, the
-- most recently hidden window will be restored.
popNewestHiddenWindow :: X ()
popNewestHiddenWindow = sendMessage PopNewestHiddenWindow

--------------------------------------------------------------------------------
hideWindowMsg :: HiddenWindows a -> Window -> X (Maybe (HiddenWindows a))
hideWindowMsg (HiddenWindows hidden) win = do
  windows (W.delete' win)
  return . Just . HiddenWindows $ hidden ++ [win]

--------------------------------------------------------------------------------
popNewestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popNewestMsg (HiddenWindows [])     = return Nothing
popNewestMsg (HiddenWindows hidden) = do
  let (win, rest) = (last hidden, init hidden)
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
popOldestMsg :: HiddenWindows a -> X (Maybe (HiddenWindows a))
popOldestMsg (HiddenWindows [])         = return Nothing
popOldestMsg (HiddenWindows (win:rest)) = do
  restoreWindow win
  return . Just . HiddenWindows $ rest

--------------------------------------------------------------------------------
restoreWindow :: Window -> X ()
restoreWindow = windows . W.insertUp
