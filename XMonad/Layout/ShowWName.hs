{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE CPP                   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ShowWName
-- Description :  A layout modifier that will show the workspace name.
-- Copyright   :  (c) Andrea Rossato 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a layout modifier that will show the workspace name
-----------------------------------------------------------------------------

module XMonad.Layout.ShowWName
    ( -- * Usage
      -- $usage
      showWName
    , showWName'
    , def
    , SWNConfig(..)
    , ShowWName
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
import XMonad.Util.Font
import XMonad.Util.Timer
import XMonad.Util.XUtils

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ShowWName
-- > myLayout = layoutHook def
-- > main = xmonad def { layoutHook = showWName myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A layout modifier to show the workspace name when switching
showWName :: l a -> ModifiedLayout ShowWName l a
showWName = ModifiedLayout (SWN True def Nothing)

-- | A layout modifier to show the workspace name when switching. It
-- is possible to provide a custom configuration.
showWName' :: SWNConfig -> l a -> ModifiedLayout ShowWName l a
showWName' c = ModifiedLayout (SWN True c Nothing)

type ShowWNState = Maybe (TimerId, Window)
data ShowWName a = SWN Bool SWNConfig ShowWNState deriving (Read, Show)

data SWNConfig =
    SWNC { swn_font    :: String   -- ^ Font name
         , swn_bgcolor :: String   -- ^ Background color
         , swn_color   :: String   -- ^ String color
         , swn_fade    :: Rational -- ^ Time in seconds of the name visibility
    } deriving (Read, Show)

instance Default SWNConfig where
  def =
#ifdef XFT
    SWNC { swn_font    = "xft:monospace-20"
#else
    SWNC { swn_font    = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
#endif
         , swn_bgcolor = "black"
         , swn_color   = "white"
         , swn_fade    = 1
         }

instance LayoutModifier ShowWName a where
    redoLayout      sn r _ = doShow sn r

    handleMess (SWN _ c (Just (i,w))) m
        | Just e    <- fromMessage m = handleTimer i e (deleteWindow w >> return Nothing)
        | Just Hide <- fromMessage m = do deleteWindow w
                                          return . Just $ SWN True c Nothing

    handleMess (SWN _ c s) m
        | Just Hide <- fromMessage m = return . Just $ SWN True c s
        | otherwise                  = return Nothing

doShow :: ShowWName a -> Rectangle -> [(a,Rectangle)] -> X ([(a, Rectangle)], Maybe (ShowWName a))
doShow (SWN True  c (Just (_,w))) r wrs = deleteWindow w >> flashName c r wrs
doShow (SWN True  c  Nothing    ) r wrs = flashName c r wrs
doShow (SWN False _  _          ) _ wrs = return (wrs, Nothing)

flashName :: SWNConfig -> Rectangle -> [(a, Rectangle)] -> X ([(a, Rectangle)], Maybe (ShowWName a))
flashName c (Rectangle sx sy wh ht) wrs = do
  d <- asks display
  n <- withWindowSet (return . S.currentTag)
  f <- initXMF (swn_font c)
  width <- (\w -> w + w `div` length n) <$> textWidthXMF d f n
  (as,ds) <- textExtentsXMF f n
  let hight = as + ds
      y     = fi sy + (fi ht - hight + 2) `div` 2
      x     = fi sx + (fi wh - width + 2) `div` 2
  w <- createNewWindow (Rectangle (fi x) (fi y) (fi width) (fi hight)) Nothing "" True
  showWindow w
  paintAndWrite w f (fi width) (fi hight) 0 (swn_bgcolor c) "" (swn_color c) (swn_bgcolor c) [AlignCenter] [n]
  releaseXMF f
  i <- startTimer (swn_fade c)
  return (wrs, Just $ SWN False c $ Just (i,w))
