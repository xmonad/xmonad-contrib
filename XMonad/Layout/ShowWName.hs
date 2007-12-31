{-# OPTIONS_GHC -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ShowWName
-- Copyright   :  (c) Andrea Rossato 2007
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a layout modifier that will show the workspace name using
-- dzen.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ShowWName
    ( -- * Usage
      -- $usage
      showWName
    , showWName'
    , defaultSWNConfig
    , SWNConfig(..)
    ) where

import XMonad
import qualified XMonad.StackSet as S
import XMonad.Layout.LayoutModifier
import XMonad.Util.Font
import XMonad.Util.Dzen

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.ShowWName
-- > myLayout = layoutHook defaultConfig
-- > main = xmonad defaultConfig { layoutHook = showWName myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | XXX
showWName :: l a -> ModifiedLayout ShowWName l a
showWName = ModifiedLayout (SWN True defaultSWNConfig)

-- | XXX
showWName' :: SWNConfig -> l a -> ModifiedLayout ShowWName l a
showWName' c = ModifiedLayout (SWN True c)

data ShowWName a = SWN Bool SWNConfig deriving (Read, Show)

data SWNConfig = 
    SWNC { swn_font    :: String
         , swn_bgcolor :: String
         , swn_color   :: String
         , swn_fade    :: Rational
    } deriving (Read, Show)

defaultSWNConfig :: SWNConfig
defaultSWNConfig =
    SWNC { swn_font    = "-misc-fixed-*-*-*-*-20-*-*-*-*-*-*-*"
         , swn_bgcolor = "black"
         , swn_color   = "white"
         , swn_fade    = 1
         }

instance LayoutModifier ShowWName Window where
    redoLayout (SWN True  c) r _ wrs = flashName c r >> return (wrs, Just $ SWN False c)
    redoLayout (SWN False _) _ _ wrs = return (wrs, Nothing)

    handleMess (SWN _ c) m
                    | Just Hide <- fromMessage m = return . Just $ SWN True c
                    | otherwise                  = return Nothing

flashName :: SWNConfig -> Rectangle -> X ()
flashName c (Rectangle _ _ wh ht) = do
  d <- asks display
  n <- withWindowSet (return . S.tag . S.workspace . S.current)
  f <- initXMF (swn_font c)
  width       <- textWidthXMF d f n
  (_,as,ds,_) <- textExtentsXMF f n
  releaseXMF f
  let hight = as + ds + 2
      y     = (fromIntegral ht - hight) `div` 2
      x     = (fromIntegral wh - width) `div` 2
      args  = [ "-fn", swn_font    c
              , "-fg", swn_color   c
              , "-bg", swn_bgcolor c
              , "-x" , show x
              , "-y" , show y
              , "-w" , show $ 3 * (width + 2)
              ]
  dzenWithArgs n args ((swn_fade c) `seconds`)
