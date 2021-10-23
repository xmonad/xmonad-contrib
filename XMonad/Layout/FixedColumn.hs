{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FixedColumn
-- Description :  Like Tall, but split at a fixed column (or a window's smallest resize amount).
-- Copyright   :  (c) 2008 Justin Bogner <mail@justinbogner.com>
-- License     :  BSD3-style (as xmonad)
--
-- Maintainer  :  Justin Bogner <mail@justinbogner.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout much like Tall, but using a multiple of a window's minimum
-- resize amount instead of a percentage of screen to decide where to
-- split. This is useful when you usually leave a text editor or
-- terminal in the master pane and like it to be 80 columns wide.
--
-----------------------------------------------------------------------------

module XMonad.Layout.FixedColumn (
    -- * Usage
    -- $usage
        FixedColumn(..)
) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.FixedColumn
--
-- Then edit your @layoutHook@ by adding the FixedColumn layout:
--
-- > myLayout = FixedColumn 1 20 80 10 ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | A tiling mode based on preserving a nice fixed width
--   window. Supports 'Shrink', 'Expand' and 'IncMasterN'.
data FixedColumn a = FixedColumn !Int -- Number of windows in the master pane
                                 !Int -- Number to increment by when resizing
                                 !Int -- Default width of master pane
                                 !Int -- Column width for normal windows
                        deriving (Read, Show)

instance LayoutClass FixedColumn Window where
    doLayout (FixedColumn nmaster _ ncol fallback) r s = do
            fws <- mapM (widthCols fallback ncol) ws
            let frac = maximum (take nmaster fws) // rect_width r
                rs   = tile frac r nmaster (length ws)
            return (zip ws rs, Nothing)
        where ws     = W.integrate s
              x // y = fromIntegral x / fromIntegral y

    pureMessage (FixedColumn nmaster delta ncol fallback) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]
        where resize Shrink
                  = FixedColumn nmaster delta (max 0 $ ncol - delta) fallback
              resize Expand
                  = FixedColumn nmaster delta (ncol + delta) fallback
              incmastern (IncMasterN d)
                  = FixedColumn (max 0 (nmaster+d)) delta ncol fallback

    description _ = "FixedColumn"

-- | Determine the width of @w@ given that we would like it to be @n@
--   columns wide, using @inc@ as a resize increment for windows that
--   don't have one
widthCols :: Int -> Int -> Window -> X Int
widthCols inc n w = do
    d  <- asks display
    bw <- asks $ fi . borderWidth . config
    sh <- io $ getWMNormalHints d w
    let widthHint f = f sh <&> fromIntegral . fst
        oneCol      = fromMaybe inc $ widthHint sh_resize_inc
        base        = fromMaybe 0 $ widthHint sh_base_size
    return $ 2 * bw + base + n * oneCol
