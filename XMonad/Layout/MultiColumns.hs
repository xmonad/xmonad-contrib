{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.MultiColumns
-- Description :  A layout that tiles windows in a growing number of columns.
-- Copyright   :  (c) Anders Engstrom <ankaan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Anders Engstrom <ankaan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This layout tiles windows in a growing number of columns. The number of
-- windows in each column can be controlled by messages.
-----------------------------------------------------------------------------

module XMonad.Layout.MultiColumns (
                              -- * Usage
                              -- $usage

                              multiCol,
                              MultiCol,
                             ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Prelude

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.MultiColumns
--
-- Then edit your @layoutHook@ by adding the multiCol layout:
--
-- > myLayouts = multiCol [1] 4 0.01 0.5 ||| etc..
-- > main = xmonad def { layoutHook = myLayouts }
--
-- Or alternatively:
--
-- > myLayouts = Mirror (multiCol [1] 2 0.01 (-0.25)) ||| etc..
-- > main = xmonad def { layoutHook = myLayouts }
--
-- The maximum number of windows in a column can be controlled using the
-- IncMasterN messages and the column containing the focused window will be
-- modified. If the value is 0, all remaining windows will be placed in that
-- column when all columns before that has been filled.
--
-- The size can be set to between 1 and -0.5. If the value is positive, the
-- master column will be of that size. The rest of the screen is split among
-- the other columns. But if the size is negative, it instead indicates the
-- size of all non-master columns and the master column will cover the rest of
-- the screen. If the master column would become smaller than the other
-- columns, the screen is instead split equally among all columns. Therefore,
-- if equal size among all columns are desired, set the size to -0.5.
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- | Layout constructor.
multiCol
  :: [Int]    -- ^ Windows in each column, starting with master. Set to 0 to catch the rest.
  -> Int      -- ^ Default value for all following columns.
  -> Rational -- ^ How much to change size each time.
  -> Rational -- ^ Initial size of master area, or column area if the size is negative.
  -> MultiCol a
multiCol n defn ds s = MultiCol (map (max 0) n) (max 0 defn) ds s 0

data MultiCol a = MultiCol
  { multiColNWin      :: ![Int]
  , multiColDefWin    :: !Int
  , multiColDeltaSize :: !Rational
  , multiColSize      :: !Rational
  , multiColActive    :: !Int
  } deriving (Show,Read,Eq)

instance LayoutClass MultiCol a where
    doLayout l r s = return (combine s rlist, resl)
        where rlist = doL (multiColNWin l') (multiColSize l') r wlen
              wlen = length $ W.integrate s
              -- Make sure the list of columns is big enough and update active column
              nw = multiColNWin l ++ repeat (multiColDefWin l)
              l' = l { multiColNWin = take (max (length $ multiColNWin l) $ getCol (wlen-1) nw + 1) nw
                     , multiColActive = getCol (length $ W.up s) nw
                     }
              -- Only return new layout if it has been modified
              resl = if l'==l
                     then Nothing
                     else Just l'
              combine (W.Stack foc left right) rs = zip (foc : reverse left ++ right) $ raiseFocused (length left) rs
    handleMessage l m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = l { multiColSize = max (-0.5) $ s-ds }
                  resize Expand = l { multiColSize = min 1 $ s+ds }
                  incmastern (IncMasterN x) = l { multiColNWin = take a n ++ [newval] ++ tail r }
                      where newval =  max 0 $ head r + x
                            r = drop a n
                  n = multiColNWin l
                  ds = multiColDeltaSize l
                  s = multiColSize l
                  a = multiColActive l
    description _ = "MultiCol"

raiseFocused :: Int -> [a] -> [a]
raiseFocused n xs = actual ++ before ++ after
    where (before,rest) = splitAt n xs
          (actual,after) = splitAt 1 rest

-- | Get which column a window is in, starting at 0.
getCol :: Int -> [Int] -> Int
getCol w (n:ns) = if n<1 || w < n
                  then 0
                  else 1 + getCol (w-n) ns
-- Should never occur...
getCol _ _ = -1

doL :: [Int] -> Rational -> Rectangle -> Int -> [Rectangle]
doL nwin s r n = rlist
    where -- Number of columns to tile
          ncol = getCol (n-1) nwin + 1
          -- Compute the actual size
          size = floor $ abs s * fromIntegral (rect_width r)
          -- Extract all but last column to tile
          c = take (ncol-1) nwin
          -- Compute number of windows in last column and add it to the others
          col = c ++ [n-sum c]
          -- Compute width of columns
          width
            | s>0 = if ncol==1
                    -- Only one window
                    then [fromIntegral $ rect_width r]
                    -- Give the master it's space and split the rest equally for the other columns
                    else size:replicate (ncol-1) ((fromIntegral (rect_width r) - size) `div` (ncol-1))
            | fromIntegral ncol * abs s >= 1 = replicate ncol $ fromIntegral (rect_width r) `div` ncol
            | otherwise = (fromIntegral (rect_width r) - (ncol-1)*size):replicate (ncol-1) size
          -- Compute the horizontal position of columns
          xpos = accumEx (fromIntegral $ rect_x r) width
          -- Exclusive accumulation
          accumEx a (x:xs) = a:accumEx (a+x) xs
          accumEx _ _ = []
          -- Create a rectangle for each column
          cr = zipWith (\x w -> r { rect_x=fromIntegral x, rect_width=fromIntegral w }) xpos width
          -- Split the columns into the windows
          rlist = concat $ zipWith splitVertically col cr
