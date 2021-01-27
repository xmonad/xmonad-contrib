{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SortedLayout
-- Copyright   :  (c) 2016 Kurt Dietrich
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  kurto@mac.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A 'LayoutModifier' that sorts the windows in another layout, given a
-- list of properties. The order of properties in the list determines
-- the order of windows in the final layout. Any unmatched windows
-- go to the end of the order.
-----------------------------------------------------------------------------

module XMonad.Layout.SortedLayout
  ( -- *Usage:
    -- $usage
  sorted
  , Property(..)
  ) where

import           Control.Monad
import           Data.List

import           XMonad
import           XMonad.Layout.LayoutModifier
import           XMonad.StackSet              as W
import           XMonad.Util.WindowProperties

-- $usage
-- You can use this module with the following in your
-- @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.SortedLayout
--
-- Then edit your @layoutHook@ to sort another layout (in this case, 'XMonad.Layout.Grid.Grid'):
--
-- > myLayout = sorted [ClassName "Firefox", ClassName "URxvt"] Grid
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"


-- | Modify a layout using a list of properties to sort its windows.
sorted :: [Property]
       -> l a
       -> ModifiedLayout SortedLayout l a
sorted props = ModifiedLayout . SortedLayout $ props ++ [Const True]

data WindowDescriptor = WindowDescriptor { wdSeqn :: !Integer
                                         , wdProp :: !Property
                                         , wdId   :: !Window
                                         } deriving (Show, Read)

instance Eq WindowDescriptor where
  (==) a b = wdId a == wdId b

instance Ord WindowDescriptor where
  compare a b = compare (wdSeqn a) (wdSeqn b)

data SortedLayout a = SortedLayout [Property] deriving (Show, Read)

instance LayoutModifier SortedLayout Window where
    modifyLayout (SortedLayout props) = sortLayout props
    modifierDescription _             = "Sorted"

findMatchingWindows :: Integer -> Property -> [Window] -> X [WindowDescriptor]
findMatchingWindows seqn prop wids =  fmap (fmap (WindowDescriptor seqn prop)) matching  where
  matching = filterM (hasProperty prop) wids

sortLayout :: (LayoutClass l Window)
           => [Property]
           -> W.Workspace WorkspaceId (l Window) Window
           -> Rectangle
           -> X ([(Window, Rectangle)], Maybe (l Window))
sortLayout props (W.Workspace w l r) rect = do
  let wids = W.integrate' r
  sortedWids <- map wdId . nub . sort . concat <$> zipWithM (\s p -> findMatchingWindows s p wids) [0..] props
  let sr = W.differentiate sortedWids
  runLayout (W.Workspace w l sr) rect
