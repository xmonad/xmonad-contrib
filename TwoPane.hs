-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.TwoPane
-- Copyright   :  (c) Spencer Janssen <sjanssen@cse.unl.edu>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  Spencer Janssen <sjanssen@cse.unl.edu>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout that splits the screen horizontally and shows two windows.  The
-- left window is always the master window, and the right is either the
-- currently focused window or the second window in layout order.
--
-----------------------------------------------------------------------------

module XMonadContrib.TwoPane (
                              -- * Usage
                              -- $usage
                              TwoPane (..)
                             ) where

import XMonad
import Operations ( Resize(..), splitHorizontallyBy )
import StackSet ( focus, up, down)

-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.TwoPane
--
--  and add, to the list of layouts:
--
-- > ,("twopane", SomeLayout $ TwoPane 0.03 0.5) 

-- %import XMonadContrib.TwoPane
-- %layout , ,("twopane", SomeLayout $ TwoPane 0.03 0.5) 

data TwoPane a = 
    TwoPane Rational Rational 
    deriving ( Show, Read )

instance Layout TwoPane a where
    doLayout (TwoPane _ split) r s = return (arrange r s,Nothing)
        where
          arrange rect st = case reverse (up st) of
                              (master:_) -> [(master,left),(focus st,right)]
                              [] -> case down st of
                                      (next:_) -> [(focus st,left),(next,right)]
                                      [] -> [(focus st, rect)]
              where (left, right) = splitHorizontallyBy split rect

    modifyLayout (TwoPane delta split) x = 
        return $ case fromMessage x of
                   Just Shrink -> Just (TwoPane delta (split - delta))
                   Just Expand -> Just (TwoPane delta (split + delta))
                   _           -> Nothing

