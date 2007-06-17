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
                              twoPane
                             ) where

import XMonad
import Operations
import qualified StackSet as W
import Control.Monad.State (gets)


-- $usage
--
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.TwoPane
--
--  and add, to the list of layouts:
--
-- > twoPane defaultDelta (1%2)

twoPane :: Rational -> Rational -> Layout
twoPane delta split = Layout { doLayout = \r -> arrange r . W.integrate, modifyLayout = message }
 where
    arrange rect ws@(w:x:_) = do
        -- TODO this is buggy, it might peek another workspace
        (Just f) <- gets (W.peek . windowset) -- safe because of pattern match above
        let y = if f == w then x else f
            (left, right) = splitHorizontallyBy split rect
        mapM_ hide . filter (\a -> a /= w && a /= y)  $ ws
        return [(w, left), (y, right)]
    -- there are one or zero windows
    arrange rect ws         = return . map (\w -> (w, rect)) $ ws

    message x = return $ case fromMessage x of
                    Just Shrink -> Just (twoPane delta (split - delta))
                    Just Expand -> Just (twoPane delta (split + delta))
                    _           -> Nothing
