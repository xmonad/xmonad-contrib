-- A layout that splits the screen horizontally and shows two windows.  The
-- left window is always the master window, and the right is either the
-- currently focused window or the second window in layout order.

module XMonadContrib.TwoPane where

import XMonad
import Operations
import qualified StackSet as W
import Control.Monad.State (gets)

twoPane :: Rational -> Rational -> Layout
twoPane delta split = Layout { doLayout = arrange, modifyLayout = message }
 where
    arrange rect (w:x:_) = do
        (Just f) <- gets (W.peek . workspace) -- safe because of pattern match above
        let (left, right) = splitHorizontallyBy split rect
        return [(w, left), (if f == w then x else f, right)]
    -- there are one or zero windows
    arrange rect ws       = return . map (\w -> (w, rect)) $ ws

    message x = case fromMessage x of
                    Just Shrink -> Just (twoPane delta (split - delta))
                    Just Expand -> Just (twoPane delta (split + delta))
                    _           -> Nothing
