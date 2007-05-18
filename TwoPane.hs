-- A layout that splits the screen horizontally and shows two windows.  The
-- left window is always the master window, and the right is either the
-- currently focused window or the second window in layout order.
--
-- To use this layout, 'import XMonadContrib.TwoPane' and add
-- 'twoPane defaultDelta (1%2)' to the list of layouts

module XMonadContrib.TwoPane where

import XMonad
import Operations
import qualified StackSet as W
import Control.Monad.State (gets)

twoPane :: Rational -> Rational -> Layout
twoPane delta split = Layout { doLayout = arrange, modifyLayout = message }
 where
    arrange rect ws@(w:x:_) = do
        (Just f) <- gets (W.peek . workspace) -- safe because of pattern match above
        let y = if f == w then x else f
            (left, right) = splitHorizontallyBy split rect
        mapM_ hide . filter (\a -> a /= w && a /= y)  $ ws
        return [(w, left), (y, right)]
    -- there are one or zero windows
    arrange rect ws         = return . map (\w -> (w, rect)) $ ws

    message x = case fromMessage x of
                    Just Shrink -> Just (twoPane delta (split - delta))
                    Just Expand -> Just (twoPane delta (split + delta))
                    _           -> Nothing
