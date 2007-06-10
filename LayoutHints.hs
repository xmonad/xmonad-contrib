module XMonadContrib.LayoutHints ( layoutHints ) where

-- to use:
-- defaultLayouts = [ layoutHints tiled, layoutHints $ mirror tiled , full ]

import Operations ( applySizeHints )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( getWMNormalHints )
import XMonad hiding ( trace )

layoutHints :: Layout -> Layout
layoutHints l = l { doLayout = \r x -> doLayout l r x >>= applyHints
                  , modifyLayout = \x -> fmap layoutHints `fmap` modifyLayout l x }

applyHints :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
applyHints xs = mapM applyHint xs
    where applyHint (w,Rectangle a b c d) =
              withDisplay $ \disp ->
                  do sh <- io $ getWMNormalHints disp w
                     let (c',d') = applySizeHints sh (c,d)
                     return (w, Rectangle a b c' d')
