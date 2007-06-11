module XMonadContrib.LayoutHints ( layoutHints ) where

-- to use:
-- defaultLayouts = [ layoutHints tiled, layoutHints $ mirror tiled , full ]

import Operations ( applySizeHints, D )
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras ( getWMNormalHints )
import {-#SOURCE#-} Config (borderWidth)
import XMonad hiding ( trace )

-- | Expand a size by the given multiple of the border width.  The
-- multiple is most commonly 1 or -1.
adjBorders             :: Dimension -> D -> D
adjBorders mult (w,h)  = (w+2*mult*borderWidth, h+2*mult*borderWidth)

layoutHints :: Layout -> Layout
layoutHints l = l { doLayout = \r x -> doLayout l r x >>= applyHints
                  , modifyLayout = \x -> fmap layoutHints `fmap` modifyLayout l x }

applyHints :: [(Window, Rectangle)] -> X [(Window, Rectangle)]
applyHints xs = mapM applyHint xs
    where applyHint (w,Rectangle a b c d) =
              withDisplay $ \disp ->
                  do sh <- io $ getWMNormalHints disp w
                     let (c',d') = adjBorders 1 . applySizeHints sh . adjBorders (-1) $ (c,d)
                     return (w, Rectangle a b c' d')
