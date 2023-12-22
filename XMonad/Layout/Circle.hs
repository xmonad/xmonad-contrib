{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Circle
-- Description :  An elliptical, overlapping layout.
-- Copyright   :  (c) Peter De Wachter
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Peter De Wachter <pdewacht@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Circle is an elliptical, overlapping layout, by Peter De Wachter
--
-----------------------------------------------------------------------------

module XMonad.Layout.Circle {-# DEPRECATED "Use XMonad.Layout.CircleEx instead" #-}
  ( -- * Usage
    -- $usage
    pattern Circle
  ) where -- actually it's an ellipse

import GHC.Real (Ratio(..))
import XMonad.Layout.CircleEx

-- $usage
-- You can use this module with the following in your @xmonad.hs@:
--
-- > import XMonad.Layout.Circle
--
-- Then edit your @layoutHook@ by adding the Circle layout:
--
-- > myLayout = Circle ||| Full ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see
-- <https://xmonad.org/TUTORIAL.html#customizing-xmonad the tutorial> and
-- "XMonad.Doc.Extending#Editing_the_layout_hook".

pattern Circle :: CircleEx a
pattern Circle = CircleEx 1 (70 :% 99) (2 :% 5) 1 0

