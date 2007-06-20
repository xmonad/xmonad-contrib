-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Accordion
-- Copyright   :  (c) glasser@mit.edu
-- License     :  BSD
--
-- Maintainer  :  glasser@mit.edu
-- Stability   :  unstable
-- Portability :  unportable
--
-- Layout that puts non-focused windows in ribbons at the top and bottom
-- of the screen.
-----------------------------------------------------------------------------

module XMonadContrib.Accordion (
    -- * Usage
    -- $ usage
    accordion) where

import XMonad
import Operations
import qualified StackSet as W
import Graphics.X11.Xlib
import Data.Ratio

-- $ usage
-- > import XMonadContrib.Accordion
-- > defaultLayouts = [ accordion ]

accordion :: Eq a => Layout a
accordion = Layout { doLayout = accordionLayout
                    , modifyLayout = const $ return Nothing }

accordionLayout :: Eq a => Rectangle -> W.Stack a -> X [(a, Rectangle)]
accordionLayout sc ws = return $ (zip ups tops) ++
                               [(W.focus ws, mainPane)] ++
                               (zip dns bottoms)
 where ups    = W.up ws
       dns    = W.down ws
       (top, allButTop) = splitVerticallyBy (1%8) sc
       (center, bottom) = splitVerticallyBy (6%7) allButTop
       (allButBottom, _) = splitVerticallyBy (7%8) sc
       mainPane | ups /= [] && dns /= [] = center
                | ups /= []              = allButTop
                | dns /= []              = allButBottom
                | otherwise              = sc
       tops   = if ups /= [] then splitVertically (length ups) top else []
       bottoms= if dns /= [] then splitVertically (length dns) bottom else []
