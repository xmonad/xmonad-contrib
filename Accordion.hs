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
    -- $usage
    Accordion(Accordion)) where

import XMonad
import Operations
import qualified StackSet as W
import Graphics.X11.Xlib
import Data.Ratio

-- $usage
-- > import XMonadContrib.Accordion
-- > defaultLayouts = [ accordion ]

-- %import XMonadContrib.Accordion
-- %layout , accordion

data Accordion a = Accordion deriving ( Read, Show )

instance Layout Accordion Window where
    doLayout _ = accordionLayout
 
accordionLayout :: Eq a => Rectangle -> W.Stack a -> X ([(a, Rectangle)], Maybe (Accordion a))
accordionLayout sc ws = return ((zip ups tops) ++
                                [(W.focus ws, mainPane)] ++
                                (zip dns bottoms)
                               ,Nothing)
 where ups    = W.up ws
       dns    = W.down ws
       (top,  allButTop) = splitVerticallyBy (1%8 :: Ratio Int) sc
       (center,  bottom) = splitVerticallyBy (6%7 :: Ratio Int) allButTop
       (allButBottom, _) = splitVerticallyBy (7%8 :: Ratio Int) sc
       mainPane | ups /= [] && dns /= [] = center
                | ups /= []              = allButTop
                | dns /= []              = allButBottom
                | otherwise              = sc
       tops    = if ups /= [] then splitVertically (length ups) top    else []
       bottoms = if dns /= [] then splitVertically (length dns) bottom else []
