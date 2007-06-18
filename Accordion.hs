module XMonadContrib.Accordion (accordion) where

import XMonad
import Operations
import qualified StackSet as W
import Graphics.X11.Xlib
import Data.Ratio

accordion :: Layout
accordion = Layout { doLayout = accordionLayout
                    , modifyLayout = const $ return Nothing }

accordionLayout :: Rectangle -> W.Stack Window -> X [(Window, Rectangle)]
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
