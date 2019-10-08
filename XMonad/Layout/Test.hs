{-# LANGUAGE PatternGuards, FlexibleContexts, FlexibleInstances, DeriveDataTypeable, TypeSynonymInstances, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}

module XMonad.Layout.Test where

import XMonad hiding (focus)
import XMonad.StackSet (Workspace(..),integrate',Stack(..))
import qualified XMonad.StackSet as W
import Data.Maybe (fromJust,isJust)
import Data.List (delete,takeWhile)
import Control.Monad (join, foldM)
import XMonad.Layout (Choose, (|||), Tall)
import XMonad.Layout.Simplest
import Data.Typeable
import XMonad.Layout.TallMastersCombo
import XMonad.Layout.Tabbed
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Hooks.ManageDocks

class (LayoutClass l a) => F l a where
  getFocused :: l a -> Int -> Int

instance {-# OVERLAPPING #-} (Typeable l1, Typeable l2, Typeable r1, Typeable r2, 
                              LayoutClass l1 Window, LayoutClass l2 Window, 
                              LayoutClass r1 Window, LayoutClass r2 Window) 
                              => F(TMSCombineTwo (TMSCombineTwo l1 r1) (TMSCombineTwo l2 r2)) Window where
  getFocused (TMSCombineTwo f _ _ vsp nmaster _ frac lay1 lay2) x = (getFocused lay1 x) + (getFocused lay2 x)

instance {-# OVERLAPS #-} (Typeable l1, Typeable l2, Typeable r1,
                              LayoutClass l1 Window, LayoutClass l2 Window, 
                              LayoutClass r1 Window) 
                              => F(TMSCombineTwo (TMSCombineTwo l1 r1) l2) Window where
  getFocused (TMSCombineTwo f _ _ vsp nmaster _ frac lay1 lay2) x = (getFocused lay1 x) + (getFocused lay2 x)

instance {-# OVERLAPS #-} (Typeable l1, Typeable l2, Typeable r2, 
                              LayoutClass l1 Window, LayoutClass l2 Window, 
                              LayoutClass r2 Window) 
                              => F(TMSCombineTwo l1 (TMSCombineTwo l2 r2)) Window where
  getFocused (TMSCombineTwo f _ _ vsp nmaster _ frac lay1 lay2) x = (getFocused lay1 x) + (getFocused lay2 x)

instance {-# OVERLAPS #-} (Typeable l1, Typeable l2,
                              LayoutClass l1 Window, LayoutClass l2 Window) 
                              => F(TMSCombineTwo l1 l2) Window where
  getFocused (TMSCombineTwo f _ _ vsp nmaster _ frac lay1 lay2) x = (getFocused lay1 x) + (getFocused lay2 x)

instance {-# OVERLAPPING #-} (Typeable l, LayoutClass l a) => F l a where
  getFocused l x 
      | lstr == "Choose" = x + 1
      | otherwise = 0
    where lstr = takeWhile (/=' ') (show l)


-- layouts
-- tall = Tall 0 (3/100) (0)
rows = RowsOrColumns True
myTheme = def { fontName = "xft:DejaVu Sans:size=8" }
--myTheme = (theme donaldTheme) { fontName = "xft:DejaVu Sans:size=10" }
myTabbed = tabbed shrinkText myTheme

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
  transform _ x k = k myTabbed (const x)

subLayout = tmsCombineTwo False 1 (3/100) (1/2) Simplest myTabbed
layout1 = myTabbed ||| subLayout
layout2 = subLayout ||| myTabbed ||| rows
-- rightColumnLayout = myTabbed ||| subcomplayout ||| rowcol 

baseLayout = tmsCombineTwoDefault layout1 layout2

mylayouts = smartBorders $
            avoidStrutsOn [] $
            mkToggle (FULL ?? EOT) $
            mkToggle (single MIRROR) $
            baseLayout

-- tall0 = Tall 1 (3/100) (1/2)
-- mylayout1 = tall0 ||| Simplest
-- mylayout2 = tmsCombineTwoDefault tall0 tall0
