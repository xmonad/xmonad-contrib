-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.SetLayout
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle through non-empty workspaces.
--
-----------------------------------------------------------------------------

module XMonadContrib.SetLayout (
                              -- * Usage
                              -- $usage
                              setLayout
                             ) where

import Graphics.X11.Xlib ( Window )
import XMonad
import StackSet hiding (filter)
import Operations

-- $usage
-- You can use this module with the following in your Config.hs file:
-- 
-- > import XMonadContrib.SetLayout
--
-- >   , ((modMask .|. shiftMask, xK_space ), setLayout $ SomeLayout $ LayoutSelection defaultLayouts) -- %! Reset this layout

-- %import XMonadContrib.SetLayout
-- %keybind , ((modMask .|. shiftMask, xK_space ), setLayout $ SomeLayout $ LayoutSelection defaultLayouts) -- %! Reset this layout

setLayout :: SomeLayout Window -> X ()
setLayout l = do sendMessage ReleaseResources
                 windows $ \s -> s { current = r $ current s }
    where r scr = scr { workspace = r' $ workspace scr }
          r' ws = ws { layout = l }
