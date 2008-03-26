--------------------------------------------------------------------
-- |
-- Module    : XMonad.Config.Dons
-- Copyright : (c) Galois, Inc. 2007
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
--
-- An example, simple configuration file.
--
--------------------------------------------------------------------

module XMonad.Config.Dons where

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Layout.NoBorders

donsMain :: IO ()
donsMain = dzen $ \x -> xmonad $ x
        { terminal           = "term"
        , normalBorderColor  = "#333333"
        , focusedBorderColor = "red"
        , layoutHook         = smartBorders (layoutHook x)
        , manageHook         =
                manageHook x <+>
                    (className =? "Toplevel" --> doFloat)
        }
