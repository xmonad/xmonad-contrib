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

donsMain :: IO ()
donsMain = dzen $ \conf -> xmonad $ conf
        { borderWidth        = 2
        , terminal           = "term"
        , normalBorderColor  = "#cccccc"
        , focusedBorderColor = "#cd8b00" }

