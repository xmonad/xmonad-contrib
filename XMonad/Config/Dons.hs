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
import XMonad.Config
import XMonad.Hooks.DynamicLog

config :: XConfig
config = defaultConfig
    { borderWidth        = 2
    , defaultGaps        = [(18,0,0,0)]
    , terminal           = "term"
    , normalBorderColor  = "#cccccc"
    , focusedBorderColor = "#cd8b00"
    , logHook            = dynamicLogDzen }

