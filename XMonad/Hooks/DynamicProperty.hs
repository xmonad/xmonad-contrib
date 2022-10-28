-- |
-- Module      :  XMonad.Hooks.DynamicProperty
-- Description :  Apply a ManageHook to an already-mapped window.
-- Copyright   :  (c) Brandon S Allbery, 2015
-- License     :  BSD3-style (see LICENSE)
-- Maintainer  :  allbery.b@gmail.com
--
module XMonad.Hooks.DynamicProperty {-# DEPRECATED "Use \"XMonad.Hooks.OnPropertyChange\" instead." #-}
                                    ( module XMonad.Hooks.OnPropertyChange
                                    , dynamicPropertyChange
                                    , dynamicTitle
                                    ) where

import XMonad
import XMonad.Hooks.OnPropertyChange
import XMonad.Prelude

-- | 'dynamicPropertyChange' = 'onXPropertyChange'
dynamicPropertyChange :: String -> ManageHook -> Event -> X All
dynamicPropertyChange = onXPropertyChange

-- | 'dynamicTitle' = 'onTitleChange'
dynamicTitle :: ManageHook -> Event -> X All
dynamicTitle = onTitleChange
