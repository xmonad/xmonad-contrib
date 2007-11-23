{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Named
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for assigning a name to a given layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Named (
                                -- * Usage
                                -- $usage
                                Named(Named)
                               ) where

import XMonad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.Named
--
-- Then edit your @layoutHook@ by adding the Named layout modifier
-- to some layout:
--
-- > myLayouts = Named "real big" Full ||| etc..
-- > main = xmonad dafaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

data Named l a = Named String (l a) deriving ( Read, Show )

instance (LayoutClass l a) => LayoutClass (Named l) a where
    doLayout (Named n l) r s = do (ws, ml') <- doLayout l r s
                                  return (ws, Named n `fmap` ml')
    handleMessage (Named n l) mess = do ml' <- handleMessage l mess
                                        return $ Named n `fmap` ml'
    description (Named n _) = n
