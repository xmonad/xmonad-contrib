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
-----------------------------------------------------------------------------

module XMonad.Layout.Named (
                                -- * Usage
                                -- $usage
                                Named(Named)
                               ) where

import XMonad

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonad.Layout.Named
--
-- and change the name of a given layout by
--
-- > layout = Named "real big" Full ||| ...

data Named l a = Named String (l a) deriving ( Read, Show )

instance (LayoutClass l a) => LayoutClass (Named l) a where
    doLayout (Named n l) r s = do (ws, ml') <- doLayout l r s
                                  return (ws, Named n `fmap` ml')
    handleMessage (Named n l) mess = do ml' <- handleMessage l mess
                                        return $ Named n `fmap` ml'
    description (Named n _) = n
