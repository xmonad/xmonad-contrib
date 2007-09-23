{-# OPTIONS -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.LayoutHelpers
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for writing easy Layouts
-----------------------------------------------------------------------------

module XMonadContrib.LayoutHelpers (
    -- * Usage
    -- $usage
    LayoutModifier(..)
    ) where

import Control.Monad ( mplus )
import Graphics.X11.Xlib ( Rectangle )
import XMonad
import StackSet ( Stack )

-- $usage
-- Use LayoutHelpers to help write easy Layouts.

class (Show (m l a), Read (m l a), Layout l a) => LayoutModifier m l a where
    extractLayout :: m l a -> l a
    wrapLayout :: m l a -> l a -> m l a
    modifyModify :: m l a -> SomeMessage -> X (Maybe (l a ->  m l a))
    modifyModify _ _ = return Nothing
    redoLayout :: m l a -> Rectangle -> Stack a -> [(a, Rectangle)]
               -> X ([(a, Rectangle)], Maybe (l a -> m l a))
    redoLayout _ _ _ wrs = return (wrs, Nothing)

instance LayoutModifier m l a => Layout (m l) a where
    doLayout m r s = do (ws, ml') <- doLayout (extractLayout m) r s
                        (ws', mmod') <- redoLayout m r s ws
                        let ml'' = case mmod' of
                                   Just mod' -> Just $ mod' $ maybe (extractLayout m) id ml'
                                   Nothing -> wrapLayout m `fmap` ml'
                        return (ws', ml'')
    modifyLayout m mess = do ml' <- modifyLayout (extractLayout m) mess
                             mmod' <- modifyModify m mess
                             return $ case mmod' of
                                      Just  mod' -> Just $ mod' $ maybe (extractLayout m) id ml'
                                      Nothing -> wrapLayout m `fmap` ml'
