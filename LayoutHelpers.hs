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
-- Make layouts respect size hints.
-----------------------------------------------------------------------------

module XMonadContrib.LayoutHelpers (
    -- * usage
    -- $usage
    DoLayout, ModDo, ModMod, ModLay,
    layoutModify,
    l2lModDo, idModify,
    idModMod,
    ) where

import Graphics.X11.Xlib ( Rectangle )
import XMonad
import StackSet ( Stack, integrate )

-- $usage
-- Use LayoutHelpers to help write easy Layouts.

--type DoLayout a = Rectangle -> Stack a -> X ([(a, Rectangle)], Maybe (Layout a))
type DoLayout a = Rectangle -> Stack a -> X [(a, Rectangle)]
type ModifyLayout a = SomeMessage -> X (Maybe (Layout a))

type ModDo a = Rectangle -> Stack a -> [(a, Rectangle)] -> X ([(a, Rectangle)], Maybe (ModLay a))
type ModMod a = SomeMessage -> X (Maybe (ModLay a))

type ModLay a = Layout a -> Layout a

layoutModify :: ModDo a -> ModMod a -> ModLay a
layoutModify fdo fmod l = Layout { doLayout = dl, modifyLayout = modl }
    where dl r s = do --(ws, ml') <- doLayout l r s
                      ws <- doLayout l r s
                      (ws', mmod') <- fdo r s ws
                      --let ml'' = case mmod' of
                      --           Just mod' -> Just $ mod' $ maybe l id ml'
                      --           Nothing -> layoutModify fdo mod `fmap` ml'
                      --return (ws', ml'')
                      case mmod' of
                        Just _ -> fail "Sorry, can't yet safely modify layouts in doLayout."
                        Nothing -> return ws'
          modl m = do ml' <- modifyLayout l m
                      mmod' <- fmod m
                      return $ case mmod' of
                               Just mod' -> Just $ mod' $ maybe l id ml'
                               Nothing -> layoutModify fdo fmod `fmap` ml'

l2lModDo :: (Rectangle -> [a] -> [(a,Rectangle)]) -> DoLayout a
--l2lModDo dl r s = return (dl r $ integrate s, Nothing)
l2lModDo dl r s = return (dl r $ integrate s)

idModify :: ModifyLayout a
idModify _ = return Nothing

idModMod :: ModMod a
idModMod _ = return Nothing
