{-# -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.LayoutModifier
-- Copyright    : (c) David Roundy <droundy@darcs.net>
-- License      : BSD
--
-- Maintainer   : David Roundy <droundy@darcs.net>
-- Stability    : unstable
-- Portability  : portable
--
-- A module for writing easy Layouts
-----------------------------------------------------------------------------

module XMonadContrib.LayoutModifier (
    -- * Usage
    -- $usage
    LayoutModifier(..), ModifiedLayout(..)
    ) where

import Graphics.X11.Xlib ( Rectangle )
import XMonad
import StackSet ( Stack )
import Operations ( LayoutMessages(Hide, ReleaseResources) )

-- $usage
-- Use LayoutHelpers to help write easy Layouts.

class (Show (m a), Read (m a)) => LayoutModifier m a where
    handleMess :: m a -> SomeMessage -> X (Maybe (m a))
    handleMess m mess | Just Hide <- fromMessage mess             = doUnhook
                        | Just ReleaseResources <- fromMessage mess = doUnhook
                        | otherwise = return Nothing
     where doUnhook = do unhook m; return Nothing
    redoLayout :: m a -> Rectangle -> Stack a -> [(a, Rectangle)]
               -> X ([(a, Rectangle)], Maybe (m a))
    redoLayout m _ _ wrs = do hook m; return (wrs, Nothing)
    hook :: m a -> X ()
    hook _ = return ()
    unhook :: m a -> X ()
    unhook _ = return ()
    modifierDescription :: m a -> String
    modifierDescription = const ""

instance (LayoutModifier m a, LayoutClass l a) => LayoutClass (ModifiedLayout m l) a where
    doLayout (ModifiedLayout m l) r s =
        do (ws, ml') <- doLayout l r s
           (ws', mm') <- redoLayout m r s ws
           let ml'' = case mm' of
                      Just m' -> Just $ (ModifiedLayout m') $ maybe l id ml'
                      Nothing -> ModifiedLayout m `fmap` ml'
           return (ws', ml'')
    handleMessage (ModifiedLayout m l) mess =
        do ml' <- handleMessage l mess
           mm' <- handleMess m mess
           return $ case mm' of
                    Just m' -> Just $ (ModifiedLayout m') $ maybe l id ml'
                    Nothing -> (ModifiedLayout m) `fmap` ml'
    description (ModifiedLayout m l) = modifierDescription m <> description l
     where "" <> x = x
           x <> y = x ++ " " ++ y

data ModifiedLayout m l a = ModifiedLayout (m a) (l a) deriving ( Read, Show )
