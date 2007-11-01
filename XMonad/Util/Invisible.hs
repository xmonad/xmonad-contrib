{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Invisible
-- Copyright   :  (c) 2007 Andrea Rossato, David Roundy
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  andrea.rossato@unibz.it, droundy@darcs.net
-- Stability   :  unstable
-- Portability :  unportable
--
-- A data type to store the layout state
--
-----------------------------------------------------------------------------

module XMonad.Util.Invisible ( 
                             -- * Usage:
                             -- $usage
                             Invisible (..)
                            , whenIJust
                            , fromIMaybe
                            ) where

-- $usage
-- A wrapper data type to store layout state that shouldn't be persisted across
-- restarts. A common wrapped type to use is @Maybe a@.
-- Invisible derives trivial definitions for Read and Show, so the wrapped data
-- type need not do so.

newtype Invisible m a = I (m a) deriving (Monad, Functor)

instance (Functor m, Monad m) => Read (Invisible m a) where
    readsPrec _ s = [(fail "Read Invisible", s)]

instance Monad m => Show (Invisible m a) where
    show _ = ""

whenIJust :: (Monad m) => Invisible Maybe a -> (a -> m ()) -> m ()
whenIJust (I (Just x)) f  = f x
whenIJust (I  Nothing) _  = return ()

fromIMaybe :: a -> Invisible Maybe a -> a
fromIMaybe _ (I (Just x)) = x
fromIMaybe a (I  Nothing) = a
