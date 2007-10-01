{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Invisible
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

module XMonadContrib.Invisible ( 
                             -- * Usage:
                             -- $usage
                             Invisible (..)
                            , whenIJust
                            , fromIMaybe
                            ) where

-- $usage
-- A data type to store the layout state

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
