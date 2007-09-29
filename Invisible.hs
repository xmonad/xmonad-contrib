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
                            ) where

-- $usage
-- A data type to store the layout state

data Invisible m a = I (m a)

instance (Functor m, Monad m) => Read (Invisible m a) where
    readsPrec _ s = [(fail "Read Invisible", s)]

instance Monad m => Show (Invisible m a) where
    show _ = ""

instance (Functor m, Monad m) => Monad (Invisible m) where
    return a = I (return a)
    m >>= f  = m >>= f
    fail   s = I (fail s)

instance (Functor m, Monad m) => Functor (Invisible m) where
    fmap f (I x) = I (fmap f x)

whenIJust :: (Monad m) => Invisible Maybe a -> (a -> m ()) -> m ()
whenIJust (I (Just x)) f  = f x
whenIJust (I  Nothing) _  = return ()
