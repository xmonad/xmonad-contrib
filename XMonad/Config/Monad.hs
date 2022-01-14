{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- experimental, not expected to work

{- our goal:
config = do
    add layout Full
    set terminal "urxvt"
    add keys [blah blah blah]
-}

{-
ideas:
    composability!
    "only once" features like avoidStruts, ewmhDesktops
-}

module XMonad.Config.Monad {-# DEPRECATED "This module does not work." #-} where

import XMonad hiding (terminal, keys)
import qualified XMonad as X
import Control.Monad.Writer
import XMonad.Prelude
import Data.Accessor
import Data.Accessor.Basic hiding (set)

-- Ugly!  To fix this we'll need to change the kind of XConfig.
newtype LayoutList a = LL [Layout a] deriving Monoid

type W = Dual (Endo (XConfig LayoutList))
mkW = Dual . Endo

newtype Config a = C (WriterT W IO a)
    deriving (Functor, Monad, MonadWriter W)

-- references:
layout = fromSetGet (\x c -> c { layoutHook = x }) layoutHook
terminal = fromSetGet (\x c -> c { X.terminal = x }) X.terminal
keys = fromSetGet (\x c -> c { X.keys = x }) X.keys

set :: Accessor (XConfig LayoutList) a -> a -> Config ()
set r x = tell (mkW $ r ^= x)
add r x = tell (mkW (r ^: mappend x))

--
example :: Config ()
example = do
    add layout $ LL [Layout Full] -- make this better
    set terminal "urxvt"
