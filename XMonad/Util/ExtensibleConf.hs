{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  XMonad.Util.ExtensibleConf
-- Description :  Extensible and composable configuration for contrib modules.
-- Copyright   :  (c) 2021 Tomáš Janoušek <tomi@nomi.cz>
-- License     :  BSD3
-- Maintainer  :  Tomáš Janoušek <tomi@nomi.cz>
--
-- Extensible and composable configuration for contrib modules.
--
-- This is the configuration counterpart of "XMonad.Util.ExtensibleState". It
-- allows contrib modules to store custom configuration values inside
-- 'XConfig'. This lets them create custom hooks, ensure they hook into xmonad
-- core only once, and possibly more.
--

module XMonad.Util.ExtensibleConf (
    -- * Usage
    -- $usage

    -- * High-level idioms based on Semigroup
    with,
    add,
    once,
    onceM,

    -- * High-level idioms based on Default
    withDef,
    modifyDef,
    modifyDefM,

    -- * Low-level primitivies
    ask,
    lookup,
    alter,
    alterF,
    ) where

import Prelude hiding (lookup)
import XMonad hiding (ask, modify, trace)
import XMonad.Prelude ((<|>), (<&>), fromMaybe)

import Data.Typeable
import qualified Data.Map as M


-- ---------------------------------------------------------------------
-- $usage
--
-- To utilize this feature in a contrib module, create a data type for the
-- configuration, then use the helper functions provided here to implement
-- a user-friendly composable interface for your contrib module.
--
-- Example:
--
-- > import qualified XMonad.Util.ExtensibleConf as XC
-- >
-- > {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- > newtype MyConf = MyConf{ fromMyConf :: [Int] } deriving Semigroup
-- >
-- > customLogger :: Int -> XConfig l -> XConfig l
-- > customLogger i = XC.once (MyConf [i]) $ \c -> c{ logHook = logHook c <> lh }
-- >   where
-- >     lh :: X ()
-- >     lh = XC.with $ io . print . fromMyConf
--
-- The above defines an xmonad configuration combinator that can be applied
-- any number of times like so:
--
-- > main = xmonad $ … . customLogger 1 . ewmh . customLogger 2 . … $ def{…}
--
-- and will always result in just one 'print' invocation in 'logHook'.


-- ---------------------------------------------------------------------
-- Low-level primitivies

-- | Run-time: Retrieve a configuration value of the requested type.
ask :: (MonadReader XConf m, Typeable a) => m (Maybe a)
ask = asks $ lookup . config

-- | Config-time: Retrieve a configuration value of the requested type.
lookup :: forall a l. Typeable a => XConfig l -> Maybe a
lookup c = fromConfExt =<< typeRep (Proxy @a) `M.lookup` extensibleConf c

-- | Config-time: Alter a configuration value, or absence thereof.
alter :: forall a l. Typeable a => (Maybe a -> Maybe a) -> XConfig l -> XConfig l
alter f = mapEC $ M.alter (mapConfExt f) (typeRep (Proxy @a))
  where
    mapEC g c = c{ extensibleConf = g (extensibleConf c) }

-- | Config-time: Functor variant of 'alter', useful if the configuration
-- modifications needs to do some 'IO'.
alterF :: forall a l f. (Typeable a, Functor f)
       => (Maybe a -> f (Maybe a)) -> XConfig l -> f (XConfig l)
alterF f = mapEC $ M.alterF (mapConfExtF f) (typeRep (Proxy @a))
  where
    mapEC g c = g (extensibleConf c) <&> \ec -> c{ extensibleConf = ec }


fromConfExt :: Typeable a => ConfExtension -> Maybe a
fromConfExt (ConfExtension val) = cast val

mapConfExt :: Typeable a
           => (Maybe a -> Maybe a) -> Maybe ConfExtension -> Maybe ConfExtension
mapConfExt f = fmap ConfExtension . f . (>>= fromConfExt)

mapConfExtF :: (Typeable a, Functor f)
            => (Maybe a -> f (Maybe a)) -> Maybe ConfExtension -> f (Maybe ConfExtension)
mapConfExtF f = fmap (fmap ConfExtension) . f . (>>= fromConfExt)


-- ---------------------------------------------------------------------
-- High-level idioms based on Semigroup

-- | Run-time: Run a monadic action with the value of the custom
-- configuration, if set.
with :: (MonadReader XConf m, Typeable a, Monoid b) => (a -> m b) -> m b
with a = ask >>= maybe (pure mempty) a

-- | Config-time: Add (append) a piece of custom configuration to an 'XConfig'
-- using the 'Semigroup' instance of the configuration type.
add :: (Semigroup a, Typeable a)
    => a -- ^ configuration to add
    -> XConfig l -> XConfig l
add x = alter (<> Just x)

-- | Config-time: 'add' a piece of custom configuration, and if it's the first
-- piece of this type, also modify the 'XConfig' using the provided function.
--
-- This can be used to implement a composable interface for modules that must
-- only hook into xmonad core once.
--
-- (The piece of custom configuration is the last argument as it's expected to
-- come from the user.)
once :: forall a l. (Semigroup a, Typeable a)
     => (XConfig l -> XConfig l) -- ^ 'XConfig' modification done only once
     -> a -- ^ configuration to add
     -> XConfig l -> XConfig l
once f x c = maybe f (const id) (lookup @a c) $ add x c

-- | Config-time: Applicative (monadic) variant of 'once', useful if the
-- 'XConfig' modification needs to do some 'IO' (e.g. create an
-- 'Data.IORef.IORef').
onceM :: forall a l m. (Applicative m, Semigroup a, Typeable a)
      => (XConfig l -> m (XConfig l)) -- ^ 'XConfig' modification done only once
      -> a -- ^ configuration to add
      -> XConfig l -> m (XConfig l)
onceM f x c = maybe f (const pure) (lookup @a c) $ add x c


-- ---------------------------------------------------------------------
-- High-level idioms based on Default

-- | Run-time: Run a monadic action with the value of the custom
-- configuration, or the 'Default' value thereof, if absent.
withDef :: (MonadReader XConf m, Typeable a, Default a) => (a -> m b) -> m b
withDef a = ask >>= a . fromMaybe def

-- | Config-time: Modify a configuration value in 'XConfig', initializing it
-- to its 'Default' value first if absent. This is an alternative to 'add' for
-- when a 'Semigroup' instance is unavailable or unsuitable.
--
-- Note that this must /not/ be used together with any variant of 'once'!
modifyDef :: forall a l. (Default a, Typeable a)
          => (a -> a) -- ^ modification of configuration
          -> XConfig l -> XConfig l
modifyDef f = alter ((f <$>) . (<|> Just def))

-- | Config-time: Applicative (monadic) variant of 'modifyDef', useful if the
-- configuration value modification needs to do some 'IO' (e.g. create an
-- 'Data.IORef.IORef').
--
-- Note that this must /not/ be used together with any variant of 'once'!
modifyDefM :: forall a l m. (Applicative m, Default a, Typeable a)
           => (a -> m a) -- ^ modification of configuration
           -> XConfig l -> m (XConfig l)
modifyDefM f = alterF (traverse f . (<|> Just def))
