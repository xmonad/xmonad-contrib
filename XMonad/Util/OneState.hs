{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall -Werror #-}

module XMonad.Util.OneState
  ( OneState (..)
  , get
  , put
  , modify
  , add
  , once
  , onceM
  ) where

import           Control.Monad               ((>=>))
import           Data.Maybe                  (fromMaybe)
import           XMonad                      hiding (config, get, modify, put,
                                              state, trace)
import qualified XMonad.Util.ExtensibleConf  as XC
import qualified XMonad.Util.ExtensibleState as XS


{- |

OneState is a replacement for both @XMonad.Util.ExtensibleState@ and @XMonad.Util.ExtensibleConf@

A comparison of these three modules is as follows:

- @ExtensibleConf@ allows the programmer to accept a user-supplied value at config-time.
  However, this value cannot be changed during runtime.

- @ExtensibleState@ allows the programmer to keep mutable state.
  However, the initial value for this state must be known at compile-time and is not
  configurable at config-time.

- @OneState@ proves an API which matches the power of both @ExtensibleConf@ and @ExtensibleState@,
  allowing the programmer to keep mutable state *and* allowing this mutable state to be configured
  at config-time.

-}


class Typeable state => OneState state where

  -- | Associated type of config-time modifications to state
  type Mod state

  -- |
  --
  -- How to apply a modification
  --
  -- This operation may have effects in the X monad. However, no strong
  -- guarantees are made about its evaluation, such as guarantees about
  -- timing or multiplicity. Beware!
  merge :: Mod state -> (state -> X state)

  -- | Default value for the state
  defaultState :: state


-- hook into ExtensibleState
newtype State state = State (Maybe state)
  deriving (Typeable)

instance OneState state => ExtensionClass (State state) where
  initialValue = State Nothing

-- hook into ExtensibleConf
newtype Config state = Config [Mod state]
  deriving newtype (Typeable, Semigroup)

trivialConfig :: Config state
trivialConfig = Config []


-- |
--
-- Like @ExtensibleState.get@
--
-- Retrieve the current state value
--
-- * If the state has been explicitly set during runtime, then the most recent
--   set value will be returned
--
-- * Otherwise, if the state was configured during config-time, then all the
--   config-time @Mod state@ values will be applied to @defaultState@, and
--   that will be returned
--
-- * Otherwise, @default@ is returned
get :: forall state. OneState state => X state
get = XS.get >>= \case
  State (Just state) -> pure state
  State Nothing      -> foldConfig

  where

  foldConfig :: X state
  foldConfig = do
    Config deltas :: Config state <- fromMaybe trivialConfig <$> XC.ask
    let bigDelta = foldr (>=>) pure $ merge <$> deltas
    result <- bigDelta defaultState
    put result  -- modifications are monadic; ensure we only perform them once
    pure result


-- | Like @ExtensibleState.put@
put :: OneState state => state -> X ()
put = XS.put . State . Just

-- | Like @ExtensibleState.modify@
modify :: OneState state => (state -> state) -> X ()
modify f = put =<< (f <$> get)


-- | Like @ExtensibleConf.onceM@
onceM
  :: forall state m l
   . (OneState state, Applicative m)
  => (XConfig l -> m (XConfig l))
  -> Mod state
  -> (XConfig l -> m (XConfig l))
onceM modX modState = XC.onceM modX (Config @state . one $ modState)
  where one x = [x]

-- | Like @ExtensibleConf.once@
once
  :: forall state l
   . OneState state
   => (XConfig l -> XConfig l)
   -> Mod state
   -> (XConfig l -> XConfig l)
once modX modState = XC.once modX (Config @state . one $ modState)
  where one x = [x]

-- | Like @ExtensibleConf.add@
add :: forall state l. OneState state => Mod state -> (XConfig l -> XConfig l)
add = once @state id
