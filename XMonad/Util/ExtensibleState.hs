{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExtensibleState
-- Description :  Module for storing custom mutable state in xmonad.
-- Copyright   :  (c) Daniel Schoepe 2009
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  daniel.schoepe@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module for storing custom mutable state in xmonad.
--
-----------------------------------------------------------------------------

module XMonad.Util.ExtensibleState (
                              -- * Usage
                              -- $usage
                              put
                              , modify
                              , modify'
                              , remove
                              , get
                              , gets
                              , modified
                              , modifiedM
                              ) where

import Data.Typeable (typeOf,cast)
import qualified Data.Map as M
import XMonad.Core
import XMonad.Util.PureX
import qualified Control.Monad.State as State
import XMonad.Prelude (fromMaybe)

-- ---------------------------------------------------------------------
-- $usage
--
-- To utilize this feature in a contrib module, create a data type
-- and make it an instance of ExtensionClass. You can then use
-- the functions from this module for storing and retrieving your data:
--
-- > import qualified XMonad.Util.ExtensibleState as XS
-- >
-- > data ListStorage = ListStorage [Integer]
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
-- >
-- > .. XS.put (ListStorage [23,42])
--
-- To retrieve the stored value call:
--
-- > .. XS.get
--
-- If the type can't be inferred from the usage of the retrieved data, you
-- have to add an explicit type signature:
--
-- > .. XS.get :: X ListStorage
--
-- To make your data persistent between restarts, the data type needs to be
-- an instance of Read and Show and the instance declaration has to be changed:
--
-- > data ListStorage = ListStorage [Integer] deriving (Read,Show)
-- >
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
-- >   extensionType = PersistentExtension
--
-- One should take care that the string representation of the chosen type
-- is unique among the stored values, otherwise it will be overwritten.
-- Normally these string representations contain fully qualified module names
-- when automatically deriving Typeable, so
-- name collisions should not be a problem in most cases.
-- A module should not try to store common datatypes(e.g. a list of Integers)
-- without a custom data type as a wrapper to avoid collisions with other modules
-- trying to store the same data type without a wrapper.
--

-- | Modify the map of state extensions by applying the given function.
modifyStateExts
  :: XLike m
  => (M.Map String (Either String StateExtension)
  -> M.Map String (Either String StateExtension))
  -> m ()
modifyStateExts f = State.modify $ \st -> st { extensibleState = f (extensibleState st) }

-- | Apply a function to a stored value of the matching type or the initial value if there
-- is none.
modify :: (ExtensionClass a, XLike m) => (a -> a) -> m ()
modify f = put . f =<< get

-- | Like @modify@ but the result value is applied strictly in respect to
-- the monadic environment.
modify' :: (ExtensionClass a, XLike m) => (a -> a) -> m ()
modify' f = (put $!) . f =<< get

-- | Add a value to the extensible state field. A previously stored value with the same
-- type will be overwritten. (More precisely: A value whose string representation of its type
-- is equal to the new one's)
put :: (ExtensionClass a, XLike m) => a -> m ()
put v = modifyStateExts . M.insert (show . typeOf $ v) . Right . extensionType $ v

-- | Try to retrieve a value of the requested type, return an initial value if there is no such value.
get :: (ExtensionClass a, XLike m) => m a
get = getState' undefined -- `trick' to avoid needing -XScopedTypeVariables
  where toValue val = fromMaybe initialValue $ cast val
        getState' :: (ExtensionClass a, XLike m) => a -> m a
        getState' k = do
          v <- State.gets $ M.lookup (show . typeOf $ k) . extensibleState
          case v of
            Just (Right (StateExtension val)) -> return $ toValue val
            Just (Right (PersistentExtension val)) -> return $ toValue val
            Just (Left str) | PersistentExtension x <- extensionType k -> do
                let val = fromMaybe initialValue $ cast =<< safeRead str `asTypeOf` Just x
                put (val `asTypeOf` k)
                return val
            _ -> return initialValue
        safeRead str = case reads str of
                         [(x,"")] -> Just x
                         _ -> Nothing

gets :: (ExtensionClass a, XLike m) => (a -> b) -> m b
gets = flip fmap get

-- | Remove the value from the extensible state field that has the same type as the supplied argument
remove :: (ExtensionClass a, XLike m) => a -> m ()
remove wit = modifyStateExts $ M.delete (show . typeOf $ wit)

modified :: (ExtensionClass a, Eq a, XLike m) => (a -> a) -> m Bool
modified = modifiedM . (pure .)

modifiedM :: (ExtensionClass a, Eq a, XLike m) => (a -> m a) -> m Bool
modifiedM f = do
    v <- get
    f v >>= \case
        v' | v' == v   -> return False
           | otherwise -> put v' >> return True
