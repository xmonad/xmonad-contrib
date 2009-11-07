-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExtensibleState
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
                              putState
                              , modifyState
                              , removeState
                              , getState
                              ) where

import Control.Applicative
import Data.Typeable (typeOf,Typeable,cast)
import qualified Data.Map as M
import XMonad.Core
import Control.Monad.State

-- ---------------------------------------------------------------------
-- $usage
--
-- To utilize this feature in a contrib module create a data type,
-- and make it an instance of ExtensionClass. You can then use
-- the functions from this module for storing your data:
--
-- > {-# LANGUAGE DeriveDataTypeable #-}
-- >
-- > data ListStorage = ListStorage [Integer] deriving Typeable
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
-- >
-- > .. putState (ListStorage [23,42])
--
-- To retrieve the stored data call:
--
-- > .. getState
--
-- If the type can't be infered from the usage of the retrieved data, you
-- might need to add an explicit type signature:
--
-- > .. getState :: X ListStorage
--
-- To make your data persistent between restarts, the data type needs to be
-- an instance of Read and Show and the instance declaration has to be changed:
--
-- > data ListStorage = ListStorage [Integer] deriving (Typeable,Read,Show)
-- >
-- > instance ExtensionClass ListStorage where
-- >   initialValue = ListStorage []
-- >   extensionType = PersistentExtension
--
-- One should take care that the string representation of the chosen type
-- is unique among the stored values, otherwise it will be overwritten.
-- Normally these values contain fully qualified module names when deriving Typeable, so
-- name collisions should not be a problem in most cases.
-- A module should not try to store common datatypes(e.g. a list of Integers)
-- without a custom data type as a wrapper to avoid those collisions.
--

-- | Modify the map of state extensions by applying the given function.
modifyStateExts :: (M.Map String (Either String StateExtension)
                   -> M.Map String (Either String StateExtension))
                -> X ()
modifyStateExts f = modify $ \st -> st { extensibleState = f (extensibleState st) }

-- | Apply a function to a stored value of the matching type or the initial value if there
-- is none.
modifyState :: ExtensionClass a => (a -> a) -> X ()
modifyState f = putState =<< f <$> getState

-- | Add a value to the extensible state field. A previously stored value with the same
-- type will be overwritten. (More precisely: A value whose string representation of its type
-- is equal to the new one's)
putState :: ExtensionClass a => a -> X ()
putState v = modifyStateExts . M.insert (show . typeOf $ v) . Right . extensionType $ v

-- | Try to retrieve a value of the requested type, return an initial value if there is no such value.
getState :: ExtensionClass a => X a
getState = getState' undefined -- `trick' to avoid needing -XScopedTypeVariables
  where toValue val = maybe initialValue id $ cast val
        getState' :: ExtensionClass a => a -> X a
        getState' k = do
          v <- gets $ M.lookup (show . typeOf $ k) . extensibleState
          case v of
            Just (Right (StateExtension val)) -> return $ toValue val
            Just (Right (PersistentExtension val)) -> return $ toValue val
            Just (Left str) -> case extensionType (undefined `asTypeOf` k) of
                                PersistentExtension x -> do
                                  let val = maybe initialValue id $
                                            cast =<< safeRead str `asTypeOf` (Just x)
                                  putState (val `asTypeOf` k)
                                  return val
                                _ -> return $ initialValue
            _ -> return $ initialValue
        safeRead str = case reads str of
                         [(x,"")] -> Just x
                         _ -> Nothing

-- | Remove the value from the extensible state field that has the same type as the supplied argument
removeState :: ExtensionClass a => a -> X ()
removeState wit = modifyStateExts $ M.delete (show . typeOf $ wit)
