{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving,
  FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Util.WindowState
-- Description  :  Functions for saving per-window data.
-- Copyright    : (c) Dmitry Bogatov <KAction@gnu.org>
-- License      : BSD
--
-- Maintainer   : Dmitry Bogatov <KAction@gnu.org>
-- Stability    : unstable
-- Portability  : unportable
--
-- Functions for saving per-window data.
-----------------------------------------------------------------------------

module XMonad.Util.WindowState ( -- * Usage
                                 -- $usage
                                 get,
                                 put,
                                 StateQuery(..),
                                 runStateQuery,
                                 catchQuery ) where
import XMonad hiding (get, put, modify)
import Control.Monad.Reader(ReaderT(..))
import Control.Monad.State.Class
import Data.Typeable (typeOf)
-- $usage
--
-- This module allow to store state data with some 'Window'.
-- It is implemented with XProperties, so resources will be freed when
-- 'Window' is destoyed.
--
-- This module have advantage over "XMonad.Actions.TagWindows" in that it
-- hides from you implementation details and provides simple type-safe
-- interface.  Main datatype is "StateQuery", which is simple wrapper around
-- "Query", which is instance of MonadState, with 'put' and 'get' are
-- functions to acess data, stored in "Window".
--
-- To save some data in window you probably want to do following:
-- > (runStateQuery  (put $ Just value)  win) :: X ()
-- To retrive it, you can use
-- > (runStateQuery get win) :: X (Maybe YourValueType)
-- "Query" can be promoted to "StateQuery" simply by constructor,
-- and reverse is 'getQuery'.
--
-- For example, I use it to have all X applications @russian@ or @dvorak@
-- layout, but emacs have only @us@, to not screw keybindings. Use your
-- imagination!

-- | Wrapper around "Query" with phanom type @s@, representing state, saved in
-- window.
newtype StateQuery s a = StateQuery {
      getQuery :: Query a
    } deriving (Monad, MonadIO, Applicative, Functor)

packIntoQuery :: (Window -> X a) -> Query a
packIntoQuery = Query . ReaderT

-- | Apply "StateQuery" to "Window".
runStateQuery :: StateQuery s a -> Window ->  X a
runStateQuery = runQuery . getQuery

-- | Lifted to "Query" version of 'catchX'
catchQuery :: Query a -> Query (Maybe a)
catchQuery q = packIntoQuery $ \win -> userCode $ runQuery q win

-- | Instance of MonadState for StateQuery.
instance (Show s, Read s, Typeable s) => MonadState (Maybe s) (StateQuery s) where
    get = StateQuery  $ read' <$> get' undefined where
        get'   :: Maybe s -> Query String
        get' x = stringProperty (typePropertyName x)
        read'  :: (Read s) => String -> Maybe s
        read' "" = Nothing
        read' s  = Just $ read s
    put = StateQuery . packIntoQuery <$> setWindowProperty' where
        setWindowProperty' val = setWindowProperty prop strValue where
            prop = typePropertyName val
            strValue = maybe "" show val

typePropertyName :: (Typeable a) => a -> String
typePropertyName x = "_XMONAD_WINSTATE__" ++ show (typeOf x)

type PropertyName = String
setWindowProperty :: PropertyName -> String -> Window -> X ()
setWindowProperty prop val win = withDisplay $ \d -> io $
                                 internAtom d prop False >>=
                                 setTextProperty d win val
