-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WindowProperties
-- Description :  EDSL for specifying window properties.
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- EDSL for specifying window properties; various utilities related to window
-- properties.
--
-----------------------------------------------------------------------------
module XMonad.Util.WindowProperties (
    -- * EDSL for window properties
    -- $edsl
    Property(..), hasProperty, focusedHasProperty, allWithProperty,
    propertyToQuery,
    -- * Helper functions
    -- $helpers
    getProp32, getProp32s)
where

import Foreign.C.Types (CLong)
import XMonad
import XMonad.Actions.TagWindows (hasTag)
import XMonad.Prelude (filterM)
import qualified XMonad.StackSet as W

-- $edsl
-- Allows to specify window properties, such as title, classname or
-- resource, and to check them.
--
-- In contrast to ManageHook properties, these are instances of Show and Read,
-- so they can be used in layout definitions etc. For example usage see "XMonad.Layout.IM"

-- | Most of the property constructors are quite self-explaining.
data Property = Title String
              | ClassName String
              | Resource String
              | Role String -- ^ WM_WINDOW_ROLE property
              | Machine String -- ^ WM_CLIENT_MACHINE property
              | And Property Property
              | Or  Property Property
              | Not Property
              | Const Bool
              | Tagged String -- ^ Tagged via 'XMonad.Actions.TagWindows'
              deriving (Read, Show)
infixr 9 `And`
infixr 8 `Or`

-- | Does given window have this property?
hasProperty :: Property -> Window -> X Bool
hasProperty p = runQuery (propertyToQuery p)

-- | Does the focused window have this property?
focusedHasProperty :: Property -> X Bool
focusedHasProperty p = do
    ws <- gets windowset
    let ms = W.stack $ W.workspace $ W.current ws
    case ms of
        Just s  -> hasProperty p $ W.focus s
        Nothing -> return False

-- | Find all existing windows with specified property
allWithProperty :: Property -> X [Window]
allWithProperty prop = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_,_,wins) <- io $ queryTree dpy rootw
    hasProperty prop `filterM` wins

-- | Convert property to 'Query' 'Bool' (see "XMonad.ManageHook")
propertyToQuery :: Property -> Query Bool
propertyToQuery (Title s) = title =? s
propertyToQuery (Resource s) = resource =? s
propertyToQuery (ClassName s) = className =? s
propertyToQuery (Role s) = stringProperty "WM_WINDOW_ROLE" =? s
propertyToQuery (Machine s) = stringProperty "WM_CLIENT_MACHINE" =? s
propertyToQuery (And p1 p2) = propertyToQuery p1 <&&> propertyToQuery p2
propertyToQuery (Or p1 p2) = propertyToQuery p1 <||> propertyToQuery p2
propertyToQuery (Not p) = not <$> propertyToQuery p
propertyToQuery (Const b) = return b
propertyToQuery (Tagged s) = ask >>= \w -> liftX (hasTag s w)

-- $helpers

-- | Get a window property from atom
getProp32 :: Atom -> Window -> X (Maybe [CLong])
getProp32 a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- | Get a window property from string
getProp32s :: String -> Window -> X (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }
