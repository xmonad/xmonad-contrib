-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WindowProperties
-- Copyright   :  (c) Roman Cheplyaka
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Roman Cheplyaka <roma@ro-che.info>
-- Stability   :  unstable
-- Portability :  unportable
--
-- EDSL for specifying window properties, such as title, classname or resource.
--
-----------------------------------------------------------------------------
module XMonad.Util.WindowProperties (
    -- * Usage
    -- $usage
    Property(..), hasProperty, focusedHasProperty, allWithProperty,
    getProp32, getProp32s,
    propertyToQuery)
where
import XMonad
import qualified XMonad.StackSet as W
import Foreign.C.Types (CLong)
import Control.Monad

-- $usage
-- This module allows to specify window properties, such as title, classname or
-- resource, and to check them.
--
-- In contrast to ManageHook properties, these are instances of Show and Read,
-- so they can be used in layout definitions etc. For example usage see "XMonad.Layout.IM"

-- | Most of the property constructors are quite self-explaining.
data Property = Title String
              | ClassName String
              | Resource String
              | Role String -- ^ WM_WINDOW_ROLE property
              | And Property Property  
              | Or  Property Property
              | Not Property
              | Const Bool
              deriving (Read, Show)
infixr 9 `And`
infixr 8 `Or`

-- | Does given window have this property?
hasProperty :: Property -> Window -> X Bool
hasProperty (Title s)     w = withDisplay $ \d -> fmap (Just s ==) $ io $ fetchName d w
hasProperty (Resource s)  w = withDisplay $ \d -> fmap ((==) s . resName ) $ io $ getClassHint d w
hasProperty (ClassName s) w = withDisplay $ \d -> fmap ((==) s . resClass) $ io $ getClassHint d w
hasProperty (Role s) w = withDisplay $ \d -> fmap ((==) (Just s)) $ getStringProperty d w "WM_WINDOW_ROLE"
hasProperty (And p1 p2)   w = do { r1 <- hasProperty p1 w; r2 <- hasProperty p2 w; return $ r1 && r2 }
hasProperty (Or p1 p2)    w = do { r1 <- hasProperty p1 w; r2 <- hasProperty p2 w; return $ r1 || r2 }
hasProperty (Not p1)      w = do { r1 <- hasProperty p1 w; return $ not r1 }
hasProperty (Const b)     _ = return b

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
propertyToQuery (And p1 p2) = propertyToQuery p1 <&&> propertyToQuery p2
propertyToQuery (Or p1 p2) = propertyToQuery p1 <||> propertyToQuery p2
propertyToQuery (Not p) = not `fmap` propertyToQuery p
propertyToQuery (Const b) = return b

-- | Get a window property from atom
getProp32 :: Atom -> Window -> X (Maybe [CLong])
getProp32 a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- | Get a window property from string
getProp32s :: String -> Window -> X (Maybe [CLong])
getProp32s str w = do { a <- getAtom str; getProp32 a w }
