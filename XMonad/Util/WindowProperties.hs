-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowProperties
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
    Property(..), hasProperty)
where
import XMonad

-- $usage
-- This module allows to specify window properties, such as title, classname or
-- resource, and to check them.
--
-- In contrast to ManageHook properties, these are instances of Show and Read,
-- so they can be used in layout definitions etc. For example usage see "XMonad.Layout.IM"

-- | Property constructors are quite self-explaining.
data Property = Title String
              | ClassName String
              | Resource String
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
hasProperty (And p1 p2)   w = do { r1 <- hasProperty p1 w; r2 <- hasProperty p2 w; return $ r1 && r2 }
hasProperty (Or p1 p2)    w = do { r1 <- hasProperty p1 w; r2 <- hasProperty p2 w; return $ r1 || r2 }
hasProperty (Not p1)      w = do { r1 <- hasProperty p1 w; return $ not r1 }
hasProperty (Const b)     _ = return b

