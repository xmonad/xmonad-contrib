{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses
  , PatternGuards, DeriveDataTypeable, ExistentialQuantification
  , FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ZoomRow
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Row layout with individually resizable elements.
--
-----------------------------------------------------------------------------

module XMonad.Layout.ZoomRow ( -- * Usage
                               -- $usage
                               ZoomRow
                               -- * Creation
                             , zoomRow
                               -- * Messages
                             , ZoomMessage(..)
                             , zoomIn
                             , zoomOut
                             , zoomReset
                               -- * Use with non-'Eq' elements
                               -- $noneq
                             , zoomRowWith
                             , EQF(..)
                             , ClassEQ(..)
                             ) where

import XMonad
import qualified XMonad.StackSet as W

import XMonad.Util.Stack
import XMonad.Layout.Decoration (fi)

import Data.Maybe (fromMaybe)
import Control.Arrow (second)

-- $usage
-- This module provides a layout which places all windows in a single
-- row; the size occupied by each individual window can be increased
-- and decreased, and a window can be set to use the whole available
-- space whenever it has focus.
--
-- You can use this module by including  the following in your @~\/.xmonad/xmonad.hs@:
--
-- > import XMonad.Layout.ZoomRow
--
-- and using 'zoomRow' somewhere in your 'layoutHook', for example:
--
-- > myLayout = zoomRow ||| Mirror zoomRow
--
-- To be able to resize windows, you can create keybindings to send
-- the relevant 'ZoomMessage's:
--
-- >   -- Increase the size occupied by the focused window
-- > , ((modMask .|. shifMask, xK_minus), sendMessage zoomIn)
-- >   -- Decrease the size occupied by the focused window
-- > , ((modMayk             , xK_minus), sendMessage zoomOut)
-- >   -- Reset the size occupied by the focused window
-- > , ((modMask             , xK_equal), sendMessage zoomReset)
-- >   -- (Un)Maximize the focused window
-- > , ((modMask             , xK_f    ), sendMessage ToggleZoomFull)
--
-- For more information on editing your layout hook and key bindings,
-- see "XMonad.Doc.Extending".

-- * Creation functions

-- | 'ZoomRow' layout for laying out elements which are instances of
-- 'Eq'. Perfect for 'Window's.
zoomRow :: (Eq a, Show a, Read a) => ZoomRow ClassEQ a
zoomRow = ZC ClassEQ emptyZ

-- $noneq
-- Haskell's 'Eq' class is usually concerned with structural equality, whereas
-- what this layout really wants is for its elements to have a unique identity,
-- even across changes. There are cases (such as, importantly, 'Window's) where
-- the 'Eq' instance for a type actually does that, but if you want to lay
-- out something more exotic than windows and your 'Eq' means something else,
-- you can use the following.

-- | ZoomRow layout with a custom equality predicate. It should
-- of course satisfy the laws for 'Eq', and you should also make
-- sure that the layout never has to handle two \"equal\" elements
-- at the same time (it won't do any huge damage, but might behave
-- a bit strangely).
zoomRowWith :: (EQF f a, Show (f a), Read (f a), Show a, Read a)
               => f a -> ZoomRow f a
zoomRowWith f = ZC f emptyZ


-- * The datatypes

-- | A layout that arranges its windows in a horizontal row,
-- and allows to change the relative size of each element
-- independently.
data ZoomRow f a = ZC { zoomEq ::  f a
                          -- ^ Function to compare elements for
                          -- equality, a real Eq instance might
                          -- not be what you want in some cases
                      , zoomRatios :: (Zipper (Elt a))
                          -- ^ Element specs. The zipper is so we
                          -- know what the focus is when we handle
                          --  a message
                      }
  deriving (Show, Read, Eq)

-- | Class for equivalence relations. Must be transitive, reflexive.
class EQF f a where
    eq :: f a -> a -> a -> Bool

-- | To use the usual '==':
data ClassEQ a = ClassEQ
  deriving (Show, Read, Eq)

instance Eq a => EQF ClassEQ a where
    eq _ a b = a == b

-- | Size specification for an element.
data Elt a = E { elt :: a -- ^ The element
               , ratio :: Rational -- ^ Its size ratio
               , full :: Bool -- ^ Whether it should occupy all the
                              -- available space when it has focus.
               }
  deriving (Show, Read, Eq)


-- * Helpers

getRatio :: Elt a -> (a, Rational)
getRatio (E a r _) = (a,r)

lookupBy :: (a -> a -> Bool) -> a -> [Elt a] -> Maybe (Elt a)
lookupBy _ _ [] = Nothing
lookupBy f a (E a' r b : _) | f a a' = Just $ E a r b
lookupBy f a (_:es) = lookupBy f a es

setFocus :: Zipper a -> a -> Zipper a
setFocus Nothing a = Just $ W.Stack a [] []
setFocus (Just s) a = Just s { W.focus = a }


-- * Messages

-- | The type of messages accepted by a 'ZoomRow' layout
data ZoomMessage = Zoom Rational
                 -- ^ Multiply the focused window's size factor
                 -- by the given number.
                 | ZoomTo Rational
                 -- ^ Set the focused window's size factor to the
                 -- given number.
                 | ZoomFull Bool
                 -- ^ Set whether the focused window should occupy
                 -- all available space when it has focus
                 | ZoomFullToggle
                 -- ^ Toggle whether the focused window should
                 -- occupy all available space when it has focus
  deriving (Typeable, Show)

instance Message ZoomMessage

-- | Increase the size of the focused window.
-- Defined as @Zoom 1.5@
zoomIn :: ZoomMessage
zoomIn = Zoom 1.5

-- | Decrease the size of the focused window.
-- Defined as @Zoom (2/3)@
zoomOut :: ZoomMessage
zoomOut = Zoom $ 2/3

-- | Reset the size of the focused window.
-- Defined as @ZoomTo 1@
zoomReset :: ZoomMessage
zoomReset = ZoomTo 1


-- * LayoutClass instance

instance (EQF f a, Show a, Read a, Show (f a), Read (f a), Typeable f)
    => LayoutClass (ZoomRow f) a where
    description (ZC _ Nothing) = "ZoomRow"
    description (ZC _ (Just s)) = "ZoomRow" ++ if full $ W.focus s
                                                then " (Max)"
                                                else ""

    emptyLayout (ZC _ Nothing) _ = return ([], Nothing)
    emptyLayout (ZC f _) _ = return ([], Just $ ZC f Nothing)

    doLayout (ZC f zelts) r@(Rectangle _ _ w _) s
        = let elts = W.integrate' zelts
              zelts' = mapZ_ (\a -> fromMaybe (E a 1 False)
                                    $ lookupBy (eq f) a elts) $ Just s
              elts' = W.integrate' zelts'

              maybeL' = if zelts `noChange` zelts'
                          then Nothing
                          else Just $ ZC f zelts'

              total = sum  $ map ratio elts'

              widths =  map (second ((* fi w) . (/total)) . getRatio) elts'

          in case getFocusZ zelts' of
               Just (E a _ True) -> return ([(a, r)], maybeL')
               _ -> return (makeRects r widths, maybeL')

        where makeRects :: Rectangle -> [(a, Rational)] -> [(a, Rectangle)]
              makeRects r pairs = let as = map fst pairs
                                      widths = map snd pairs
                                      discreteWidths = snd $ foldr discretize (0, []) widths
                                      rectangles = snd $ foldr makeRect (r, []) discreteWidths
                                  in zip as rectangles

              -- | Make a new rectangle by substracting the given width from the available
              -- space (from the right, since this is a foldr)
              makeRect :: Dimension -> (Rectangle, [Rectangle]) -> (Rectangle, [Rectangle])
              makeRect w (Rectangle x y w0 h, rs) = ( Rectangle x y (w0-w) h
                                                    , Rectangle (x+fi w0-fi w) y w h : rs )

              -- | Round a list of fractions in a way that maintains the total.
              -- If you know a better way to do this I'm very interested.
              discretize :: Rational -> (Rational, [Dimension]) -> (Rational, [Dimension])
              discretize r (carry, ds) = let (d, carry') = properFraction $ carry+r
                                         in (carry', d:ds)

              noChange z1 z2 = toTags z1 `helper` toTags z2
                  where helper [] [] = True
                        helper (Right a:as) (Right b:bs) = a `sameAs` b && as `helper` bs
                        helper (Left a:as) (Left b:bs) = a `sameAs` b && as `helper` bs
                        helper _ _ = False
                        E a1 r1 b1 `sameAs` E a2 r2 b2 = (eq f a1 a2) && (r1 == r2) && (b1 == b2)

    pureMessage (ZC f zelts) sm | Just (ZoomFull False) <- fromMessage sm
                                , Just (E a r True) <- getFocusZ zelts
        = Just $ ZC f $ setFocus zelts $ E a r False

    pureMessage (ZC f zelts) sm | Just (ZoomFull True) <- fromMessage sm
                                , Just (E a r False) <- getFocusZ zelts
        = Just $ ZC f $ setFocus zelts $ E a r True

    pureMessage (ZC f zelts) sm | Just (E a r b) <- getFocusZ zelts
        = case fromMessage sm of
            Just (Zoom r') -> Just $ ZC f $ setFocus zelts $ E a (r*r') b
            Just (ZoomTo r') -> Just $ ZC f $ setFocus zelts $ E a r' b
            Just ZoomFullToggle -> pureMessage (ZC f zelts)
                                     $ SomeMessage $ ZoomFull $ not b
            _ -> Nothing

    pureMessage _ _ = Nothing