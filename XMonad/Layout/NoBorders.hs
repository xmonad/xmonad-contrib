{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.NoBorders
-- Copyright   :  (c) David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Make a given layout display without borders.  This is useful for
-- full-screen or tabbed layouts, where you don't really want to waste a
-- couple of pixels of real estate just to inform yourself that the visible
-- window has focus.
--
-----------------------------------------------------------------------------

module XMonad.Layout.NoBorders (
                                -- * Usage
                                -- $usage
                                noBorders,
                                smartBorders,
                                withBorder,
                                lessBorders,
                                SetsAmbiguous(..),
                                Ambiguity(..),
                                With(..)
                               ) where

import XMonad
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W
import Data.List
import qualified Data.Map as M
import Data.Function (on)

-- $usage
-- You can use this module with the following in your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.NoBorders
--
-- and modify the layouts to call noBorders on the layouts you want to lack
-- borders:
--
-- > layoutHook = ... ||| noBorders Full ||| ...
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- todo, use an InvisibleList.
data WithBorder a = WithBorder Dimension [a] deriving ( Read, Show )

instance LayoutModifier WithBorder Window where
    unhook (WithBorder _ s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (WithBorder n s) _ _ wrs = do
        asks (borderWidth . config) >>= setBorders (s \\ ws)
        setBorders ws n
        return (wrs, Just $ WithBorder n ws)
     where
        ws = map fst wrs

-- | Removes all window borders from the specified layout.
noBorders :: LayoutClass l Window => l Window -> ModifiedLayout WithBorder l Window
noBorders = withBorder 0

-- | Forces a layout to use the specified border width. 'noBorders' is
-- equivalent to @'withBorder' 0@.
withBorder :: LayoutClass l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout $ WithBorder b []

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

singleton :: [a] -> Bool
singleton = null . drop 1

type SmartBorder = ConfigurableBorder Ambiguity

-- | Removes the borders from a window under one of the following conditions:
--
--  * There is only one screen and only one window. In this case it's obvious
--  that it has the focus, so no border is needed.
--
--  * A floating window covers the entire screen (e.g. mplayer).
--
smartBorders :: LayoutClass l a => l a -> ModifiedLayout SmartBorder l a
smartBorders = lessBorders Never

-- | Apply a datatype that has a SetsAmbiguous instance to provide a list of
-- windows that should not have borders.
--
-- This gives flexibility over when borders should be drawn, in particular with
-- xinerama setups: 'Ambiguity' has a number of useful 'SetsAmbiguous'
-- instances
lessBorders :: (SetsAmbiguous p, Read p, Show p, LayoutClass l a) =>
        p -> l a -> ModifiedLayout (ConfigurableBorder p) l a
lessBorders amb = ModifiedLayout (ConfigurableBorder amb [])

data ConfigurableBorder p w = ConfigurableBorder p [w] deriving (Read, Show)

instance (Read p, Show p, SetsAmbiguous p) => LayoutModifier (ConfigurableBorder p) Window where
    unhook (ConfigurableBorder _p s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (ConfigurableBorder p s) _ mst wrs = do
        ws <- withWindowSet (\wset -> return (hiddens p wset mst wrs))
        asks (borderWidth . config) >>= setBorders (s \\ ws)
        setBorders ws 0
        return (wrs, Just $ ConfigurableBorder p ws)

-- | SetsAmbiguous allows custom actions to generate lists of windows that
-- should not have borders drawn through 'ConfigurableBorder'
--
-- To add your own (though perhaps those options would better belong as an
-- aditional constructor to 'Ambiguity'), you can add the function as such:
--
-- > data MyAmbiguity = MyAmbiguity deriving (Read, Show)
--
-- > instance SetsAmbiguous MyAmbiguity where
-- >  hiddens _ wset mst wrs = otherHiddens Screen \\ otherHiddens OnlyFloat
-- >     where otherHiddens p = hiddens p wset mst wrs
--
-- The above example is redundant, because you can have the same result with:
--
-- > layoutHook = lessBorders (Combine Difference Screen OnlyFloat) (Tall 1 0.5 0.03 ||| ... )
--
-- To get the same result as 'smartBorders':
--
-- > layoutHook = lessBorders (Combine Never) (Tall 1 0.5 0.03 ||| ...)
--
-- This indirect method is required to keep the 'Read' and 'Show' for
-- ConfigurableBorder so that xmonad can serialize state.
class SetsAmbiguous p where
    hiddens :: p -> WindowSet -> Maybe (W.Stack Window) -> [(Window, Rectangle)] -> [Window]

instance SetsAmbiguous Ambiguity where
    hiddens amb wset mst wrs
      | Combine Union a b <- amb = on union next a b
      | Combine Difference a b <- amb = on (\\) next a b
      | Combine Intersection a b <- amb = on intersect next a b
      | otherwise = tiled ms ++ floating
      where next p = hiddens p wset mst wrs
            nonzerorect (Rectangle _ _ 0 0) = False
            nonzerorect _ = True

            screens =
              [ scr | scr <- W.screens wset,
                      case amb of
                            Never -> True
                            _ -> not $ null $ integrate scr,
                      nonzerorect . screenRect $ W.screenDetail scr]
            floating = [ w |
                        (w, W.RationalRect px py wx wy) <- M.toList . W.floating $ wset,
                        px <= 0, py <= 0,
                        wx + px >= 1, wy + py >= 1]
            ms = filter (`elem` W.integrate' mst) $ map fst wrs
            tiled [w]
              | Screen <- amb = [w]
              | OnlyFloat <- amb = []
              | OtherIndicated <- amb
              , let nonF = map integrate $ W.current wset : W.visible wset
              , length (concat nonF) > length wrs
              , singleton $ filter (1==) $ map length nonF = [w]
              | singleton screens = [w]
            tiled _ = []
            integrate y = W.integrate' . W.stack $ W.workspace y

-- | In order of increasing ambiguity (less borders more frequently), where
-- subsequent constructors add additional cases where borders are not drawn
-- than their predecessors. These behaviors make most sense with with multiple
-- screens: for single screens, 'Never' or 'smartBorders' makes more sense.
data Ambiguity = Combine With Ambiguity Ambiguity
                             -- ^ This constructor is used to combine the
                             -- borderless windows provided by the
                             -- SetsAmbiguous instances from two other
                             -- 'Ambiguity' data types.
               | OnlyFloat   -- ^ Only remove borders on floating windows that
                             -- cover the whole screen
               | Never       -- ^ Never remove borders when ambiguous:
                             -- this is the same as smartBorders
               | EmptyScreen -- ^ Focus in an empty screens does not count as
                             -- ambiguous.
               | OtherIndicated
                             -- ^ No borders on full when all other screens
                             -- have borders.
               | Screen      -- ^ Borders are never drawn on singleton screens.
                             -- With this one you really need another way such
                             -- as a statusbar to detect focus.
    deriving (Read, Show)

-- | Used to indicate to the 'SetsAmbiguous' instance for 'Ambiguity' how two
-- lists should be combined.
data With = Union        -- ^ uses 'Data.List.union'
          | Difference   -- ^ uses 'Data.List.\\'
          | Intersection -- ^ uses 'Data.List.intersect'
        deriving (Read, Show)
