{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Spacing
-- Copyright   :  (C) --   Brent Yorgey
--                    2018 Yclept Nemo
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  <byorgey@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Add a configurable amount of space around windows.
--
-- Note: For space\/gaps along edges of the /screen/ see "XMonad.Layout.Gaps".
-----------------------------------------------------------------------------

module XMonad.Layout.Spacing
    ( -- * Usage
      -- $usage
      Border (..)
    , Spacing (..)
    , ModifySpacing (..)
    , spacing
    , setWindowSpacing, setScreenSpacing
    , toggleSmartSpacing
    , incWindowSpacing, incScreenSpacing
    , decWindowSpacing, decScreenSpacing
    , borderIncrementBy
    ) where

import           XMonad
import           XMonad.Layout.LayoutModifier
import qualified XMonad.Util.Rectangle          as R

import           Control.Arrow


-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@
-- file:
--
-- > import XMonad.Layout.Spacing
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = spacing True (Border 0 10 10 10) (Border 10 10 10 10) $
-- >              layoutHook def

-- | Represent the borders of a rectangle.
data Border = Border
    { top       :: Integer
    , bottom    :: Integer
    , right     :: Integer
    , left      :: Integer
    } deriving (Show,Read)

-- | A 'LayoutModifier' providing customizable screen and window borders.
-- Borders are clamped to @[0,Infinity]@ before being applied.
data Spacing a = Spacing
    { smartBorder   :: Bool     -- ^ When @True@ borders are not applied if
                                --   there fewer than two windows.
    , screenBorder  :: Border   -- ^ The screen border.
    , windowBorder  :: Border   -- ^ The window borders.
    } deriving (Show,Read)

instance LayoutModifier Spacing a where
    -- This is a bit of a chicken-and-egg problem - the visible window list has
    -- yet to be generated. Several workarounds to incorporate the screen
    -- border:
    -- 1. Call 'runLayout' twice, with/without the screen border. Since layouts
    --    run arbitrary X actions, this breaks an important underlying
    --    assumption. Also, doesn't really solve the chicken-egg problem.
    -- 2. Create the screen border after and if the child layout returns more
    --    than one window. Unfortunately this breaks the window ratios
    --    presented by the child layout, another important assumption.
    -- 3. Create the screen border before, and remove it after and if the child
    --    layout returns fewer than two visible windows. This is somewhat hacky
    --    but probably the best option. Could significantly modify the child
    --    layout if it would have returned more than one window given the space
    --    of the screen border, but this is the underlying chicken-egg problem,
    --    and some concession must be made:
    --      * no border -> multiple windows
    --      * border -> single window
    --    Also slightly breaks layouts that expect to present absolutely-sized
    --    windows; a single window will be scaled up by the border size.
    --    Overall these are trivial assumptions.
    --
    -- Note #1: the original code counted the windows of the 'Workspace' stack,
    -- and so generated incorrect results even for the builtin 'Full' layout.
    -- Even though most likely true, it isn't guaranteed that a layout will
    -- never return windows not in the stack, specifically that an empty stack
    -- will lead to 0 visible windows and a stack with a single window will
    -- lead to 0-1 visible windows (see 'XMonad.Layout.Decoration'). So as much
    -- as I would like to pass a rectangle without screen borders to the child
    -- layout when appropriate (per the original approach), I can't. Since the
    -- screen border is always present whether displayed or not, child layouts
    -- can't depend on an accurate layout rectangle.
    modifyLayout (Spacing b sb _) wsp lr = do
        let sb1 = borderClampGTZero sb
        (wl,ml) <- runLayout wsp (withBorder' sb1 2 lr)
        let wl' = case wl of
                [(w,r)] | b ->
                    let sb2 = borderMap negate sb1
                        r'  = withBorder' sb2 2 r
                    in  [(w,r')]
                _ ->
                    wl
        return (wl',ml)

    -- This is run after 'modifyLayout'.
    pureModifier (Spacing True  _ _)  _ _ [x] =
        ([x], Nothing)
    pureModifier (Spacing _     _ wb) _ _ wrs =
        let wb' = borderClampGTZero wb
        in  (map (second $ withBorder' wb' 2) wrs, Nothing)

    pureMess (Spacing b sb wb) m
        | Just (ModifyWindowSpacing f) <- fromMessage m
        = Just $ Spacing b sb (f wb)
        | Just (ModifyScreenSpacing f) <- fromMessage m
        = Just $ Spacing b (f sb) wb
        | Just (ModifySmartSpacing f) <- fromMessage m
        = Just $ Spacing (f b) sb wb
        | otherwise
        = Nothing

    modifierDescription Spacing {} =
        "Spacing"


-- | Generate the 'ModifiedLayout', exposing all initial state of 'Spacing'.
spacing :: Bool     -- ^ The 'smartBorder'.
        -> Border   -- ^ The 'screenBorder'.
        -> Border   -- ^ The 'windowBorder'.
        -> l a -> ModifiedLayout Spacing l a
spacing b sb wb = ModifiedLayout (Spacing b sb wb)

-- | Messages to alter the state of 'Spacing' using the endomorphic function
-- arguments.
data ModifySpacing
    = ModifyWindowSpacing (Border -> Border)
    | ModifyScreenSpacing (Border -> Border)
    | ModifySmartSpacing (Bool -> Bool)
    deriving (Typeable)

instance Message ModifySpacing

-- | Set 'windowBorder' to the given 'Border'.
setWindowSpacing :: Border -> X ()
setWindowSpacing = sendMessage . ModifyWindowSpacing . const

-- | Set 'screenBorder' to the given 'Border'.
setScreenSpacing :: Border -> X ()
setScreenSpacing = sendMessage . ModifyScreenSpacing . const

-- | Toggle 'smartBorder'.
toggleSmartSpacing :: X ()
toggleSmartSpacing = sendMessage $ ModifySmartSpacing not

-- | Increment the borders of 'windowBorder' using 'borderIncrementBy', which
-- preserves border ratios during clamping.
incWindowSpacing :: Integer -> X ()
incWindowSpacing = sendMessage . ModifyWindowSpacing . borderIncrementBy

-- | Increment the borders of 'screenBorder' using 'borderIncrementBy'.
incScreenSpacing :: Integer -> X ()
incScreenSpacing = sendMessage . ModifyScreenSpacing . borderIncrementBy

-- | Inverse of 'incWindowSpacing', equivalent to applying 'negate'.
decWindowSpacing :: Integer -> X ()
decWindowSpacing = incWindowSpacing . negate

-- | Inverse of 'incScreenSpacing'.
decScreenSpacing :: Integer -> X ()
decScreenSpacing = incScreenSpacing . negate

-- | Map a function over a 'Border'. That is, over the four individual borders.
borderMap :: (Integer -> Integer) -> Border -> Border
borderMap f (Border t b r l) = Border (f t) (f b) (f r) (f l)

-- | Change the border spacing by the provided amount, adjusted so that at
-- least one border field is @>=0@.
borderIncrementBy :: Integer -> Border -> Border
borderIncrementBy i (Border t b r l) =
    let bl = [t,b,r,l]
        o  = maximum bl
        o' = max i $ negate o
        [t',b',r',l'] = map (+o') bl
    in  Border t' b' r' l'

-- | Interface to 'XMonad.Util.Rectangle.withBorder'.
withBorder' :: Border -> Integer -> Rectangle -> Rectangle
withBorder' (Border t b r l) = R.withBorder t b r l

-- | Clamp borders to within @[0,Infinity]@.
borderClampGTZero :: Border -> Border
borderClampGTZero (Border t b r l) =
    let bl = [t,b,r,l]
        [t',b',r',l'] = map (max 0) bl
    in  Border t' b' r' l'
