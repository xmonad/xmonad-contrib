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
    , spacingRaw
    , setSmartSpacing
    , setSpacing
    , setScreenSpacing, setScreenSpacingEnabled
    , setWindowSpacing, setWindowSpacingEnabled
    , toggleSmartSpacing
    , toggleScreenSpacingEnabled
    , toggleWindowSpacingEnabled
    , incSpacing
    , incWindowSpacing, incScreenSpacing
    , decWindowSpacing, decScreenSpacing
    , borderIncrementBy
      -- * Backwards Compatibility
      -- $backwardsCompatibility
    , SmartSpacing, SpacingWithEdge, SmartSpacingWithEdge
    , spacing, spacingWithEdge
    , smartSpacing, smartSpacingWithEdge
    ) where

import           XMonad
import           XMonad.StackSet                as W
import           XMonad.Layout.LayoutModifier
import qualified XMonad.Util.Rectangle          as R


-- $usage
-- You can use this module by importing it into your @~\/.xmonad\/xmonad.hs@
-- file:
--
-- > import XMonad.Layout.Spacing
--
-- and modifying your layoutHook as follows (for example):
--
-- > layoutHook = spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True $
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
    { smartBorder           :: Bool
        -- ^ When @True@ borders are not applied if
        --   there fewer than two windows.
    , screenBorder          :: Border
        -- ^ The screen border.
    , screenBorderEnabled   :: Bool
        -- ^ Is the screen border enabled?
    , windowBorder          :: Border
        -- ^ The window borders.
    , windowBorderEnabled   :: Bool
        -- ^ Is the window border enabled?
    } deriving (Show,Read)

instance Eq a => LayoutModifier Spacing a where
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
    --
    -- Note #2: If there are fewer than two stack windows displayed, the stack
    -- window (if present) is scaled up while the non-stack windows are moved a
    -- border-dependent amount based on their quadrant. So a non-stack window
    -- in the top-left quadrant will be moved using only the border's top and
    -- left components. Originally I was going to use an edge-attachment
    -- algorithm, but this is much simpler and covers most cases. Edge
    -- attachment would have scaled non-stack windows, but most non-stack
    -- windows are created by XMonad and therefore cannot be scaled. I suggest
    -- this layout be disabled for any incompatible child layouts.
    modifyLayout (Spacing _b _sb False _wb _wbe) wsp lr =
        runLayout wsp lr
    modifyLayout (Spacing b sb _sbe _wb _wbe) wsp lr = do
        let sb1 = borderClampGTZero sb
            lr' = withBorder' sb1 2 lr
            sb2 = toBorder lr' lr
        (wrs,ml) <- runLayout wsp lr'
        let ff (w,wr) (i,ps) = if w `elem` (W.integrate' . W.stack $ wsp)
                               then let wr' = withBorder' sb2 2 wr
                                    in  (i+1,(w,wr'):ps)
                               else let wr' = moveByQuadrant lr wr sb2
                                    in  (i,(w,wr'):ps)
            (c,wrs') = foldr ff (0::Integer,[]) wrs
        return $ if c <= 1 && b
            then (wrs',ml)
            else (wrs,ml)
      where
        moveByQuadrant :: Rectangle -> Rectangle -> Border -> Rectangle
        moveByQuadrant rr mr@(Rectangle {rect_x = x, rect_y = y}) (Border bt bb br bl) =
            let (rcx,rcy) = R.center rr
                (mcx,mcy) = R.center mr
                dx = orderSelect (compare mcx rcx) (bl,0,negate br)
                dy = orderSelect (compare mcy rcy) (bt,0,negate bb)
            in  mr { rect_x = x + fromIntegral dx, rect_y = y + fromIntegral dy }

    -- This is run after 'modifyLayout' but receives the original stack, not
    -- one possibly modified by the child layout. Does not remove borders from
    -- windows not in the stack, i.e. decorations generated by
    -- 'XMonad.Layout.Decorations'.
    pureModifier (Spacing _b _sb _sbe _wb False) _lr _mst wrs =
        (wrs, Nothing)
    pureModifier (Spacing b _sb _sbe wb _wbe) _lr mst wrs =
        let wb' = borderClampGTZero wb
            ff p@(w,wr) (i,ps) = if w `elem` W.integrate' mst
                                 then let wr' = withBorder' wb' 2 wr
                                      in  (i+1,(w,wr'):ps)
                                 else (i,p:ps)
            (c,wrs') = foldr ff (0::Integer,[]) wrs
        in  if c <= 1 && b
            then (wrs, Nothing)
            else (wrs', Nothing)

    pureMess s@(Spacing b sb sbe wb wbe) m
        | Just (ModifySmartBorder f) <- fromMessage m
        = Just $ s { smartBorder = f b }
        | Just (ModifyScreenBorder f) <- fromMessage m
        = Just $ s { screenBorder = f sb }
        | Just (ModifyScreenBorderEnabled f) <- fromMessage m
        = Just $ s { screenBorderEnabled = f sbe }
        | Just (ModifyWindowBorder f) <- fromMessage m
        = Just $ s { windowBorder = f wb }
        | Just (ModifyWindowBorderEnabled f) <- fromMessage m
        = Just $ s { windowBorderEnabled = f wbe }
        | Just (ModifySpacing f) <- fromMessage m
        = Just $ let modSp = borderMap (fromIntegral . max 0 . f . fromIntegral)
                 in  s { screenBorder = modSp sb, windowBorder = modSp wb }
        | otherwise
        = Nothing

    modifierDescription Spacing {} =
        "Spacing"


-- | Generate the 'ModifiedLayout', exposing all initial state of 'Spacing'.
spacingRaw :: Bool     -- ^ The 'smartBorder'.
           -> Border   -- ^ The 'screenBorder'.
           -> Bool     -- ^ The 'screenBorderEnabled'.
           -> Border   -- ^ The 'windowBorder'.
           -> Bool     -- ^ The 'windowBorderEnabled'.
           -> l a -> ModifiedLayout Spacing l a
spacingRaw b sb sbe wb wbe = ModifiedLayout (Spacing b sb sbe wb wbe)

-- | Messages to alter the state of 'Spacing' using the endomorphic function
-- arguments.
data ModifySpacing
    = ModifySmartBorder (Bool -> Bool)
    | ModifyScreenBorder (Border -> Border)
    | ModifyScreenBorderEnabled (Bool -> Bool)
    | ModifyWindowBorder (Border -> Border)
    | ModifyWindowBorderEnabled (Bool -> Bool)
    | ModifySpacing (Int -> Int)
    deriving (Typeable)

instance Message ModifySpacing

-- | Set 'smartBorder' to the given 'Bool'.
setSmartSpacing :: Bool -> X ()
setSmartSpacing = sendMessage . ModifySmartBorder . const

-- | Set 'screenBorder' to the given 'Border'.
setScreenSpacing :: Border -> X ()
setScreenSpacing = sendMessage . ModifyScreenBorder . const

-- | Set 'screenBorderEnabled' to the given 'Bool'.
setScreenSpacingEnabled :: Bool -> X ()
setScreenSpacingEnabled = sendMessage . ModifyScreenBorderEnabled . const

-- | Set 'windowBorder' to the given 'Border'.
setWindowSpacing :: Border -> X ()
setWindowSpacing = sendMessage . ModifyWindowBorder . const

-- | Set 'windowBorderEnabled' to the given 'Bool'.
setWindowSpacingEnabled :: Bool -> X ()
setWindowSpacingEnabled = sendMessage . ModifyWindowBorderEnabled . const

-- | Toggle 'smartBorder'.
toggleSmartSpacing :: X ()
toggleSmartSpacing = sendMessage $ ModifySmartBorder not

-- | Toggle 'screenBorderEnabled'.
toggleScreenSpacingEnabled :: X ()
toggleScreenSpacingEnabled = sendMessage $ ModifyScreenBorderEnabled not

-- | Toggle 'windowBorderEnabled'.
toggleWindowSpacingEnabled :: X ()
toggleWindowSpacingEnabled = sendMessage $ ModifyWindowBorderEnabled not

-- | Increment the borders of 'windowBorder' using 'borderIncrementBy', which
-- preserves border ratios during clamping.
incWindowSpacing :: Integer -> X ()
incWindowSpacing = sendMessage . ModifyWindowBorder . borderIncrementBy

-- | Increment the borders of 'screenBorder' using 'borderIncrementBy'.
incScreenSpacing :: Integer -> X ()
incScreenSpacing = sendMessage . ModifyScreenBorder . borderIncrementBy

-- | Inverse of 'incWindowSpacing', equivalent to applying 'negate'.
decWindowSpacing :: Integer -> X ()
decWindowSpacing = incWindowSpacing . negate

-- | Inverse of 'incScreenSpacing'.
decScreenSpacing :: Integer -> X ()
decScreenSpacing = incScreenSpacing . negate

-- | Set all borders to a uniform size.
setSpacing :: Int -> X ()
setSpacing = sendMessage . ModifySpacing . const

-- | Increment both screen and window borders uniformly.
incSpacing :: Int -> X ()
incSpacing = sendMessage . ModifySpacing . (+)

-- | Map a function over a 'Border'. That is, over the four individual borders.
borderMap :: (Integer -> Integer) -> Border -> Border
borderMap f (Border t b r l) = Border (f t) (f b) (f r) (f l)

-- | Clamp borders to within @[0,Infinity]@.
borderClampGTZero :: Border -> Border
borderClampGTZero = borderMap (max 0)

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

-- | Return the border necessary to derive the second rectangle from the first.
-- Since 'R.withBorder' may scale the borders to stay within rectangle bounds,
-- it is not an invertible operation, i.e. applying a negated border may not
-- return the original rectangle. Use this instead.
toBorder :: Rectangle -> Rectangle -> Border
toBorder r1 r2 =
    let R.PointRectangle r1_x1 r1_y1 r1_x2 r1_y2 = R.pixelsToCoordinates r1
        R.PointRectangle r2_x1 r2_y1 r2_x2 r2_y2 = R.pixelsToCoordinates r2
        l = r2_x1 - r1_x1
        r = r1_x2 - r2_x2
        t = r2_y1 - r1_y1
        b = r1_y2 - r2_y2
    in Border t b r l

-- | Given an ordering and a three-tuple, return the first tuple entry if 'LT',
-- second if 'EQ' and third if 'GT'.
orderSelect :: Ordering -> (a,a,a) -> a
orderSelect o (lt,eq,gt) = case o of
    LT -> lt
    EQ -> eq
    GT -> gt

-----------------------------------------------------------------------------
-- Backwards Compatibility:
-----------------------------------------------------------------------------
{-# DEPRECATED SmartSpacing, SpacingWithEdge, SmartSpacingWithEdge
  "Use Spacing instead." #-}
{-# DEPRECATED spacing, spacingWithEdge, smartSpacing, smartSpacingWithEdge "Use spacingRaw instead." #-}

-- $backwardsCompatibility
-- The following functions and types exist solely for compatibility with
-- pre-0.14 releases.

-- | A type synonym for the @Spacing@ @LayoutModifier@.
type SmartSpacing = Spacing

-- | A type synonym for the @Spacing@ @LayoutModifier@.
type SpacingWithEdge = Spacing

-- | A type synonym for the @Spacing@ @LayoutModifier@.
type SmartSpacingWithEdge = Spacing

-- | Surround all windows by a certain number of pixels of blank space. See
-- 'spacingRaw'.
spacing :: Int -> l a -> ModifiedLayout Spacing l a
spacing i' = spacingRaw False (Border 0 0 0 0) False (Border i i i i) True
    where i = fromIntegral i'

-- | Surround all windows by a certain number of pixels of blank space, and
-- additionally adds the same amount of spacing around the edge of the screen.
-- See 'spacingRaw'.
spacingWithEdge :: Int -> l a -> ModifiedLayout SpacingWithEdge l a
spacingWithEdge i' = spacingRaw False (Border i i i i) True (Border i i i i) True
    where i = fromIntegral i'

-- | Surrounds all windows with blank space, except when the window is the only
-- visible window on the current workspace. See 'spacingRaw'.
smartSpacing :: Int -> l a -> ModifiedLayout SmartSpacing l a
smartSpacing i' = spacingRaw True (Border 0 0 0 0) False (Border i i i i) True
    where i = fromIntegral i'

-- | Surrounds all windows with blank space, and adds the same amount of
-- spacing around the edge of the screen, except when the window is the only
-- visible window on the current workspace. See 'spacingRaw'.
smartSpacingWithEdge :: Int -> l a -> ModifiedLayout SmartSpacingWithEdge l a
smartSpacingWithEdge i' = spacingRaw True (Border i i i i) True (Border i i i i) True
    where i = fromIntegral i'
