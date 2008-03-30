{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS -fglasgow-exts #-}
-- deriving Typeable
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageDocks
-- Copyright    : (c) Joachim Breitner <mail@joachim-breitner.de>
-- License      : BSD
--
-- Maintainer   : Joachim Breitner <mail@joachim-breitner.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides tools to automatically manage 'dock' type programs,
-- such as gnome-panel, kicker, dzen, and xmobar.

module XMonad.Hooks.ManageDocks (
    -- * Usage
    -- $usage
    manageDocks, AvoidStruts, avoidStruts, ToggleStruts(ToggleStruts)
    ) where


-----------------------------------------------------------------------------
import XMonad
import Foreign.C.Types (CLong)
import Control.Monad
import XMonad.Layout.LayoutModifier

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ManageDocks
--
-- The first component is a 'ManageHook' which recognizes these windows.  To
-- enable it:
--
-- > manageHook = ... <+> manageDocks
--
-- The second component is a layout modifier that prevents windows from
-- overlapping these dock windows.  It is intended to replace xmonad's
-- so-called \"gap\" support.  First, you must add it to your list of layouts:
--
-- > layoutHook = avoidStruts (tall ||| mirror tall ||| ...)
-- >                   where  tall = Tall 1 (3/100) (1/2)
--
-- 'AvoidStruts' also supports toggling the dock gap, add a keybinding similar
-- to:
--
-- > ,((modMask x, xK_b     ), sendMessage ToggleStruts)
--
-- /Important note/: if you are switching from manual gaps
-- (defaultGaps in your config) to avoidStruts (recommended, since
-- manual gaps will probably be phased out soon), be sure to switch
-- off all your gaps (with mod-b) /before/ reloading your config with
-- avoidStruts!  Toggling struts with a 'ToggleStruts' message will
-- not work unless your gaps are set to zero.
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- |
-- Detects if the given window is of type DOCK and if so, reveals it, but does
-- not manage it. If the window has the STRUT property set, adjust the gap accordingly.
manageDocks :: ManageHook
manageDocks = checkDock --> doIgnore

-- |
-- Checks if a window is a DOCK or DESKTOP window
checkDock :: Query Bool
checkDock = ask >>= \w -> liftX $ do
    a <- getAtom "_NET_WM_WINDOW_TYPE"
    dock <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
    desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
    mbr <- getProp a w
    case mbr of
        Just [r] -> return $ elem (fromIntegral r) [dock, desk]
        _        -> return False

-- |
-- Gets the STRUT config, if present, in xmonad gap order
getStrut :: Window -> X [Strut]
getStrut w = do
    spa <- getAtom "_NET_WM_STRUT_PARTIAL"
    sa  <- getAtom "_NET_WM_STRUT"
    msp <- getProp spa w
    case msp of
        Just sp -> return $ parseStrutPartial sp
        Nothing -> fmap (maybe [] parseStrut) $ getProp sa w
 where
    parseStrut xs@[_, _, _, _] = parseStrutPartial . take 12 $ xs ++ cycle [minBound, maxBound]
    parseStrut _ = []

    parseStrutPartial [l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2]
     = filter (\(_, n, _, _) -> n /= 0)
        [(L, l, ly1, ly2), (R, r, ry1, ry2), (T, t, tx1, tx2), (B, b, bx1, bx2)]
    parseStrutPartial _ = []

-- |
-- Helper to read a property
getProp :: Atom -> Window -> X (Maybe [CLong])
getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w

-- |
-- Goes through the list of windows and find the gap so that all STRUT
-- settings are satisfied.
calcGap :: X (Rectangle -> Rectangle)
calcGap = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    -- We don't keep track of dock like windows, so we find all of them here
    (_,_,wins) <- io $ queryTree dpy rootw
    struts <- concat `fmap` mapM getStrut wins

    -- we grab the window attributes of the root window rather than checking
    -- the width of the screen because xlib caches this info and it tends to
    -- be incorrect after RAndR
    wa <- io $ getWindowAttributes dpy rootw
    let screen = r2c $ Rectangle (fi $ wa_x wa) (fi $ wa_y wa) (fi $ wa_width wa) (fi $ wa_height wa)
    return $ \r -> c2r $ foldr (reduce screen) (r2c r) struts

-- | Adjust layout automagically.
avoidStruts :: LayoutClass l a => l a -> ModifiedLayout AvoidStruts l a
avoidStruts = ModifiedLayout (AvoidStruts True)

data AvoidStruts a = AvoidStruts Bool deriving ( Read, Show )

data ToggleStruts = ToggleStruts deriving (Read,Show,Typeable)
instance Message ToggleStruts

instance LayoutModifier AvoidStruts a where
    modifyLayout (AvoidStruts b) w r = do
        nr <- if b then fmap ($ r) calcGap else return r
        runLayout w nr

    handleMess (AvoidStruts b ) m
        | Just ToggleStruts <- fromMessage m = return $ Just $ AvoidStruts (not b)
        | otherwise = return Nothing

data Side = L | R | T | B

-- | (Side, height\/width, initial pixel, final pixel).

type Strut = (Side, CLong, CLong, CLong)

-- | (Initial x pixel, initial y pixel,
--    final x pixel, final y pixel).

type RectC = (CLong, CLong, CLong, CLong)

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Invertible conversion.

r2c :: Rectangle -> RectC
r2c (Rectangle x y w h) = (fi x, fi y, fi x + fi w - 1, fi y + fi h - 1)

-- | Invertible conversion.

c2r :: RectC -> Rectangle
c2r (x1, y1, x2, y2) = Rectangle (fi x1) (fi y1) (fi $ x2 - x1 + 1) (fi $ y2 - y1 + 1)

-- TODO: Add these QuickCheck properties to the test suite, along with
-- suitable Arbitrary instances.

-- prop_r2c_c2r :: RectC -> Bool
-- prop_r2c_c2r r = r2c (c2r r) == r

-- prop_c2r_r2c :: Rectangle -> Bool
-- prop_c2r_r2c r = c2r (r2c r) == r

reduce :: RectC -> Strut -> RectC -> RectC
reduce (sx0, sy0, sx1, sy1) (s, n, l, h) (x0, y0, x1, y1) = case s of
    L | p (y0, y1) -> (mx x0 sx0    , y0       , x1       , y1       )
    R | p (y0, y1) -> (x0           , y0       , mn x1 sx1, y1       )
    T | p (x0, x1) -> (x0           , mx y0 sy0, x1       , y1       )
    B | p (x0, x1) -> (x0           , y0       , x1       , mn y1 sy1)
    _              -> (x0           , y0       , x1       , y1       )
 where
    mx a b = max a (b + n)
    mn a b = min a (b - n)
    p r = r `overlaps` (l, h)

-- | Do the two ranges overlap?
--
-- Precondition for every input range @(x, y)@: @x '<=' y@.
--
-- A range @(x, y)@ is assumed to include every pixel from @x@ to @y@.

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
(a, b) `overlaps` (x, y) =
  inRange (a, b) x || inRange (a, b) y || inRange (x, y) a
  where
  inRange (i, j) k = i <= k && k <= j
