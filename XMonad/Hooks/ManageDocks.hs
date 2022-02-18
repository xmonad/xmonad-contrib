{-# LANGUAGE PatternGuards, FlexibleInstances, MultiParamTypeClasses, CPP, LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageDocks
-- Description :  Automatically manage 'dock' type programs.
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
    docks, manageDocks, checkDock, AvoidStruts(..), avoidStruts, avoidStrutsOn,
    ToggleStruts(..),
    SetStruts(..),
    module XMonad.Util.Types,

#ifdef TESTING
    r2c,
    c2r,
    RectC(..),
#endif

    -- * For developers of other modules ("XMonad.Actions.FloatSnap")
    calcGap,

    -- * Standalone hooks (deprecated)
    docksEventHook, docksStartupHook,
    ) where


-----------------------------------------------------------------------------
import XMonad
import Foreign.C.Types (CLong)
import XMonad.Layout.LayoutModifier
import XMonad.Util.Types
import XMonad.Util.WindowProperties (getProp32s)
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prelude

import qualified Data.Set        as S
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

-- $usage
-- To use this module, add the following import to @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.ManageDocks
--
-- Wrap your xmonad config with a call to 'docks', like so:
--
-- > main = xmonad $ … . docks . … $ def{…}
--
-- Then add 'avoidStruts' or 'avoidStrutsOn' layout modifier to your layout
-- to prevent windows from overlapping these windows.
--
-- > layoutHook = avoidStruts (tall ||| mirror tall ||| ...)
-- >                   where  tall = Tall 1 (3/100) (1/2)
--
-- 'AvoidStruts' also supports toggling the dock gaps; add a keybinding
-- similar to:
--
-- > ,((modm, xK_b     ), sendMessage ToggleStruts)
--
-- If you have multiple docks, you can toggle their gaps individually.
-- For example, to toggle only the top gap:
--
-- > ,((modm .|. controlMask, xK_t), sendMessage $ ToggleStrut U)
--
-- Similarly, you can use 'D', 'L', and 'R' to individually toggle
-- gaps on the bottom, left, or right.
--
-- If you want certain docks to be avoided but others to be covered by
-- default, you can manually specify the sides of the screen on which
-- docks should be avoided, using 'avoidStrutsOn'.  For example:
--
-- > layoutHook = avoidStrutsOn [U,L] (tall ||| mirror tall ||| ...)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--

-- | Add docks functionality to the given config.  See above for an example.
docks :: XConfig a -> XConfig a
docks c = c { startupHook     = docksStartupHook <+> startupHook c
            , handleEventHook = docksEventHook <+> handleEventHook c
            , manageHook      = manageDocks <+> manageHook c }

type WindowStruts = M.Map Window [Strut]

data UpdateDocks = UpdateDocks
instance Message UpdateDocks

refreshDocks :: X ()
refreshDocks = sendMessage UpdateDocks

-- Nothing means cache hasn't been initialized yet
newtype StrutCache = StrutCache { fromStrutCache :: Maybe WindowStruts }
    deriving Eq

instance ExtensionClass StrutCache where
    initialValue = StrutCache Nothing

modifiedStrutCache :: (Maybe WindowStruts -> X WindowStruts) -> X Bool
modifiedStrutCache f = XS.modifiedM $ fmap (StrutCache . Just) . f . fromStrutCache

getStrutCache :: X WindowStruts
getStrutCache = do
    cache <- maybeInitStrutCache =<< XS.gets fromStrutCache
    cache <$ XS.put (StrutCache (Just cache))

updateStrutCache :: Window -> X Bool
updateStrutCache w = modifiedStrutCache $ updateStrut w <=< maybeInitStrutCache

deleteFromStrutCache :: Window -> X Bool
deleteFromStrutCache w = modifiedStrutCache $ fmap (M.delete w) . maybeInitStrutCache

maybeInitStrutCache :: Maybe WindowStruts -> X WindowStruts
maybeInitStrutCache = maybe (queryDocks >>= foldlM (flip updateStrut) M.empty) pure
  where
    queryDocks = withDisplay $ \dpy -> do
        (_, _, wins) <- io . queryTree dpy =<< asks theRoot
        filterM (runQuery checkDock) wins

updateStrut :: Window -> WindowStruts -> X WindowStruts
updateStrut w cache = do
    when (w `M.notMember` cache) $ requestDockEvents w
    strut <- getStrut w
    pure $ M.insert w strut cache

-- | Detects if the given window is of type DOCK and if so, reveals
--   it, but does not manage it.
manageDocks :: ManageHook
manageDocks = checkDock --> (doIgnore <+> doRequestDockEvents)
  where
    doRequestDockEvents = ask >>= liftX . requestDockEvents >> mempty

-- | Request events for a dock window.
-- (Only if not already a client to avoid overriding 'clientMask')
requestDockEvents :: Window -> X ()
requestDockEvents w = whenX (not <$> isClient w) $ withDisplay $ \dpy ->
    withWindowAttributes dpy w $ \attrs -> io $ selectInput dpy w $
        wa_your_event_mask attrs .|. propertyChangeMask .|. structureNotifyMask

-- | Checks if a window is a DOCK or DESKTOP window.
-- Ignores xmonad's own windows (usually _NET_WM_WINDOW_TYPE_DESKTOP) to avoid
-- unnecessary refreshes.
checkDock :: Query Bool
checkDock = isDockOrDesktop <&&> (not <$> isXMonad)
  where
    isDockOrDesktop = ask >>= \w -> liftX $ do
        dock <- getAtom "_NET_WM_WINDOW_TYPE_DOCK"
        desk <- getAtom "_NET_WM_WINDOW_TYPE_DESKTOP"
        mbr <- getProp32s "_NET_WM_WINDOW_TYPE" w
        case mbr of
            Just rs -> return $ any ((`elem` [dock,desk]) . fromIntegral) rs
            _       -> return False
    isXMonad = className =? "xmonad"

-- | Whenever a new dock appears, refresh the layout immediately to avoid the
-- new dock.
{-# DEPRECATED docksEventHook "Use docks instead." #-}
docksEventHook :: Event -> X All
docksEventHook MapNotifyEvent{ ev_window = w } = do
    whenX (runQuery checkDock w <&&> (not <$> isClient w)) $
        whenX (updateStrutCache w) refreshDocks
    return (All True)
docksEventHook PropertyEvent{ ev_window = w
                            , ev_atom = a } = do
    nws <- getAtom "_NET_WM_STRUT"
    nwsp <- getAtom "_NET_WM_STRUT_PARTIAL"
    when (a == nws || a == nwsp) $
        whenX (updateStrutCache w) refreshDocks
    return (All True)
docksEventHook DestroyWindowEvent{ ev_window = w } = do
    whenX (deleteFromStrutCache w) refreshDocks
    return (All True)
docksEventHook _ = return (All True)

{-# DEPRECATED docksStartupHook "Use docks instead." #-}
docksStartupHook :: X ()
docksStartupHook = void getStrutCache

-- | Gets the STRUT config, if present, in xmonad gap order
getStrut :: Window -> X [Strut]
getStrut w = do
    msp <- getProp32s "_NET_WM_STRUT_PARTIAL" w
    case msp of
        Just sp -> return $ parseStrutPartial sp
        Nothing -> maybe [] parseStrut <$> getProp32s "_NET_WM_STRUT" w
 where
    parseStrut xs@[_, _, _, _] = parseStrutPartial . take 12 $ xs ++ cycle [minBound, maxBound]
    parseStrut _ = []

    parseStrutPartial [l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2]
     = filter (\(_, n, _, _) -> n /= 0)
        [(L, l, ly1, ly2), (R, r, ry1, ry2), (U, t, tx1, tx2), (D, b, bx1, bx2)]
    parseStrutPartial _ = []

-- | Goes through the list of windows and find the gap so that all
--   STRUT settings are satisfied.
calcGap :: S.Set Direction2D -> X (Rectangle -> Rectangle)
calcGap ss = do
    rootw <- asks theRoot
    struts <- filter careAbout . concat . M.elems <$> getStrutCache

    -- If possible, we grab the window attributes of the root window rather
    -- than checking the width of the screen because xlib caches this info
    -- and it tends to be incorrect after RAndR
    screen <- safeGetWindowAttributes rootw >>= \case
        Nothing -> gets $ r2c . screenRect . W.screenDetail . W.current . windowset
        Just wa -> pure . r2c $ Rectangle (fi $ wa_x wa) (fi $ wa_y wa) (fi $ wa_width wa) (fi $ wa_height wa)
    return $ \r -> c2r $ foldr (reduce screen) (r2c r) struts
  where careAbout (s,_,_,_) = s `S.member` ss

-- | Adjust layout automagically: don't cover up any docks, status
--   bars, etc.
--
--   Note that this modifier must be applied before any modifier that
--   changes the screen rectangle, or struts will be applied in the wrong
--   place and may affect the other modifier(s) in odd ways. This is
--   most commonly seen with the 'spacing' modifier and friends.
avoidStruts :: LayoutClass l a => l a -> ModifiedLayout AvoidStruts l a
avoidStruts = avoidStrutsOn [U,D,L,R]

-- | Adjust layout automagically: don't cover up docks, status bars,
--   etc. on the indicated sides of the screen.  Valid sides are 'U'
--   (top), 'D' (bottom), 'R' (right), or 'L' (left). The warning in
--   'avoidStruts' applies to this modifier as well.
avoidStrutsOn :: LayoutClass l a =>
                 [Direction2D]
              -> l a
              -> ModifiedLayout AvoidStruts l a
avoidStrutsOn ss = ModifiedLayout $ AvoidStruts (S.fromList ss)

newtype AvoidStruts a = AvoidStruts (S.Set Direction2D) deriving ( Read, Show )

-- | Message type which can be sent to an 'AvoidStruts' layout
--   modifier to alter its behavior.
data ToggleStruts = ToggleStruts
                  | ToggleStrut Direction2D
  deriving (Read,Show)

instance Message ToggleStruts

-- | SetStruts is a message constructor used to set or unset specific struts,
-- regardless of whether or not the struts were originally set. Here are some
-- example bindings:
--
-- Show all gaps:
--
-- >   ,((modm .|. shiftMask  ,xK_b),sendMessage $ SetStruts [minBound .. maxBound] [])
--
-- Hide all gaps:
--
-- >   ,((modm .|. controlMask,xK_b),sendMessage $ SetStruts [] [minBound .. maxBound])
--
-- Show only upper and left gaps:
--
-- >   ,((modm .|. controlMask .|. shiftMask,xK_b),sendMessage $ SetStruts [U,L] [minBound .. maxBound])
--
-- Hide the bottom keeping whatever the other values were:
--
-- >   ,((modm .|. controlMask .|. shiftMask,xK_g),sendMessage $ SetStruts [] [D])
data SetStruts = SetStruts { addedStruts   :: [Direction2D]
                           , removedStruts :: [Direction2D] -- ^ These are removed from the currently set struts before 'addedStruts' are added.
                           }
  deriving (Read,Show)

instance Message SetStruts

instance LayoutModifier AvoidStruts a where
    modifyLayout (AvoidStruts ss) w r = do
        srect <- fmap ($ r) (calcGap ss)
        -- Ensure _NET_WORKAREA is not set.
        -- See: https://github.com/xmonad/xmonad-contrib/pull/79
        rmWorkarea
        runLayout w srect

    pureMess as@(AvoidStruts ss) m
        | Just ToggleStruts    <- fromMessage m = Just $ AvoidStruts (toggleAll ss)
        | Just (ToggleStrut s) <- fromMessage m = Just $ AvoidStruts (toggleOne s ss)
        | Just (SetStruts n k) <- fromMessage m
        , let newSS = S.fromList n `S.union` (ss S.\\ S.fromList k)
        , newSS /= ss = Just $ AvoidStruts newSS
        | Just UpdateDocks <- fromMessage m = Just as
        | otherwise = Nothing
      where toggleAll x | S.null x = S.fromList [minBound .. maxBound]
                        | otherwise = S.empty
            toggleOne x xs | x `S.member` xs = S.delete x xs
                           | otherwise   = x `S.insert` xs

rmWorkarea :: X ()
rmWorkarea = withDisplay $ \dpy -> do
    a <- getAtom "_NET_WORKAREA"
    r <- asks theRoot
    io (deleteProperty dpy r a)

-- | (Direction, height\/width, initial pixel, final pixel).

type Strut = (Direction2D, CLong, CLong, CLong)

-- | (Initial x pixel, initial y pixel,
--    final x pixel, final y pixel).

newtype RectC = RectC (CLong, CLong, CLong, CLong) deriving (Eq,Show)

-- | Invertible conversion.

r2c :: Rectangle -> RectC
r2c (Rectangle x y w h) = RectC (fi x, fi y, fi x + fi w - 1, fi y + fi h - 1)

-- | Invertible conversion.

c2r :: RectC -> Rectangle
c2r (RectC (x1, y1, x2, y2)) = Rectangle (fi x1) (fi y1) (fi $ x2 - x1 + 1) (fi $ y2 - y1 + 1)


reduce :: RectC -> Strut -> RectC -> RectC
reduce (RectC (sx0, sy0, sx1, sy1)) (s, n, l, h) (RectC (x0, y0, x1, y1)) =
 RectC $ case s of
    L | p (y0, y1) && qh x1     -> (mx x0 sx0, y0       , x1       , y1       )
    R | p (y0, y1) && qv sx1 x0 -> (x0       , y0       , mn x1 sx1, y1       )
    U | p (x0, x1) && qh y1     -> (x0       , mx y0 sy0, x1       , y1       )
    D | p (x0, x1) && qv sy1 y0 -> (x0       , y0       , x1       , mn y1 sy1)
    _                           -> (x0       , y0       , x1       , y1       )
 where
    mx a b = max a (b + n)
    mn a b = min a (b - n)
    p r = r `overlaps` (l, h)
    -- Filter out struts that cover the entire rectangle:
    qh d1 = n <= d1
    qv sd1 d0 = sd1 - n >= d0

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
