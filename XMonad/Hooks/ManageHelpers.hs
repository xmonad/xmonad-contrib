{-# LANGUAGE LambdaCase #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageHelpers
-- Description  : Helper functions to be used in manageHook.
-- Copyright    : (c) Lukas Mai
-- License      : BSD
--
-- Maintainer   : Lukas Mai <l.mai@web.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides helper functions to be used in @manageHook@. Here's
-- how you might use this:
--
-- > import XMonad.Hooks.ManageHelpers
-- > main =
-- >     xmonad def{
-- >         ...
-- >         manageHook = composeOne [
-- >             isKDETrayWindow -?> doIgnore,
-- >             transience,
-- >             isFullscreen -?> doFullFloat,
-- >             resource =? "stalonetray" -?> doIgnore
-- >         ],
-- >         ...
-- >     }

module XMonad.Hooks.ManageHelpers (
    Side(..),
    composeOne,
    (-?>), (/=?), (<==?), (</=?), (-->>), (-?>>),
    currentWs,
    windowTag,
    isInProperty,
    isKDETrayWindow,
    isFullscreen,
    isDialog,
    pid,
    transientTo,
    maybeToDefinite,
    MaybeManageHook,
    transience,
    transience',
    clientLeader,
    sameBy,
    shiftToSame,
    shiftToSame',
    doRectFloat,
    doFullFloat,
    doCenterFloat,
    doSideFloat,
    doFloatAt,
    doFloatDep,
    doHideIgnore,
    doSink,
    doLower,
    doRaise,
    doFocus,
    Match,
) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Util.WindowProperties (getProp32s)

import System.Posix (ProcessID)

-- | Denotes a side of a screen. @S@ stands for South, @NE@ for Northeast
-- etc. @C@ stands for Center.
data Side = SC | NC | CE | CW | SE | SW | NE | NW | C
    deriving (Read, Show, Eq)

-- | A ManageHook that may or may not have been executed; the outcome is embedded in the Maybe
type MaybeManageHook = Query (Maybe (Endo WindowSet))
-- | A grouping type, which can hold the outcome of a predicate Query.
-- This is analogous to group types in regular expressions.
-- TODO: create a better API for aggregating multiple Matches logically
data Match a = Match Bool a

-- | An alternative 'ManageHook' composer. Unlike 'composeAll' it stops as soon as
-- a candidate returns a 'Just' value, effectively running only the first match
-- (whereas 'composeAll' continues and executes all matching rules).
composeOne :: (Monoid a, Monad m) => [m (Maybe a)] -> m a
composeOne = foldr try (return mempty)
    where
    try q z = do
        x <- q
        maybe z return x

infixr 0 -?>, -->>, -?>>

-- | q \/=? x. if the result of q equals x, return False
(/=?) :: (Eq a, Functor m) => m a -> a -> m Bool
q /=? x = fmap (/= x) q

-- | q <==? x. if the result of q equals x, return True grouped with q
(<==?) :: (Eq a, Functor m) => m a -> a -> m (Match a)
q <==? x = fmap (`eq` x) q
    where
    eq q' x' = Match (q' == x') q'

-- | q <\/=? x. if the result of q notequals x, return True grouped with q
(</=?) :: (Eq a, Functor m) => m a -> a -> m (Match a)
q </=? x = fmap (`neq` x) q
    where
    neq q' x' = Match (q' /= x') q'

-- | A helper operator for use in 'composeOne'. It takes a condition and an action;
-- if the condition fails, it returns 'Nothing' from the 'Query' so 'composeOne' will
-- go on and try the next rule.
(-?>) :: (Functor m, Monad m) => m Bool -> m a -> m (Maybe a)
p -?> f = do
    x <- p
    if x then fmap Just f else return Nothing

-- | A helper operator for use in 'composeAll'. It takes a condition and a function taking a grouped datum to action.  If 'p' is true, it executes the resulting action.
(-->>) :: (Monoid b, Monad m) => m (Match a) -> (a -> m b) -> m b
p -->> f = do
    Match b m <- p
    if b then f m else return mempty

-- | A helper operator for use in 'composeOne'.  It takes a condition and a function taking a groupdatum to action.  If 'p' is true, it executes the resulting action.  If it fails, it returns 'Nothing' from the 'Query' so 'composeOne' will go on and try the next rule.
(-?>>) :: (Functor m, Monad m) => m (Match a) -> (a -> m b) -> m (Maybe b)
p -?>> f = do
    Match b m <- p
    if b then fmap  Just (f m) else return Nothing

-- | Return the current workspace
currentWs :: Query WorkspaceId
currentWs = liftX (withWindowSet $ return . W.currentTag)

-- | Return the workspace tag of a window, if already managed
windowTag :: Query (Maybe WorkspaceId)
windowTag = ask >>= \w -> liftX $ withWindowSet $ return . W.findTag w

-- | A predicate to check whether a window is a KDE system tray icon.
isKDETrayWindow :: Query Bool
isKDETrayWindow = ask >>= \w -> liftX $ do
    r <- getProp32s "_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR" w
    return $ case r of
        Just [_] -> True
        _ -> False

-- | Helper to check if a window property contains certain value.
isInProperty :: String -> String -> Query Bool
isInProperty p v = ask >>= \w -> liftX $ do
    va <- getAtom v
    r <- getProp32s p w
    return $ case r of
        Just xs -> fromIntegral va `elem` xs
        _ -> False

-- | A predicate to check whether a window wants to fill the whole screen.
-- See also 'doFullFloat'.
isFullscreen :: Query Bool
isFullscreen = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_FULLSCREEN"

-- | A predicate to check whether a window is a dialog.
isDialog :: Query Bool
isDialog = isInProperty "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"

-- | This function returns 'Just' the @_NET_WM_PID@ property for a
-- particular window if set, 'Nothing' otherwise.
--
-- See <https://specifications.freedesktop.org/wm-spec/wm-spec-1.5.html#idm45623487788432>.
pid :: Query (Maybe ProcessID)
pid = ask >>= \w -> liftX $ getProp32s "_NET_WM_PID" w <&> \case
    Just [x] -> Just (fromIntegral x)
    _        -> Nothing

-- | A predicate to check whether a window is Transient.
-- It holds the result which might be the window it is transient to
-- or it might be 'Nothing'.
transientTo :: Query (Maybe Window)
transientTo = do
    w <- ask
    d <- (liftX . asks) display
    liftIO $ getTransientForHint d w

-- | A convenience 'MaybeManageHook' that will check to see if a window
-- is transient, and then move it to its parent.
transience :: MaybeManageHook
transience = transientTo </=? Nothing -?>> maybe idHook doShiftTo

-- | 'transience' set to a 'ManageHook'
transience' :: ManageHook
transience' = maybeToDefinite transience

-- | This function returns 'Just' the @WM_CLIENT_LEADER@ property for a
-- particular window if set, 'Nothing' otherwise. Note that, generally,
-- the window ID returned from this property (by firefox, for example)
-- corresponds to an unmapped or unmanaged dummy window. For this to be
-- useful in most cases, it should be used together with 'sameBy'.
--
-- See <https://tronche.com/gui/x/icccm/sec-5.html>.
clientLeader :: Query (Maybe Window)
clientLeader = ask >>= \w -> liftX $ getProp32s "WM_CLIENT_LEADER" w <&> \case
    Just [x] -> Just (fromIntegral x)
    _        -> Nothing

-- | For a given window, 'sameBy' returns all windows that have a matching
-- property (e.g. those obtained from Queries of 'clientLeader' and 'pid').
sameBy :: Eq prop => Query (Maybe prop) -> Query [Window]
sameBy prop = prop >>= \case
    Nothing -> pure []
    propVal -> ask >>= \w -> liftX . withWindowSet $ \s ->
        filterM (fmap (propVal ==) . runQuery prop) (W.allWindows s \\ [w])

-- | 'MaybeManageHook' that moves the window to the same workspace as the
-- first other window that has the same value of a given 'Query'. Useful
-- Queries for this include 'clientLeader' and 'pid'.
shiftToSame :: Eq prop => Query (Maybe prop) -> MaybeManageHook
shiftToSame prop = sameBy prop </=? [] -?>> maybe idHook doShiftTo . listToMaybe

-- | 'shiftToSame' set to a 'ManageHook'
shiftToSame' :: Eq prop => Query (Maybe prop) -> ManageHook
shiftToSame' = maybeToDefinite . shiftToSame

-- | converts 'MaybeManageHook's to 'ManageHook's
maybeToDefinite :: (Monoid a, Functor m) => m (Maybe a) -> m a
maybeToDefinite = fmap (fromMaybe mempty)

-- | Move the window to the same workspace as another window.
doShiftTo :: Window -> ManageHook
doShiftTo target = doF . shiftTo =<< ask
  where shiftTo w s = maybe s (\t -> W.shiftWin t w s) (W.findTag target s)

-- | Floats the new window in the given rectangle.
doRectFloat :: W.RationalRect  -- ^ The rectangle to float the window in. 0 to 1; x, y, w, h.
            -> ManageHook
doRectFloat r = ask >>= \w -> doF (W.float w r)

-- | Floats the window and makes it use the whole screen. Equivalent to
-- @'doRectFloat' $ 'W.RationalRect' 0 0 1 1@.
doFullFloat :: ManageHook
doFullFloat = doRectFloat $ W.RationalRect 0 0 1 1

-- | Floats a new window using a rectangle computed as a function of
--   the rectangle that it would have used by default.
doFloatDep :: (W.RationalRect -> W.RationalRect) -> ManageHook
doFloatDep move = ask >>= \w -> doF . W.float w . move . snd =<< liftX (floatLocation w)

-- | Floats a new window with its original size, and its top left
--   corner at a specific point on the screen (both coordinates should
--   be in the range 0 to 1).
doFloatAt :: Rational -> Rational -> ManageHook
doFloatAt x y = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect x y w h

-- | Floats a new window with its original size on the specified side of a
-- screen
doSideFloat :: Side -> ManageHook
doSideFloat side = doFloatDep move
  where
    move (W.RationalRect _ _ w h) = W.RationalRect cx cy w h
      where cx
              | side `elem` [SC,C ,NC] = (1-w)/2
              | side `elem` [SW,CW,NW] = 0
              | otherwise = {- side `elem` [SE,CE,NE] -} 1-w
            cy
              | side `elem` [CE,C ,CW] = (1-h)/2
              | side `elem` [NE,NC,NW] = 0
              | otherwise = {- side `elem` [SE,SC,SW] -} 1-h

-- | Floats a new window with its original size, but centered.
doCenterFloat :: ManageHook
doCenterFloat = doSideFloat C

-- | Hides window and ignores it.
doHideIgnore :: ManageHook
doHideIgnore = ask >>= \w -> liftX (hide w) >> doF (W.delete w)

-- | Sinks a window
doSink :: ManageHook
doSink = doF . W.sink =<< ask

-- | Lower an unmanaged window. Useful together with 'doIgnore' to lower
-- special windows that for some reason don't do it themselves.
doLower :: ManageHook
doLower = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (lowerWindow dpy w) >> mempty

-- | Raise an unmanaged window. Useful together with 'doIgnore' to raise
-- special windows that for some reason don't do it themselves.
doRaise :: ManageHook
doRaise = ask >>= \w -> liftX $ withDisplay $ \dpy -> io (raiseWindow dpy w) >> mempty

-- | Focus a window (useful in 'XMonad.Hooks.EwmhDesktops.setActivateHook').
doFocus :: ManageHook
doFocus = doF . W.focusWindow =<< ask
