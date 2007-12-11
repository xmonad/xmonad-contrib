-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.ManageHelpers
-- Copyright    : (c) Lukas Mai
-- License      : BSD
--
-- Maintainer   : Lukas Mai <l.mai@web.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- This module provides helper functions to be used in @manageHook@. Here's how you
-- might use this:
--
-- > import XMonad.Hooks.ManageHelpers
-- > main =
-- >     xmonad defaultConfig{
-- >         ...
-- >         manageHook = composeOne [
-- >             isKDETrayWindow -?> doIgnore,
-- >             transience,
-- >             resource =? "stalonetray" -?> doIgnore
-- >         ],
-- >         ...
-- >     }

module XMonad.Hooks.ManageHelpers (
    composeOne,
    (-?>),
    isKDETrayWindow,
    transience,
    transience'
) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe
import Data.Monoid

-- | An alternative 'ManageHook' composer. Unlike 'composeAll' it stops as soon as
-- a candidate returns a 'Just' value, effectively running only the first match
-- (whereas 'composeAll' continues and executes all matching rules).
composeOne :: [Query (Maybe (Endo WindowSet))] -> ManageHook
composeOne = foldr try idHook
    where
    try q z = do
        x <- q
        case x of
            Just h -> return h
            Nothing -> z

infixr 0 -?>
-- | A helper operator for use in 'composeOne'. It takes a condition and an action;
-- if the condition fails, it returns 'Nothing' from the 'Query' so 'composeOne' will
-- go on and try the next rule.
(-?>) :: Query Bool -> Query (Endo WindowSet) -> Query (Maybe (Endo WindowSet))
p -?> f = do
    x <- p
    if x then fmap Just f else return Nothing

-- | A predicate to check whether a window is a KDE system tray icon.
isKDETrayWindow :: Query Bool
isKDETrayWindow = ask >>= \w -> liftX $ do
    dpy <- asks display
    kde_tray <- getAtom "_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR"
    r <- io $ getWindowProperty32 dpy kde_tray w
    return $ case r of
        Just [_] -> True
        _ -> False

-- | A special rule that moves transient windows to the workspace of their
-- associated primary windows.
transience :: Query (Maybe (Endo WindowSet))
transience = do
    w <- ask
    d <- (liftX . asks) display
    x <- liftIO $ getTransientForHint d w
    case x of
        Nothing -> return Nothing
        Just w' -> do
            return . Just . Endo $ \s ->
                maybe s (`W.shift` s) (W.findTag w' s)

-- | Like 'transience' but with a type that can be used in 'composeAll'.
transience' :: ManageHook
transience' = fmap (fromMaybe mempty) transience
