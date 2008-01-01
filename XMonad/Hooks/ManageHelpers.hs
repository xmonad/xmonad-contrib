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
    (-?>), (/=?), (<==?), (</=?), (-->>), (-?>>),
    isKDETrayWindow,
    transientTo,
    maybeToDefinite,
    MaybeManageHook,
    transience,
    transience'
) where

import XMonad
import qualified XMonad.StackSet as W

import Data.Maybe
import Data.Monoid

-- | A ManageHook that may or may not have been executed; the outcome is embedded in the Maybe
type MaybeManageHook = Query (Maybe (Endo WindowSet))
-- | A grouping type, which can hold the outcome of a predicate Query
-- This is analogous to group types in regular expressions
-- TODO create a better API for aggregating multiple Matches logically
data Match a = Match Bool a

-- | An alternative 'ManageHook' composer. Unlike 'composeAll' it stops as soon as
-- a candidate returns a 'Just' value, effectively running only the first match
-- (whereas 'composeAll' continues and executes all matching rules).
composeOne :: [MaybeManageHook] -> ManageHook
composeOne = foldr try idHook
    where
    try q z = do
        x <- q
        case x of
            Just h -> return h
            Nothing -> z

infixr 0 -?>, -->>, -?>>

-- | q \/=? x. if the result of q equals x, return False
(/=?) :: Eq a => Query a -> a -> Query Bool
q /=? x = fmap (/= x) q

-- | q <==? x. if the result of q equals x, return True grouped with q
(<==?) :: Eq a => Query a -> a -> Query (Match a)
q <==? x = fmap (`eq` x) q
           where eq q' x' = Match (q' == x') q'

-- | q <\/=? x. if the result of q notequals x, return True grouped with q
(</=?) :: Eq a => Query a -> a -> Query (Match a)
q </=? x = fmap (`neq` x) q
           where neq q' x' = Match (q' /= x') q'

-- | A helper operator for use in 'composeOne'. It takes a condition and an action;
-- if the condition fails, it returns 'Nothing' from the 'Query' so 'composeOne' will
-- go on and try the next rule.
(-?>) :: Query Bool -> ManageHook -> MaybeManageHook
p -?> f = do
    x <- p
    if x then fmap Just f else return Nothing

-- | A helper operator for use in 'composeAll'. It takes a condition and a function taking a grouped datum to action.  If 'p' is true, it executes the resulting action.
(-->>) :: Query (Match a) -> (a -> ManageHook) -> ManageHook
p -->> f = do Match b m <- p
              if b then (f m) else mempty

-- | A helper operator for use in 'composeOne'.  It takes a condition and a function taking a groupdatum to action.  If 'p' is true, it executes the resulting action.  If it fails, it returns 'Nothing' from the 'Query' so 'composeOne' will go on and try the next rule.
(-?>>) :: Query (Match a) -> (a -> ManageHook) -> MaybeManageHook
p -?>> f = do Match b m <- p
              if b then fmap  Just (f m) else return Nothing

-- | A predicate to check whether a window is a KDE system tray icon.
isKDETrayWindow :: Query Bool
isKDETrayWindow = ask >>= \w -> liftX $ do
    dpy <- asks display
    kde_tray <- getAtom "_KDE_NET_WM_SYSTEM_TRAY_WINDOW_FOR"
    r <- io $ getWindowProperty32 dpy kde_tray w
    return $ case r of
        Just [_] -> True
        _ -> False

-- | A predicate to check whether a window is Transient.
-- It holds the result which might be the window it is transient to
-- or it might be 'Nothing'.
transientTo :: Query (Maybe Window)
transientTo = do w <- ask
                 d <- (liftX . asks) display
                 liftIO $ getTransientForHint d w

-- | A convenience 'MaybeManageHook' that will check to see if a window
-- is transient, and then move it to it's parent.
transience :: MaybeManageHook
transience = transientTo </=? Nothing 
             -?>> move
                 where move :: Maybe Window -> ManageHook
                       move mw = do 
                         case mw of
                           Just w -> do return . Endo $ \s ->
                                            maybe s (`W.shift` s) (W.findTag w s)
                           Nothing -> do return . Endo $ \s -> s

-- | 'transience' set to a 'ManageHook'
transience' :: ManageHook
transience' = maybeToDefinite transience

-- | converts 'MaybeManageHook's to 'ManageHook's
maybeToDefinite :: MaybeManageHook -> ManageHook
maybeToDefinite = fmap (fromMaybe mempty)
