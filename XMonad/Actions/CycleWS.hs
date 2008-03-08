-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.CycleWS
-- Copyright   :  (c) Joachim Breitner <mail@joachim-breitner.de>,
--                    Nelson Elhage <nelhage@mit.edu> (`toggleWS' function)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Joachim Breitner <mail@joachim-breitner.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to cycle forward or backward through the list of
-- workspaces, to move windows between workspaces, and to cycle
-- between screens.  More general combinators provide ways to cycle
-- through workspaces in various orders, to only cycle through some
-- subset of workspaces, and to cycle by more than one workspace at a
-- time.
--
-- Note that this module now subsumes the functionality of the former
-- @XMonad.Actions.RotView@.  Former users of @rotView@ can simply replace
-- @rotView True@ with @moveTo Next NonEmptyWS@, and so on.
--
-- If you want to exactly replicate the action of @rotView@ (cycling
-- through workspace in order lexicographically by tag, instead of in
-- the order specified in the config), it can be implemented as:
--
-- > rotView b  = do t <- findWorkspace getSortByTag (bToDir b) NonEmptyWS 1
-- >                 windows . greedyView $ t
-- >   where bToDir True  = Next
-- >         bToDir False = Prev
--
-----------------------------------------------------------------------------

module XMonad.Actions.CycleWS (
                                -- * Usage
                                -- $usage

                                -- * Moving between workspaces
                                -- $moving

                                nextWS
                              , prevWS
                              , shiftToNext
                              , shiftToPrev
                              , toggleWS

                                -- * Moving between screens (xinerama)

                              , nextScreen
                              , prevScreen
                              , shiftNextScreen
                              , shiftPrevScreen
                              , swapNextScreen
                              , swapPrevScreen

                                -- * Moving between workspaces, take two!
                                -- $taketwo

                              , WSDirection(..)
                              , WSType(..)

                              , shiftTo
                              , moveTo

                                -- * The mother-combinator

                              , findWorkspace

                             ) where

import Data.List ( findIndex )
import Data.Maybe ( isNothing, isJust )

import XMonad hiding (workspaces)
import XMonad.StackSet hiding (filter)
import XMonad.Util.WorkspaceCompare

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.CycleWS
-- >
-- > -- a basic CycleWS setup
-- >
-- >   , ((modMask x,               xK_Down),  nextWS)
-- >   , ((modMask x,               xK_Up),    prevWS)
-- >   , ((modMask x .|. shiftMask, xK_Down),  shiftToNext)
-- >   , ((modMask x .|. shiftMask, xK_Up),    shiftToPrev)
-- >   , ((modMask x,               xK_Right), nextScreen)
-- >   , ((modMask x,               xK_Left),  prevScreen)
-- >   , ((modMask x .|. shiftMask, xK_Right), shiftNextScreen)
-- >   , ((modMask x .|. shiftMask, xK_Left),  shiftPrevScreen)
-- >   , ((modMask x,               xK_z),     toggleWS)
--
-- If you want to follow the moved window, you can use both actions:
--
-- >   , ((modMask x .|. shiftMask, xK_Down), shiftToNext >> nextWS)
-- >   , ((modMask x .|. shiftMask, xK_Up),   shiftToPrev >> prevWS)
--
-- You can also get fancier with 'moveTo', 'shiftTo', and 'findWorkspace'.
-- For example:
--
-- >   , ((modMask x     , xK_f), moveTo Next EmptyWS)  -- find a free workspace
-- >   , ((modMask x .|. controlMask, xK_Right),        -- a crazy keybinding!
-- >         do t <- findWorkspace getXineramaWsCompare Next NonEmptyWS 2
-- >            windows . view $ t                                         )
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

{- $moving

The following commands for moving the view and windows between
workspaces are somewhat inflexible, but are very simple and probably
Do The Right Thing for most users.

All of the commands in this section cycle through workspaces in the
order in which they are given in your config.

-}

-- | Switch to the next workspace.
nextWS :: X ()
nextWS = switchWorkspace 1

-- | Switch to the previous workspace.
prevWS :: X ()
prevWS = switchWorkspace (-1)

-- | Move the focused window to the next workspace.
shiftToNext :: X ()
shiftToNext = shiftBy 1

-- | Move the focused window to the previous workspace.
shiftToPrev :: X ()
shiftToPrev = shiftBy (-1)

-- | Toggle to the workspace displayed previously.
toggleWS :: X ()
toggleWS = windows $ view =<< tag . head . hidden

switchWorkspace :: Int -> X ()
switchWorkspace d = wsBy d >>= windows . greedyView

shiftBy :: Int -> X ()
shiftBy d = wsBy d >>= windows . shift

wsBy :: Int -> X (WorkspaceId)
wsBy = findWorkspace getSortByIndex Next AnyWS

{- $taketwo

A few more general commands are also provided, which allow cycling
through subsets of workspaces.

For example,

>   moveTo Next EmptyWS

will move to the first available workspace with no windows, and

>   shiftTo Prev (WSIs $ return (('p' `elem`) . tag))

will move the focused window backwards to the first workspace containing
the letter 'p' in its name. =)

-}

-- | Direction to cycle through the sort order.
data WSDirection = Next | Prev

-- | What type of workspaces should be included in the cycle?
data WSType = EmptyWS     -- ^ cycle through empty workspaces
            | NonEmptyWS  -- ^ cycle through non-empty workspaces
            | HiddenNonEmptyWS  -- ^ cycle through non-empty non-visible workspaces
            | AnyWS       -- ^ cycle through all workspaces
            | WSIs (X (WindowSpace -> Bool))
                          -- ^ cycle through workspaces satisfying
                          --   an arbitrary predicate

-- | Convert a WSType value to a predicate on workspaces.
wsTypeToPred :: WSType -> X (WindowSpace -> Bool)
wsTypeToPred EmptyWS    = return (isNothing . stack)
wsTypeToPred NonEmptyWS = return (isJust . stack)
wsTypeToPred HiddenNonEmptyWS = do hs <- gets (map tag . hidden . windowset)
                                   return (\w -> isJust (stack w) && tag w `elem` hs)
wsTypeToPred AnyWS      = return (const True)
wsTypeToPred (WSIs p)   = p

-- | View the next workspace in the given direction that satisfies
--   the given condition.
moveTo :: WSDirection -> WSType -> X ()
moveTo dir t = findWorkspace getSortByIndex dir t 1 >>= windows . greedyView

-- | Move the currently focused window to the next workspace in the
--   given direction that satisfies the given condition.
shiftTo :: WSDirection -> WSType -> X ()
shiftTo dir t = findWorkspace getSortByIndex dir t 1 >>= windows . shift

-- | Given a function @s@ to sort workspaces, a direction @dir@, a
--   predicate @p@ on workspaces, and an integer @n@, find the tag of
--   the workspace which is @n@ away from the current workspace in
--   direction @dir@ (wrapping around if necessary), among those
--   workspaces, sorted by @s@, which satisfy @p@.
--
--   For some useful workspace sorting functions, see
--   "XMonad.Util.WorkspaceCompare".
--
--   For ideas of what to do with a workspace tag once obtained, note
--   that 'moveTo' and 'shiftTo' are implemented by applying @(>>=
--   (windows . greedyView))@ and @(>>= (windows . shift))@, respectively,
--   to the output of 'findWorkspace'.
findWorkspace :: X WorkspaceSort -> WSDirection -> WSType -> Int -> X WorkspaceId
findWorkspace s dir t n = findWorkspaceGen s (wsTypeToPred t) (maybeNegate dir n)
  where
    maybeNegate Next d = d
    maybeNegate Prev d = (-d)

findWorkspaceGen :: X WorkspaceSort -> X (WindowSpace -> Bool) -> Int -> X WorkspaceId
findWorkspaceGen _ _ 0 = (tag . workspace . current) `fmap` gets windowset
findWorkspaceGen sortX wsPredX d = do
    wsPred <- wsPredX
    sort   <- sortX
    ws     <- gets windowset
    let cur     = workspace (current ws)
        sorted  = sort (workspaces ws)
        pivoted = let (a,b) = span ((/= (tag cur)) . tag) sorted in b ++ a
        ws'     = filter wsPred $ pivoted
        mCurIx  = findWsIndex cur ws'
        d'      = if d > 0 then d - 1 else d
        next    = if null ws'
                      then cur
                      else case mCurIx of
                            Nothing -> ws' !! (d' `mod` length ws')
                            Just ix -> ws' !! ((ix + d) `mod` length ws')
    return $ tag next

findWsIndex :: WindowSpace -> [WindowSpace] -> Maybe Int
findWsIndex ws wss = findIndex ((== tag ws) . tag) wss

-- | View next screen
nextScreen :: X ()
nextScreen = switchScreen 1

-- | View prev screen
prevScreen :: X ()
prevScreen = switchScreen (-1)

switchScreen :: Int -> X ()
switchScreen d = do s <- screenBy d
                    mws <- screenWorkspace s
                    case mws of
                         Nothing -> return ()
                         Just ws -> windows (view ws)

screenBy :: Int -> X (ScreenId)
screenBy d = do ws <- gets windowset
                --let ss = sortBy screen (screens ws)
                let now = screen (current ws)
                return $ (now + fromIntegral d) `mod` fromIntegral (length (screens ws))

-- | Swap current screen with next screen
swapNextScreen :: X ()
swapNextScreen = swapScreen 1

-- | Swap current screen with previous screen
swapPrevScreen :: X ()
swapPrevScreen = swapScreen (-1)

swapScreen :: Int -> X ()
swapScreen d = do s <- screenBy d
                  mws <- screenWorkspace s
                  case mws of
                    Nothing -> return ()
                    Just ws -> windows (greedyView ws)

-- | Move focused window to workspace on next screen
shiftNextScreen :: X ()
shiftNextScreen = shiftScreenBy 1

-- | Move focused window to workspace on prev screen
shiftPrevScreen :: X ()
shiftPrevScreen = shiftScreenBy (-1)

shiftScreenBy :: Int -> X ()
shiftScreenBy d = do s <- screenBy d
                     mws <- screenWorkspace s
                     case mws of
                         Nothing -> return ()
                         Just ws -> windows (shift ws)
