-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.LinkWorkspaces
-- Description : Bindings to add and delete links between workspaces.
-- Copyright   :  (c) Jan-David Quesel <quesel@gmail.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides bindings to add and delete links between workspaces. It is aimed
-- at providing useful links between workspaces in a multihead setup. Linked
-- workspaces are view at the same time.
--
-----------------------------------------------------------------------------

module XMonad.Actions.LinkWorkspaces (
                                         -- * Usage
                                         -- $usage
                                        switchWS,
                                        removeAllMatchings,
                                        unMatch,
                                        toggleLinkWorkspaces,
                                        defaultMessageConf,
                                        MessageConfig(..)
                                       ) where

import XMonad
import XMonad.Prelude (for_)
import qualified XMonad.StackSet as W
import XMonad.Layout.IndependentScreens(countScreens)
import qualified XMonad.Util.ExtensibleState as XS (get, put)
import XMonad.Actions.OnScreen(Focus(FocusCurrent), onScreen')
import qualified Data.Map as M
    ( insert, delete, Map, lookup, empty, filter )

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.LinkWorkspaces
--
-- and add a function to print messages like
--
-- > message_command (S screen) = " dzen2 -p 1 -w 300 -xs " ++ show (screen + 1)
-- > message_color_func c1 c2 msg = dzenColor c1 c2 msg
-- > message screen c1 c2 msg = spawn $ "echo '" ++ (message_color_func c1 c2 msg) ++ "' | " ++ message_command screen
--
-- alternatively you can use the noMessages function as the argument
--
-- Then add keybindings like the following:
--
-- > ,((modm, xK_p), toggleLinkWorkspaces message)
-- > ,((modm .|. shiftMask, xK_p), removeAllMatchings message)
--
-- >   [ ((modm .|. m, k), a i)
-- >       | (a, m) <- [(switchWS (\y -> windows $ view y) message, 0),(switchWS (\x -> windows $ shift x . view x) message, shiftMask)]
-- >       , (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data MessageConfig = MessageConfig {  messageFunction :: ScreenId -> [Char] -> [Char] -> [Char] -> X()
                    , foreground :: [Char]
                    , alertedForeground :: [Char]
                    , background :: [Char]
                   }

defaultMessageConf :: MessageConfig
defaultMessageConf = MessageConfig { messageFunction = noMessageFn
                     , background = "#000000"
                     , alertedForeground = "#ff7701"
                     , foreground = "#00ff00" }

noMessageFn :: ScreenId -> [Char] -> [Char] -> [Char] -> X()
noMessageFn _ _ _ _ = return () :: X ()

-- | Stuff for linking workspaces
newtype WorkspaceMap = WorkspaceMap (M.Map WorkspaceId WorkspaceId) deriving (Read, Show)
instance ExtensionClass WorkspaceMap
    where initialValue = WorkspaceMap M.empty
          extensionType = PersistentExtension

switchWS :: (WorkspaceId -> X ()) -> MessageConfig -> WorkspaceId -> X ()
switchWS f m ws = switchWS' f m ws Nothing

-- | Switch to the given workspace in a non greedy way, stop if we reached the first screen
-- | we already did switching on
switchWS' :: (WorkspaceId -> X ()) -> MessageConfig  -> WorkspaceId -> Maybe ScreenId -> X ()
switchWS' switchFn message workspace stopAtScreen = do
  ws <- gets windowset
  nScreens <- countScreens
  let now = W.screen (W.current ws)
  let next = (now + 1) `mod` nScreens
  switchFn workspace
  case stopAtScreen of
    Nothing -> sTM now next (Just now)
    Just sId -> if sId == next then return () else sTM now next (Just sId)
  where sTM = switchToMatching (switchWS' switchFn message) message workspace

-- | Switch to the workspace that matches the current one, executing switches for that workspace as well.
-- | The function switchWorkspaceNonGreedy' will take of stopping if we reached the first workspace again.
switchToMatching :: (WorkspaceId -> Maybe ScreenId -> X ()) -> MessageConfig -> WorkspaceId -> ScreenId
    -> ScreenId -> Maybe ScreenId -> X ()
switchToMatching f message t now next stopAtScreen = do
    WorkspaceMap matchings <- XS.get :: X WorkspaceMap
    case M.lookup t matchings of
        Nothing -> return () :: X()
        Just newWorkspace -> do
            onScreen' (f newWorkspace stopAtScreen) FocusCurrent next
            messageFunction message now (foreground message) (background message) ("Switching to: " ++ (t ++ " and " ++ newWorkspace))

-- | Insert a mapping between t1 and t2 or remove it was already present
toggleMatching :: MessageConfig -> WorkspaceId -> WorkspaceId -> X ()
toggleMatching message t1 t2 = do
    WorkspaceMap matchings <- XS.get :: X WorkspaceMap
    case M.lookup t1 matchings of
        Nothing -> setMatching message t1 t2 matchings
        Just t -> if t == t2 then removeMatching' message t1 t2 matchings else setMatching message t1 t2 matchings
    return ()

-- | Insert a mapping between t1 and t2 and display a message
setMatching :: MessageConfig -> WorkspaceId -> WorkspaceId -> M.Map WorkspaceId WorkspaceId -> X ()
setMatching message t1 t2 matchings = do
   ws <- gets windowset
   let now = W.screen (W.current ws)
   XS.put $ WorkspaceMap $ M.insert t1 t2 matchings
   messageFunction message now (foreground message) (background message) ("Linked: " ++ (t1 ++ " " ++ t2))

-- currently this function is called manually this means that if workspaces
-- were deleted, some links stay in the RAM even though they are not used
-- anymore... because of the small amount of memory used for those there is no
-- special cleanup so far
removeMatching' :: MessageConfig -> WorkspaceId -> WorkspaceId -> M.Map WorkspaceId WorkspaceId -> X ()
removeMatching' message t1 t2 matchings = do
   ws <- gets windowset
   let now = W.screen (W.current ws)
   XS.put $ WorkspaceMap $ M.delete t1 matchings
   messageFunction message now (alertedForeground message) (background message) ("Unlinked: " ++ t1 ++ " " ++ t2)

-- | Remove all maps between workspaces
removeAllMatchings :: MessageConfig -> X ()
removeAllMatchings message = do
   ws <- gets windowset
   let now = W.screen (W.current ws)
   XS.put $ WorkspaceMap M.empty
   messageFunction message now (alertedForeground message) (background message) "All links removed!"

-- | remove all matching regarding a given workspace
unMatch :: WorkspaceId -> X ()
unMatch workspace = do
    WorkspaceMap matchings <- XS.get :: X WorkspaceMap
    XS.put $ WorkspaceMap $ M.delete workspace (M.filter (/= workspace) matchings)

-- | Toggle the currently displayed workspaces as matching. Starting from the one with focus
-- | a linked list of workspaces is created that will later be iterated by switchToMatching.
toggleLinkWorkspaces :: MessageConfig -> X ()
toggleLinkWorkspaces message = withWindowSet $ \ws -> toggleLinkWorkspaces' (W.screen (W.current ws)) message

toggleLinkWorkspaces' :: ScreenId -> MessageConfig -> X ()
toggleLinkWorkspaces' first message = do
    ws <- gets windowset
    nScreens <- countScreens
    let now = W.screen (W.current ws)
    let next = (now + 1) `mod` nScreens
    if next == first then return () else do -- this is also the case if there is only one screen
        for_ (W.lookupWorkspace next ws)
             (toggleMatching message (W.currentTag ws))
        onScreen' (toggleLinkWorkspaces' first message) FocusCurrent next
