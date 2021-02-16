{-# LANGUAGE RankNTypes, TemplateHaskell, DeriveDataTypeable, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.WorkspaceSet
-- Copyright   :  (c) 2021 Will Pierlot <willp@outlook.com.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Will Pierlot <willp@outlook.com.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Workspace Sets for multiple sets of workspaces
-----------------------------------------------------------------------------

module WorkspaceSet (
        -- * Usage
        -- $usage

        -- * Output
        workspaceSetHook, cleanWS', filterOutInvalidWSet,

        -- * Keybinds
        createDefaultWorkspaceKeybinds,
        createKeybinds',
        createWsKeybind,
        changeWorkspaces,
        runOnWorkspace,

        -- * Navigation
        switchToWsSet, moveToWsSet,moveToNextWsSet,moveToPrevWsSet,nextWSSet, prevWSSet,

        -- * State Manipulation
        fixTag, workspaceState,
        WorkspaceState(..),WorkspaceSet(..),
        WorkspaceSetId,
        ) where
import           XMonad
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W
import           XMonad.Hooks.DynamicLog (PP(..))
import           Lens.Micro
import           Lens.Micro.Extras
import           Lens.Micro.TH
import           Data.List (nub,nubBy,find)
import           Data.Bifunctor (first)
import           Data.Maybe (fromMaybe)
import           Control.Monad (when,join)
import           Control.Applicative (liftA2)
import           Data.Monoid (Endo(..))
import qualified XMonad.Core as Core

-- $usage
-- Allows for multiple and dynamic sets of workspaces
--
-- NOTE: The default workspace is known internally as "default"
--
-- Depending on which 'WorkspaceSet', there can be dynamic keybinds
-- For example,
--
-- > workspaceSets :: [(WorkspaceSetId,[WorkspaceId])]
-- > workspaceSets =
-- >    [ ("work",map show [1..5])
-- >    , ("school",map show [1..4])
-- >    ]
--
-- and then add the ManageHook
--
-- > manageHook = workspaceSetHook workspaceSets
--
-- To enable workspace navigation, workspace keybinds need to be dynamic
--
-- > main = xmonad $ def
-- >    {
-- >    } `additionalKeys` (createDefaultWorkspaceKeybinds myConfig workspaceSets)
--
-- Other keybinds can also be added
--
-- >  main = xmonad $ def
-- >    {
-- >    } `additionalKeysP` 
-- >        (createKeybinds "M-S" 
-- >            [ ("work", spawn "slack")
-- >            , ("school", spawn "spotify")
-- >            ])
--

-- | Type for 'WorkspaceSet' names
type WorkspaceSetId = String
-- | A set of workspaces
data WorkspaceSet  =
    WorkspaceSet { _workspaceSetName :: WorkspaceSetId
                 , _workspaceNames :: [WorkspaceId]
                 , _currentWorkspaceTag :: WorkspaceId
                 } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceSet)

-- | Internal state for 'WorkspaceSet's
data WorkspaceState =
    WorkspaceState { _currentWorkspaceSet :: WorkspaceSet
                   , _workspaceSetsUp :: [WorkspaceSet]
                   , _workspaceSetsDown :: [WorkspaceSet]
                   , _initialised :: Bool
                   } deriving (Typeable,Read,Show)

$(makeLenses ''WorkspaceState)



--Discriminator
discrim = "-|-"

instance  ExtensionClass WorkspaceState where
    initialValue = WorkspaceState (WorkspaceSet "default" [] mempty) mempty  mempty False
    extensionType = PersistentExtension


--- | Only runs when the 'WorkspaceSet' is a specific 'WorkspaceSetId'
runOnWorkspace :: WorkspaceSetId -> X () -> X ()
runOnWorkspace wsSet act = do
    currentWorkspaceSet <- XS.gets (^. currentWorkspaceSet)
    when (_workspaceSetName currentWorkspaceSet == wsSet) act

newWorkspaceState :: [WorkspaceId] -> [WorkspaceSet] -> WorkspaceState
newWorkspaceState orig wsSets =WorkspaceState (createWorkspaceSet "default" orig ) mempty wsSets False


getWorkspaceSets :: WorkspaceState -> [WorkspaceSet]
getWorkspaceSets = (<>) <$> (^. workspaceSetsUp) <*> (^. workspaceSetsDown)

getWorkspacesWithCurrent :: WorkspaceState -> [WorkspaceSet]
getWorkspacesWithCurrent = (<>)
    <$> (reverse . view workspaceSetsUp)
    <*> ((:) <$> view currentWorkspaceSet <*> view workspaceSetsDown)

createWorkspaceSet :: WorkspaceSetId -> [WorkspaceId] -> WorkspaceSet
createWorkspaceSet wsId (x:xs) = WorkspaceSet wsId (x:xs) x


-- | 'MangeHook' for showing and managing 'WorkspaceSet's
workspaceSetHook :: [(WorkspaceSetId,[WorkspaceId])] -> ManageHook
workspaceSetHook wssets = liftX $ do
    l <-  asks (layoutHook . config)
    initialised' <-  XS.gets _initialised
    if not initialised'
    then do
        let wssets' = map (uncurry createWorkspaceSet) wssets
        ws <-  asks (workspaces . config)
        XS.put (newWorkspaceState ws wssets')
        XS.modify (set initialised True)
        modifiedTags <- XS.gets (concatMap modifyTags  . getWorkspaceSets)
        pure (Endo $ f modifiedTags l)
    else do
        tag' <- gets (W.currentTag . windowset)
        XS.modify (
            set (currentWorkspaceSet . currentWorkspaceTag) .  flip cleanWS tag' . modifyTags <$> view currentWorkspaceSet  <*> id)
        mempty
   where
    f modifiedTags layout winset =
        let tags = map W.tag $ W.workspaces winset
            createWorkspace tag = W.Workspace tag layout Nothing
        in
            foldr (\x acc -> if x`elem` tags then acc else over W.hiddenL (createWorkspace x:) acc)
                winset modifiedTags

-- | Given a 'WorkspaceSetId' and a 'WorkspaceId' within that 'WorkspaceSet', show the true 'WorkspaceId'
fixTag :: WorkspaceSetId  -- ^ 'WorkspaceSet' that it is part of
       -> WorkspaceId  -- ^ 'WorkspaceId' to fix
       -> WorkspaceId -- ^ Fixed 'WorkspaceId'
fixTag wsSet
    | wsSet /= "default" = ((wsSet++discrim)++)
    | otherwise = id

modifyTags :: WorkspaceSet -> [WorkspaceId]
modifyTags = liftA2 (map . fixTag ) (view workspaceSetName) (view workspaceNames)

-- | Given a 'WorkspaceSet' , switch to it
moveToWsSet :: WorkspaceSetId -> X ()
moveToWsSet wsSetId = do
    workspaceSets <- XS.gets getWorkspacesWithCurrent
    case break ((==wsSetId) . view workspaceSetName) workspaceSets of
        (_,ws:_) -> windows $ W.shift (fixTag (_workspaceSetName ws) (_currentWorkspaceTag ws))
        _ -> pure ()

-- | Switch to the next WorkspaceSet
moveToNextWsSet :: Bool  -- ^ Enable cycling
                -> X ()
moveToNextWsSet cycle = do
    a <- XS.gets ( view workspaceSetsDown)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsUp)
           case a of
                (x:xs) -> moveToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> moveToWsSet (_workspaceSetName x)

-- | Switch to previous WorkspaceSet
moveToPrevWsSet :: Bool -- ^ Enable Cycling
                -> X ()
moveToPrevWsSet cycle = do
    a <- XS.gets (  view workspaceSetsUp)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsDown)
           case a of
                (x:xs) -> moveToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> moveToWsSet (_workspaceSetName x)

-- | Change 'WorkspaceState' given a function
workspaceState :: (WorkspaceState -> WorkspaceState) -> X ()
workspaceState = XS.modify


switchToWsSet :: WorkspaceSetId -> X ()
switchToWsSet wsSetId = do
    runQuery (workspaceSetHook mempty) 0
    workspaceSets <- XS.gets getWorkspacesWithCurrent
    case break ((==wsSetId) . view workspaceSetName) workspaceSets of
        (xs,ws:ss) -> do
            workspaceState $ set currentWorkspaceSet ws
            windows $ W.greedyView (fixTag (_workspaceSetName ws) (_currentWorkspaceTag ws))
            workspaceState $  set workspaceSetsDown ss
            workspaceState $  set workspaceSetsUp (reverse xs)
            join (asks (logHook . config))
        _ ->  pure ()


-- | Given a 'PP' , filter out not visible 'WorkspaceSet's
filterOutInvalidWSet :: PP -> X PP
filterOutInvalidWSet pp = do
    invalid <- ignoreWsSet
    right <- XS.gets (modifyTags . _currentWorkspaceSet)
    name <- XS.gets (view (currentWorkspaceSet . workspaceSetName))
    pure $
        pp { ppSort = fmap (. {-over (traverse . SL.tagLens) (cleanWS right)  .-}   f invalid) (ppSort pp)
           , ppCurrent = ppSection ppCurrent right
           , ppVisible = ppSection ppVisible right
           , ppHidden = ppSection ppHidden right
           , ppHiddenNoWindows = ppSection ppHiddenNoWindows  right
           , ppOrder = \(x:xs) -> if name == "default" then x:xs else x:name:xs
           }
  where
    f invalid = filter (not . (`elem` invalid) . W.tag)
    ppSection f at =  f pp . cleanWS at

-- | Given a key and a series of actions for that key, create a keybind
createWsKeybind :: Eq a
                => a -- ^ The key to use (Can be ('KeyMask','KeySym') or a 'String) depending on what keys are in use
                -> [(WorkspaceSetId, X ())] -- ^ The 'WorkspaceSetId' and accompanying X actions
                -> (a,X ()) -- ^ The outputted Keybind
createWsKeybind key binds = (key,f)
  where
    cleanWS = nubBy (\x y -> fst x == fst y) binds
    f = do
        origWS <- asks (workspaces . config)
        XS.gets (view (currentWorkspaceSet . workspaceSetName)) >>= \ws -> maybe (return ()) snd (find ((ws ==) . fst) cleanWS)

-- | Function to be used in filters
cleanWS' :: X ([WindowSpace]  -> [WindowSpace])
cleanWS' = do
    invalid <- ignoreWsSet
    right <- XS.gets _currentWorkspaceSet
    let b = map (fixTag (_workspaceSetName right)) (_workspaceNames right)
    pure $ map (over W.tagL  (cleanWS b) ) .  f invalid
    where f invalid = filter (not . (`elem` invalid) . W.tag)

changeWorkspaces :: WorkspaceSetId -> [WorkspaceId] -> (WorkspaceSetId,[((KeyMask,KeySym ),X ())])
changeWorkspaces wsSet ws = (wsSet,[((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip (map (fixTag wsSet ) ws) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])

-- | Create sensible defaults based on XMonad defaults.
createDefaultWorkspaceKeybinds :: XConfig l -> [(WorkspaceSetId,[WorkspaceId])] -> [((KeyMask,KeySym),X ())]
createDefaultWorkspaceKeybinds xconf = createKeybinds' . map (uncurry changeWorkspaces) . (("default",workspaces xconf):)

-- | From a set of a 'WorkspaceSetId' and keybinds, reify them into a list of keybinds
createKeybinds' :: Eq a => [(WorkspaceSetId,[(a,X ())])] -> [(a,X ())]
createKeybinds' keys = map f keySets
    where keySets = nub $ concatMap (map fst . snd) keys
          f key = createWsKeybind key $ [(wsId, action) | (wsId,actions) <- keys,(key',action) <- actions,key' == key]

nextWSSet :: Bool  -- ^ Cycle workspacsets
             -> X ()
nextWSSet cycle = do
    a <- XS.gets ( view workspaceSetsDown)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsUp)
           case a of
                (x:xs) -> switchToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> switchToWsSet (_workspaceSetName x)

prevWSSet :: Bool
             -> X ()
prevWSSet cycle = do
    a <- XS.gets (  view workspaceSetsUp)
    case a of
        [] -> when cycle $ do
           a <- XS.gets (reverse . view workspaceSetsDown)
           case a of
                (x:xs) -> switchToWsSet (_workspaceSetName x)
                _ -> pure ()
        (x:_) -> switchToWsSet (_workspaceSetName x)

-- | Filter out invalid 'WorkspaceSet'
cleanWS :: [WorkspaceId] -> WorkspaceId -> WorkspaceId
cleanWS at wsId =
    if wsId `elem` at then
        fromMaybe wsId (checkFunc wsId [])
    else wsId
  where
    checkFunc :: String -> String -> Maybe String
    checkFunc (x:xs) acc
        | take (length discrim) acc  == discrim =  Just (x:xs)
        | otherwise = checkFunc xs (x:acc)
    checkFunc [] _ = Nothing

ignoreWsSet :: X [WorkspaceId]
ignoreWsSet = XS.gets allTags

allTags :: WorkspaceState -> [WorkspaceId]
allTags = concatMap modifyTags . liftA2 (<>) (view workspaceSetsUp) (view workspaceSetsDown)
