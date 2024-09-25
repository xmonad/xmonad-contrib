{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DerivingVia   #-}


--------------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Profiles
-- Description :  Group your workspaces by similarity.
-- Copyright   :  (c) Mislav Zanic
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Mislav Zanic <mislavzanic3@gmail.com>
-- Stability   :  experimental
-- Portability :  unportable
--
--------------------------------------------------------------------------------

module XMonad.Actions.Profiles
  ( -- * Overview
    -- $overview

    -- * Usage
    -- $usage

    -- * Types
    ProfileId
  , Profile(..)
  , ProfileConfig(..)

  -- * Hooks
  , addProfiles
  , addProfilesWithHistory

  -- * Switching profiles
  , switchToProfile

  -- * Workspace navigation and keybindings
  , wsFilter
  , bindOn

  -- * Loggers and pretty printers
  , excludeWSPP
  , profileLogger

  -- * Prompts
  , switchProfilePrompt
  , addWSToProfilePrompt
  , removeWSFromProfilePrompt
  , switchProfileWSPrompt
  , shiftProfileWSPrompt

  -- * Utilities
  , currentProfile
  , profileIds
  , previousProfile
  , profileHistory
  , allProfileWindows
  , profileWorkspaces
  )where

--------------------------------------------------------------------------------
import Data.Map.Strict (Map)
import Data.List
import qualified Data.Map.Strict as Map

import Control.DeepSeq

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS

import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Loggers (Logger)
import XMonad.Prompt.Window (XWindowMap)
import XMonad.Actions.WindowBringer (WindowBringerConfig(..))
import XMonad.Actions.OnScreen (greedyViewOnScreen)
import XMonad.Hooks.Rescreen (addAfterRescreenHook)
import XMonad.Hooks.DynamicLog (PP(ppRename))
import XMonad.Prompt 

--------------------------------------------------------------------------------
-- $overview
-- This module allows you to group your workspaces into 'Profile's based on certain similarities.
-- The idea is to expand upon the philosophy set by "XMonad.Actions.TopicSpace"
-- which states that you can look at a topic/workspace as a
-- single unit of work instead of multiple related units of work.
-- This comes in handy if you have lots of workspaces with windows open and need only to
-- work with a few of them at a time. With 'Profile's, you can focus on those few workspaces that
-- require your attention by not displaying, or allowing you to switch to the rest of the workspaces.
-- The best example is having a profile for development and a profile for leisure activities.

--------------------------------------------------------------------------------
-- $usage
-- To use @Profiles@ you need to add it to your XMonad configuration
-- and configure your profiles.
--  
-- First you'll need to handle the imports.
--  
-- > import XMonad.Actions.Profiles 
-- > import XMonad.Util.EZConfig -- for keybindings
-- > import qualified XMonad.StackSet as W
-- > import qualified XMonad.Actions.DynamicWorkspaceOrder as DO -- for workspace navigation
--
-- Next you'll need to define your profiles.
--
-- > myStartingProfile :: ProfileId
-- > myStartingProfile = "Work"
-- >
-- > myProfiles :: [Profile]
-- > myProfiles =
-- >  [ Profile { profileId = "Home"
-- >            , profileWS = [ "www"
-- >                          , "rss"
-- >                          , "vid"
-- >                          , "vms"
-- >                          , "writing"
-- >                          , "notes"
-- >                          ]
-- >            }
-- >  , Profile { profileId = "Work"
-- >            , profileWS = [ "www"
-- >                          , "slack"
-- >                          , "dev"
-- >                          , "k8s"
-- >                          , "notes"
-- >                          ]
-- >            }
-- >  ]
-- 
-- So, while using @Home@ 'Profile', you'll only be able to see, navigate to and 
-- do actions with @["www", "rss", "vid", "vms", "writing", "notes"]@ workspaces.
--
-- You may also need to define some keybindings. Since @M-1@ .. @M-9@ are
-- sensible keybindings for switching workspaces, you'll need to use
-- 'bindOn' to have different keybindings per profile.
-- Here, we'll use "XMonad.Util.EZConfig" syntax:
-- 
-- > myKeys :: [(String, X())]
-- > myKeys = 
-- >   [ ("M-p",  switchProfilePrompt   xpConfig)
-- >   , ("M-g",  switchProfileWSPrompt xpConfig)
-- >   , ("M1-j", DO.moveTo Next wsFilter)
-- >   , ("M1-k", DO.moveTo Prev wsFilter)
-- >   ]
-- >   <>
-- >   [ ("M-" ++ m ++ k, bindOn $ map (\x -> (fst x, f $ snd x)) i)
-- >   | (i, k) <- map (\(x:xs) -> (map fst (x:xs), snd x)) $ sortGroupBy snd tupleList
-- >   , (f, m) <- [(mby $ windows . W.greedyView, ""), (mby $ windows . W.shift, "S-")]
-- >   ]
-- >   where
-- >     mby f wid = if wid == "" then return () else f wid
-- >     sortGroupBy f = groupBy (\ x y -> f x == f y) . sortBy (\x y -> compare (f x) (f y))
-- >     tupleList = concatMap (\p -> zip (map (\wid -> (profileId p, wid)) (profileWS p <> repeat "")) (map show [1..9 :: Int])) myProfiles
-- 
-- After that, you'll need to hook @Profiles@ into your XMonad config:
-- 
-- > main = xmonad $ addProfiles def { profiles        = myProfiles
-- >                                 , startingProfile = myStartingProfile
-- >                                 }
-- >               $ def `additionalKeysP` myKeys
-- 

--------------------------------------------------------------------------------
type ProfileId  = String
type ProfileMap = Map ProfileId Profile

--------------------------------------------------------------------------------
-- | Profile representation.
data Profile = Profile
  { profileId :: !ProfileId     -- ^ Profile name.
  , profileWS :: ![WorkspaceId] -- ^ A list of workspaces contained within a profile.
  }

--------------------------------------------------------------------------------
-- | Internal profile state.
data ProfileState = ProfileState
  { profilesMap :: !ProfileMap
  , current     :: !(Maybe Profile)
  , previous    :: !(Maybe ProfileId)
  }

--------------------------------------------------------------------------------
-- | User config for profiles.
data ProfileConfig = ProfileConfig
  { workspaceExcludes :: ![WorkspaceId] -- ^ A list of workspaces to exclude from the @profileHistoryHook@.
  , profiles          :: ![Profile]     -- ^ A list of user-defined profiles.
  , startingProfile   :: !ProfileId     -- ^ Profile shown on startup.
  }

--------------------------------------------------------------------------------
instance Default ProfileConfig where
  def            = ProfileConfig { workspaceExcludes = []
                                 , profiles          = []
                                 , startingProfile   = ""
                                 }

--------------------------------------------------------------------------------
instance ExtensionClass ProfileState where
  initialValue = ProfileState Map.empty Nothing Nothing

--------------------------------------------------------------------------------
-- Internal type for history tracking.
-- Main problem with @XMonad.Hooks.HistoryHook@ is that it isn't profile aware.
-- Because of that, when switching to a previous workspace, you might switch to
-- a workspace
newtype ProfileHistory = ProfileHistory
  { history :: Map ProfileId [(ScreenId, WorkspaceId)]
  }
  deriving (Read, Show)
  deriving NFData via Map ProfileId [(Int, WorkspaceId)]

--------------------------------------------------------------------------------
instance ExtensionClass ProfileHistory where
  extensionType = PersistentExtension
  initialValue = ProfileHistory Map.empty

--------------------------------------------------------------------------------
newtype ProfilePrompt = ProfilePrompt String

--------------------------------------------------------------------------------
instance XPrompt ProfilePrompt where
  showXPrompt (ProfilePrompt x) = x

--------------------------------------------------------------------------------
defaultProfile :: Profile
defaultProfile = defaultProfile

--------------------------------------------------------------------------------
-- | Returns current profile.
currentProfile :: X ProfileId
currentProfile = profileId . fromMaybe defaultProfile . current <$> XS.get

--------------------------------------------------------------------------------
-- | Returns previous profile.
previousProfile :: X (Maybe ProfileId)
previousProfile = XS.gets previous

--------------------------------------------------------------------------------
-- | Returns the history of viewed workspaces per profile.
profileHistory :: X (Map ProfileId [(ScreenId, WorkspaceId)])
profileHistory = XS.gets history

--------------------------------------------------------------------------------
profileMap :: X ProfileMap
profileMap = XS.gets profilesMap

--------------------------------------------------------------------------------
-- | Returns ids of all profiles.
profileIds :: X [ProfileId]
profileIds = Map.keys <$> XS.gets profilesMap

--------------------------------------------------------------------------------
currentProfileWorkspaces :: X [WorkspaceId]
currentProfileWorkspaces = XS.gets current <&> profileWS . fromMaybe defaultProfile

--------------------------------------------------------------------------------
-- | Hook profiles into XMonad. This function adds a startup hook that
-- sets up ProfileState. Also adds an afterRescreenHook for viewing correct
-- workspaces when adding new screens.
addProfiles :: ProfileConfig -> XConfig a -> XConfig a
addProfiles profConf conf = addAfterRescreenHook hook $ conf
  { startupHook = profileStartupHook' <> startupHook conf
  }
 where
   profileStartupHook' :: X()
   profileStartupHook' = profilesStartupHook (profiles profConf) (startingProfile profConf)
   hook = currentProfile >>= switchWSOnScreens

--------------------------------------------------------------------------------
-- | Hooks profiles into XMonad and enables Profile history logging.
addProfilesWithHistory :: ProfileConfig -> XConfig a -> XConfig a
addProfilesWithHistory profConf conf = conf'
  { logHook = profileHistoryHookExclude (workspaceExcludes profConf) <> logHook conf
  }
  where
   conf' = addProfiles profConf conf

--------------------------------------------------------------------------------
profileHistoryHookExclude :: [WorkspaceId] -> X()
profileHistoryHookExclude ews = do
  cur <- gets $ W.current . windowset
  vis <- gets $ W.visible . windowset
  pws <- currentProfileWorkspaces
  p <- currentProfile

  updateHist p $ workspaceScreenPairs $ filterWS pws $ cur:vis
  where
    workspaceScreenPairs wins = zip (W.screen <$> wins) (W.tag . W.workspace <$> wins)
    filterWS pws = filter ((\wid -> (wid `elem` pws) && (wid `notElem` ews)) . W.tag . W.workspace)

--------------------------------------------------------------------------------
updateHist :: ProfileId -> [(ScreenId, WorkspaceId)] -> X()
updateHist pid xs = profileWorkspaces pid >>= XS.modify' . update
  where
    update pws hs = force $ hs { history = doUpdate pws $ history hs }

    doUpdate pws hist = foldl (\acc (sid, wid) -> Map.alter (f pws sid wid) pid acc) hist xs

    f pws sid wid val = case val of
      Nothing -> pure [(sid, wid)]
      Just hs -> pure $ let new = (sid, wid) in new:filterWS pws new hs

    filterWS :: [WorkspaceId] -> (ScreenId, WorkspaceId) -> [(ScreenId, WorkspaceId)] -> [(ScreenId, WorkspaceId)]
    filterWS pws new = filter (\x -> snd x `elem` pws && x /= new)

--------------------------------------------------------------------------------
-- | Adds profiles to ProfileState and sets current profile using .

profilesStartupHook :: [Profile] -> ProfileId -> X ()
profilesStartupHook ps pid = XS.modify go >> switchWSOnScreens pid
  where
    go :: ProfileState -> ProfileState
    go s = s {profilesMap = update $ profilesMap s, current = setCurrentProfile $ Map.fromList $ map entry ps}

    update :: ProfileMap -> ProfileMap
    update = Map.union (Map.fromList $ map entry ps)

    entry :: Profile -> (ProfileId, Profile)
    entry p = (profileId p, p)

    setCurrentProfile :: ProfileMap -> Maybe Profile
    setCurrentProfile s = case Map.lookup pid s of
      Nothing -> Just $ Profile pid []
      Just pn -> Just pn

--------------------------------------------------------------------------------
setPrevious :: ProfileId -> X()
setPrevious name = XS.modify update
  where
    update ps = ps { previous = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profilesMap ps of
      Nothing -> previous ps
      Just p -> Just $ profileId p

--------------------------------------------------------------------------------
setProfile :: ProfileId -> X ()
setProfile p = currentProfile >>= setPrevious >> setProfile' p

--------------------------------------------------------------------------------
setProfile' :: ProfileId -> X ()
setProfile' name = XS.modify update
  where
    update ps = ps { current = doUpdate ps }
    doUpdate ps = case Map.lookup name $ profilesMap ps of
      Nothing -> current ps
      Just p -> Just p

--------------------------------------------------------------------------------
-- | Switch to a profile.
switchToProfile :: ProfileId -> X()
switchToProfile pid = setProfile pid >> switchWSOnScreens pid

--------------------------------------------------------------------------------
-- | Returns the workspace ids associated with a profile id.
profileWorkspaces :: ProfileId -> X [WorkspaceId]
profileWorkspaces pid = profileMap >>= findPWs
  where
    findPWs pm = return . profileWS . fromMaybe defaultProfile $ Map.lookup pid pm

--------------------------------------------------------------------------------
-- | Prompt for adding a workspace id to a profile.
addWSToProfilePrompt :: XPConfig -> X()
addWSToProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Add ws to profile:") c (mkComplFunFromList' c ps) f
  where
   f :: String -> X()
   f p = do
     vis <- gets $ fmap (W.tag . W.workspace) . W.visible . windowset
     cur <- gets $ W.tag . W.workspace . W.current . windowset
     hid <- gets $ fmap W.tag . W.hidden . windowset
     let
       arr = cur:(vis <> hid)
       in mkXPrompt (ProfilePrompt "Ws to add to profile:") c (mkComplFunFromList' c arr) (`addWSToProfile` p)

--------------------------------------------------------------------------------
-- | Prompt for switching profiles.
switchProfilePrompt :: XPConfig -> X()
switchProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Profile: ") c (mkComplFunFromList' c ps) switchToProfile
     
--------------------------------------------------------------------------------
-- | Prompt for switching workspaces.
switchProfileWSPrompt :: XPConfig -> X ()
switchProfileWSPrompt c = mkPrompt =<< currentProfileWorkspaces
  where
    mkPrompt pws = mkXPrompt (ProfilePrompt "Switch to workspace:") c (mkComplFunFromList' c pws) mbygoto 
    mbygoto wid = do
      pw <- profileWorkspaces =<< currentProfile
      unless (wid `notElem` pw) (windows . W.greedyView $ wid)

--------------------------------------------------------------------------------
-- | Prompt for shifting windows to a different workspace.
shiftProfileWSPrompt :: XPConfig -> X ()
shiftProfileWSPrompt c = mkPrompt =<< currentProfileWorkspaces
  where
    mkPrompt pws = mkXPrompt (ProfilePrompt "Send window to workspace:") c (mkComplFunFromList' c pws) mbyshift
    mbyshift wid = do
      pw <- profileWorkspaces =<< currentProfile
      unless (wid `notElem` pw) (windows . W.shift $ wid)

--------------------------------------------------------------------------------
addWSToProfile :: WorkspaceId -> ProfileId -> X()
addWSToProfile wid pid = XS.modify go
  where
   go :: ProfileState -> ProfileState
   go ps = ps {profilesMap = update $ profilesMap ps, current = update' $ fromMaybe defaultProfile $ current ps}

   update :: ProfileMap -> ProfileMap
   update mp = case Map.lookup pid mp of
     Nothing -> mp
     Just p  -> if wid `elem` profileWS p then mp else Map.adjust f pid mp

   f :: Profile -> Profile
   f p = Profile pid (wid : profileWS p)

   update' :: Profile -> Maybe Profile
   update' cp = if profileId cp == pid && wid `notElem` profileWS cp then Just (Profile pid $ wid:profileWS cp) else Just cp

--------------------------------------------------------------------------------
-- | Prompt for removing a workspace from a profile.
removeWSFromProfilePrompt :: XPConfig -> X()
removeWSFromProfilePrompt c = do
  ps <- profileIds
  mkXPrompt (ProfilePrompt "Remove ws from profile:") c (mkComplFunFromList' c ps) f
  where
   f :: String -> X()
   f p = do
     arr <- profileWorkspaces p
     mkXPrompt (ProfilePrompt "Ws to remove from profile:") c (mkComplFunFromList' c arr) $
       \ws -> do
         cp <- currentProfile
         ws `removeWSFromProfile` p 
         when (cp == p) $ currentProfile >>= switchWSOnScreens

--------------------------------------------------------------------------------
removeWSFromProfile :: WorkspaceId -> ProfileId -> X()
removeWSFromProfile wid pid = XS.modify go
  where
   go :: ProfileState -> ProfileState
   go ps = ps {profilesMap = update $ profilesMap ps, current = update' $ fromMaybe defaultProfile $ current ps}

   update :: ProfileMap -> ProfileMap
   update mp = case Map.lookup pid mp of
     Nothing -> mp
     Just p  -> if wid `elem` profileWS p then Map.adjust f pid mp else mp

   f :: Profile -> Profile
   f p = Profile pid (delete wid $ profileWS p)

   update' :: Profile -> Maybe Profile
   update' cp = if profileId cp == pid && wid `elem` profileWS cp then Just (Profile pid $ delete wid $ profileWS cp) else Just cp

--------------------------------------------------------------------------------
-- | Pretty printer for a bar. Prints workspace ids of current profile.
excludeWSPP :: PP -> X PP
excludeWSPP pp = modifyPP <$> currentProfileWorkspaces
  where
    modifyPP pws = pp { ppRename = ppRename pp . printTag pws }
    printTag pws tag = if tag `elem` pws then tag else ""

--------------------------------------------------------------------------------
-- | For cycling through workspaces associated with the current.
wsFilter :: WSType
wsFilter = WSIs $ currentProfileWorkspaces >>= (\ws -> return $ (`elem` ws) . W.tag)

--------------------------------------------------------------------------------
-- Takes care of placing correct workspaces on their respective screens.
-- It does this by reducing the history of a Profile until it gets an array of length
-- equal to the number of screens with pairs that have unique workspace ids.
switchWSOnScreens :: ProfileId -> X()
switchWSOnScreens pid = do
  hist <- profileHistory
  vis <- gets $ W.visible . windowset
  cur <- gets $ W.current . windowset
  pws <- profileMap <&> (profileWS . fromMaybe (Profile pid []) . Map.lookup pid)
  case Map.lookup pid hist of
    Nothing -> switchScreens $ zip (W.screen <$> (cur:vis)) pws
    Just xs -> compareAndSwitch (f (W.screen <$> cur:vis) xs) (cur:vis) pws
  where
    f :: [ScreenId] -> [(ScreenId, WorkspaceId)] -> [(ScreenId, WorkspaceId)]
    f sids = reorderUniq . reorderUniq . reverse . filter ((`elem` sids) . fst)

    reorderUniq :: (Ord k, Ord v) => [(k,v)] -> [(v,k)]
    reorderUniq = map (\(x,y) -> (y,x)) . uniq

    uniq :: (Ord k, Ord v) => [(k,v)] -> [(k,v)]
    uniq = Map.toList . Map.fromList

    viewWS fview sid wid = windows $ fview sid wid

    switchScreens = mapM_ (uncurry $ viewWS greedyViewOnScreen)

    compareAndSwitch hist wins pws | length hist < length wins = switchScreens $ hist <> populateScreens hist wins pws
                                   | otherwise                 = switchScreens hist

    populateScreens hist wins pws = zip (filter (`notElem` map fst hist) $ W.screen <$> wins) (filter (`notElem` map snd hist) pws)

--------------------------------------------------------------------------------
chooseAction :: (String -> X ()) -> X ()
chooseAction f = XS.gets current <&> (profileId . fromMaybe defaultProfile) >>= f

--------------------------------------------------------------------------------
-- | Create keybindings per profile.
bindOn :: [(String, X ())] -> X ()
bindOn bindings = chooseAction chooser
  where
    chooser profile = case lookup profile bindings of
        Just action -> action
        Nothing -> case lookup "" bindings of
            Just action -> action
            Nothing -> return ()

--------------------------------------------------------------------------------
-- | Loggs currentProfile and all profiles with hidden workspaces
--   (workspaces that aren't shown on a screen but have windows).
profileLogger :: (String -> String) -> (String -> String) -> Logger
profileLogger formatFocused formatUnfocused = do
  hws <- gets $ W.hidden . windowset
  p <- currentProfile
  hm <- map fst
      . filter (\(p', xs) -> any ((`elem` htags hws) . snd) xs || p' == p)
      . Map.toList <$> profileHistory
  return $ Just $ foldl (\a b -> a ++ " " ++ b) "" $ format p <$> hm
  where
    format p a = if a == p then formatFocused a else formatUnfocused a
    htags wins = W.tag <$> filter (isJust . W.stack) wins

--------------------------------------------------------------------------------
-- | @XWindowMap@ of all windows contained in a profile.
allProfileWindows :: XWindowMap
allProfileWindows = allProfileWindows' def

--------------------------------------------------------------------------------
allProfileWindows' :: WindowBringerConfig -> XWindowMap
allProfileWindows' WindowBringerConfig{ windowTitler = titler, windowFilter = include } = do
  pws <- currentProfileWorkspaces
  windowSet <- gets windowset
  Map.fromList . concat <$> mapM keyValuePairs (filter ((`elem` pws) . W.tag) $ W.workspaces windowSet)
   where keyValuePairs ws = let wins = W.integrate' (W.stack ws)
                           in mapM (keyValuePair ws) =<< filterM include wins
         keyValuePair ws w = (, w) <$> titler ws w
