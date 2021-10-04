{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TopicSpace
-- Description :  Turns your workspaces into a more topic oriented system.
-- Copyright   :  (c) Nicolas Pouillard
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Nicolas Pouillard <nicolas.pouillard@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Turns your workspaces into a more topic oriented system.
-----------------------------------------------------------------------------

module XMonad.Actions.TopicSpace
  (
  -- * Overview
  -- $overview

  -- * Usage
  -- $usage

  -- * Types for Building Topics
    Topic
  , Dir
  , TopicConfig(..)
  , TopicItem(..)

    -- * Managing 'TopicItem's
  , topicNames
  , tiActions
  , tiDirs
  , noAction
  , inHome

    -- * Switching and Shifting Topics
  , switchTopic
  , switchNthLastFocused
  , switchNthLastFocusedByScreen
  , switchNthLastFocusedExclude
  , shiftNthLastFocused

    -- * Topic Actions
  , topicActionWithPrompt
  , topicAction
  , currentTopicAction

    -- * Getting the Topic History
  , getLastFocusedTopics
  , workspaceHistory
  , workspaceHistoryByScreen

    -- * Modifying the Topic History
  , setLastFocusedTopic
  , reverseLastFocusedTopics

    -- * History hooks
  , workspaceHistoryHook
  , workspaceHistoryHookExclude

    -- * Pretty Printing
  , pprWindowSet

    -- * Utility
  , currentTopicDir
  , checkTopicConfig
  , (>*>)
  )
where

import XMonad
import XMonad.Prelude

import qualified Data.Map.Strict           as M
import qualified XMonad.Hooks.StatusBar.PP as SBPP
import qualified XMonad.StackSet           as W

import Data.Map (Map)

import XMonad.Prompt (XPConfig)
import XMonad.Prompt.Workspace (workspacePrompt)

import XMonad.Hooks.StatusBar.PP (PP(ppHidden, ppVisible))
import XMonad.Hooks.UrgencyHook (readUrgents)
import XMonad.Hooks.WorkspaceHistory
    ( workspaceHistory
    , workspaceHistoryByScreen
    , workspaceHistoryHook
    , workspaceHistoryHookExclude
    , workspaceHistoryModify
    )

-- $overview
-- This module allows to organize your workspaces on a precise topic basis.  So
-- instead of having a workspace called `work' you can setup one workspace per
-- task.  Here we call these workspaces, topics. The great thing with
-- topics is that one can attach a directory that makes sense to each
-- particular topic.  One can also attach an action which will be triggered
-- when switching to a topic that does not have any windows in it.  So you can
-- attach your mail client to the mail topic, some terminals in the right
-- directory to the xmonad topic... This package also provides a nice way to
-- display your topics in an historical way using a custom `pprWindowSet'
-- function. You can also easily switch to recent topics using this history
-- of last focused topics.

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import qualified Data.Map.Strict as M
-- > import qualified XMonad.StackSet as W
-- >
-- > import XMonad.Actions.TopicSpace
-- > import XMonad.Util.EZConfig    -- for the keybindings
-- > import XMonad.Prompt.Workspace -- if you want to use the prompt
--
-- You will then have to
--
--   * Define a new 'TopicConfig' via 'TopicItem's
--
--   * Add the appropriate keybindings
--
--   * Replace the @workspaces@ field in your 'XConfig' with a list of
--     your topics names
--
--   * Optionally, if you want to use the history features, add
--     'workspaceHistoryHook' from "XMonad.Hooks.WorkspaceHistory"
--     (re-exported by this module) or an equivalent function to your
--     @logHook@.  See the documentation of
--     "XMonad.Hooks.WorkspaceHistory" for further details
--
-- Let us go through a full example together.
--
-- A 'TopicItem' consists of three things: the name of the topic, its
-- root directory, and the action associated to it—to be executed if the
-- topic is empty or the action is forced via a keybinding.
--
-- We start by specifying our chosen topics as a list of such
-- 'TopicItem's:
--
-- > topicItems :: [TopicItem]
-- > topicItems =
-- >   [ inHome   "1:WEB"              (spawn "firefox")
-- >   , noAction "2"      "."
-- >   , noAction "3:VID"  "videos"
-- >   , TI       "4:VPN"  "openvpn"   (spawn "urxvt -e randomVPN.sh")
-- >   , inHome   "5:IM"               (spawn "signal" *> spawn "telegram")
-- >   , inHome   "6:IRC"              (spawn "urxvt -e weechat")
-- >   , TI       "dts"    ".dotfiles" spawnShell
-- >   , TI       "xm-con" "hs/xm-con" (spawnShell *> spawnShellIn "hs/xm")
-- >   ]
--
-- Then we just need to put together our topic config:
--
-- > myTopicConfig :: TopicConfig
-- > myTopicConfig = def
-- >   { topicDirs          = tiDirs    topicItems
-- >   , topicActions       = tiActions topicItems
-- >   , defaultTopicAction = const (pure ()) -- by default, do nothing
-- >   , defaultTopic       = "1:WEB"         -- fallback
-- >   }
--
-- Above, we have used the `spawnShell` and `spawnShellIn` helper
-- functions; here they are:
--
-- > spawnShell :: X ()
-- > spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
-- >
-- > spawnShellIn :: Dir -> X ()
-- > spawnShellIn dir = spawn $ "alacritty --working-directory " ++ dir
--
-- Next, we define some other other useful helper functions.  It is
-- rather common to have a lot of topics—much more than available keys!
-- In a situation like that, it's very convenient to switch topics with
-- a prompt; the following use of 'workspacePrompt' does exactly that.
--
-- > goto :: Topic -> X ()
-- > goto = switchTopic myTopicConfig
-- >
-- > promptedGoto :: X ()
-- > promptedGoto = workspacePrompt def goto
-- >
-- > promptedShift :: X ()
-- > promptedShift = workspacePrompt def $ windows . W.shift
-- >
-- > -- Toggle between the two most recently used topics, but keep
-- > -- screens separate.  This needs @workspaceHistoryHook@.
-- > toggleTopic :: X ()
-- > toggleTopic = switchNthLastFocusedByScreen myTopicConfig 1
--
-- Hopefully you've gotten a general feeling of how to define these kind of
-- small helper functions using what's provided in this module.
--
-- Adding the appropriate keybindings works as it normally would.  Here,
-- we'll use "XMonad.Util.EZConfig" syntax:
--
-- > myKeys :: [(String, X ())]
-- > myKeys =
-- >   [ ("M-n"        , spawnShell)
-- >   , ("M-a"        , currentTopicAction myTopicConfig)
-- >   , ("M-g"        , promptedGoto)
-- >   , ("M-S-g"      , promptedShift)
-- >   , ("M-S-<Space>", toggleTopic)
-- >   ]
-- >   ++
-- >   -- The following does two things:
-- >   --   1. Switch topics (no modifier)
-- >   --   2. Move focused window to topic N (shift modifier)
-- >   [ ("M-" ++ m ++ k, f i)
-- >   | (i, k) <- zip (topicNames topicItems) (map show [1 .. 9 :: Int])
-- >   , (f, m) <- [(goto, ""), (windows . W.shift, "S-")]
-- >   ]
--
-- This makes @M-1@ to @M-9@ switch to the first nine topics that we
-- have specified in @topicItems@.
--
-- You can also switch to the nine last-used topics instead:
--
-- >   [ ("M-" ++ show i, switchNthLastFocused myTopicConfig i)
-- >   | i <- [1 .. 9]
-- >   ]
--
-- We can now put the whole configuration together with the following:
--
-- > main :: IO ()
-- > main = xmonad $ def
-- >   { workspaces = topicNames topicItems
-- >   }
-- >  `additionalKeysP` myKeys

-- | An alias for @flip replicateM_@
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
infix >*>

-- | 'Topic' is just an alias for 'WorkspaceId'
type Topic = WorkspaceId

-- | 'Dir' is just an alias for 'FilePath', but should point to a directory.
type Dir = FilePath

-- | Here is the topic space configuration area.
data TopicConfig = TopicConfig { topicDirs          :: Map Topic Dir
                                 -- ^ This mapping associates a directory to each topic.
                               , topicActions       :: Map Topic (X ())
                                 -- ^ This mapping associates an action to trigger when
                                 -- switching to a given topic which workspace is empty.
                               , defaultTopicAction :: Topic -> X ()
                                 -- ^ This is the default topic action.
                               , defaultTopic       :: Topic
                                 -- ^ This is the default (= fallback) topic.
                               , maxTopicHistory    :: Int
                                 -- ^ This specifies the maximum depth of the topic history;
                                 -- usually 10 is a good default since we can bind all of
                                 -- them using numeric keypad.
                               }
{-# DEPRECATED maxTopicHistory "This field will be removed in the future; history is now handled by XMonad.Hooks.WorkspaceHistory" #-}

instance Default TopicConfig where
  def            = TopicConfig { topicDirs = M.empty
                               , topicActions = M.empty
                               , defaultTopicAction = const (ask >>= spawn . terminal . config)
                               , defaultTopic = "1"
                               , maxTopicHistory = 10
                               }

-- | Return the (possibly empty) list of last focused topics.
getLastFocusedTopics :: X [Topic]
getLastFocusedTopics = workspaceHistory
{-# DEPRECATED getLastFocusedTopics "Use XMonad.Hooks.WorkspaceHistory.workspaceHistory (re-exported by this module) instead" #-}

-- | Given a 'TopicConfig', a topic, and a predicate to select topics that one
-- wants to keep, this function will cons the topic in front of the list of
-- last focused topics and filter it according to the predicate.  Note that we
-- prune the list in case that its length exceeds 'maxTopicHistory'.
setLastFocusedTopic :: TopicConfig -> Topic -> (Topic -> Bool) -> X ()
setLastFocusedTopic tc w predicate = do
  sid <- gets $ W.screen . W.current . windowset
  workspaceHistoryModify $
    take (maxTopicHistory tc) . nub . filter (predicate . snd) . ((sid, w) :)
{-# DEPRECATED setLastFocusedTopic "Use XMonad.Hooks.WorkspaceHistory instead" #-}

-- | Reverse the list of "last focused topics"
reverseLastFocusedTopics :: X ()
reverseLastFocusedTopics = workspaceHistoryModify reverse

-- | This function is a variant of 'SBPP.pprWindowSet' which takes a topic
-- configuration and a pretty-printing record 'PP'. It will show the list of
-- topics sorted historically and highlight topics with urgent windows.
pprWindowSet :: TopicConfig -> PP -> X String
pprWindowSet tg pp = do
  winset <- gets windowset
  urgents <- readUrgents
  let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
      maxDepth = maxTopicHistory tg
  setLastFocusedTopic tg
                      (W.tag . W.workspace . W.current $ winset)
                      (`notElem` empty_workspaces)
  lastWs <- workspaceHistory
  let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
      add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
      pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
      sortWindows = take maxDepth . sortOn (depth . W.tag)
  return $ SBPP.pprWindowSet sortWindows urgents pp' winset

-- | Given a prompt configuration and a topic configuration, trigger the action associated with
-- the topic given in prompt.
topicActionWithPrompt :: XPConfig -> TopicConfig -> X ()
topicActionWithPrompt xp tg = workspacePrompt xp (liftA2 (>>) (switchTopic tg) (topicAction tg))

-- | Given a configuration and a topic, trigger the action associated with the given topic.
topicAction :: TopicConfig -> Topic -> X ()
topicAction tg topic = fromMaybe (defaultTopicAction tg topic) $ M.lookup topic $ topicActions tg

-- | Trigger the action associated with the current topic.
currentTopicAction :: TopicConfig -> X ()
currentTopicAction tg = topicAction tg =<< gets (W.tag . W.workspace . W.current . windowset)

-- | Switch to the given topic.
switchTopic :: TopicConfig -> Topic -> X ()
switchTopic tc topic = do
  -- Switch to topic and add it to the last seen topics
  windows $ W.greedyView topic

  -- If applicable, execute the topic action
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ topicAction tc topic

-- | Switch to the Nth last focused topic or fall back to the 'defaultTopic'.
switchNthLastFocused :: TopicConfig -> Int -> X ()
switchNthLastFocused = switchNthLastFocusedExclude []

-- | Like 'switchNthLastFocused', but also filter out certain topics.
switchNthLastFocusedExclude :: [Topic] -> TopicConfig -> Int -> X ()
switchNthLastFocusedExclude excludes tc depth = do
  lastWs <- filter (`notElem` excludes) <$> workspaceHistory
  switchTopic tc $ (lastWs ++ repeat (defaultTopic tc)) !! depth

-- | Like 'switchNthLastFocused', but only consider topics that used to
-- be on the current screen.
--
-- For example, the following function allows one to toggle between the
-- currently focused and the last used topic, while treating different
-- screens completely independently from one another.
--
-- > toggleTopicScreen = switchNthLastFocusedByScreen myTopicConfig 1
switchNthLastFocusedByScreen :: TopicConfig -> Int -> X ()
switchNthLastFocusedByScreen tc depth = do
  sid <- gets $ W.screen . W.current . windowset
  sws <- fromMaybe []
       . listToMaybe
       . map snd
       . filter ((== sid) . fst)
     <$> workspaceHistoryByScreen
  switchTopic tc $ (sws ++ repeat (defaultTopic tc)) !! depth

-- | Shift the focused window to the Nth last focused topic, or fall back to doing nothing.
shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused n = do
  ws <- fmap (listToMaybe . drop n) workspaceHistory
  whenJust ws $ windows . W.shift

-- | Return the directory associated with the current topic, or return the empty
-- string if the topic could not be found.
currentTopicDir :: TopicConfig -> X FilePath
currentTopicDir tg = do
  topic <- gets (W.tag . W.workspace . W.current . windowset)
  return . fromMaybe "" . M.lookup topic $ topicDirs tg

-- | Check the given topic configuration for duplicate or undefined topics.
checkTopicConfig :: [Topic] -> TopicConfig -> IO ()
checkTopicConfig tags tg = do
  -- tags <- gets $ map W.tag . workspaces . windowset

  let
    seenTopics = nub $ sort $ M.keys (topicDirs tg) ++ M.keys (topicActions tg)
    dups       = tags \\ nub tags
    diffTopic  = seenTopics \\ sort tags
    check lst msg = unless (null lst) $ xmessage $ msg ++ " (tags): " ++ show lst

  check diffTopic "Seen but missing topics/workspaces"
  check dups      "Duplicate topics/workspaces"

-- | Convenience type for specifying topics.
data TopicItem = TI
  { tiName   :: !Topic  -- ^ 'Topic' ≡ 'String'
  , tiDir    :: !Dir    -- ^ Directory associated with topic; 'Dir' ≡ 'String'
  , tiAction :: !(X ()) -- ^ Startup hook when topic is empty
  }

-- | Extract the names from a given list of 'TopicItem's.
topicNames :: [TopicItem] -> [Topic]
topicNames = map tiName

-- | From a list of 'TopicItem's, build a map that can be supplied as
-- the 'topicDirs'.
tiDirs :: [TopicItem] -> Map Topic Dir
tiDirs = M.fromList . map (\TI{ tiName, tiDir } -> (tiName, tiDir))

-- | From a list of 'TopicItem's, build a map that can be supplied as
-- the 'topicActions'.
tiActions :: [TopicItem] -> Map Topic (X ())
tiActions = M.fromList . map (\TI{ tiName, tiAction } -> (tiName, tiAction))

-- | Associate a directory with the topic, but don't spawn anything.
noAction :: Topic -> Dir -> TopicItem
noAction n d = TI n d (pure ())

-- | Topic with @tiDir = ~/@.
inHome :: Topic -> X () -> TopicItem
inHome n = TI n "."
