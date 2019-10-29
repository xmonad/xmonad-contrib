{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TopicSpace
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
   Topic
  , Dir
  , TopicConfig(..)
  , def
  , defaultTopicConfig
  , getLastFocusedTopics
  , setLastFocusedTopic
  , reverseLastFocusedTopics
  , pprWindowSet
  , topicActionWithPrompt
  , topicAction
  , currentTopicAction
  , switchTopic
  , switchNthLastFocused
  , shiftNthLastFocused
  , currentTopicDir
  , checkTopicConfig
  , (>*>)
  )
where

import XMonad

import Data.List
import Data.Maybe (fromMaybe, isNothing, listToMaybe, fromJust)
import Data.Ord
import qualified Data.Map as M
import Control.Applicative (liftA2)
import Control.Monad (when,unless,replicateM_)
import System.IO

import qualified XMonad.StackSet as W

import XMonad.Prompt
import XMonad.Prompt.Workspace

import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.DynamicLog (PP(..))
import qualified XMonad.Hooks.DynamicLog as DL

import XMonad.Util.Run (spawnPipe)
import qualified XMonad.Util.ExtensibleState as XS

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
-- Here is an example of configuration using TopicSpace:
--
-- > -- The list of all topics/workspaces of your xmonad configuration.
-- > -- The order is important, new topics must be inserted
-- > -- at the end of the list if you want hot-restarting
-- > -- to work.
-- > myTopics :: [Topic]
-- > myTopics =
-- >   [ "dashboard" -- the first one
-- >   , "admin", "build", "cleaning", "conf", "darcs", "haskell", "irc"
-- >   , "mail", "movie", "music", "talk", "text", "tools", "web", "xmonad"
-- >   , "yi", "documents", "twitter", "pdf"
-- >   ]
-- >
-- > myTopicConfig :: TopicConfig
-- > myTopicConfig = def
-- >   { topicDirs = M.fromList $
-- >       [ ("conf", "w/conf")
-- >       , ("dashboard", "Desktop")
-- >       , ("yi", "w/dev-haskell/yi")
-- >       , ("darcs", "w/dev-haskell/darcs")
-- >       , ("haskell", "w/dev-haskell")
-- >       , ("xmonad", "w/dev-haskell/xmonad")
-- >       , ("tools", "w/tools")
-- >       , ("movie", "Movies")
-- >       , ("talk", "w/talks")
-- >       , ("music", "Music")
-- >       , ("documents", "w/documents")
-- >       , ("pdf", "w/documents")
-- >       ]
-- >   , defaultTopicAction = const $ spawnShell >*> 3
-- >   , defaultTopic = "dashboard"
-- >   , topicActions = M.fromList $
-- >       [ ("conf",       spawnShell >> spawnShellIn "wd/ertai/private")
-- >       , ("darcs",      spawnShell >*> 3)
-- >       , ("yi",         spawnShell >*> 3)
-- >       , ("haskell",    spawnShell >*> 2 >>
-- >                        spawnShellIn "wd/dev-haskell/ghc")
-- >       , ("xmonad",     spawnShellIn "wd/x11-wm/xmonad" >>
-- >                        spawnShellIn "wd/x11-wm/xmonad/contrib" >>
-- >                        spawnShellIn "wd/x11-wm/xmonad/utils" >>
-- >                        spawnShellIn ".xmonad" >>
-- >                        spawnShellIn ".xmonad")
-- >       , ("mail",       mailAction)
-- >       , ("irc",        ssh somewhere)
-- >       , ("admin",      ssh somewhere >>
-- >                        ssh nowhere)
-- >       , ("dashboard",  spawnShell)
-- >       , ("twitter",    spawnShell)
-- >       , ("web",        spawn browserCmd)
-- >       , ("movie",      spawnShell)
-- >       , ("documents",  spawnShell >*> 2 >>
-- >                        spawnShellIn "Documents" >*> 2)
-- >       , ("pdf",        spawn pdfViewerCmd)
-- >       ]
-- >   }
-- >
-- > -- extend your keybindings
-- > myKeys conf@XConfig{modMask=modm} =
-- >   [ ((modm              , xK_n     ), spawnShell) -- %! Launch terminal
-- >   , ((modm              , xK_a     ), currentTopicAction myTopicConfig)
-- >   , ((modm              , xK_g     ), promptedGoto)
-- >   , ((modm .|. shiftMask, xK_g     ), promptedShift)
-- >   {- more  keys ... -}
-- >   ]
-- >   ++
-- >   [ ((modm, k), switchNthLastFocused myTopicConfig i)
-- >   | (i, k) <- zip [1..] workspaceKeys]
-- >
-- > spawnShell :: X ()
-- > spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
-- >
-- > spawnShellIn :: Dir -> X ()
-- > spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"
-- >
-- > goto :: Topic -> X ()
-- > goto = switchTopic myTopicConfig
-- >
-- > promptedGoto :: X ()
-- > promptedGoto = workspacePrompt myXPConfig goto
-- >
-- > promptedShift :: X ()
-- > promptedShift = workspacePrompt myXPConfig $ windows . W.shift
-- >
-- > myConfig = do
-- >     checkTopicConfig myTopics myTopicConfig
-- >     myLogHook <- makeMyLogHook
-- >     return $ def
-- >          { borderWidth = 1 -- Width of the window border in pixels.
-- >          , workspaces = myTopics
-- >          , layoutHook = myModifiers myLayout
-- >          , manageHook = myManageHook
-- >          , logHook = myLogHook
-- >          , handleEventHook = myHandleEventHook
-- >          , terminal = myTerminal -- The preferred terminal program.
-- >          , normalBorderColor = "#3f3c6d"
-- >          , focusedBorderColor = "#4f66ff"
-- >          , XMonad.modMask = mod1Mask
-- >          , keys = myKeys
-- >          , mouseBindings = myMouseBindings
-- >          }
-- >
-- > main :: IO ()
-- > main = xmonad =<< myConfig

-- | An alias for @flip replicateM_@
(>*>) :: Monad m => m a -> Int -> m ()
(>*>) = flip replicateM_
infix >*>

-- | 'Topic' is just an alias for 'WorkspaceId'
type Topic = WorkspaceId

-- | 'Dir' is just an alias for 'FilePath' but should points to a directory.
type Dir = FilePath

-- | Here is the topic space configuration area.
data TopicConfig = TopicConfig { topicDirs          :: M.Map Topic Dir
                                 -- ^ This mapping associate a directory to each topic.
                               , topicActions       :: M.Map Topic (X ())
                                 -- ^ This mapping associate an action to trigger when
                                 -- switching to a given topic which workspace is empty.
                               , defaultTopicAction :: Topic -> X ()
                                 -- ^ This is the default topic action.
                               , defaultTopic       :: Topic
                                 -- ^ This is the default topic.
                               , maxTopicHistory    :: Int
                                 -- ^ This setups the maximum depth of topic history, usually
                                 -- 10 is a good default since we can bind all of them using
                                 -- numeric keypad.
                               }

instance Default TopicConfig where
    def            = TopicConfig { topicDirs = M.empty
                                 , topicActions = M.empty
                                 , defaultTopicAction = const (ask >>= spawn . terminal . config)
                                 , defaultTopic = "1"
                                 , maxTopicHistory = 10
                                 }

{-# DEPRECATED defaultTopicConfig "Use def (from Data.Default, and re-exported by XMonad.Actions.TopicSpace) instead." #-}
defaultTopicConfig :: TopicConfig
defaultTopicConfig = def

newtype PrevTopics = PrevTopics { getPrevTopics :: [String] } deriving (Read,Show,Typeable)
instance ExtensionClass PrevTopics where
    initialValue = PrevTopics []
    extensionType = PersistentExtension

-- | Returns the list of last focused workspaces the empty list otherwise.
getLastFocusedTopics :: X [String]
getLastFocusedTopics = XS.gets getPrevTopics

-- | Given a 'TopicConfig', the last focused topic, and a predicate that will
-- select topics that one want to keep, this function will set the property
-- of last focused topics.
setLastFocusedTopic :: Topic -> (Topic -> Bool) -> X ()
setLastFocusedTopic w predicate =
  XS.modify $ PrevTopics
    . seqList . nub . (w:) . filter predicate
    . getPrevTopics
  where seqList xs = length xs `seq` xs

-- | Reverse the list of "last focused topics"
reverseLastFocusedTopics :: X ()
reverseLastFocusedTopics =
  XS.modify $ PrevTopics . reverse . getPrevTopics

-- | This function is a variant of 'DL.pprWindowSet' which takes a topic configuration
-- and a pretty-printing record 'PP'. It will show the list of topics sorted historically
-- and highlighting topics with urgent windows.
pprWindowSet :: TopicConfig -> PP -> X String
pprWindowSet tg pp = do
    winset <- gets windowset
    urgents <- readUrgents
    let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
        maxDepth = maxTopicHistory tg
    setLastFocusedTopic (W.tag . W.workspace . W.current $ winset)
                        (`notElem` empty_workspaces)
    lastWs <- getLastFocusedTopics
    let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
        add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
        pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
        sortWindows = take maxDepth . sortBy (comparing $ depth . W.tag)
    return $ DL.pprWindowSet sortWindows urgents pp' winset

-- | Given a prompt configuration and a topic configuration, triggers the action associated with
-- the topic given in prompt.
topicActionWithPrompt :: XPConfig -> TopicConfig -> X ()
topicActionWithPrompt xp tg = workspacePrompt xp (liftA2 (>>) (switchTopic tg) (topicAction tg))

-- | Given a configuration and a topic, triggers the action associated with the given topic.
topicAction :: TopicConfig -> Topic -> X ()
topicAction tg topic = fromMaybe (defaultTopicAction tg topic) $ M.lookup topic $ topicActions tg

-- | Trigger the action associated with the current topic.
currentTopicAction :: TopicConfig -> X ()
currentTopicAction tg = topicAction tg =<< gets (W.tag . W.workspace . W.current . windowset)

-- | Switch to the given topic.
switchTopic :: TopicConfig -> Topic -> X ()
switchTopic tg topic = do
  windows $ W.greedyView topic
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ topicAction tg topic

-- | Switch to the Nth last focused topic or failback to the 'defaultTopic'.
switchNthLastFocused :: TopicConfig -> Int -> X ()
switchNthLastFocused tg depth = do
  lastWs <- getLastFocusedTopics
  switchTopic tg $ (lastWs ++ repeat (defaultTopic tg)) !! depth

-- | Shift the focused window to the Nth last focused topic, or fallback to doing nothing.
shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused n = do
  ws <- fmap (listToMaybe . drop n) getLastFocusedTopics
  whenJust ws $ windows . W.shift

-- | Returns the directory associated with current topic returns the empty string otherwise.
currentTopicDir :: TopicConfig -> X String
currentTopicDir tg = do
  topic <- gets (W.tag . W.workspace . W.current . windowset)
  return . fromMaybe "" . M.lookup topic $ topicDirs tg

-- | Check the given topic configuration for duplicates topics or undefined topics.
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

-- | Display the given message using the @xmessage@ program.
xmessage :: String -> IO ()
xmessage s = do
  h <- spawnPipe "xmessage -file -"
  hPutStr h s
  hClose h
