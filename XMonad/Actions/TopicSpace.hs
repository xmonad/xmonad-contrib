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
  , getLastFocusedTopics
  , setLastFocusedTopic
  , reverseLastFocusedTopics
  , pprWindowSet
  , topicActionWithPrompt
  , topicAction
  , currentTopicAction
  , switchTopic
  , switchNthLastFocused
  , switchNthLastFocusedExclude
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
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import qualified Data.Map        as M
-- > import qualified XMonad.StackSet as W
-- >
-- > import XMonad.Actions.TopicSpace
--
-- You will then have to
--
--   * Define new a new 'TopicConfig'
--
--   * Add the appropriate keybindings
--
--   * Replace the @workspaces@ field in your 'XConfig' with a list of your
--     topics names
--
-- Let us go through a full example together.  Given the following topic names
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
--
-- we can define a 'TopicConfig' like this
--
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
--
-- Above we have used the `spawnShell` and `spawnShellIn` helper functions; here
-- they are:
--
-- > spawnShell :: X ()
-- > spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn
-- >
-- > spawnShellIn :: Dir -> X ()
-- > spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"
-- > -- Some terminals support a working-directory option directly:
-- > -- spawnShellIn dir = spawn $ "alacritty --working-directory " ++ dir
--
-- Next, we define some other other useful helper functions.  Note that some of
-- these function make use of the 'workspacePrompt' function.  You will also
-- have to have an already defined 'XPConfig' (here called @myXPConfig@).
--
-- > goto :: Topic -> X ()
-- > goto = switchTopic myTopicConfig
-- >
-- > promptedGoto :: X ()
-- > promptedGoto = workspacePrompt myXPConfig goto
-- >
-- > promptedShift :: X ()
-- > promptedShift = workspacePrompt myXPConfig $ windows . W.shift
-- >
-- > -- Toggle between the two most recently used topics while filtering
-- > -- out the scratchpad topic.
-- > toggleTopic :: X ()
-- > toggleTopic = switchNthLastFocusedExclude ["NSP"] myTopicConfig 1
--
-- Hopefully you've gotten a general feeling of how to define these kind of
-- small helper functions using what's provided in this module.
--
-- Adding the appropriate keybindings works as it normally would:
--
-- > -- extend your keybindings
-- > myKeys conf@XConfig{modMask=modm} =
-- >   [ ((modm              , xK_n     ), spawnShell) -- %! Launch terminal
-- >   , ((modm              , xK_a     ), currentTopicAction myTopicConfig)
-- >   , ((modm              , xK_g     ), promptedGoto)
-- >   , ((modm .|. shiftMask, xK_g     ), promptedShift)
-- >   , ((modm .|. shiftMask, xK_space ), toggleTopic)
-- >   {- more  keys ... -}
-- >   ]
-- >   ++
-- >   -- Switching to recently used topics
-- >   [ ((modm, k), switchNthLastFocused myTopicConfig i)
-- >   | (i, k) <- zip [1..] workspaceKeys]
--
-- If you want a more "default" experience with regards to @M-1@ through @M-9@
-- (i.e. switch to the first nine topics in `myTopics` instead of switching to
-- the last used ones), you can replace the last list above with the following
-- (using "EZConfig" syntax):
--
-- >   -- The following does two things:
-- >   --   1. Switch topics (no modifier)
-- >   --   2. Move focused window to topic N (shift modifier)
-- >   [ ("M-" ++ m ++ k, f i)
-- >   | (i, k) <- zip myTopics (map show [1 .. 9 :: Int])
-- >   , (f, m) <- [(goto, ""), (windows . W.shift, "S-")]
-- >   ]
--
-- We can now put the whole configuration together with the following (while
-- also checking that we haven't made any mistakes):
--
-- > myConfig = do
-- >     checkTopicConfig myTopics myTopicConfig
-- >     return $ def
-- >          { workspaces = myTopics
-- >          , keys       = myKeys
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

-- | 'Dir' is just an alias for 'FilePath' but should point to a directory.
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
                                 -- ^ This is the default (= fallback) topic.
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

newtype PrevTopics = PrevTopics { getPrevTopics :: [String] } deriving (Read,Show,Typeable)
instance ExtensionClass PrevTopics where
  initialValue  = PrevTopics []
  extensionType = PersistentExtension

-- | Return the (possibly empty) list of last focused topics.
getLastFocusedTopics :: X [String]
getLastFocusedTopics = XS.gets getPrevTopics

-- | Given a 'TopicConfig', a topic, and a predicate to select topics that one
-- wants to keep, this function will filter the list of last focused topics
-- according to the predicate and cons the topic in front of that list.  Note
-- that we prune the list in case its length exceeds 'maxTopicHistory'.
setLastFocusedTopic :: TopicConfig -> Topic -> (Topic -> Bool) -> X ()
setLastFocusedTopic tc w predicate =
  XS.modify $ PrevTopics
            . take (maxTopicHistory tc)
            . nub . (w :) . filter predicate . getPrevTopics

-- | Reverse the list of "last focused topics"
reverseLastFocusedTopics :: X ()
reverseLastFocusedTopics =
  XS.modify $ PrevTopics . reverse . getPrevTopics

-- | This function is a variant of 'DL.pprWindowSet' which takes a topic configuration
-- and a pretty-printing record 'PP'. It will show the list of topics sorted historically
-- and highlight topics with urgent windows.
pprWindowSet :: TopicConfig -> PP -> X String
pprWindowSet tg pp = do
  winset <- gets windowset
  urgents <- readUrgents
  let empty_workspaces = map W.tag $ filter (isNothing . W.stack) $ W.workspaces winset
      maxDepth = maxTopicHistory tg
  setLastFocusedTopic tg
                      (W.tag . W.workspace . W.current $ winset)
                      (`notElem` empty_workspaces)
  lastWs <- getLastFocusedTopics
  let depth topic = fromJust $ elemIndex topic (lastWs ++ [topic])
      add_depth proj topic = proj pp . (((topic++":")++) . show) . depth $ topic
      pp' = pp { ppHidden = add_depth ppHidden, ppVisible = add_depth ppVisible }
      sortWindows = take maxDepth . sortBy (comparing $ depth . W.tag)
  return $ DL.pprWindowSet sortWindows urgents pp' winset

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
switchTopic tg topic = do
  -- Switch to topic and add it to the last seen topics
  windows $ W.greedyView topic
  setLastFocusedTopic tg topic (const True)

  -- If applicable, execute the topic action
  wins <- gets (W.integrate' . W.stack . W.workspace . W.current . windowset)
  when (null wins) $ topicAction tg topic

-- | Switch to the Nth last focused topic or fall back to the 'defaultTopic'.
switchNthLastFocused :: TopicConfig -> Int -> X ()
switchNthLastFocused = switchNthLastFocusedExclude []

-- | Like 'switchNthLastFocused', but also filter out certain topics.
switchNthLastFocusedExclude :: [Topic] -> TopicConfig -> Int -> X ()
switchNthLastFocusedExclude excludes tc depth = do
  lastWs <- filter (`notElem` excludes) <$> getLastFocusedTopics
  switchTopic tc $ (lastWs ++ repeat (defaultTopic tc)) !! depth

-- | Shift the focused window to the Nth last focused topic, or fall back to doing nothing.
shiftNthLastFocused :: Int -> X ()
shiftNthLastFocused n = do
  ws <- fmap (listToMaybe . drop n) getLastFocusedTopics
  whenJust ws $ windows . W.shift

-- | Return the directory associated with the current topic, or return the empty
-- string if the topic could not be found.
currentTopicDir :: TopicConfig -> X String
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

-- | Display the given message using the @xmessage@ program.
xmessage :: String -> IO ()
xmessage s = do
  h <- spawnPipe "xmessage -file -"
  hPutStr h s
  hClose h
