{- |
Module      :  XMonad.Actions.Launcher
Copyright   :  (C) 2012 Carlos López-Camey
License     :  None; public domain

Maintainer  :  <c.lopez@kmels.net>
Stability   :  unstable

A set of prompts for XMonad
-}

module XMonad.Actions.Launcher(
  -- * Description and use
  -- $description
  defaultLauncherModes
  , ExtensionActions
  , LauncherConfig(..)
  , LocateFileMode
  , LocateFileRegexMode
  , launcherPrompt
  -- * ToDo
  -- $todo
) where

import           Data.List        (find, findIndex, isPrefixOf, tails)
import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe, isJust)
import           System.Directory (doesDirectoryExist)
import           XMonad           hiding (config)
import           XMonad.Prompt
import           XMonad.Util.Run

{- $description
    This module lets you combine and switch between different types of prompts (XMonad.Prompt). It includes a set of default modes:
    
       * Hoogle mode: Search for functions using hoogle, choosing a function leads you to documentation in Haddock.
       
       * Locate mode: Search for files using locate, choosing a file opens it with a program you specify depending on the file's extension.
       
       * Locate regexp: Same as locate mode but autocomplete works with regular expressions.  
       
       * Calc: Uses the program calc to do calculations.

    To use the default modes, modify your .xmonad:
   
    > import XMonad.Prompt(defaultXPConfig)
    > import XMonad.Actions.Launcher

    > ((modm .|. controlMask, xK_l), launcherPrompt kmelsXPConfig $ defaultLauncherModes launcherConfig)   
    
    A LauncherConfig contains settings for the default modes, modify them accordingly. 
    
    > launcherConfig = LauncherConfig { pathToHoogle = "/home/YOU/.cabal/bin/hoogle" , actionsByExtension  = extensionActions }
    
@extensionActions :: M.Map String (String -> X())
extensionActions = M.fromList $ [
 (\".hs\", \p -> spawn $ \"emacs \" ++ p)
 , (\".pdf\", \p -> spawn $ \"acroread \" ++ p)
 , (\".*\", \p -> spawn $ \"emacs \" ++ p) --match with any files
 , (\"/\", \p -> spawn $ \"nautilus \" ++ p) --match with directories 
 ]@
 
 To try it, restart xmonad. Press Ctrl + Your_Modkey + L and the first prompt should pop up. 
 
 You can change mode with xK_grave if you used defaultXP or change the value of changeModeKey in your XPConfig-}

data LocateFileMode = LMode ExtensionActions
data LocateFileRegexMode = LRMode ExtensionActions
data HoogleMode = HMode FilePath String --path to hoogle e.g. "/home/me/.cabal/bin/hoogle"
data CalculatorMode = CalcMode

data LauncherConfig = LauncherConfig {  
  browser                :: String
  , pathToHoogle         :: String
  , actionsByExtension   :: ExtensionActions
}

type ExtensionActions = M.Map String (String -> X())

-- | Uses the program `locate` to list files
instance XPrompt LocateFileMode where
  showXPrompt (LMode _) = "locate %s> "
  completionFunction (LMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5",s])
  modeAction (LMode actions) _ fp = spawnWithActions actions fp

-- | Uses the program `locate --regex` to list files
instance XPrompt LocateFileRegexMode where
  showXPrompt (LRMode _) = "locate --regexp %s> "
  completionFunction (LRMode _) = \s -> if (s == "" || last s == ' ') then return [] else (completionFunctionWith "locate" ["--limit","5","--regexp",s])
  modeAction (LRMode actions) _ fp = spawnWithActions actions fp

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
  showXPrompt CalcMode = "calc %s> "
  commandToComplete CalcMode = id --send the whole string to `calc`
  completionFunction CalcMode = \s -> if (length s == 0) then return [] else do
    fmap lines $ runProcessWithInput "calc" [s] ""
  modeAction CalcMode _ _ = return () -- do nothing; this might copy the result to the clipboard

-- | Uses the program `hoogle` to search for functions
instance XPrompt HoogleMode where
  showXPrompt _ = "hoogle %s> "
  commandToComplete _ = id
  completionFunction (HMode pathToHoogleBin' _) = \s -> completionFunctionWith pathToHoogleBin' ["--count","5",s]
  -- This action calls hoogle again to find the URL corresponding to the autocompleted item
  modeAction (HMode pathToHoogleBin'' browser) query result = do
    completionsWithLink <- liftIO $ completionFunctionWith pathToHoogleBin'' ["--count","5","--link",query]
    let link = do
          s <- find (isJust . \c -> findSeqIndex c result) completionsWithLink
          i <- findSeqIndex s "http://"
          return $ drop i s
    case link of
       Just l -> spawn $ browser ++ " " ++ l
       _      -> return ()
    where
      -- | Receives a sublist and a list. It returns the index where the sublist appears in the list.
      findSeqIndex :: (Eq a) => [a] -> [a] -> Maybe Int
      findSeqIndex xs xss = findIndex (isPrefixOf xss) $ tails xs

-- | Creates an autocompletion function for a programm given the program's name and a list of args to send to the command.
completionFunctionWith :: String -> [String] -> IO [String]
completionFunctionWith cmd args = do fmap lines $ runProcessWithInput cmd args ""

-- | Creates a prompt with the given modes
launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config

-- | Create a list of modes based on :
-- a list of extensions mapped to actions
-- the path to hoogle
defaultLauncherModes :: LauncherConfig -> [XPMode]
defaultLauncherModes cnf = let
  ph           = pathToHoogle cnf
  actions      = actionsByExtension cnf
  in [ hoogleMode ph $ browser cnf
     , locateMode actions
     , locateRegexMode actions
     , calcMode]

locateMode, locateRegexMode :: ExtensionActions -> XPMode
locateMode actions = XPT $ LMode actions
locateRegexMode actions = XPT $ LRMode actions
hoogleMode :: FilePath -> String -> XPMode
hoogleMode pathToHoogleBin browser = XPT $ HMode pathToHoogleBin browser
calcMode :: XPMode
calcMode = XPT CalcMode

-- | This function takes a map of extensions and a path file. It uses the map to find the pattern that matches the file path, then the corresponding program (listed in the map) is spawned.
spawnWithActions :: ExtensionActions -> FilePath -> X()
spawnWithActions actions fp = do
  isDirectoryPath <- liftIO $ doesDirectoryExist fp
  let
    takeExtension = \p -> "." ++ (reverse . takeWhile (/= '.') $ reverse p) --it includes the dot
    -- Patterns defined by the user
    extAction = M.lookup (takeExtension fp) actions
    dirAction = if (isDirectoryPath) then M.lookup "/" actions else Nothing -- / represents a directory
    anyFileAction = M.lookup ".*" actions  -- .* represents any file
    action = fromMaybe (spawnNoPatternMessage (takeExtension fp)) $ extAction `orElse1` dirAction `orElse1` anyFileAction
  action fp
     where
       -- | This function is defined in Data.Generics.Aliases (package syb "Scrap your boilerplate"), defined here to avoid dependency
       orElse1 :: Maybe a -> Maybe a -> Maybe a
       x `orElse1` y = case x of
         Just _  -> x
         Nothing -> y
       spawnNoPatternMessage :: String -> String -> X ()
       spawnNoPatternMessage fileExt _ = spawn $ "xmessage No action specified for file extension " ++ fileExt ++ ", add a default action by matching the extension \".*\" in the action map sent to launcherPrompt"

{- $todo  
     * Switch to mode by name of the prompt, 1. ':' at an empty(?) buffer, 2. autocomplete name in buffer should happen, 3. switch to mode with enter (cancel switch with C-g)
     
     * Support for actions of type String -> X a
     
     * Hoogle mode: add a setting in the action to either go to documentation or to the source code (needs hoogle change?)
     
     * Hoogle mode: add setting to query hoogle at haskell.org instead (with &mode=json)
-}