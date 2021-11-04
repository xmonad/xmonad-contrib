{- |
Module      :  XMonad.Actions.Launcher
Description :  A set of prompts for XMonad.
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
  , launcherPrompt
) where

import qualified Data.Map        as M
import           XMonad          hiding (config)
import           XMonad.Prelude
import           XMonad.Prompt
import           XMonad.Util.Run

{- $description
    This module exemplifies usage of `XMonad.Prompt.mkXPromptWithModes`. It includes two modes:

       * Hoogle mode: Search for functions using hoogle, choosing a function leads you to documentation in Haddock.

       * Calc: Uses the program calc to do calculations.

    To test it, modify your local .xmonad:

    > import XMonad.Prompt(def)
    > import XMonad.Actions.Launcher

    > ((modm .|. controlMask, xK_l), launcherPrompt def $ defaultLauncherModes launcherConfig)

    A LauncherConfig contains settings for the default modes, modify them accordingly.

    > launcherConfig = LauncherConfig { pathToHoogle = "/home/YOU/.cabal/bin/hoogle" , browser = "firefox"}

Restart xmonad. Press Ctrl + Your_Modkey + L and the first prompt should pop up.

 If you used the default 'XPConfig', you can change mode with 'xK_grave'. If you are using your own 'XPConfig', define the value for 'changeModeKey'.
 -}

data HoogleMode = HMode FilePath String --path to hoogle and browser
data CalculatorMode = CalcMode

data LauncherConfig = LauncherConfig {
  browser        :: String
  , pathToHoogle :: String
}

type ExtensionActions = M.Map String (String -> X())

-- | Uses the command `calc` to compute arithmetic expressions
instance XPrompt CalculatorMode where
  showXPrompt CalcMode = "calc %s> "
  commandToComplete CalcMode = id --send the whole string to `calc`
  completionFunction CalcMode s = if null s then return [] else
    lines <$> runProcessWithInput "calc" [s] ""
  modeAction CalcMode _ _ = return () -- do nothing; this might copy the result to the clipboard

-- | Uses the program `hoogle` to search for functions
instance XPrompt HoogleMode where
  showXPrompt _ = "hoogle %s> "
  commandToComplete _ = id
  completionFunction (HMode pathToHoogleBin' _) s = completionFunctionWith pathToHoogleBin' ["--count","8",s]
  -- This action calls hoogle again to find the URL corresponding to the autocompleted item
  modeAction (HMode pathToHoogleBin'' browser') query result = do
    completionsWithLink <- liftIO $ completionFunctionWith pathToHoogleBin'' ["--count","5","--link",query]
    let link = do
          s <- find (isJust . \complStr -> findSeqIndex complStr result) completionsWithLink
          i <- findSeqIndex s "http://"
          return $ drop i s
    case link of
       Just l -> spawn $ browser' ++ " " ++ l
       _      -> return ()
    where
      -- | Receives a sublist and a list. It returns the index where the sublist appears in the list.
      findSeqIndex :: (Eq a) => [a] -> [a] -> Maybe Int
      findSeqIndex xs xss = findIndex (isPrefixOf xss) $ tails xs

-- | Creates an autocompletion function for a programm given the program's name and a list of args to send to the command.
completionFunctionWith :: String -> [String] -> IO [String]
completionFunctionWith cmd args = lines <$> runProcessWithInput cmd args ""

-- | Creates a prompt with the given modes
launcherPrompt :: XPConfig -> [XPMode] -> X()
launcherPrompt config modes = mkXPromptWithModes modes config

-- | Create a list of modes based on :
-- a list of extensions mapped to actions
-- the path to hoogle
defaultLauncherModes :: LauncherConfig -> [XPMode]
defaultLauncherModes cnf = let
  ph           = pathToHoogle cnf
  in [ hoogleMode ph $ browser cnf
     , calcMode]

hoogleMode :: FilePath -> String -> XPMode
hoogleMode pathToHoogleBin browser' = XPT $ HMode pathToHoogleBin browser'
calcMode :: XPMode
calcMode = XPT CalcMode

{-

  -- ideas for XMonad.Prompt running on mode XPMultipleModes
     * Switch to mode by name of the prompt, 1. ':' at an empty(?) buffer, 2. autocomplete name in buffer should happen, 3. switch to mode with enter (cancel switch with C-g)

     * Support for actions of type String -> X a

  -- ideas for this module

     * Hoogle mode: add a setting in the action to either go to documentation or to the source code (needs hoogle change?)

     * Hoogle mode: add setting to query hoogle at haskell.org instead (with &mode=json)
-}
