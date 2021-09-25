{- |
Module      :  XMonad.Prompt.Zsh
Description :  Zsh-specific version of "XMonad.Prompt.Shell".
Copyright   :  (C) 2020 Zubin Duggal
License     :  BSD3

Maintainer  :  zubin.duggal@gmail.com
Stability   :  unstable
Portability :  unportable

A version of "XMonad.Prompt.Shell" that lets you access the awesome power of Zsh
completions in your xmonad prompt
-}

module XMonad.Prompt.Zsh
    ( -- * Usage
      -- $usage
      Zsh (..)
    , zshPrompt
    -- * Utility functions
    , getZshCompl
    , stripZsh
    ) where

import XMonad
import XMonad.Prompt
import XMonad.Util.Run

{- $usage
1. Grab the @capture.zsh@ script to capture zsh completions from <https://github.com/Valodim/zsh-capture-completion>
2. In your @~\/.xmonad\/xmonad.hs@:

> import XMonad.Prompt
> import XMonad.Prompt.Zsh

3. In your keybindings add something like:

>   , ((modm .|. controlMask, xK_x), zshPrompt def "/path/to/capture.zsh")

For detailed instruction on editing the key binding see
"XMonad.Doc.Extending#Editing_key_bindings". -}

data Zsh = Zsh

instance XPrompt Zsh where
    showXPrompt Zsh       = "Run: "
    completionToCommand _ = stripZsh
    commandToComplete _ s = s
    nextCompletion _ s cs = getNextCompletion s (map stripZsh cs)

zshPrompt :: XPConfig -> FilePath -> X ()
zshPrompt c capture = mkXPrompt Zsh c (getZshCompl capture) (\x -> safeSpawn "zsh" ["-c",x])

getZshCompl :: FilePath -> String -> IO [String]
getZshCompl capture s
  | s == ""   = return []
  | otherwise = processCompls <$> runProcessWithInput capture [s] ""
    where processCompls = map (\x -> skipLastWord s ++ filter (/= '\r') x) . lines

-- | Removes the argument description from the zsh completion
stripZsh :: String -> String
stripZsh "" = ""
stripZsh (' ':'-':'-':' ':_) = ""
stripZsh (x:xs) = x : stripZsh xs
