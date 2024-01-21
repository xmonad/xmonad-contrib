-----------------------------------------------------------------------------

{- |
Module      :  XMonad.Util.DocumentedKeys
Description :  Document key bindings.
License     :  BSD-style (see LICENSE)

Maintainer  :  Jan Esser <jesser@gmx.de>
Stability   :  unstable
Portability :  portable

Allows writing X.U.NamedActions to a markdown/man file.

FIXME For now ~/.ghcup/share/man is assumed to be in the manpath.
-}
module XMonad.Util.DocumentedKeys (writeKeysMarkdown) where

import XMonad.Util.NamedActions

import Control.Exception
import Data.Time
import System.IO.Unsafe
import System.Process
import XMonad

writeKeysMarkdown :: [((KeyMask, KeySym), NamedAction)] -> IO ()
writeKeysMarkdown keyList = do
    callCommandH $ "mkdir -p " ++ workDir
    callCommandH $ "mkdir -p " ++ manDir
    writeFile mdFile content
    callCommandH $ "pandoc " ++ mdFile ++ " -s -t man -o " ++ manFile
    callCommandH "mandb"
    callCommandH $ "rm -R " ++ workDir
  where
    workDir = "/tmp/xmonad-mykeys/"
    mdFile = workDir ++ "xmonad-mykeys.md"
    manDir = "~/.ghcup/share/man/man7/"
    manFile = manDir ++ "xmonad-mykeys.7"
    manTitle =
        "---\n"
            ++ "title: xmonad-mykeys\n"
            ++ "section: 7\n"
            ++ "header: xmonad keybindings manual\n"
            ++ "date: "
            ++ todayDate
            ++ "\n---"
    header =
        "| Key | Command |\n\
        \| --- | --------|"
    content = manTitle ++ "\n\n" ++ header ++ "\n"
        ++ unlines (showKm keyList) -- TODO fit in markdown syntax

--

todayDate :: String
{-# NOINLINE todayDate #-}
todayDate = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y]" <$> getCurrentTime

callCommandH :: String -> IO ()
callCommandH cmd = catch (callCommand cmd) handler
  where
    handler e = do
        let err = show (e :: IOException)
        putStrLn $ "Warning: " ++ err
