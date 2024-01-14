-----------------------------------------------------------------------------

{- |
Module      :  XMonad.Util.DocumentedKeys
Description :  Document key bindings.
License     :  BSD-style (see LICENSE)

Maintainer  :  Jan Esser <jesser@gmx.de>
Stability   :  stable
Portability :  portable

A convenient way to document the keybinding.
Outputs can be used with `XMonad.Prompt.Man`.

For now ~/.ghcup/share/man is assumed to be in the manpath.
-}
module XMonad.Util.DocumentedKeys (additionalKeysPdoc, docKey, docSpawn) where

import Control.Exception
import Data.Bifunctor
import Data.Time
import XMonad
import XMonad.Util.EZConfig

import System.IO.Unsafe
import System.Process

-- like additionalKeys but for docKey and for docSpawn
additionalKeysPdoc :: XConfig l -> [(String, BindingAction)] -> XConfig l
additionalKeysPdoc conf keyList = unsafePerformIO $ do
    _ <- writeKeysMarkdown keyList $ todayDate
    return $ additionalKeysP conf (map (second toAction) keyList)
infixl 4 `additionalKeysPdoc`

-- example: docKey "layoutScreens 4 Grid" $ layoutScreens 4 Grid
docKey :: String -> X () -> BindingAction
docKey d f = Doc f d

-- example: docSpawn "pcmanfm")
docSpawn :: String -> BindingAction
docSpawn p = docKey p $ spawn p

--

todayDate :: String
todayDate = unsafePerformIO $ formatTime defaultTimeLocale "[%d.%m.%Y]" <$> getCurrentTime

writeKeysMarkdown :: [(String, BindingAction)] -> String -> IO ()
writeKeysMarkdown keyList today = do
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
            ++ today
            ++ "\n---"
    header =
        "| Key | Command |\n\
        \| --- | --------|"
    content = manTitle ++ "\n\n" ++ header ++ "\n" ++ docKeys
    docKeys = concatMap keyStr keyList
    keyStr (k, c) = "|<kbd>" ++ escapedKey k ++ "</kbd>|```" ++ show c ++ "```|\n"
    escapedKey ('<' : xs) = "&lt;" ++ escapedKey xs
    escapedKey ('>' : xs) = "&gt;" ++ escapedKey xs
    escapedKey (x : xs) = x : escapedKey xs
    escapedKey [] = []

data BindingAction = Doc (X ()) String

instance Show BindingAction where
    show (Doc _ m) = m

toAction :: BindingAction -> X ()
toAction (Doc f _) = f

callCommandH :: String -> IO ()
callCommandH cmd = catch (callCommand cmd) handler
  where
    handler = \e -> do
        let err = show (e :: IOException)
        putStrLn $ "Warning: " ++ err
