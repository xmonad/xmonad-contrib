{- |
Module      :  XMonad.Prompt.Unicode
Copyright   :  (c) 2016 Joachim Breitner
License     :  BSD-style (see LICENSE)

Maintainer  :  <mail@joachim-breitner.de>
Stability   :  stable

A prompt for searching unicode characters by name and inserting them into
the clipboard.

Requires the file @\/usr\/share\/unicode\/UnicodeData.txt@ (shipped in the package
@unicode-data@ on Debian) and the @xsel@ tool.
-}

module XMonad.Prompt.Unicode (
 -- * Usage
 -- $usage
 unicodePrompt
 ) where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 hiding (take)
import Data.Char
import Control.Applicative
import System.Environment
import System.IO
import System.IO.Unsafe
import System.IO.Error
import Control.Arrow
import Data.List
import Text.Printf

import XMonad
import XMonad.Util.Run
import XMonad.Prompt hiding (endOfLine)

{- $usage

You can use this module by importing it, along with
"XMonad.Prompt", into your ~\/.xmonad\/xmonad.hs file:

> import XMonad.Prompt
> import XMonad.Prompt.Unicode

and adding an appropriate keybinding, for example:

>  , ((modm .|. controlMask, xK_u), unicodePrompt def)

-}

unicodeDataFilename :: String
unicodeDataFilename =  "/usr/share/unicode/UnicodeData.txt"

entries :: [(Char, BS.ByteString)]
entries = unsafePerformIO $ do
    datE <- tryIOError $ BS.readFile unicodeDataFilename
    case datE of
        Left e -> do
            hPutStrLn stderr $ "Could not read file \"" ++ unicodeDataFilename ++ "\""
            hPutStrLn stderr $ show e
            hPutStrLn stderr $ "Do you have unicode-data installed?"
            return []
        Right dat -> case parseOnly (p <* endOfInput) dat of
            Left e -> do
                hPutStrLn stderr $ "Failed to parse UnicodeData: " ++ show e
                return []
            Right r -> return $ sortOn (BS.length.snd) r
{-# NOINLINE entries #-}

p = many' $ do
    c <- chr <$> hexadecimal
    char ';'
    d <- takeTill (== ';')
    char ';'
    skipWhile (/= '\n')
    endOfLine
    return (c,d)

searchUnicode :: String -> [(Char, String)]
searchUnicode s = map (second BS.unpack) $ filter go entries
  where w = map BS.pack $  filter (all isAscii) $ filter ((> 1) . length) $ words $ map toUpper s
        go (c,d) = all (`BS.isInfixOf` d) w

-- | Prompt the user for a unicode character to be inserted into the paste buffer of the X server.
unicodePrompt :: XPConfig -> X ()
unicodePrompt config = mkXPrompt Unicode config unicodeCompl paste
  where
    unicodeCompl [] = return []
    unicodeCompl s = do
        return $ map (\(c,d) -> printf "%s %s" [c] d) $ take 20 $ searchUnicode s

    paste [] = return ()
    paste (c:_) = do
        runProcessWithInput "xsel" ["-i"] [c]
        return ()

data Unicode = Unicode
instance XPrompt Unicode where
    showXPrompt Unicode = "Unicode: "
    commandToComplete Unicode s = s
    nextCompletion Unicode = getNextCompletion

