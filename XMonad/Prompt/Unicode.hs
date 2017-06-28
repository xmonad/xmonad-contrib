{-# LANGUAGE DeriveDataTypeable #-}

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
  UnicodeData,
  UnicodePrompt(..),
  UnicodeState(..),
  getUnicodeDataFromFile,
  unicodeCompletion,
  unicodePrompt,
  unicodePromptFromFile,
  parseUnicodeData,
  readUnicodeDataFile,
  searchUnicode
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Maybe
import Data.Ord
import Numeric
import System.IO
import System.IO.Error
import Control.Arrow
import Data.List

import XMonad
import XMonad.Util.Run
import XMonad.Prompt
import qualified XMonad.Util.ExtensibleState as XS

{- $usage

You can use this module by importing it, along with
"XMonad.Prompt", into your ~\/.xmonad\/xmonad.hs file:

> import XMonad.Prompt
> import XMonad.Prompt.Unicode

and adding an appropriate keybinding, for example:

>  , ((modm .|. controlMask, xK_u), unicodePrompt def)

-}


-- | Type synonym for a list of pairs, containing both the Unicode character
-- itself and its description.
type UnicodeData = [(Char, BS.ByteString)]

-- | Data type which is used to read and write 'UnicodeData' within the 'X'
-- monad using "XMonad.Util.ExtensibleState".
data UnicodeState = UnicodeState { getUnicodeData :: [(Char, BS.ByteString)] }
  deriving Typeable

instance ExtensionClass UnicodeState where
  initialValue = UnicodeState []

-- | Data type which is used to describe the bahavior of the prompt.
data UnicodePrompt = UnicodePrompt

instance XPrompt UnicodePrompt where
    showXPrompt       _ = "Unicode: "
    commandToComplete _ = id
    nextCompletion    _ = getNextCompletion

-- | Prompt for an Unicode character to be inserted into the paste buffer of
-- the X server. It only looks at @\"/usr/share/unicode/UnicodeData.txt\"@ to
-- find the Unicode data as that\'s the default path on many Linux
-- distribution. (On Debian-based distributions you\'ll have to install the
-- @unicode-data@ package.)
--
-- If you want to specify a custom path, use 'unicodePromptFromFile'.
unicodePrompt :: XPConfig -> X ()
unicodePrompt = unicodePromptFromFile "/usr/share/unicode/UnicodeData.txt"

-- | Just like 'unicodePrompt' except that it\'s given a custom file path to
-- the Unicode data.
unicodePromptFromFile :: FilePath -> XPConfig -> X ()
unicodePromptFromFile filepath xpconfig = do
    completion <- fmap unicodeCompletion (getUnicodeDataFromFile filepath)
    mkXPrompt UnicodePrompt xpconfig (return . completion) xsel

-- | Uses the command-line tool @xsel@ to copy the selected Unicode character
-- into the primary selection of X clipboard.
xsel :: String -> X ()
xsel ""    = return ()
xsel (c:_) = do
    runProcessWithInput "xsel" ["-i"] [c]
    return ()

-- | Given the file path to file containing Unicode data, returns the Unicode
-- data the file contains.
--
-- The file will only be read iff it hasn't been read already within this
-- session.
getUnicodeDataFromFile :: FilePath -> X UnicodeData
getUnicodeDataFromFile filepath = do
  UnicodeState unicodeData <- XS.get
  if null unicodeData
    then liftIO (readUnicodeDataFile filepath)
    else return unicodeData

-- | Given all Unicode data and a query, suggests 20 completions.
unicodeCompletion :: UnicodeData -> String -> [String]
unicodeCompletion _           "" = []
unicodeCompletion unicodeData s  =
    map (\(c,d) -> [c] ++ " " ++ d) $
    take 20 $ searchUnicode unicodeData s

-- | Given all Unicode data and a query, returns that data whose description
-- contains the query (as a whole).
searchUnicode :: UnicodeData -> String -> [(Char, String)]
searchUnicode unicodeData s =
    map (second BS.unpack) $
      sortBy (comparing (BS.length . snd)) $
      filter go unicodeData
  where
    go (_,d) = all (`BS.isInfixOf` d) w
    w = map BS.pack $ filter (all isAscii) $
        filter ((> 1) . length) $ words $ map toUpper s

-- | Given the file path to Unicode data, parses the file and returns the
-- contained Unicode data.
readUnicodeDataFile :: FilePath -> IO [(Char, BS.ByteString)]
readUnicodeDataFile filepath = do
    datE <- tryIOError $ BS.readFile filepath
    case datE of
        Left e -> do
            hPutStrLn stderr $ "Could not read file \"" ++ filepath ++ "\""
            hPrint stderr e
            return []
        Right dat ->
          return $ sortBy (comparing (BS.length . snd)) $ parseUnicodeData dat

-- | Given the content of a Unicode data file, parses it into Unicode data.
parseUnicodeData :: BS.ByteString -> UnicodeData
parseUnicodeData = mapMaybe parseLine . BS.lines
  where
    parseLine l = do
        field1 : field2 : _ <- return $ BS.split ';' l
        [(c,"")] <- return $ readHex (BS.unpack field1)
        return (chr c, field2)
