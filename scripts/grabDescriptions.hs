#!/usr/bin/env runhaskell

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import System.Directory
import System.Environment

-- needs haskell-src-exts
import qualified Language.Haskell.Exts.Annotated as H

getComments = (fmap . fmap) (map (\(H.Comment _ _ x) -> x) . snd)
            . H.parseFileWithComments H.defaultParseMode

-- | Used to grab the description fields from all the modules in the current
-- directory for updating XMonad.Docs.Extending
main = putStrLn . intercalate "\n"
    =<< mapM (fmap . handleFailure description <*> getComments) =<< filterM doesFileExist . sort
    =<< getDirectoryContents . fromMaybe "." . listToMaybe
    =<< getArgs -- somehow only the "." fallback works...

handleFailure :: (String -> [String] -> String) -> String -> H.ParseResult [String] -> String
handleFailure f n (H.ParseOk x) = f n x
handleFailure f n (H.ParseFailed _ msg) = n ++ " Parse Failure: " ++ msg

description :: String -> [String] -> String
description path xs =
    let (hs,desc)
            = uncurry (\x (y,descr) -> (x++y,takeWhile (not . or . sequence [null,("* Usage" `isInfixOf`),all (=='-'),all isSpace]) . dropWhile (all isSpace) $ descr))
            . second (splitAt 1)
            . break (isPrefixOf "Portability")
            . map (dropWhile isSpace) $ concatMap lines xs
        modName = maybe path (takeWhile (not . isSpace) . dropWhile isSpace . drop 1 . dropWhile (/=':')) $ find ("Module" `isInfixOf`) hs
    in "* \""++modName++"\":\n"++unlines (map ("    "++) desc)
