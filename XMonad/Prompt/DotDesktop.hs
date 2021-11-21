{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module XMonad.Prompt.DotDesktop
    ( appLaunchPrompt
    ) where

import XMonad ( spawn, io, X )
import XMonad.Prompt ( mkXPrompt, XPConfig(searchPredicate) )
import XMonad.Prompt.Shell ( Shell(Shell) )
import XMonad.Prompt.DotDesktopParser ( runDotDesktopParser )

import qualified Data.Map as M
import Control.Monad (filterM)
import Control.Monad.Except
    ( runExceptT, ExceptT (ExceptT), liftEither )
import Control.Exception ( try, Exception )
import Data.Functor ( (<&>) )
import Data.List ( isSuffixOf, dropWhileEnd )
import Data.Maybe ( listToMaybe )
import System.Directory (listDirectory, doesDirectoryExist, getXdgDirectory, XdgDirectory (XdgData), XdgDirectoryList (XdgDataDirs), getXdgDirectoryList)
import System.FilePath ((</>))

import Data.Char (isSpace)
import Data.Either (rights, lefts)
import XMonad.Prelude (join)

isDotDesktop :: FilePath -> Bool
isDotDesktop = isSuffixOf ".desktop"

trimWhitespace :: String -> String
trimWhitespace = dropWhileEnd isSpace . dropWhile isSpace

cmdFilter :: String -> String  -- fixme future do something other than dropping these
cmdFilter ('%':'f':xs) = cmdFilter xs
cmdFilter ('%':'F':xs) = cmdFilter xs
cmdFilter ('%':'u':xs) = cmdFilter xs
cmdFilter ('%':'U':xs) = cmdFilter xs
cmdFilter ('%':'c':xs) = cmdFilter xs
cmdFilter ('%':'k':xs) = cmdFilter xs
cmdFilter ('%':'i':xs) = cmdFilter xs
cmdFilter ('%':'%':xs) = '%' : cmdFilter xs
cmdFilter (x:xs) = x : cmdFilter xs
cmdFilter "" = ""

convertExceptionToString :: Exception e => IO (Either e a) -> IO (Either String a)
convertExceptionToString = fmap convertExceptionToStringHelper

convertExceptionToStringHelper :: Exception e => Either e a -> Either String a
convertExceptionToStringHelper = either (Left . convertExceptionToStringHelperHelper) Right

convertExceptionToStringHelperHelper :: Exception e => e -> String
convertExceptionToStringHelperHelper = show :: Exception e => e -> String

doReadFileLBS :: String -> ExceptT String IO String
doReadFileLBS = ExceptT . convertExceptionToString . try @IOError . readFile

getVal :: String -> String -> M.Map String String -> Either String String
getVal msg k kvmap = maybeToEither msg $ M.lookup k kvmap

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither b Nothing = Left b

doParseFile :: String -> ExceptT String IO DotDesktopApp
doParseFile filePath = do
  content <- doReadFileLBS filePath
  parsed <- liftEither $ runDotDesktopParser content
  let kvMaybe = snd <$> listToMaybe (rights parsed)
  keyVals <- liftEither $
    maybe
      (Left $ "Parse Resulted in no KeyVals in file " ++ filePath)
      Right
      kvMaybe
  let errMsg = "Unable to find Name in file " ++ filePath
  nom <- liftEither $ getVal errMsg "Name" keyVals
  exc <- liftEither $ getVal errMsg "Exec" keyVals
  typ <- liftEither $ getVal errMsg "Type" keyVals
  return DotDesktopApp { fileName = filePath
                       , name = nom
                       , type_ = typ
                       , exec = exc
                       , cmd = (trimWhitespace . cmdFilter) exc
                       }

data DotDesktopApp = DotDesktopApp { fileName :: String
                             , name :: String
                             , type_ :: String
                             , exec :: String
                             , cmd :: String
                             } deriving Show


getAppFolders :: IO [FilePath]
getAppFolders = do
  xdgDataHome <- getXdgDirectory XdgData ""
  xdgDataDirs <- getXdgDirectoryList XdgDataDirs
  let possibleAppDirs = (xdgDataHome : xdgDataDirs) <&> (</> "applications")
  filterM doesDirectoryExist possibleAppDirs

getDirContents :: FilePath -> ExceptT String IO [FilePath]
getDirContents dir = do
  fn <- ExceptT . convertExceptionToString . try @IOError . listDirectory $ dir
  return $ (dir </>) <$> fn

getDotDesktopApps :: IO [DotDesktopApp]
getDotDesktopApps = do
  appFolders <- getAppFolders
  contentsPerFolder <- mapM (runExceptT . getDirContents) appFolders
  let folderFiles = join $ rights contentsPerFolder
      dotDesktopFiles = filter isDotDesktop folderFiles
      folderWarnings = join $ lefts contentsPerFolder
  mapM_ print folderWarnings
  parseResults <- mapM (runExceptT . doParseFile) dotDesktopFiles
  let parseErrs = lefts parseResults
      dotDesktopApps = rights parseResults
  mapM_ print parseErrs
  return dotDesktopApps

appLaunchPrompt :: XPConfig -> X ()
appLaunchPrompt cfg = do
    cmdNameMap <- io $ getDotDesktopApps <&> map (\el -> (name el, cmd el)) <&> M.fromList
    let cmdNameMapKeys = M.keys cmdNameMap
        complFunc :: String -> [String]
        complFunc s = filter (searchPredicate cfg s) cmdNameMapKeys
        --
        complAction :: String -> X ()
        complAction s = do
          spawn $ cmdNameMap M.! s
    mkXPrompt Shell cfg (pure . complFunc) complAction
