{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
module XMonad.Prompt.DotDesktop
    ( appLaunchPrompt
    ) where

import XMonad ( spawn, io, X )
import XMonad.Prompt ( mkXPrompt, XPConfig(searchPredicate) )
import XMonad.Prompt.Shell ( Shell(Shell) )
import XMonad.Prompt.DotDesktopParser ( doParseContent
                                      , DotDesktopApp (..) )

import qualified Data.Map as M
import Control.Monad ( filterM )
import Control.Exception ( try, Exception )
import Data.Functor ( (<&>) )
import Data.List ( isSuffixOf )
import System.Directory ( listDirectory
                        , doesDirectoryExist
                        , getXdgDirectory
                        , XdgDirectory (XdgData)
                        , XdgDirectoryList (XdgDataDirs)
                        , getXdgDirectoryList)
import System.FilePath ( (</>) )

import Data.Either ( rights, lefts )
import XMonad.Prelude ( join )

isDotDesktop :: FilePath -> Bool
isDotDesktop = isSuffixOf ".desktop"

exceptionToString :: Exception e => Either e a -> Either String a
exceptionToString = either (Left . show) Right

doReadFileLBS :: String -> IO (Either String String)
doReadFileLBS = fmap exceptionToString . try @IOError . readFile

doParseFile :: String -> IO (Either String DotDesktopApp)
doParseFile filePath = doReadFileLBS filePath
                   <&> (>>= doParseContent filePath)

getAppFolders :: IO [FilePath]
getAppFolders = do
  xdgDataHome <- getXdgDirectory XdgData ""
  xdgDataDirs <- getXdgDirectoryList XdgDataDirs
  let possibleAppDirs = (xdgDataHome : xdgDataDirs) <&> (</> "applications")
  filterM doesDirectoryExist possibleAppDirs

getDirContents :: FilePath -> IO (Either String [FilePath])
getDirContents dir = do
  fn <- fmap exceptionToString . try @IOError . listDirectory $ dir
  return $ (fmap . fmap) (dir </>) fn

getDotDesktopApps :: IO [DotDesktopApp]
getDotDesktopApps = do
  appFolders <- getAppFolders
  contentsPerFolder <- mapM getDirContents appFolders
  let folderFiles = join $ rights contentsPerFolder
      dotDesktopFiles = filter isDotDesktop folderFiles
      folderWarnings = join $ lefts contentsPerFolder
  mapM_ print folderWarnings
  parseResults <- mapM doParseFile dotDesktopFiles
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
        complAction s = spawn $ cmdNameMap M.! s
    mkXPrompt Shell cfg (pure . complFunc) complAction
