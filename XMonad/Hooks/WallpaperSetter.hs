{-# LANGUAGE DeriveDataTypeable #-}
-----------------------------------
-- |
-- Module      : XMonad.Hooks.WallpaperSetter
-- Copyright   : (c) Anton Pirogov, 2014
-- License     : BSD3
--
-- Maintainer  : Anton Pirogov <anton.pirogov@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Log hook which changes the wallpapers depending on visible workspaces.
-----------------------------------
module XMonad.Hooks.WallpaperSetter (
  -- * Usage
  -- $usage
  wallpaperSetter
, WallpaperConf(..)
, Wallpaper(..)
, WallpaperList(..)
, defWallpaperConf
, defWPNames
  -- *TODO
  -- $todo
) where
import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import System.IO
import System.Process
import System.Exit
import System.Directory (getHomeDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Random (getStdRandom, randomR)

import qualified Data.Map as M
import qualified Data.Text as T
import Data.List (intersperse, sortBy)
import Data.Char (isAlphaNum)
import Data.Ord (comparing)

import Control.Monad (when, unless, join)
import Data.Maybe (isNothing, fromJust, fromMaybe)
import Data.Monoid

-- $usage
-- This module requires imagemagick and feh to be installed, as these are utilized
-- for the required image transformations and the actual setting of the wallpaper.
--
-- This was especially tested with multi-head setups - if you have two monitors and swap
-- the workspaces, the wallpapers will be swapped too, scaled accordingly and rotated if necessary
-- (e.g. if you are using your monitor rotated but only have wide wallpapers).
--
-- Add a log hook like this:
--
-- > myWorkspaces = ["1:main","2:misc","3","4"]
-- > ...
-- > main = xmonad $ defaultConfig {
-- >   logHook = wallpaperSetter defWallpaperConf {
-- >                                wallpapers = defWPNames myWorkspaces
-- >                                          <> WallpaperList [("1:main",WallpaperDir "1")]
-- >                             }
-- >   }
-- > ...

-- $todo
-- * implement a kind of image cache like in wallpaperd to remove or at least reduce the lag
--
-- * find out how to merge multiple images from stdin to one (-> for caching all pictures in memory)

-- | internal. to use XMonad state for memory in-between log-hook calls and remember PID of old external call
data WCState = WCState [(String,String)] (Maybe ProcessHandle) deriving Typeable
instance ExtensionClass WCState where
  initialValue = WCState [] Nothing

-- | Represents a wallpaper
data Wallpaper = WallpaperFix FilePath -- ^ Single, fixed wallpaper
               | WallpaperDir FilePath -- ^ Random wallpaper from this subdirectory
               deriving (Eq, Show, Read)

newtype WallpaperList = WallpaperList [(WorkspaceId, Wallpaper)]
  deriving (Show,Read)

instance Monoid WallpaperList where
  mempty = WallpaperList []
  mappend (WallpaperList w1) (WallpaperList w2) =
    WallpaperList $ M.toList $ (M.fromList w2) `M.union` (M.fromList w1)

-- | Complete wallpaper configuration passed to the hook
data WallpaperConf = WallpaperConf {
    wallpaperBaseDir :: FilePath  -- ^ Where the wallpapers reside (if empty, will look in \~\/.wallpapers/)
  , wallpapers :: WallpaperList   -- ^ List of the wallpaper associations for workspaces
  } deriving (Show, Read)

-- | default configuration. looks in \~\/.wallpapers/ for WORKSPACEID.jpg
defWallpaperConf = WallpaperConf "" $ WallpaperList []

-- |returns the default association list (maps name to name.jpg, non-alphanumeric characters are omitted)
defWPNames :: [WorkspaceId] -> WallpaperList
defWPNames xs = WallpaperList $ map (\x -> (x,WallpaperFix (filter isAlphaNum x++".jpg"))) xs

-- | Add this to your log hook with the workspace configuration as argument.
wallpaperSetter :: WallpaperConf -> X ()
wallpaperSetter wpconf = do
  WCState st h <- XS.get
  let oldws = fromMaybe "" $ M.lookup "oldws" $ M.fromList st
  visws <- getVisibleWorkspaces
  when (show visws /= oldws) $ do
    -- debug $ show visws

    wpconf' <- completeWPConf wpconf
    wspicpaths <- getPicPathsAndWSRects wpconf'

    -- terminate old call if any to prevent unnecessary CPU overload when switching WS too fast
    case h of
      Nothing -> return ()
      Just pid -> liftIO $ terminateProcess pid

    handle <- applyWallpaper wspicpaths
    XS.put $ WCState [("oldws", show visws)] $ Just handle

-- Helper functions
-------------------

-- | Picks a random element from a list
pickFrom :: [a] -> IO a
pickFrom list = do
  i <- getStdRandom $ randomR (0,length list - 1)
  return $ list !! i

-- | get absolute picture path of the given wallpaper picture
-- or select a random one if it is a directory
getPicPath :: WallpaperConf -> Wallpaper -> IO (Maybe FilePath)
getPicPath conf (WallpaperDir dir) = do
  direxists <- doesDirectoryExist $ wallpaperBaseDir conf </> dir
  if direxists
    then do files <- getDirectoryContents $ wallpaperBaseDir conf </> dir
            let files' = filter ((/='.').head) files
            file <- pickFrom files'
            return $ Just $ wallpaperBaseDir conf </> dir </> file
    else return Nothing
getPicPath conf (WallpaperFix file) = do
  exist <- doesFileExist path
  return $ if exist then Just path else Nothing
  where path = wallpaperBaseDir conf </> file

-- | Take a path to a picture, return (width, height) if the path is a valid picture
-- (requires imagemagick tool identify to be installed)
getPicRes :: FilePath -> IO (Maybe (Int,Int))
getPicRes picpath = do
  (_, Just outh,_,pid) <- createProcess $ (proc "identify" [picpath]) { std_out = CreatePipe }
  output <- hGetContents outh
  return $ if (length $ words output) < 3 then Nothing else splitRes (words output !! 2)

-- |complete unset fields to default values (wallpaper directory = ~/.wallpapers,
--  expects a file "NAME.jpg" for each workspace named NAME)
completeWPConf :: WallpaperConf -> X WallpaperConf
completeWPConf (WallpaperConf dir (WallpaperList ws)) = do
  home <- liftIO getHomeDirectory
  winset <- gets windowset
  let tags = map S.tag $ S.workspaces winset
      dir' = if null dir then home </> ".wallpapers" else dir
      ws'  = if null ws then defWPNames tags else WallpaperList ws
  return (WallpaperConf dir' ws')

getVisibleWorkspaces :: X [WorkspaceId]
getVisibleWorkspaces = do
  winset <- gets windowset
  return $ map (S.tag . S.workspace) . sortBy (comparing S.screen) $ S.current winset : S.visible winset

getPicPathsAndWSRects :: WallpaperConf -> X [(Rectangle, FilePath)]
getPicPathsAndWSRects wpconf = do
  winset <- gets windowset
  paths <- liftIO $ getPicPaths wpconf
  visws <- getVisibleWorkspaces
  let visscr = S.current winset : S.visible winset
      visrects = M.fromList $ map (\x -> ((S.tag . S.workspace) x, S.screenDetail x)) visscr
      hasPicAndIsVisible (n, mp) = n `elem` visws && (not$isNothing mp)
      getRect tag = screenRect $ fromJust $ M.lookup tag visrects
      foundpaths = map (\(n,Just p)->(getRect n,p)) $ filter hasPicAndIsVisible paths
  return foundpaths
  where getPicPaths wpconf = mapM (\(x,y) -> getPicPath wpconf y
                             >>= \p -> return (x,p)) wl
        WallpaperList wl   = wallpapers wpconf

-- | Gets a list of geometry rectangles and filenames, builds and sets wallpaper
applyWallpaper :: [(Rectangle, FilePath)] -> X ProcessHandle
applyWallpaper parts = do
  winset <- gets windowset
  let (vx,vy) = getVScreenDim winset
  layers <- liftIO $ mapM layerCommand parts
  let basepart ="convert -size "++show vx++"x"++show vy++" xc:black "
      endpart =" jpg:- | feh --no-xinerama --bg-tile --no-fehbg -"
      cmd = basepart ++ (concat $ intersperse " " layers) ++ endpart
  liftIO $ runCommand cmd
  where
  getVScreenDim = foldr maxXY (0,0) . map (screenRect . S.screenDetail) . S.screens
    where maxXY (Rectangle x y w h) (mx,my) = ( fromIntegral ((fromIntegral x)+w) `max` mx
                                              , fromIntegral ((fromIntegral y)+h) `max` my )
  needsRotation (px,py) rect = let wratio = (fromIntegral $ rect_width rect) / (fromIntegral $ rect_height rect)
                                   pratio = fromIntegral px / fromIntegral py
                               in wratio > 1 && pratio < 1 || wratio < 1 && pratio > 1
  layerCommand (rect, path) = do
    res <- getPicRes path
    if isNothing res then return ""
      else do let rotate = needsRotation (fromJust res) rect
              return $ " \\( '"++path++"' "++(if rotate then "-rotate 90 " else "")
                        ++ " -scale "++(show$rect_width rect)++"x"++(show$rect_height rect)++"! \\)"
                        ++ " -geometry +"++(show$rect_x rect)++"+"++(show$rect_y rect)++" -composite "


-- | internal. output string to /tmp/DEBUG
debug str = liftIO $ runCommand $ "echo \"" ++ str ++ "\" >> /tmp/DEBUG"

-- |split a string at a delimeter
split delim str = map T.unpack $ T.splitOn (T.pack delim) (T.pack str)
-- |XxY -> Maybe (X,Y)
splitRes str = ret
  where toks = map (\x -> read x :: Int) $ split "x" str
        ret  = if length toks < 2 then Nothing else Just (toks!!0,toks!!1)

