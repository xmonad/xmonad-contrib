-----------------------------------
-- |
-- Module      : XMonad.Hooks.WallpaperSetter
-- Description : Change the wallpapers depending on visible workspaces.
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
, defWPNamesJpg, defWPNamesPng, defWPNames
  -- *TODO
  -- $todo
) where
import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

import System.IO
import System.Process
import System.Directory (getHomeDirectory, doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Random (randomRIO)

import qualified Data.Map as M

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
-- > main = xmonad $ def {
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
data WCState = WCState (Maybe [WorkspaceId]) (Maybe ProcessHandle)
instance ExtensionClass WCState where
  initialValue = WCState Nothing Nothing

-- | Represents a wallpaper
data Wallpaper = WallpaperFix FilePath -- ^ Single, fixed wallpaper
               | WallpaperDir FilePath -- ^ Random wallpaper from this subdirectory
               deriving (Eq, Show, Read)

newtype WallpaperList = WallpaperList [(WorkspaceId, Wallpaper)]
  deriving (Show,Read)

instance Semigroup WallpaperList where
  WallpaperList w1 <> WallpaperList w2 =
    WallpaperList $ M.toList $ M.fromList w2 `M.union` M.fromList w1

instance Monoid WallpaperList where
  mempty = WallpaperList []

-- | Complete wallpaper configuration passed to the hook
data WallpaperConf = WallpaperConf {
    wallpaperBaseDir :: FilePath  -- ^ Where the wallpapers reside (if empty, will look in \~\/.wallpapers/)
  , wallpapers :: WallpaperList   -- ^ List of the wallpaper associations for workspaces
  } deriving (Show, Read)

-- | default configuration. looks in \~\/.wallpapers/ for WORKSPACEID.jpg
defWallpaperConf :: WallpaperConf
defWallpaperConf = WallpaperConf "" $ WallpaperList []

instance Default WallpaperConf where
    def = defWallpaperConf

{-# DEPRECATED defWPNames "Use defWPNamesJpg instead" #-}
defWPNames :: [WorkspaceId] -> WallpaperList
defWPNames = defWPNamesJpg

-- | Return the default association list (maps @name@ to @name.jpg@, non-alphanumeric characters are omitted)
defWPNamesJpg :: [WorkspaceId] -> WallpaperList
defWPNamesJpg xs = WallpaperList $ map (\x -> (x, WallpaperFix (filter isAlphaNum x ++ ".jpg"))) xs

-- | Like 'defWPNamesJpg', but map @name@ to @name.png@ instead.
defWPNamesPng :: [WorkspaceId] -> WallpaperList
defWPNamesPng xs = WallpaperList $ map (\x -> (x, WallpaperFix (filter isAlphaNum x ++ ".png"))) xs

-- | Add this to your log hook with the workspace configuration as argument.
wallpaperSetter :: WallpaperConf -> X ()
wallpaperSetter wpconf = do
  WCState oldws h <- XS.get
  visws <- getVisibleWorkspaces
  when (Just visws /= oldws) $ do

    wpconf' <- completeWPConf wpconf
    wspicpaths <- getPicPathsAndWSRects wpconf'

    -- terminate old call if any to prevent unnecessary CPU overload when switching WS too fast
    case h of
      Nothing -> return ()
      Just pid -> liftIO $ terminateProcess pid

    handle <- applyWallpaper wspicpaths
    XS.put $ WCState (Just visws) $ Just handle

-- Helper functions
-------------------

-- | Picks a random element from a list
pickFrom :: [a] -> IO a
pickFrom list = do
  i <- randomRIO (0,length list - 1)
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
  (_, Just outh,_,_pid) <- createProcess $ (proc "identify" ["-format", "%w %h", picpath]) { std_out = CreatePipe }
  output <- hGetContents outh
  return $ case map reads (words output) of
    -- mapM Text.Read.readMaybe is better but only in ghc>=7.6
    [[(w,"")],[(h,"")]] -> Just (w,h)
    _ -> Nothing

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
  return $ map (S.tag . S.workspace) . sortOn S.screen $ S.current winset : S.visible winset

getPicPathsAndWSRects :: WallpaperConf -> X [(Rectangle, FilePath)]
getPicPathsAndWSRects wpconf = do
  winset <- gets windowset
  paths <- liftIO getPicPaths
  visws <- getVisibleWorkspaces
  let visscr = S.current winset : S.visible winset
      visrects = M.fromList $ map (\x -> ((S.tag . S.workspace) x, S.screenDetail x)) visscr
      getRect tag = screenRect $ fromJust $ M.lookup tag visrects
      foundpaths = [ (getRect n, p) | (n, Just p) <- paths, n `elem` visws ]
  return foundpaths
  where getPicPaths = mapM (\(x,y) -> getPicPath wpconf y
                             >>= \p -> return (x,p)) wl
        WallpaperList wl   = wallpapers wpconf

-- | Gets a list of geometry rectangles and filenames, builds and sets wallpaper
applyWallpaper :: [(Rectangle, FilePath)] -> X ProcessHandle
applyWallpaper parts = do
  winset <- gets windowset
  let (vx,vy) = getVScreenDim winset
  layers <- liftIO $ mapM layerCommand parts
  let basepart ="convert -size " ++ show vx ++ "x" ++ show vy ++ " xc:black"
      endpart =" jpg:- | feh --no-xinerama --bg-tile --no-fehbg -"
      cmd = basepart ++ unwords layers ++ endpart
  liftIO $ runCommand cmd


getVScreenDim :: S.StackSet i l a sid ScreenDetail -> (Integer, Integer)
getVScreenDim = foldr (maxXY . screenRect . S.screenDetail) (0,0) . S.screens
  where maxXY (Rectangle x y w h) (mx,my) = ( fromIntegral (fromIntegral x+w) `max` mx
                                            , fromIntegral (fromIntegral y+h) `max` my )

needsRotation :: Rectangle -> (Int,Int) -> Bool
needsRotation rect (px,py) = let wratio, pratio :: Double
                                 wratio = fromIntegral (rect_width rect) / fromIntegral (rect_height rect)
                                 pratio = fromIntegral px / fromIntegral py
                             in wratio > 1 && pratio < 1 || wratio < 1 && pratio > 1

layerCommand :: (Rectangle, FilePath) -> IO String
layerCommand (rect, path) = do
  res <- getPicRes path
  return $ case needsRotation rect <$> res of
    Nothing -> ""
    Just rotate -> let size = show (rect_width rect) ++ "x" ++ show (rect_height rect) in
                     " \\( '"++path++"' "++(if rotate then "-rotate 90 " else "")
                      ++ " -scale "++size++"^ -gravity center -extent "++size++" +gravity \\)"
                      ++ " -geometry +" ++ show (rect_x rect) ++ "+" ++ show (rect_y rect) ++ " -composite "
