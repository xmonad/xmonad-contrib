{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TreeSelect
-- Copyright   :  (c) Tom Smeets <tom.tsmeets@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Tom Smeets <tom.tsmeets@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
--
-- TreeSelect displays your workspaces or actions in a Tree-like format.
-- You can select the desired workspace/action with the cursor or hjkl keys.
--
-- This module is fully configurable and very useful if you like to have a
-- lot of workspaces.
--
-- Only the nodes up to the currently selected are displayed.
-- This will be configurable in the near future by changing 'ts_hidechildren' to @False@, this is not yet implemented.
--
-- <<https://wiki.haskell.org/wikiupload/thumb/0/0b/Treeselect-Workspace.png/800px-Treeselect-Workspace.png>>
--
-----------------------------------------------------------------------------
module XMonad.Actions.TreeSelect
    (
      -- * Usage
      -- $usage
      treeselectWorkspace
    , toWorkspaces
    , treeselectAction

      -- * Configuring
      -- $config
    , Pixel
      -- $pixel

    , TSConfig(..)
    , tsDefaultConfig

      -- * Navigation
      -- $navigation
    , defaultNavigation
    , select
    , cancel
    , moveParent
    , moveChild
    , moveNext
    , movePrev
    , moveHistBack
    , moveHistForward
    , moveTo

      -- * Advanced usage
      -- $advusage
    , TSNode(..)
    , treeselect
    , treeselectAt
    ) where

import Control.Monad.Reader
import Control.Monad.State
import Data.List (find)
import Data.Maybe
import Data.Tree
import Foreign
import System.IO
import System.Posix.Process (forkProcess, executeFile)
import XMonad hiding (liftX)
import XMonad.StackSet as W
import XMonad.Util.Font
import XMonad.Util.NamedWindows
import XMonad.Util.TreeZipper
import XMonad.Hooks.WorkspaceHistory
import qualified Data.Map as M

#ifdef XFT
import Graphics.X11.Xft
import Graphics.X11.Xrender
#endif

-- $usage
--
-- These imports are used in the following example
--
-- > import Data.Tree
-- > import XMonad.Actions.TreeSelect
-- > import XMonad.Hooks.WorkspaceHistory
-- > import qualified XMonad.StackSet as W
--
-- For selecting Workspaces, you need to define them in a tree structure using 'Data.Tree.Node' instead of just a standard list
--
-- Here is an example workspace-tree
--
-- > myWorkspaces :: Forest String
-- > myWorkspaces = [ Node "Browser" [] -- a workspace for your browser
-- >                , Node "Home"       -- for everyday activity's
-- >                    [ Node "1" []   --  with 4 extra sub-workspaces, for even more activity's
-- >                    , Node "2" []
-- >                    , Node "3" []
-- >                    , Node "4" []
-- >                    ]
-- >                , Node "Programming" -- for all your programming needs
-- >                    [ Node "Haskell" []
-- >                    , Node "Docs"    [] -- documentation
-- >                    ]
-- >                ]
--
-- Then add it to your 'XMonad.Core.workspaces' using the 'toWorkspaces' function.
--
-- Optionally, if you add 'workspaceHistoryHook' to your 'logHook' you can use the \'o\' and \'i\' keys to select from previously-visited workspaces
--
-- > xmonad $ defaultConfig { ...
-- >                        , workspaces = toWorkspaces myWorkspaces
-- >                        , logHook = workspaceHistoryHook
-- >                        }
--
-- After that you still need to bind buttons to 'treeselectWorkspace' to start selecting a workspaces and moving windows
--
-- you could bind @Mod-f@ to switch workspace
--
-- >  , ((modMask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.greedyView)
--
-- and bind @Mod-Shift-f@ to moving the focused windows to a workspace
--
-- >  , ((modMask .|. shiftMask, xK_f), treeselectWorkspace myTreeConf myWorkspaces W.shift)

-- $config
-- The selection menu is very configurable, you can change the font, all colors and the sizes of the boxes.
--
-- The default config defined as 'tsDefaultConfig'
--
-- > tsDefaultConfig = TSConfig { ts_hidechildren = True
-- >                            , ts_background   = 0xc0c0c0c0
-- >                            , ts_font         = "xft:Sans-16"
-- >                            , ts_node         = (0xff000000, 0xff50d0db)
-- >                            , ts_nodealt      = (0xff000000, 0xff10b8d6)
-- >                            , ts_highlight    = (0xffffffff, 0xffff0000)
-- >                            , ts_extra        = 0xff000000
-- >                            , ts_node_width   = 200
-- >                            , ts_node_height  = 30
-- >                            , ts_originX      = 0
-- >                            , ts_originY      = 0
-- >                            , ts_indent       = 80
-- >                            , ts_navigate     = defaultNavigation
-- >                            }

-- $pixel
--
-- The 'Pixel' Color format is in the form of @0xaarrggbb@
--
-- Note that transparency is only supported if you have a window compositor running like <https://github.com/chjj/compton compton>
--
-- Some Examples:
--
-- @
-- white       = 0xffffffff
-- black       = 0xff000000
-- red         = 0xffff0000
-- blue        = 0xff00ff00
-- green       = 0xff0000ff
-- transparent = 0x00000000
-- @

-- $navigation
--
-- Keybindings for navigations can also be modified
--
-- This is the definition of 'defaultNavigation'
--
-- > defaultNavigation :: M.Map (KeyMask, KeySym) (TreeSelect a (Maybe a))
-- > defaultNavigation = M.fromList
-- >     [ ((0, xK_Escape), cancel)
-- >     , ((0, xK_Return), select)
-- >     , ((0, xK_space),  select)
-- >     , ((0, xK_Up),     movePrev)
-- >     , ((0, xK_Down),   moveNext)
-- >     , ((0, xK_Left),   moveParent)
-- >     , ((0, xK_Right),  moveChild)
-- >     , ((0, xK_k),      movePrev)
-- >     , ((0, xK_j),      moveNext)
-- >     , ((0, xK_h),      moveParent)
-- >     , ((0, xK_l),      moveChild)
-- >     , ((0, xK_o),      moveHistBack)
-- >     , ((0, xK_i),      moveHistForward)
-- >     ]

-- $advusage
-- This module can also be used to select any other action

-- | Extensive configuration for displaying the tree.
--
-- This class also has a 'Default' instance
data TSConfig a = TSConfig { ts_hidechildren :: Bool -- ^ when enabled, only the parents (and their first children) of the current node will be shown (This feature is not yet implemented!)
                           , ts_background :: Pixel -- ^ background color filling the entire screen.

                           , ts_font :: String -- ^ XMF font for drawing the node name extra text

                           , ts_node      :: (Pixel, Pixel) -- ^ node foreground (text) and background color when not selected
                           , ts_nodealt   :: (Pixel, Pixel) -- ^ every other node will use this color instead of 'ts_node'
                           , ts_highlight :: (Pixel, Pixel) -- ^ node foreground (text) and background color when selected

                           , ts_extra :: Pixel -- ^ extra text color

                           , ts_node_width   :: Int -- ^ node width in pixels
                           , ts_node_height  :: Int -- ^ node height in pixels
                           , ts_originX :: Int -- ^ tree X position on the screen in pixels
                           , ts_originY :: Int -- ^ tree Y position on the screen in pixels

                           , ts_indent :: Int -- ^ indentation amount for each level in pixels

                           , ts_navigate :: M.Map (KeyMask, KeySym) (TreeSelect a (Maybe a)) -- ^ key bindings for navigating the tree
                           }

instance Default (TSConfig a) where
    def = TSConfig { ts_hidechildren = True
                   , ts_background   = 0xc0c0c0c0
                   , ts_font         = "xft:Sans-16"
                   , ts_node         = (0xff000000, 0xff50d0db)
                   , ts_nodealt      = (0xff000000, 0xff10b8d6)
                   , ts_highlight    = (0xffffffff, 0xffff0000)
                   , ts_extra        = 0xff000000
                   , ts_node_width   = 200
                   , ts_node_height  = 30
                   , ts_originX      = 0
                   , ts_originY      = 0
                   , ts_indent       = 80
                   , ts_navigate     = defaultNavigation
                   }

-- | Default navigation
--
-- * navigation using either arrow key or vi style hjkl
-- * Return or Space to confirm
-- * Escape or Backspace to cancel to
defaultNavigation :: M.Map (KeyMask, KeySym) (TreeSelect a (Maybe a))
defaultNavigation = M.fromList
    [ ((0, xK_Escape), cancel)
    , ((0, xK_Return), select)
    , ((0, xK_space),  select)
    , ((0, xK_Up),     movePrev)
    , ((0, xK_Down),   moveNext)
    , ((0, xK_Left),   moveParent)
    , ((0, xK_Right),  moveChild)
    , ((0, xK_k),      movePrev)
    , ((0, xK_j),      moveNext)
    , ((0, xK_h),      moveParent)
    , ((0, xK_l),      moveChild)
    , ((0, xK_o),      moveHistBack)
    , ((0, xK_i),      moveHistForward)
    ]

-- | Default configuration.
--
-- Using nice alternating blue nodes
tsDefaultConfig :: TSConfig a
tsDefaultConfig = def

-- | Tree Node With a name and extra text
data TSNode a = TSNode { tsn_name  :: String
                       , tsn_extra :: String -- ^ extra text, displayed next to the node name
                       , tsn_value :: a      -- ^ value to return when this node is selected
                       }

-- | State used by TreeSelect.
--
-- Contains all needed information such as the window, font and a zipper over the tree.
data TSState a = TSState { tss_tree     :: TreeZipper (TSNode a)
                         , tss_window   :: Window
                         , tss_display  :: Display
                         , tss_size     :: (Int, Int) -- ^ size of 'tz_window'
                         , tss_xfont    :: XMonadFont
                         , tss_gc       :: GC
                         , tss_visual   :: Visual
                         , tss_colormap :: Colormap
                         , tss_history  :: ([[String]], [[String]]) -- ^ history zipper, navigated with 'moveHistBack' and 'moveHistForward'
                         }

-- | State monad transformer using 'TSState'
newtype TreeSelect a b = TreeSelect { runTreeSelect :: ReaderT (TSConfig a) (StateT (TSState a) X) b }
    deriving (Monad, Applicative, Functor, MonadState (TSState a),  MonadReader (TSConfig a), MonadIO)

-- | Lift the 'X' action into the 'XMonad.Actions.TreeSelect.TreeSelect' monad
liftX :: X a -> TreeSelect b a
liftX = TreeSelect . lift . lift

-- | Run Treeselect with a given config and tree.
-- This can be used for selectiong anything
--
-- * for switching workspaces and moving windows use 'treeselectWorkspace'
-- * for selecting actions use 'treeselectAction'
treeselect :: TSConfig a         -- ^ config file
           -> Forest (TSNode a)  -- ^ a list of 'Data.Tree.Tree's to select from.
           -> X (Maybe a)
treeselect c t = treeselectAt c (fromForest t) []

-- | Same as 'treeselect' but ad a specific starting position
treeselectAt :: TSConfig a         -- ^ config file
             -> TreeZipper (TSNode a)  -- ^ tree structure with a cursor position (starting node)
             -> [[String]] -- ^ list of paths that can be navigated with 'moveHistBack' and 'moveHistForward' (bound to the 'o' and 'i' keys)
             -> X (Maybe a)
treeselectAt conf@TSConfig{..} zipper hist = withDisplay $ \display -> do
    -- create a window on the currently focused screen
    rootw <- asks theRoot
    Rectangle{..} <- gets $ screenRect . W.screenDetail . W.current . windowset

    Just vinfo <- liftIO $ matchVisualInfo display (defaultScreen display) 32 4

    colormap <- liftIO $ createColormap display rootw (visualInfo_visual vinfo) allocNone

    win <- liftIO $ allocaSetWindowAttributes $ \attributes -> do
        set_override_redirect attributes True
        set_colormap attributes colormap
        set_background_pixel attributes ts_background
        set_border_pixel attributes 0
        createWindow display rootw rect_x rect_y rect_width rect_height 0 (visualInfo_depth vinfo) inputOutput (visualInfo_visual vinfo) (cWColormap .|. cWBorderPixel .|. cWBackPixel) attributes

    liftIO $ do
        -- TODO: move below?
        -- make the window visible
        mapWindow display win

        -- listen to key and mouse button events
        selectInput display win (exposureMask .|. keyPressMask .|. buttonReleaseMask)

        -- TODO: enable mouse select?
        -- and mouse button 1
        grabButton display button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none

    -- grab the keyboard
    status <- liftIO $ grabKeyboard display win True grabModeAsync grabModeAsync currentTime

    r <- if status == grabSuccess
        then do
            -- load the XMF font
            gc <- liftIO $ createGC display win
            xfont <- initXMF ts_font

            -- run the treeselect Monad
            ret <- evalStateT (runReaderT (runTreeSelect (redraw >> navigate)) conf)
                TSState{ tss_tree     = zipper
                       , tss_window   = win
                       , tss_display  = display
                       , tss_xfont    = xfont
                       , tss_size     = (fromIntegral rect_width, fromIntegral rect_height)
                       , tss_gc       = gc
                       , tss_visual   = visualInfo_visual vinfo
                       , tss_colormap = colormap
                       , tss_history = ([], hist)
                       }

            -- release the XMF font
            releaseXMF xfont
            liftIO $ freeGC display gc
            return ret

        else return Nothing

    -- destroy the window
    liftIO $ do
        unmapWindow display win
        destroyWindow display win
        freeColormap display colormap
        -- Flush the output buffer and wait for all the events to be processed
        -- TODO: is this needed?
        sync display False
    return r

-- | Select a workspace and execute a \"view\" function from "XMonad.StackSet" on it.
treeselectWorkspace :: TSConfig WorkspaceId
                    -> Forest String -- ^ your tree of workspace-names
                    -> (WorkspaceId -> WindowSet -> WindowSet) -- ^ the \"view\" function.
                                                               -- Instances can be 'W.greedyView' for switching to a workspace
                                                               -- and/or 'W.shift' for moving the focused window to a selected workspace.
                                                               --
                                                               -- These actions can also be combined by doing
                                                               --
                                                               -- > \i -> W.greedyView i . W.shift i
                    -> X ()
treeselectWorkspace c xs f = do
    -- get all defined workspaces
    -- They have to be set with 'toWorkspaces'!
    ws <- gets (W.workspaces . windowset)

    -- check the 'XConfig.workspaces'
    if all (`elem` map tag ws) (toWorkspaces xs)
      then do
        -- convert the 'Forest WorkspaceId' to 'Forest (TSNode WorkspaceId)'
        wsf <- forMForest (mkPaths xs) $ \(n, i) -> maybe (return (TSNode n "Does not exist!" "")) (mkNode n) (find (\w -> i == tag w) ws)

        -- get the current workspace path
        me <- gets (W.tag . W.workspace . W.current . windowset)
        hist <- workspaceHistory
        treeselectAt c (fromJust $ followPath tsn_name (splitPath me) $ fromForest wsf) (map splitPath hist) >>= maybe (return ()) (windows . f)

      else liftIO $ do
        -- error!
        let msg = unlines $ [ "Please add:"
                            , "    workspaces = toWorkspaces myWorkspaces"
                            , "to your XMonad config!"
                            , ""
                            , "XConfig.workspaces: "
                            ] ++ map tag ws
        hPutStrLn stderr msg
        _ <- forkProcess $ executeFile "xmessage" True [msg] Nothing
        return ()
  where
    mkNode n w = do
        -- find the focused window's name on this workspace
        name <- maybe (return "") (fmap show . getName . W.focus) $ stack w
        return $ TSNode n name (tag w)

-- | Convert the workspace-tree to a flat list of paths such that XMonad can use them
--
-- The Nodes will be separated by a dot (\'.\') character
toWorkspaces :: Forest String -> [WorkspaceId]
toWorkspaces = map snd . concatMap flatten . mkPaths

mkPaths :: Forest String -> Forest (String, WorkspaceId)
mkPaths = map (\(Node n ns) -> Node (n, n) (map (f n) ns))
  where
    f pth (Node x xs) = let pth' = pth ++ '.' : x
                         in Node (x, pth') (map (f pth') xs)

splitPath :: WorkspaceId -> [String]
splitPath i = case break (== '.') i of
    (x,   []) -> [x]
    (x, _:xs) -> x : splitPath xs

-- | Select from a Tree of 'X' actions
--
-- <<https://wiki.haskell.org/wikiupload/thumb/9/9b/Treeselect-Action.png/800px-Treeselect-Action.png>>
--
-- Each of these actions have to be specified inside a 'TSNode'
--
-- Example
--
-- > treeselectAction myTreeConf
-- >    [ Node (TSNode "Hello"    "displays hello"      (spawn "xmessage hello!")) []
-- >    , Node (TSNode "Shutdown" "Poweroff the system" (spawn "shutdown")) []
-- >    , Node (TSNode "Brightness" "Sets screen brightness using xbacklight" (return ()))
-- >        [ Node (TSNode "Bright" "FULL POWER!!"            (spawn "xbacklight -set 100")) []
-- >        , Node (TSNode "Normal" "Normal Brightness (50%)" (spawn "xbacklight -set 50"))  []
-- >        , Node (TSNode "Dim"    "Quite dark"              (spawn "xbacklight -set 10"))  []
-- >        ]
-- >    ]
treeselectAction :: TSConfig (X a) -> Forest (TSNode (X a)) -> X ()
treeselectAction c xs = treeselect c xs >>= \x -> case x of
    Just a  -> a >> return ()
    Nothing -> return ()

forMForest :: (Functor m, Applicative m, Monad m) => [Tree a] -> (a -> m b) -> m [Tree b]
forMForest x g = mapM (mapMTree g) x

mapMTree :: (Functor m, Applicative m, Monad m) => (a -> m b) -> Tree a -> m (Tree b)
mapMTree f (Node x xs) = Node <$> f x <*>  mapM (mapMTree f) xs


-- | Quit returning the currently selected node
select :: TreeSelect a (Maybe a)
select = Just <$> gets (tsn_value . cursor . tss_tree)

-- | Quit without returning anything
cancel :: TreeSelect a (Maybe a)
cancel = return Nothing

-- TODO: redraw only what is necessary.
-- Examples: redrawAboveCursor, redrawBelowCursor and redrawCursor

-- | Move the cursor to its parent node
moveParent :: TreeSelect a (Maybe a)
moveParent = moveWith parent >> redraw >> navigate

-- | Move the cursor one level down, highlighting its first child-node
moveChild :: TreeSelect a (Maybe a)
moveChild = moveWith children >> redraw >> navigate

-- | Move the cursor to the next child-node
moveNext :: TreeSelect a (Maybe a)
moveNext = moveWith nextChild >> redraw >> navigate

-- | Move the cursor to the previous child-node
movePrev :: TreeSelect a (Maybe a)
movePrev = moveWith previousChild >> redraw >> navigate

-- | Move backwards in history
moveHistBack :: TreeSelect a (Maybe a)
moveHistBack = do
    s <- get
    case tss_history s of
        (xs, a:y:ys) -> do
            put s{tss_history = (a:xs, y:ys)}
            moveTo y
        _ -> navigate

-- | Move forward in history
moveHistForward :: TreeSelect a (Maybe a)
moveHistForward = do
    s <- get
    case tss_history s of
        (x:xs, ys) -> do
            put s{tss_history = (xs, x:ys)}
            moveTo x
        _ -> navigate

-- | Move to a specific node
moveTo :: [String] -- ^ path, always starting from the top
       -> TreeSelect a (Maybe a)
moveTo i = moveWith (followPath tsn_name i . rootNode) >> redraw >> navigate

-- | Apply a transformation on the internal 'XMonad.Util.TreeZipper.TreeZipper'.
moveWith :: (TreeZipper (TSNode a) -> Maybe (TreeZipper (TSNode a))) -> TreeSelect a ()
moveWith f = do
    s <- get
    case f (tss_tree s) of
        -- TODO: redraw cursor only?
        Just t -> put s{ tss_tree = t }
        Nothing -> return ()

-- | wait for keys and run navigation
navigate :: TreeSelect a (Maybe a)
navigate = gets tss_display >>= \d -> join . liftIO . allocaXEvent $ \e -> do
    maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e

    ev <- getEvent e

    if ev_event_type ev == keyPress
      then do
        (ks, _) <- lookupString $ asKeyEvent e
        return $ do
            mask <- liftX $ cleanMask (ev_state ev)
            f <- asks ts_navigate
            fromMaybe navigate $ M.lookup (mask, fromMaybe xK_VoidSymbol ks) f
      else return navigate

-- | Request a full redraw
redraw :: TreeSelect a ()
redraw = do
    win <- gets tss_window
    dpy <- gets tss_display

    -- clear window
    -- TODO: not always needed!
    liftIO $ clearWindow dpy win

    t <- gets tss_tree
    _ <- drawLayers 0 0 (reverse $ (tz_before t, cursor t, tz_after t) : tz_parents t)
    return ()

drawLayers :: Int -- ^ indentation level
           -> Int -- ^ height
           -> [(Forest (TSNode a), TSNode a, Forest (TSNode a))] -- ^ node layers (from top to bottom!)
           -> TreeSelect a Int
drawLayers _ yl [] = return yl
drawLayers xl yl ((bs, c, as):xs) = do
    TSConfig{..} <- ask

    let nodeColor y = if odd y then ts_node else ts_nodealt

    -- draw nodes above
    forM_ (zip [yl ..] (reverse bs)) $ \(y, Node n _) ->
        drawNode xl y n (nodeColor y)
        -- drawLayers (xl + 1) (y + 1) ns
        -- TODO: draw rest? if not ts_hidechildren
        -- drawLayers (xl + 1) (y + 1) ns

    -- draw the current / parent node
    -- if this is the last (currently focused) we use the 'ts_highlight' color
    let current_level = yl + length bs
    drawNode xl current_level c $
        if null xs then ts_highlight
                   else nodeColor current_level

    l2 <- drawLayers (xl + 1) (current_level + 1) xs

    -- draw nodes below
    forM_ (zip [l2 ..] as) $ \(y, Node n _) ->
        drawNode xl y n (nodeColor y)
        -- TODO: draw rest? if not ts_hidechildren
        -- drawLayers (xl + 1) (y + 1) ns
    return (l2 + length as)


-- | Draw a node at a given indentation and height level
drawNode :: Int -- ^ indentation level (not in pixels)
         -> Int -- ^ height level (not in pixels)
         -> TSNode a -- ^ node to draw
         -> (Pixel, Pixel) -- ^ node foreground (font) and background color
         -> TreeSelect a ()
drawNode ix iy TSNode{..} col = do
    TSConfig{..} <- ask
    window       <- gets tss_window
    display      <- gets tss_display
    font         <- gets tss_xfont
    gc           <- gets tss_gc
    colormap <- gets tss_colormap
    visual   <- gets tss_visual
    liftIO $ drawWinBox window display visual colormap gc font col tsn_name ts_extra tsn_extra
        (ix * ts_indent + ts_originX) (iy * ts_node_height + ts_originY)
        ts_node_width ts_node_height

    -- TODO: draw extra text (transparent background? or ts_background)
    -- drawWinBox window fnt col2 nodeH (scW-x) (mes) (x+nodeW) y 8

-- | Draw a simple box with text
drawWinBox :: Window -> Display -> Visual -> Colormap -> GC -> XMonadFont -> (Pixel, Pixel) -> String -> Pixel -> String -> Int -> Int -> Int -> Int -> IO ()
drawWinBox win display visual colormap gc font (fg, bg) text fg2 text2 x y w h = do
    -- draw box
    setForeground display gc bg
    fillRectangle display win gc (fromIntegral x) (fromIntegral y) (fromIntegral w) (fromIntegral h)

    -- dreaw text
    drawStringXMF display win visual colormap gc font fg
        (fromIntegral $ x + 8)
        (fromIntegral $ y + h - 8)
        text

    -- dreaw extra text
    drawStringXMF display win visual colormap gc font fg2
        (fromIntegral $ x + w + 8)
        (fromIntegral $ y + h - 8)
        text2

-- | Modified version of 'XMonad.Util.Font.printStringXMF' that uses 'Pixel' as color format
drawStringXMF :: Display -> Drawable -> Visual -> Colormap -> GC
              -> XMonadFont -- ^ XMF Font
              -> Pixel -- ^ font color
              -> Position   -- ^ x-position
              -> Position   -- ^ y-position
              -> String -- ^ string text
              -> IO ()
drawStringXMF display window visual colormap gc font col x y text = case font of
    Core fnt -> do
        setForeground display gc col
        setFont display gc $ fontFromFontStruct fnt
        drawImageString display window gc x y text
    Utf8 fnt -> do
        setForeground display gc col
        wcDrawImageString display window fnt gc x y text
#ifdef XFT
    Xft fnt -> do
        withXftDraw display window visual colormap $
            \ft_draw -> withXftColorValue display visual colormap (fromARGB col) $
            \ft_color -> xftDrawString ft_draw ft_color fnt x y text

-- | Convert 'Pixel' to 'XRenderColor'
--
-- Note that it uses short to represent its components
fromARGB :: Pixel -> XRenderColor
fromARGB x = XRenderColor (fromIntegral $ 0xff00 .&. shiftR x 8)  -- red
                          (fromIntegral $ 0xff00 .&. x)           -- green
                          (fromIntegral $ 0xff00 .&. shiftL x 8)  -- blue
                          (fromIntegral $ 0xff00 .&. shiftR x 16) -- alpha
#endif
