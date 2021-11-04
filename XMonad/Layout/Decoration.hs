{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE CPP                   #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Decoration
-- Description :  A layout modifier and a class for easily creating decorated layouts.
-- Copyright   :  (c) 2007 Andrea Rossato, 2009 Jan Vornberger
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout modifier and a class for easily creating decorated
-- layouts.
-----------------------------------------------------------------------------

module XMonad.Layout.Decoration
    ( -- * Usage:
      -- $usage
      decoration
    , Theme (..), def
    , Decoration
    , DecorationMsg (..)
    , DecorationStyle (..)
    , DefaultDecoration (..)
    , Shrinker (..), DefaultShrinker
    , shrinkText, CustomShrink ( CustomShrink ), shrinkWhile
    , isInStack, isVisible, isInvisible, isWithin, fi
    , findWindowByDecoration
    , module XMonad.Layout.LayoutModifier
    , DecorationState, OrigWin
    ) where

import Foreign.C.Types(CInt)

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger (WindowArrangerMsg (..), diff, listFromList)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Invisible
import XMonad.Util.XUtils
import XMonad.Util.Font
import XMonad.Util.Image

-- $usage
-- This module is intended for layout developers, who want to decorate
-- their layouts. End users will not find here very much for them.
--
-- For examples of 'DecorationStyle' instances you can have a look at
-- "XMonad.Layout.SimpleDecoration", "XMonad.Layout.Tabbed",
-- "XMonad.Layout.DwmStyle", or "XMonad.Layout.TabBarDecoration".

-- | A layout modifier that, with a 'Shrinker', a 'Theme', a
-- 'DecorationStyle', and a layout, will decorate this layout
-- according to the decoration style provided.
--
-- For some usage examples see "XMonad.Layout.DecorationMadness".
decoration :: (DecorationStyle ds a, Shrinker s) => s -> Theme -> ds a
           -> l a -> ModifiedLayout (Decoration ds s) l a
decoration s t ds = ModifiedLayout (Decoration (I Nothing) s t ds)

-- | A 'Theme' is a record of colors, font etc., to customize a
-- 'DecorationStyle'.
--
-- For a collection of 'Theme's see "XMonad.Util.Themes"
data Theme =
    Theme { activeColor         :: String                  -- ^ Color of the active window
          , inactiveColor       :: String                  -- ^ Color of the inactive window
          , urgentColor         :: String                  -- ^ Color of the urgent window
          , activeBorderColor   :: String                  -- ^ Color of the border of the active window
          , inactiveBorderColor :: String                  -- ^ Color of the border of the inactive window
          , urgentBorderColor   :: String                  -- ^ Color of the border of the urgent window
          , activeBorderWidth   :: Dimension               -- ^ Width of the border of the active window
          , inactiveBorderWidth :: Dimension               -- ^ Width of the border of the inactive window
          , urgentBorderWidth   :: Dimension               -- ^ Width of the border of the urgent window
          , activeTextColor     :: String                  -- ^ Color of the text of the active window
          , inactiveTextColor   :: String                  -- ^ Color of the text of the inactive window
          , urgentTextColor     :: String                  -- ^ Color of the text of the urgent window
          , fontName            :: String                  -- ^ Font name
          , decoWidth           :: Dimension               -- ^ Maximum width of the decorations (if supported by the 'DecorationStyle')
          , decoHeight          :: Dimension               -- ^ Height of the decorations
          , windowTitleAddons   :: [(String, Align)]       -- ^ Extra text to appear in a window's title bar.
                                                           --    Refer to for a use "XMonad.Layout.ImageButtonDecoration"
          , windowTitleIcons    :: [([[Bool]], Placement)] -- ^ Extra icons to appear in a window's title bar.
                                                           --    Inner @[Bool]@ is a row in a icon bitmap.
          } deriving (Show, Read)

-- | The default xmonad 'Theme'.
instance Default Theme where
  def =
    Theme { activeColor         = "#999999"
          , inactiveColor       = "#666666"
          , urgentColor         = "#FFFF00"
          , activeBorderColor   = "#FFFFFF"
          , inactiveBorderColor = "#BBBBBB"
          , urgentBorderColor   = "##00FF00"
          , activeBorderWidth   = 1
          , inactiveBorderWidth = 1
          , urgentBorderWidth   = 1
          , activeTextColor     = "#FFFFFF"
          , inactiveTextColor   = "#BFBFBF"
          , urgentTextColor     = "#FF0000"
#ifdef XFT
          , fontName            = "xft:monospace"
#else
          , fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
#endif
          , decoWidth           = 200
          , decoHeight          = 20
          , windowTitleAddons   = []
          , windowTitleIcons    = []
          }

-- | A 'Decoration' layout modifier will handle 'SetTheme', a message
-- to dynamically change the decoration 'Theme'.
newtype DecorationMsg = SetTheme Theme
instance Message DecorationMsg

-- | The 'Decoration' state component, where the list of decorated
-- window's is zipped with a list of decoration. A list of decoration
-- is a list of tuples, a 'Maybe' 'Window' and a 'Maybe Rectangle'.
-- The 'Window' will be displayed only if the rectangle is of type
-- 'Just'.
data DecorationState =
    DS { decos :: [(OrigWin,DecoWin)]
       , font  :: XMonadFont
       }
type DecoWin = (Maybe Window, Maybe Rectangle)
type OrigWin = (Window,Rectangle)

-- | The 'Decoration' 'LayoutModifier'. This data type is an instance
-- of the 'LayoutModifier' class. This data type will be passed,
-- together with a layout, to the 'ModifiedLayout' type constructor
-- to modify the layout by adding decorations according to a
-- 'DecorationStyle'.
data Decoration ds s a =
    Decoration (Invisible Maybe DecorationState) s Theme (ds a)
    deriving (Show, Read)

-- | The 'DecorationStyle' class, defines methods used in the
-- implementation of the 'Decoration' 'LayoutModifier' instance. A
-- type instance of this class is passed to the 'Decoration' type in
-- order to decorate a layout, by using these methods.
class (Read (ds a), Show (ds a), Eq a) => DecorationStyle ds a where

    -- | The description that the 'Decoration' modifier will display.
    describeDeco :: ds a -> String
    describeDeco = show

    -- | Shrink the window's rectangle when applying a decoration.
    shrink :: ds a -> Rectangle -> Rectangle -> Rectangle
    shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    -- | The decoration event hook
    decorationEventHook :: ds a -> DecorationState -> Event -> X ()
    decorationEventHook = handleMouseFocusDrag

    -- | A hook that can be used to catch the cases when the user
    -- clicks on the decoration. If you return True here, the click event
    -- will be considered as dealt with and no further processing will take place.
    decorationCatchClicksHook :: ds a
                              -> Window
                              -> Int    -- ^ distance from the left where the click happened on the decoration
                              -> Int    -- ^ distance from the right where the click happened on the decoration
                              -> X Bool
    decorationCatchClicksHook _ _ _ _ = return False

    -- | This hook is called while a window is dragged using the decoration.
    -- The hook can be overwritten if a different way of handling the dragging
    -- is required.
    decorationWhileDraggingHook :: ds a -> CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
    decorationWhileDraggingHook _ = handleDraggingInProgress

    -- | This hoook is called after a window has been dragged using the decoration.
    decorationAfterDraggingHook :: ds a -> (Window, Rectangle) -> Window -> X ()
    decorationAfterDraggingHook _ds (mainw, _r) _decoWin = focus mainw

    -- | The pure version of the main method, 'decorate'.
    pureDecoration :: ds a -> Dimension -> Dimension -> Rectangle
                   -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> Maybe Rectangle
    pureDecoration _ _ ht _ s _ (w,Rectangle x y wh ht') = if isInStack s w && (ht < ht')
                                                             then Just $ Rectangle x y wh ht
                                                             else Nothing

    -- | Given the theme's decoration width and height, the screen
    -- rectangle, the windows stack, the list of windows and
    -- rectangles returned by the underlying layout and window to be
    -- decorated, tupled with its rectangle, produce a 'Just'
    -- 'Rectangle' or 'Nothing' if the window is not to be decorated.
    decorate :: ds a -> Dimension -> Dimension -> Rectangle
             -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> X (Maybe Rectangle)
    decorate ds w h r s wrs wr = return $ pureDecoration ds w h r s wrs wr

-- | The default 'DecorationStyle', with just the default methods'
-- implementations.
data DefaultDecoration a = DefaultDecoration deriving ( Read, Show )
instance Eq a => DecorationStyle DefaultDecoration a

-- | The long 'LayoutModifier' instance for the 'Decoration' type.
--
-- In 'redoLayout' we check the state: if there is no state we
-- initialize it.
--
-- The state is 'diff'ed against the list of windows produced by the
-- underlying layout: removed windows get deleted and new ones
-- decorated by 'createDecos', which will call 'decorate' to decide if
-- a window must be given a 'Rectangle', in which case a decoration
-- window will be created.
--
-- After that we resync the updated state with the windows' list and
-- then we process the resynced stated (as we do with a new state).
--
-- First we map the decoration windows, we update each decoration to
-- reflect any decorated window's change, and we insert, in the list
-- of windows and rectangles returned by the underlying layout, the
-- decoration for each window. This way xmonad will restack the
-- decorations and their windows accordingly. At the end we remove
-- invisible\/stacked windows.
--
-- Message handling is quite simple: when needed we release the state
-- component of the 'Decoration' 'LayoutModifier'. Otherwise we call
-- 'handleEvent', which will call the appropriate 'DecorationStyle'
-- methods to perform its tasks.
instance (DecorationStyle ds Window, Shrinker s) => LayoutModifier (Decoration ds s) Window where
    redoLayout (Decoration (I (Just s)) sh t ds) _ Nothing _ = do
        releaseResources s
        return ([], Just $ Decoration (I Nothing) sh t ds)
    redoLayout _                                 _ Nothing _  = return ([], Nothing)

    redoLayout (Decoration st sh t ds) sc (Just stack) wrs
        | I Nothing  <- st = initState t ds sc stack wrs >>= processState
        | I (Just s) <- st = do let dwrs  = decos s
                                    (d,a) = curry diff (get_ws dwrs) ws
                                    toDel = todel d dwrs
                                    toAdd = toadd a wrs
                                deleteDecos (map snd toDel)
                                let ndwrs = zip toAdd $ repeat (Nothing,Nothing)
                                ndecos <- resync (ndwrs ++ del_dwrs d dwrs) wrs
                                processState (s {decos = ndecos })

        where
          ws        = map fst wrs
          get_w     = fst . fst
          get_ws    = map get_w
          del_dwrs  = listFromList get_w notElem
          find_dw i = fst . snd . flip (!!) i
          todel   d = filter (flip elem d . get_w)
          toadd   a = filter (flip elem a . fst  )

          check_dwr dwr = case dwr of
                            (Nothing, Just dr) -> do dw <- createDecoWindow t dr
                                                     return (Just dw, Just dr)
                            _                 -> return dwr

          resync _         [] = return []
          resync d ((w,r):xs) = case  w `elemIndex` get_ws d of
                                  Just i  -> do dr   <- decorate ds (decoWidth t) (decoHeight t) sc stack wrs (w,r)
                                                dwr  <- check_dwr (find_dw i d, dr)
                                                dwrs <- resync d xs
                                                return $ ((w,r),dwr) : dwrs
                                  Nothing -> resync d xs

          -- We drop any windows that are *precisely* stacked underneath
          -- another window: these must be intended to be tabbed!
          remove_stacked rs ((w,r):xs)
              | r `elem` rs   = remove_stacked rs xs
              | otherwise     = (w,r) : remove_stacked (r:rs) xs
          remove_stacked _ [] = []

          insert_dwr ((w,r),(Just dw,Just dr)) xs = (dw,dr):(w, shrink ds dr r):xs
          insert_dwr (x    ,(     _ ,     _ )) xs = x:xs

          dwrs_to_wrs    = remove_stacked [] . foldr insert_dwr []

          processState s = do let ndwrs = decos s
                              showDecos (map snd ndwrs)
                              updateDecos sh t (font s) ndwrs
                              return (dwrs_to_wrs ndwrs, Just (Decoration (I (Just (s {decos = ndwrs}))) sh t ds))

    handleMess (Decoration (I (Just s@DS{decos = dwrs})) sh t ds) m
        | Just e <- fromMessage m                = do decorationEventHook ds s e
                                                      handleEvent sh t s e
                                                      return Nothing
        | Just Hide             <- fromMessage m = do hideDecos (map snd dwrs)
                                                      return Nothing
        | Just (SetTheme nt)    <- fromMessage m = do releaseResources s
                                                      return $ Just $ Decoration (I Nothing) sh nt ds
        | Just ReleaseResources <- fromMessage m = do releaseResources s
                                                      return $ Just $ Decoration (I Nothing) sh t  ds
    handleMess _ _ = return Nothing

    modifierDescription (Decoration _ _ _ ds) = describeDeco ds

-- | By default 'Decoration' handles 'PropertyEvent' and 'ExposeEvent'
-- only.
handleEvent :: Shrinker s => s -> Theme -> DecorationState -> Event -> X ()
handleEvent sh t (DS dwrs fs) e
    | PropertyEvent {ev_window = w} <- e
    , Just i <- w `elemIndex` map (fst . fst) dwrs      = updateDeco sh t fs (dwrs !! i)
    | ExposeEvent   {ev_window = w} <- e
    , Just i <- w `elemIndex` mapMaybe (fst . snd) dwrs = updateDeco sh t fs (dwrs !! i)
handleEvent _ _ _ _ = return ()

-- | Mouse focus and mouse drag are handled by the same function, this
-- way we can start dragging unfocused windows too.
handleMouseFocusDrag :: (DecorationStyle ds a) => ds a -> DecorationState -> Event -> X ()
handleMouseFocusDrag ds (DS dwrs _) ButtonEvent { ev_window     = ew
                                                , ev_event_type = et
                                                , ev_x_root     = ex
                                                , ev_y_root     = ey }
    | et == buttonPress
    , Just ((mainw,r), (_, decoRectM)) <- lookFor ew dwrs = do
        let Rectangle dx _ dwh _ = fromJust decoRectM
            distFromLeft = ex - fi dx
            distFromRight = fi dwh - (ex - fi dx)
        dealtWith <- decorationCatchClicksHook ds mainw (fi distFromLeft) (fi distFromRight)
        unless dealtWith $
            mouseDrag (\x y -> focus mainw >> decorationWhileDraggingHook ds ex ey (mainw, r) x y)
                        (decorationAfterDraggingHook ds (mainw, r) ew)
handleMouseFocusDrag _ _ _ = return ()

handleDraggingInProgress :: CInt -> CInt -> (Window, Rectangle) -> Position -> Position -> X ()
handleDraggingInProgress ex ey (_, r) x y = do
    let rect = Rectangle (x - (fi ex - rect_x r))
                         (y - (fi ey - rect_y r))
                         (rect_width  r)
                         (rect_height r)
    sendMessage $ SetGeometry rect

-- | Given a window and the state, if a matching decoration is in the
-- state return it with its ('Maybe') 'Rectangle'.
lookFor :: Window -> [(OrigWin,DecoWin)] -> Maybe (OrigWin,(Window,Maybe Rectangle))
lookFor w ((wr,(Just dw,dr)):dwrs) | w == dw = Just (wr,(dw,dr))
                                   | otherwise = lookFor w dwrs
lookFor w ((_, (Nothing, _)):dwrs) = lookFor w dwrs
lookFor _ [] = Nothing

findWindowByDecoration :: Window -> DecorationState -> Maybe (OrigWin,(Window,Maybe Rectangle))
findWindowByDecoration w ds = lookFor w (decos ds)

-- | Initialize the 'DecorationState' by initializing the font
-- structure and by creating the needed decorations.
initState :: DecorationStyle ds Window => Theme -> ds Window -> Rectangle
          -> W.Stack Window -> [(Window,Rectangle)] -> X DecorationState
initState t ds sc s wrs = do
  fs   <- initXMF (fontName t)
  dwrs <- createDecos t ds sc s wrs wrs
  return $ DS dwrs fs

-- | Delete windows stored in the state and release the font structure.
releaseResources :: DecorationState -> X ()
releaseResources s = do
  deleteDecos (map snd $ decos s)
  releaseXMF  (font s)

-- | Create the decoration windows of a list of windows and their
-- rectangles, by calling the 'decorate' method of the
-- 'DecorationStyle' received.
createDecos :: DecorationStyle ds Window => Theme -> ds Window -> Rectangle -> W.Stack Window
            -> [(Window,Rectangle)] -> [(Window,Rectangle)] -> X [(OrigWin,DecoWin)]
createDecos t ds sc s wrs ((w,r):xs) = do
  deco <- decorate ds (decoWidth t) (decoHeight t) sc s wrs (w,r)
  case deco of
    Just dr -> do dw   <- createDecoWindow t dr
                  dwrs <- createDecos t ds sc s wrs xs
                  return $ ((w,r), (Just dw, Just dr)) : dwrs
    Nothing -> do dwrs <- createDecos t ds sc s wrs xs
                  return $ ((w,r), (Nothing, Nothing)) : dwrs
createDecos _ _ _ _ _ [] = return []

createDecoWindow :: Theme -> Rectangle -> X Window
createDecoWindow t r = do
  let mask = Just (exposureMask .|. buttonPressMask)
  w <- createNewWindow r mask (inactiveColor t) True
  d <- asks display
  io $ setClassHint d w (ClassHint "xmonad-decoration" "xmonad")
  pure w

showDecos :: [DecoWin] -> X ()
showDecos = showWindows . mapMaybe fst . filter (isJust . snd)

hideDecos :: [DecoWin] -> X ()
hideDecos = hideWindows . mapMaybe fst

deleteDecos :: [DecoWin] -> X ()
deleteDecos = deleteWindows . mapMaybe fst

updateDecos :: Shrinker s => s -> Theme -> XMonadFont -> [(OrigWin,DecoWin)] -> X ()
updateDecos s t f = mapM_ $ updateDeco s t f

-- | Update a decoration window given a shrinker, a theme, the font
-- structure and the needed 'Rectangle's
updateDeco :: Shrinker s => s -> Theme -> XMonadFont -> (OrigWin,DecoWin) -> X ()
updateDeco sh t fs ((w,_),(Just dw,Just (Rectangle _ _ wh ht))) = do
  nw  <- getName w
  ur  <- readUrgents
  dpy <- asks display
  let focusColor win ic ac uc = maybe ic (\focusw -> case () of
                                                      _ | focusw == win -> ac
                                                        | win `elem` ur -> uc
                                                        | otherwise     -> ic) . W.peek
                                <$> gets windowset
  (bc,borderc,borderw,tc) <-
    focusColor w (inactiveColor t, inactiveBorderColor t, inactiveBorderWidth t, inactiveTextColor t)
                 (activeColor   t, activeBorderColor   t, activeBorderWidth   t, activeTextColor   t)
                 (urgentColor   t, urgentBorderColor   t, urgentBorderWidth   t, urgentTextColor   t)
  let s = shrinkIt sh
  name <- shrinkWhile s (\n -> do size <- io $ textWidthXMF dpy fs n
                                  return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) (show nw)
  let als = AlignCenter : map snd (windowTitleAddons t)
      strs = name : map fst (windowTitleAddons t)
      i_als = map snd (windowTitleIcons t)
      icons = map fst (windowTitleIcons t)
  paintTextAndIcons dw fs wh ht borderw bc borderc tc bc als strs i_als icons
updateDeco _ _ _ (_,(Just w,Nothing)) = hideWindow w
updateDeco _ _ _ _ = return ()

-- | True if the window is in the 'Stack'. The 'Window' comes second
-- to facilitate list processing, even though @w \`isInStack\` s@ won't
-- work...;)
isInStack :: Eq a => W.Stack a -> a -> Bool
isInStack s = flip elem (W.integrate s)

-- | Given a 'Rectangle' and a list of 'Rectangle's is True if the
-- 'Rectangle' is not completely contained by any 'Rectangle' of the
-- list.
isVisible :: Rectangle -> [Rectangle] -> Bool
isVisible r = and . foldr f []
    where f x xs = if r `isWithin` x then False : xs else True : xs

-- | The contrary of 'isVisible'.
isInvisible :: Rectangle -> [Rectangle] -> Bool
isInvisible r = not . isVisible r

-- | True is the first 'Rectangle' is totally within the second
-- 'Rectangle'.
isWithin :: Rectangle -> Rectangle -> Bool
isWithin (Rectangle x y w h) (Rectangle rx ry rw rh)
    | x >= rx, x <= rx + fi rw
    , y >= ry, y <= ry + fi rh
    , x + fi w <= rx + fi rw
    , y + fi h <= ry + fi rh = True
    | otherwise              = False

shrinkWhile :: (String -> [String]) -> (String -> X Bool) -> String -> X String
shrinkWhile sh p x = sw $ sh x
    where sw [n] = return n
          sw [] = return ""
          sw (n:ns) = do
                        cond <- p n
                        if cond
                          then sw ns
                          else return n

data CustomShrink = CustomShrink
instance Show CustomShrink where show _ = ""
instance Read CustomShrink where readsPrec _ s = [(CustomShrink,s)]

class (Read s, Show s) => Shrinker s where
    shrinkIt :: s -> String -> [String]

data DefaultShrinker = DefaultShrinker
instance Show DefaultShrinker where show _ = ""
instance Read DefaultShrinker where readsPrec _ s = [(DefaultShrinker,s)]
instance Shrinker DefaultShrinker where
    shrinkIt _ "" = [""]
    shrinkIt s cs = cs : shrinkIt s (init cs)

shrinkText :: DefaultShrinker
shrinkText = DefaultShrinker
