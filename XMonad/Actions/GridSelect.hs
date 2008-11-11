-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.GridSelect
-- Copyright   :  Clemens Fruhwirth <clemens@endorphin.org>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Clemens Fruhwirth <clemens@endorphin.org>
-- Stability   :  unstable
-- Portability :  unportable
--
-- GridSelect displays a 2D grid of windows to navigate with cursor
-- keys and to select with return.
--
-----------------------------------------------------------------------------

module XMonad.Actions.GridSelect (
    -- * Usage
    -- $usage
    GSConfig(..),
    defaultGSConfig,
    gridselect,
    withSelectedWindow,
    bringSelected,
    goToSelected
    ) where
import Data.Maybe
import Data.Bits
import Control.Monad.State
import Control.Arrow
import Data.List as L
import XMonad
import XMonad.Util.Font
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Util.NamedWindows
import XMonad.Actions.WindowBringer (bringWindow)
import Text.Printf

-- $usage
--
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.GridSelect
--
-- Then add a keybinding, e.g.
--
-- >    , ((modMask x, xK_g), goToSelected defaultGSConfig)
--
-- Screenshot: <http://clemens.endorphin.org/gridselect.png>

data GSConfig = GSConfig {
      gs_cellheight :: Integer,
      gs_cellwidth :: Integer,
      gs_cellpadding :: Integer,
      gs_colorizer :: Window -> Bool -> X (String, String),
      gs_font :: String
}

type TwoDPosition = (Integer, Integer)

data TwoDState = TwoDState { td_curpos :: TwoDPosition,
                             td_windowmap :: [(TwoDPosition,(String,Window))],
                             td_gsconfig :: GSConfig,
                             td_font :: XMonadFont,
                             td_paneX :: Integer,
                             td_paneY :: Integer }


type TwoD a = StateT TwoDState X a

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
-- FIXME remove nub
diamondLayer n = let ul = [ (x,n-x) | x <- [0..n] ]
        in nub $ ul ++ (map (negate *** id) ul) ++
           (map (negate *** negate) ul) ++
           (map (id *** negate) ul)

diamond :: (Enum a, Num a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: (Enum t, Num t, Ord t) => t -> t -> [(t, t)]
diamondRestrict x y = L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
                      L.takeWhile (\(x',y') -> abs x' + abs y' <= x+y) $ diamond

tupadd :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
tupadd (a,b) (c,d) = (a+c,b+d)
tupmul :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
tupmul (a,b) (c,d) = (a*c,b*d)

drawWinBox :: Display -> Window -> XMonadFont -> (String, String) -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox dpy win font (fg,bg) ch cw text x y cp = do
  gc <- liftIO $ createGC dpy win
  bordergc <- liftIO $ createGC dpy win
  liftIO $ do
    Just fgcolor <- initColor dpy fg
    Just bgcolor <- initColor dpy bg
    Just bordercolor <- initColor dpy borderColor
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    setForeground dpy bordergc bordercolor
    fillRectangle dpy win gc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
    drawRectangle dpy win bordergc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
  stext <- shrinkWhile (shrinkIt shrinkText)
           (\n -> do size <- liftIO $ textWidthXMF dpy font n
                     return $ size > (fromInteger (cw-(2*cp))))
           text
  printStringXMF dpy win font gc bg fg (fromInteger (x+cp)) (fromInteger (y+(div ch 2))) stext
  liftIO $ freeGC dpy gc
  liftIO $ freeGC dpy bordergc

updateWindows :: Display -> Window -> TwoD ()
updateWindows dpy win = do
    (TwoDState curpos windowList gsconfig font paneX paneY) <- get
    let cellwidth = gs_cellwidth gsconfig
        cellheight = gs_cellheight gsconfig
        paneX' = div (paneX-cellwidth) 2
        paneY' = div (paneY-cellheight) 2
        updateWindow (pos@(x,y),(text, clientwindow)) = lift $ do
            colors <- (gs_colorizer gsconfig) clientwindow (pos == curpos)
            drawWinBox dpy win font
                       colors
                       (gs_cellheight gsconfig)
                       (gs_cellwidth gsconfig) text
                                                 (paneX'+x*cellwidth)
                                                 (paneY'+y*cellheight)
                                                 (gs_cellpadding gsconfig)
    mapM updateWindow windowList
    return ()

eventLoop :: Display -> Window -> TwoD (Maybe Window)
eventLoop d win = do
  (keysym,string,event) <- liftIO $ allocaXEvent $ \e -> do
                             nextEvent d e
                             ev <- getEvent e
                             (ks,s) <- if ev_event_type ev == keyPress
                                       then lookupString $ asKeyEvent e
                                       else return (Nothing, "")
                             return (ks,s,ev)
  handle d win (fromMaybe xK_VoidSymbol keysym,string) event

handle :: Display
       -> Window
       -> (KeySym, String)
       -> Event
       -> StateT TwoDState X (Maybe Window)
handle d win (ks,_) (KeyEvent {ev_event_type = t})
    | t == keyPress && ks == xK_Escape = return Nothing
    | t == keyPress && ks == xK_Left = diffAndRefresh (-1,0)
    | t == keyPress && ks == xK_Right = diffAndRefresh (1,0)
    | t == keyPress && ks == xK_Down = diffAndRefresh (0,1)
    | t == keyPress && ks == xK_Up = diffAndRefresh (0,-1)
    | t == keyPress && ks == xK_Return = do
       (TwoDState pos win' _ _ _ _) <- get
       return $ fmap (snd . snd) $ find ((== pos) . fst) win'
  where diffAndRefresh diff = do
          (TwoDState pos windowmap gsconfig font paneX paneY) <- get
          let newpos = pos `tupadd` diff
          when (isJust $ find ((newpos ==).fst) windowmap) $ do
              put $ TwoDState newpos windowmap gsconfig font paneX paneY
              updateWindows d win
          eventLoop d win

handle d win _ _ = do
  updateWindows d win
  eventLoop d win

-- FIXME probably move that into Utils?
-- Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = (div h 60) `mod` 6 :: Integer
        f = (((fromInteger h)/60) - (fromInteger hi)) :: Fractional a => a
        q = v * (1-f)
        p = v * (1-s)
        t = v * (1-(1-f)*s)
    in case hi of
         0 -> (v,t,p)
         1 -> (q,v,p)
         2 -> (p,v,t)
         3 -> (p,q,v)
         4 -> (t,p,v)
         5 -> (v,p,q)
         _ -> error "The world is ending. x mod a >= a."

default_colorizer :: Window -> Bool -> X (String, String)
default_colorizer w active = do
    classname <- runQuery className w
    let seed x = toInteger (sum $ map ((*x).fromEnum) classname) :: Integer
        (r,g,b) = hsv2rgb ((seed 83) `mod` 360,
                           (fromInteger ((seed 191) `mod` 1000))/2500+0.4,
                           (fromInteger ((seed 121) `mod` 1000))/2500+0.4)
    if active
      then return ("#faff69", "black")
      else return ("#" ++ concat (map (twodigitHex.(round :: Double -> Integer).(*256)) [r, g, b] ), "white")
  where
    twodigitHex :: Integer -> String
    twodigitHex a = printf "%02x" a

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is returned.
gridselect :: GSConfig -> X (Maybe Window)
gridselect gsconfig =
 withDisplay $ \dpy -> do
    rootw <- liftIO $ rootWindow dpy (defaultScreen dpy)
    s <- gets $ screenRect . W.screenDetail . W.current . windowset
    windowList <- windowMap
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)
    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    font <- initXMF (gs_font gsconfig)
    let screenWidth = toInteger $ rect_width s;
        screenHeight = toInteger $ rect_height s;
    selectedWindow <- if (status == grabSuccess) then
                          do
                            let restrictX = floor $ ((fromInteger screenWidth)/(fromInteger $ gs_cellwidth gsconfig)-1)/2 ;
                                restrictY = floor $ ((fromInteger screenHeight)/(fromInteger $ gs_cellheight gsconfig)-1)/2 ;
                            selectedWindow <- evalStateT (do updateWindows dpy win; eventLoop dpy win)
                                                   (TwoDState (0,0)
                                                              (zipWith (,) (diamondRestrict restrictX restrictY) windowList)
                                                              gsconfig
                                                              font
                                                              screenWidth
                                                              screenHeight)
                            return selectedWindow
                      else
                          return Nothing
    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      sync dpy False
    releaseXMF font
    return selectedWindow

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow :: (Window -> X ()) -> GSConfig -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselect conf
    case mbWindow of
        Just w -> callback w
        Nothing -> return ()
    

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    wins <- mapM keyValuePair (W.allWindows ws)
    return wins
 where keyValuePair w = flip (,) w `fmap` decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  fmap show $ getName w

defaultGSConfig :: GSConfig
defaultGSConfig = GSConfig 50 130 10 default_colorizer "xft:Sans-8"

borderColor :: String
borderColor = "white"

-- | Brings selected window to the current workspace.
bringSelected :: GSConfig -> X ()
bringSelected = withSelectedWindow $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GSConfig -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

