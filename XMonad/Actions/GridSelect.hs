{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
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
-- GridSelect displays items(e.g. the opened windows) in a 2D grid and lets
-- the user select from it with the cursor/hjkl keys or the mouse.
--
-----------------------------------------------------------------------------

module XMonad.Actions.GridSelect (
    -- * Usage
    -- $usage

    -- ** Customizing
    -- *** Using a common GSConfig
    -- $commonGSConfig

    -- *** Custom keybindings
    -- $keybindings

    -- * Configuration
    GSConfig(..),
    defaultGSConfig,
    NavigateMap,
    TwoDPosition,
    buildDefaultGSConfig,

    -- * Variations on 'gridselect'
    gridselect,
    gridselectWindow,
    withSelectedWindow,
    bringSelected,
    goToSelected,
    spawnSelected,
    runSelectedAction,

    -- * Colorizers
    HasColorizer(defaultColorizer),
    fromClassName,
    stringColorizer,
    colorRangeFromClassName

    -- * Screenshots
    -- $screenshots
    ) where
import Data.Maybe
import Data.Bits
import Control.Applicative
import Control.Monad.State
import Control.Arrow
import Data.List as L
import qualified Data.Map as M
import XMonad hiding (liftX)
import XMonad.Util.Font
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Util.NamedWindows
import XMonad.Actions.WindowBringer (bringWindow)
import Text.Printf
import System.Random (mkStdGen, genRange, next)
import Data.Word (Word8)

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
-- This module also supports displaying arbitrary information in a grid and letting
-- the user select from it. E.g. to spawn an application from a given list, you
-- can use the following:
--
-- >   , ((modMask x, xK_s), spawnSelected defaultGSConfig ["xterm","gmplayer","gvim"])

-- $commonGSConfig
--
-- It is possible to bind a @gsconfig@ at top-level in your configuration. Like so:
--
-- > -- the top of your config
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import XMonad
-- > ...
-- > gsconfig1 = defaultGSConfig { gs_cellheight = 30, gs_cellWidth = 100 }
--
-- An example where 'buildDefaultGSConfig' is used instead of 'defaultGSConfig'
-- in order to specify a custom colorizer is @gsconfig2@ (found in
-- "XMonad.Actions.GridSelect#Colorizers"):
--
-- > gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellWidth = 100 }
--
-- > -- | A green monochrome colorizer based on window class
-- > greenColorizer = colorRangeFromClassName
-- >                      black            -- lowest inactive bg
-- >                      (0x70,0xFF,0x70) -- highest inactive bg
-- >                      black            -- active bg
-- >                      white            -- inactive fg
-- >                      white            -- active fg
-- >   where black = minBound
-- >         white = maxBound
--
-- Then you can bind to:
--
-- >     ,((modMask x, xK_g), goToSelected  $ gsconfig2 myWinColorizer)
-- >     ,((modMask x, xK_p), spawnSelected $ spawnSelected defaultColorizer)

-- $keybindings
--
-- Adding more keybindings for gridselect to listen to is similar:
--
-- At the top of your config:
--
-- > {-# LANGAUGE NoMonomorphismRestriction #-}
-- > import XMonad
-- > import qualified Data.Map as M
--
-- Then define @gsconfig3@ which may be used in exactly the same manner as @gsconfig1@:
--
-- > gsconfig3 = defaultGSConfig
-- >    { gs_cellheight = 30
-- >    , gs_cellwidth = 100
-- >    , gs_navigate = M.unions
-- >            [reset
-- >            ,nethackKeys
-- >            ,gs_navigate                               -- get the default navigation bindings
-- >                $ defaultGSConfig `asTypeOf` gsconfig3 -- needed to fix an ambiguous type variable
-- >            ]
-- >    }
-- >   where addPair (a,b) (x,y) = (a+x,b+y)
-- >         nethackKeys = M.map addPair $ M.fromList
-- >                               [((0,xK_y),(-1,-1))
-- >                               ,((0,xK_i),(1,-1))
-- >                               ,((0,xK_n),(-1,1))
-- >                               ,((0,xK_m),(1,1))
-- >                               ]
-- >         -- jump back to the center with the spacebar, regardless of the current position.
-- >         reset = M.singleton (0,xK_space) (const (0,0))

-- $screenshots
--
-- Selecting a workspace:
--
-- <<http://haskell.org/sitewiki/images/a/a9/Xmonad-gridselect-workspace.png>>
--
-- Selecting a window by title:
--
-- <<http://haskell.org/sitewiki/images/3/35/Xmonad-gridselect-window-aavogt.png>>

data GSConfig a = GSConfig {
      gs_cellheight :: Integer,
      gs_cellwidth :: Integer,
      gs_cellpadding :: Integer,
      gs_colorizer :: a -> Bool -> X (String, String),
      gs_font :: String,
      gs_navigate :: NavigateMap,
      gs_originFractX :: Double,
      gs_originFractY :: Double
}

-- | That is 'fromClassName' if you are selecting a 'Window', or
-- 'defaultColorizer' if you are selecting a 'String'. The catch-all instance
-- @HasColorizer a@ uses the 'focusedBorderColor' and 'normalBorderColor'
-- colors.
class HasColorizer a where
    defaultColorizer :: a -> Bool -> X (String, String)

instance HasColorizer Window where
    defaultColorizer = fromClassName

instance HasColorizer String where
    defaultColorizer = stringColorizer

instance HasColorizer a where
    defaultColorizer _ isFg =
        let getColor = if isFg then focusedBorderColor else normalBorderColor
        in asks $ flip (,) "black" . getColor . config

-- | A basic configuration for 'gridselect', with the colorizer chosen based on the type.
--
-- If you want to replace the 'gs_colorizer' field, use 'buildDefaultGSConfig'
-- instead, to avoid ambiguous type variables.
defaultGSConfig :: HasColorizer a => GSConfig a
defaultGSConfig = buildDefaultGSConfig defaultColorizer

type NavigateMap = M.Map (KeyMask,KeySym) (TwoDPosition -> TwoDPosition)

type TwoDPosition = (Integer, Integer)

type TwoDElementMap a = [(TwoDPosition,(String,a))]

data TwoDState a = TwoDState { td_curpos :: TwoDPosition
                             , td_elementmap :: TwoDElementMap a
                             , td_gsconfig :: GSConfig a
                             , td_font :: XMonadFont
                             , td_paneX :: Integer
                             , td_paneY :: Integer
                             , td_drawingWin :: Window
                             }

newtype TwoD a b = TwoD { unTwoD :: StateT (TwoDState a) X b }
    deriving (Monad,Functor,MonadState (TwoDState a))

instance Applicative (TwoD a) where
    (<*>) = ap
    pure = return

liftX ::  X a1 -> TwoD a a1
liftX = TwoD . lift

evalTwoD ::  TwoD a1 a -> TwoDState a1 -> X a
evalTwoD m s = flip evalStateT s $ unTwoD m

diamondLayer :: (Enum b', Num b') => b' -> [(b', b')]
-- FIXME remove nub
diamondLayer n = let ul = [ (x,n-x) | x <- [0..n] ]
        in nub $ ul ++ (map (negate *** id) ul) ++
           (map (negate *** negate) ul) ++
           (map (id *** negate) ul)

diamond :: (Enum a, Num a) => [(a, a)]
diamond = concatMap diamondLayer [0..]

diamondRestrict :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
diamondRestrict x y originX originY =
  L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
  map (\(x', y') -> (x' + fromInteger originX, y' + fromInteger originY)) .
  take 1000 $ diamond

tupadd :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
tupadd (a,b) (c,d) = (a+c,b+d)

findInElementMap :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findInElementMap pos = find ((== pos) . fst)

drawWinBox :: Window -> XMonadFont -> (String, String) -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox win font (fg,bg) ch cw text x y cp =
  withDisplay $ \dpy -> do
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

updateAllElements :: TwoD a ()
updateAllElements =
    do
      TwoDState { td_elementmap = els } <- get
      updateElements els

updateElements :: TwoDElementMap a -> TwoD a ()
updateElements elementmap = do
    TwoDState { td_curpos = curpos,
                td_drawingWin = win,
                td_gsconfig = gsconfig,
                td_font = font,
                td_paneX = paneX,
                td_paneY = paneY} <- get
    let cellwidth = gs_cellwidth gsconfig
        cellheight = gs_cellheight gsconfig
        paneX' = div (paneX-cellwidth) 2
        paneY' = div (paneY-cellheight) 2
        updateElement (pos@(x,y),(text, element)) = liftX $ do
            colors <- gs_colorizer gsconfig element (pos == curpos)
            drawWinBox win font
                       colors
                       cellheight
                       cellwidth
                       text
                       (paneX'+x*cellwidth)
                       (paneY'+y*cellheight)
                       (gs_cellpadding gsconfig)
    mapM_ updateElement elementmap

eventLoop :: TwoD a (Maybe a)
eventLoop = do
  (keysym,string,event) <- liftX $ withDisplay $ \d -> liftIO $ allocaXEvent $ \e -> do
                             maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e
                             ev <- getEvent e
                             (ks,s) <- if ev_event_type ev == keyPress
                                       then lookupString $ asKeyEvent e
                                       else return (Nothing, "")
                             return (ks,s,ev)
  handle (fromMaybe xK_VoidSymbol keysym,string) event

handle ::  (KeySym, t) -> Event -> TwoD a (Maybe a)
handle (ks,_) (KeyEvent {ev_event_type = t, ev_state = m })
    | t == keyPress && ks == xK_Escape = return Nothing
    | t == keyPress && ks == xK_Return = do
       (TwoDState { td_curpos = pos, td_elementmap = elmap }) <- get
       return $ fmap (snd . snd) $ findInElementMap pos elmap
    | t == keyPress = maybe eventLoop diffAndRefresh . M.lookup (m,ks)
                            =<< gets (gs_navigate . td_gsconfig)
  where diffAndRefresh diff = do
          state <- get
          let elmap = td_elementmap state
              oldPos = td_curpos state
              newPos = diff oldPos
              newSelectedEl = findInElementMap newPos elmap
          when (isJust newSelectedEl) $ do
                                put state { td_curpos =  newPos }
                                updateElements (catMaybes [(findInElementMap oldPos elmap), newSelectedEl])
          eventLoop

handle _ (ButtonEvent { ev_event_type = t, ev_x = x, ev_y = y })
    | t == buttonRelease = do
        (TwoDState { td_elementmap = elmap, td_paneX = px, td_paneY = py,
                     td_gsconfig = (GSConfig ch cw _ _ _ _ _ _) }) <- get
        let gridX = (fi x - (px - cw) `div` 2) `div` cw
            gridY = (fi y - (py - ch) `div` 2) `div` ch
        case lookup (gridX,gridY) elmap of
             Just (_,el) -> return (Just el)
             Nothing -> eventLoop
    | otherwise = eventLoop

handle _ (ExposeEvent { }) = updateAllElements >> eventLoop

handle _ _ = eventLoop

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

-- | Default colorizer for Strings
stringColorizer :: String -> Bool -> X (String, String)
stringColorizer s active =
    let seed x = toInteger (sum $ map ((*x).fromEnum) s) :: Integer
        (r,g,b) = hsv2rgb ((seed 83) `mod` 360,
                           (fromInteger ((seed 191) `mod` 1000))/2500+0.4,
                           (fromInteger ((seed 121) `mod` 1000))/2500+0.4)
    in if active
         then return ("#faff69", "black")
         else return ("#" ++ concat (map (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b] ), "white")

-- | Colorize a window depending on it's className.
fromClassName :: Window -> Bool -> X (String, String)
fromClassName w active = runQuery className w >>= flip defaultColorizer active

twodigitHex :: Word8 -> String
twodigitHex a = printf "%02x" a

-- | A colorizer that picks a color inside a range,
-- and depending on the window's class.
colorRangeFromClassName :: (Word8, Word8, Word8) -- ^ Beginning of the color range
                        -> (Word8, Word8, Word8) -- ^ End of the color range
                        -> (Word8, Word8, Word8) -- ^ Background of the active window
                        -> (Word8, Word8, Word8) -- ^ Inactive text color
                        -> (Word8, Word8, Word8) -- ^ Active text color
                        -> Window -> Bool -> X (String, String)
colorRangeFromClassName startC endC activeC inactiveT activeT w active =
    do classname <- runQuery className w
       if active
         then return (rgbToHex activeC, rgbToHex activeT)
         else return (rgbToHex $ mix startC endC
                  $ stringToRatio classname, rgbToHex inactiveT)
    where rgbToHex :: (Word8, Word8, Word8) -> String
          rgbToHex (r, g, b) = '#':twodigitHex r
                               ++twodigitHex g++twodigitHex b

-- | Creates a mix of two colors according to a ratio
-- (1 -> first color, 0 -> second color).
mix :: (Word8, Word8, Word8) -> (Word8, Word8, Word8)
        -> Double -> (Word8, Word8, Word8)
mix (r1, g1, b1) (r2, g2, b2) r = (mix' r1 r2, mix' g1 g2, mix' b1 b2)
    where  mix' a b = truncate $ (fi a * r) + (fi b * (1 - r))

-- | Generates a Double from a string, trying to
-- achieve a random distribution.
-- We create a random seed from the sum of all characters
-- in the string, and use it to generate a ratio between 0 and 1
stringToRatio :: String -> Double
stringToRatio "" = 0
stringToRatio s = let gen = mkStdGen $ sum $ map fromEnum s
                      range = (\(a, b) -> b - a) $ genRange gen
                      randomInt = foldr1 combine $ replicate 20 next
                      combine f1 f2 g = let (_, g') = f1 g in f2 g'
                  in fi (fst $ randomInt gen) / fi range

-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gridselect :: GSConfig a -> [(String,a)] -> X (Maybe a)
gridselect gsconfig elmap =
 withDisplay $ \dpy -> do
    rootw <- asks theRoot
    s <- gets $ screenRect . W.screenDetail . W.current . windowset
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x s) (rect_y s) (rect_width s) (rect_height s)
    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    io $ grabButton dpy button1 anyModifier win True buttonReleaseMask grabModeAsync grabModeAsync none none
    font <- initXMF (gs_font gsconfig)
    let screenWidth = toInteger $ rect_width s;
        screenHeight = toInteger $ rect_height s;
    selectedElement <- if (status == grabSuccess) then do
                            let restriction ss cs = (fromInteger ss/fromInteger (cs gsconfig)-1)/2 :: Double
                                restrictX = floor $ restriction screenWidth gs_cellwidth
                                restrictY = floor $ restriction screenHeight gs_cellheight
                                originPosX = floor $ ((gs_originFractX gsconfig) - (1/2)) * 2 * fromIntegral restrictX
                                originPosY = floor $ ((gs_originFractY gsconfig) - (1/2)) * 2 * fromIntegral restrictY
                                coords = diamondRestrict restrictX restrictY originPosX originPosY
                                elmap' = zip coords elmap

                            evalTwoD (updateAllElements >> eventLoop)
                                (TwoDState (head coords)
                                            elmap'
                                            gsconfig
                                            font
                                            screenWidth
                                            screenHeight
                                            win)
                      else
                          return Nothing
    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      sync dpy False
    releaseXMF font
    return selectedElement

-- | Like `gridSelect' but with the current windows and their titles as elements
gridselectWindow :: GSConfig Window -> X (Maybe Window)
gridselectWindow gsconf = windowMap >>= gridselect gsconf

-- | Brings up a 2D grid of windows in the center of the screen, and one can
-- select a window with cursors keys. The selected window is then passed to
-- a callback function.
withSelectedWindow :: (Window -> X ()) -> GSConfig Window -> X ()
withSelectedWindow callback conf = do
    mbWindow <- gridselectWindow conf
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

-- | Builds a default gs config from a colorizer function.
buildDefaultGSConfig :: (a -> Bool -> X (String,String)) -> GSConfig a
buildDefaultGSConfig col = GSConfig 50 130 10 col "xft:Sans-8" defaultGSNav (1/2) (1/2)

defaultGSNav :: NavigateMap
defaultGSNav = M.map tupadd $ M.fromList
    [((0,xK_Left) ,(-1,0))
    ,((0,xK_h)    ,(-1,0))
    ,((0,xK_Right),(1,0))
    ,((0,xK_l)    ,(1,0))
    ,((0,xK_Down) ,(0,1))
    ,((0,xK_j)    ,(0,1))
    ,((0,xK_Up)   ,(0,-1))
    ,((0,xK_k)    ,(0,-1))
    ]

borderColor :: String
borderColor = "white"

-- | Brings selected window to the current workspace.
bringSelected :: GSConfig Window -> X ()
bringSelected = withSelectedWindow $ \w -> do
    windows (bringWindow w)
    XMonad.focus w
    windows W.shiftMaster

-- | Switches to selected window's workspace and focuses that window.
goToSelected :: GSConfig Window -> X ()
goToSelected = withSelectedWindow $ windows . W.focusWindow

-- | Select an application to spawn from a given list
spawnSelected :: GSConfig String -> [String] -> X ()
spawnSelected conf lst = gridselect conf (zip lst lst) >>= flip whenJust spawn

-- | Select an action and run it in the X monad
runSelectedAction :: GSConfig (X ()) -> [(String, X ())] -> X ()
runSelectedAction conf actions = do
    selectedActionM <- gridselect conf actions
    case selectedActionM of
        Just selectedAction -> selectedAction
        Nothing -> return ()
