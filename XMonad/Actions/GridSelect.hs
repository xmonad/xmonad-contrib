{-# LANGUAGE ScopedTypeVariables, GeneralizedNewtypeDeriving, FlexibleInstances, TupleSections #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.GridSelect
-- Description :  Display items in a 2D grid and select from it with the keyboard or the mouse.
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
    def,
    TwoDPosition,
    buildDefaultGSConfig,

    -- * Variations on 'gridselect'
    gridselect,
    gridselectWindow,
    withSelectedWindow,
    bringSelected,
    goToSelected,
    gridselectWorkspace,
    gridselectWorkspace',
    spawnSelected,
    runSelectedAction,

    -- * Colorizers
    HasColorizer(defaultColorizer),
    fromClassName,
    stringColorizer,
    colorRangeFromClassName,
    stringToRatio,

    -- * Navigation Mode assembly
    TwoD,
    makeXEventhandler,
    shadowWithKeymap,

    -- * Built-in Navigation Mode
    defaultNavigation,
    substringSearch,
    navNSearch,

    -- * Navigation Components
    setPos,
    move,
    moveNext, movePrev,
    select,
    cancel,
    transformSearchString,

    -- * Rearrangers
    -- $rearrangers
    Rearranger,
    noRearranger,
    searchStringRearrangerGenerator,

    -- * Screenshots
    -- $screenshots

    -- * Types
    TwoDState,
    ) where
import Control.Arrow ((***))
import Data.Bits
import Data.Ord (comparing)
import Control.Monad.State
import Data.List as L
import qualified Data.Map as M
import XMonad hiding (liftX)
import XMonad.Prelude
import XMonad.Util.Font
import XMonad.Prompt (mkUnmanagedWindow)
import XMonad.StackSet as W
import XMonad.Layout.Decoration
import XMonad.Util.NamedWindows
import XMonad.Actions.WindowBringer (bringWindow)
import Text.Printf
import System.Random (mkStdGen, randomR)
import Data.Word (Word8)
import qualified Data.List.NonEmpty as NE

-- $usage
--
-- You can use this module with the following in your @xmonad.hs@:
--
-- >    import XMonad.Actions.GridSelect
--
-- Then add a keybinding, e.g.
--
-- >    , ((modm, xK_g), goToSelected def)
--
-- This module also supports displaying arbitrary information in a grid and letting
-- the user select from it. E.g. to spawn an application from a given list, you
-- can use the following:
--
-- >   , ((modm, xK_s), spawnSelected def ["xterm","gmplayer","gvim"])

-- $commonGSConfig
--
-- It is possible to bind a @gsconfig@ at top-level in your configuration. Like so:
--
-- > -- the top of your config
-- > {-# LANGUAGE NoMonomorphismRestriction #-}
-- > import XMonad
-- > ...
-- > gsconfig1 = def { gs_cellheight = 30, gs_cellwidth = 100 }
--
-- An example where 'buildDefaultGSConfig' is used instead of 'def'
-- in order to specify a custom colorizer is @gsconfig2@ (found in
-- "XMonad.Actions.GridSelect#Colorizers"):
--
-- > gsconfig2 colorizer = (buildDefaultGSConfig colorizer) { gs_cellheight = 30, gs_cellwidth = 100 }
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
-- >     ,((modm, xK_g), goToSelected $ gsconfig2 myWinColorizer)
-- >     ,((modm, xK_p), spawnSelected (gsconfig2 defaultColorizer) ["xterm","gvim"])

-- $keybindings
--
-- You can build you own navigation mode and submodes by combining the
-- exported action ingredients and assembling them using 'makeXEventhandler' and 'shadowWithKeymap'.
--
-- > myNavigation :: TwoD a (Maybe a)
-- > myNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
-- >  where navKeyMap = M.fromList [
-- >           ((0,xK_Escape), cancel)
-- >          ,((0,xK_Return), select)
-- >          ,((0,xK_slash) , substringSearch myNavigation)
-- >          ,((0,xK_Left)  , move (-1,0)  >> myNavigation)
-- >          ,((0,xK_h)     , move (-1,0)  >> myNavigation)
-- >          ,((0,xK_Right) , move (1,0)   >> myNavigation)
-- >          ,((0,xK_l)     , move (1,0)   >> myNavigation)
-- >          ,((0,xK_Down)  , move (0,1)   >> myNavigation)
-- >          ,((0,xK_j)     , move (0,1)   >> myNavigation)
-- >          ,((0,xK_Up)    , move (0,-1)  >> myNavigation)
-- >          ,((0,xK_y)     , move (-1,-1) >> myNavigation)
-- >          ,((0,xK_i)     , move (1,-1)  >> myNavigation)
-- >          ,((0,xK_n)     , move (-1,1)  >> myNavigation)
-- >          ,((0,xK_m)     , move (1,-1)  >> myNavigation)
-- >          ,((0,xK_space) , setPos (0,0) >> myNavigation)
-- >          ]
-- >        -- The navigation handler ignores unknown key symbols
-- >        navDefaultHandler = const myNavigation
--
-- You can then define @gsconfig3@ which may be used in exactly the same manner as @gsconfig1@:
--
-- > gsconfig3 = def
-- >    { gs_cellheight = 30
-- >    , gs_cellwidth = 100
-- >    , gs_navigate = myNavigation
-- >    }

-- $screenshots
--
-- Selecting a workspace:
--
-- <<http://haskell.org/wikiupload/a/a9/Xmonad-gridselect-workspace.png>>
--
-- Selecting a window by title:
--
-- <<http://haskell.org/wikiupload/3/35/Xmonad-gridselect-window-aavogt.png>>

-- | The 'Default' instance gives a basic configuration for 'gridselect', with
-- the colorizer chosen based on the type.
--
-- If you want to replace the 'gs_colorizer' field, use 'buildDefaultGSConfig'
-- instead of 'def' to avoid ambiguous type variables.
data GSConfig a = GSConfig {
      gs_cellheight :: Integer,
      gs_cellwidth :: Integer,
      gs_cellpadding :: Integer,
      gs_colorizer :: a -> Bool -> X (String, String),
      gs_font :: String,
      gs_navigate :: TwoD a (Maybe a),
      -- ^ Customize key bindings for a GridSelect
      gs_rearranger :: Rearranger a,
      gs_originFractX :: Double,
      gs_originFractY :: Double,
      gs_bordercolor :: String,
      gs_cancelOnEmptyClick :: Bool
      -- ^ When True, click on empty space will cancel GridSelect
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

instance {-# OVERLAPPABLE #-} HasColorizer a where
    defaultColorizer _ isFg =
        let getColor = if isFg then focusedBorderColor else normalBorderColor
        in asks $ (, "black") . getColor . config

instance HasColorizer a => Default (GSConfig a) where
    def = buildDefaultGSConfig defaultColorizer

type TwoDPosition = (Integer, Integer)

type TwoDElementMap a = [(TwoDPosition,(String,a))]

data TwoDState a = TwoDState { td_curpos :: TwoDPosition
                             , td_availSlots :: [TwoDPosition]
                             , td_elements :: [(String,a)]
                             , td_gsconfig :: GSConfig a
                             , td_font :: XMonadFont
                             , td_paneX :: Integer
                             , td_paneY :: Integer
                             , td_drawingWin :: Window
                             , td_searchString :: String
                             , td_elementmap :: TwoDElementMap a
                             }

generateElementmap :: TwoDState a -> X (TwoDElementMap a)
generateElementmap s = do
    rearrangedElements <- rearranger searchString sortedElements
    return $ zip positions rearrangedElements
  where
    TwoDState {td_availSlots = positions,
               td_gsconfig = gsconfig,
               td_searchString = searchString} = s
    GSConfig {gs_rearranger = rearranger} = gsconfig
    -- Filter out any elements that don't contain the searchString (case insensitive)
    filteredElements = L.filter ((searchString `isInfixOfI`) . fst) (td_elements s)
    -- Sorts the elementmap
    sortedElements = orderElementmap searchString filteredElements
    -- Case Insensitive version of isInfixOf
    needle `isInfixOfI` haystack = upper needle `isInfixOf` upper haystack
    upper = map toUpper


-- | We enforce an ordering such that we will always get the same result. If the
-- elements position changes from call to call of gridselect, then the shown
-- positions will also change when you search for the same string. This is
-- especially the case when using gridselect for showing and switching between
-- workspaces, as workspaces are usually shown in order of last visited.  The
-- chosen ordering is "how deep in the haystack the needle is" (number of
-- characters from the beginning of the string and the needle).
orderElementmap :: String  -> [(String,a)] -> [(String,a)]
orderElementmap searchString elements = if not $ null searchString then sortedElements else elements
  where
    upper = map toUpper
    -- Calculates a (score, element) tuple where the score is the depth of the (case insensitive) needle.
    calcScore element = ( length $ takeWhile (not . isPrefixOf (upper searchString)) (tails . upper . fst $ element)
                        , element)
    -- Use the score and then the string as the parameters for comparing, making
    -- it consistent even when two strings that score the same, as it will then be
    -- sorted by the strings, making it consistent.
    compareScore = comparing (\(score, (str,_)) -> (score, str))
    sortedElements = map snd . sortBy compareScore $ map calcScore elements


newtype TwoD a b = TwoD { unTwoD :: StateT (TwoDState a) X b }
    deriving (Functor, Applicative, Monad, MonadState (TwoDState a))

liftX ::  X a1 -> TwoD a a1
liftX = TwoD . lift

evalTwoD ::  TwoD a1 a -> TwoDState a1 -> X a
evalTwoD m s = flip evalStateT s $ unTwoD m

diamondLayer :: (Enum a, Num a, Eq a) => a -> [(a, a)]
diamondLayer 0 = [(0,0)]
diamondLayer n =
  -- tr = top right
  --  r = ur ++ 90 degree clock-wise rotation of ur
  let tr = [ (x,n-x) | x <- [0..n-1] ]
      r  = tr ++ map (\(x,y) -> (y,-x)) tr
  in r ++ map (negate *** negate) r

diamond :: (Enum a, Num a, Eq a) => Stream (a, a)
diamond = fromList $ concatMap diamondLayer [0..]

diamondRestrict :: Integer -> Integer -> Integer -> Integer -> [(Integer, Integer)]
diamondRestrict x y originX originY =
  L.filter (\(x',y') -> abs x' <= x && abs y' <= y) .
  map (\(x', y') -> (x' + fromInteger originX, y' + fromInteger originY)) .
  takeS 1000 $ diamond

findInElementMap :: (Eq a) => a -> [(a, b)] -> Maybe (a, b)
findInElementMap pos = find ((== pos) . fst)

drawWinBox :: Window -> XMonadFont -> (String, String) -> String -> Integer -> Integer -> String -> Integer -> Integer -> Integer -> X ()
drawWinBox win font (fg,bg) bc ch cw text x y cp =
  withDisplay $ \dpy -> do
  gc <- liftIO $ createGC dpy win
  bordergc <- liftIO $ createGC dpy win
  liftIO $ do
    Just fgcolor <- initColor dpy fg
    Just bgcolor <- initColor dpy bg
    Just bordercolor <- initColor dpy bc
    setForeground dpy gc fgcolor
    setBackground dpy gc bgcolor
    setForeground dpy bordergc bordercolor
    fillRectangle dpy win gc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
    drawRectangle dpy win bordergc (fromInteger x) (fromInteger y) (fromInteger cw) (fromInteger ch)
  stext <- shrinkWhile (shrinkIt shrinkText)
           (\n -> do size <- liftIO $ textWidthXMF dpy font n
                     return $ size > fromInteger (cw-(2*cp)))
           text
  -- calculate the offset to vertically centre the text based on the ascender and descender
  (asc,desc) <- liftIO $ textExtentsXMF font stext
  let offset = ((ch - fromIntegral (asc + desc)) `div` 2) + fromIntegral asc
  printStringXMF dpy win font gc bg fg (fromInteger (x+cp)) (fromInteger (y+offset)) stext
  liftIO $ freeGC dpy gc
  liftIO $ freeGC dpy bordergc

updateAllElements :: TwoD a ()
updateAllElements =
    do
      s <- get
      updateElements (td_elementmap s)

grayoutElements :: Int -> TwoD a ()
grayoutElements skip =
    do
      s <- get
      updateElementsWithColorizer grayOnly $ drop skip (td_elementmap s)
    where grayOnly _ _ = return ("#808080", "#808080")

updateElements :: TwoDElementMap a -> TwoD a ()
updateElements elementmap = do
      s <- get
      updateElementsWithColorizer (gs_colorizer (td_gsconfig s)) elementmap

updateElementsWithColorizer :: (a -> Bool -> X (String, String)) -> TwoDElementMap a -> TwoD a ()
updateElementsWithColorizer colorizer elementmap = do
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
            colors <- colorizer element (pos == curpos)
            drawWinBox win font
                       colors
                       (gs_bordercolor gsconfig)
                       cellheight
                       cellwidth
                       text
                       (paneX'+x*cellwidth)
                       (paneY'+y*cellheight)
                       (gs_cellpadding gsconfig)
    mapM_ updateElement elementmap

stdHandle :: Event -> TwoD a (Maybe a) -> TwoD a (Maybe a)
stdHandle ButtonEvent{ ev_event_type = t, ev_x = x, ev_y = y } contEventloop
    | t == buttonRelease = do
        s@TwoDState{ td_paneX = px
                   , td_paneY = py
                   , td_gsconfig = GSConfig{ gs_cellheight = ch
                                           , gs_cellwidth = cw
                                           , gs_cancelOnEmptyClick = cancelOnEmptyClick
                                           }
                   } <- get
        let gridX = (fi x - (px - cw) `div` 2) `div` cw
            gridY = (fi y - (py - ch) `div` 2) `div` ch
        case lookup (gridX,gridY) (td_elementmap s) of
             Just (_,el) -> return (Just el)
             Nothing     -> if cancelOnEmptyClick
                            then return Nothing
                            else contEventloop
    | otherwise = contEventloop

stdHandle ExposeEvent{} contEventloop = updateAllElements >> contEventloop

stdHandle _ contEventloop = contEventloop

-- | Embeds a key handler into the X event handler that dispatches key
-- events to the key handler, while non-key event go to the standard
-- handler.
makeXEventhandler :: ((KeySym, String, KeyMask) -> TwoD a (Maybe a)) -> TwoD a (Maybe a)
makeXEventhandler keyhandler = fix $ \me -> join $ liftX $ withDisplay $ \d -> liftIO $ allocaXEvent $ \e -> do
                             maskEvent d (exposureMask .|. keyPressMask .|. buttonReleaseMask) e
                             ev <- getEvent e
                             if ev_event_type ev == keyPress
                               then do
                                  (_, s) <- lookupString $ asKeyEvent e
                                  ks <- keycodeToKeysym d (ev_keycode ev) 0
                                  return $ do
                                      mask <- liftX $ cleanKeyMask <*> pure (ev_state ev)
                                      keyhandler (ks, s, mask)
                               else
                                  return $ stdHandle ev me

-- | When the map contains (KeySym,KeyMask) tuple for the given event,
-- the associated action in the map associated shadows the default key
-- handler
shadowWithKeymap :: M.Map (KeyMask, KeySym) a -> ((KeySym, String, KeyMask) -> a) -> (KeySym, String, KeyMask) -> a
shadowWithKeymap keymap dflt keyEvent@(ks,_,m') = fromMaybe (dflt keyEvent) (M.lookup (m',ks) keymap)

-- Helper functions to use for key handler functions

-- | Closes gridselect returning the element under the cursor
select :: TwoD a (Maybe a)
select = do
  s <- get
  return $ snd . snd <$> findInElementMap (td_curpos s) (td_elementmap s)

-- | Closes gridselect returning no element.
cancel :: TwoD a (Maybe a)
cancel = return Nothing

-- | Sets the absolute position of the cursor.
setPos :: (Integer, Integer) -> TwoD a ()
setPos newPos = do
  s <- get
  let elmap = td_elementmap s
      newSelectedEl = findInElementMap newPos (td_elementmap s)
      oldPos = td_curpos s
  when (isJust newSelectedEl && newPos /= oldPos) $ do
    put s { td_curpos = newPos }
    updateElements (catMaybes [findInElementMap oldPos elmap, newSelectedEl])

-- | Moves the cursor by the offsets specified
move :: (Integer, Integer) -> TwoD a ()
move (dx,dy) = do
  s <- get
  let (x,y) = td_curpos s
      newPos = (x+dx,y+dy)
  setPos newPos

moveNext :: TwoD a ()
moveNext = do
  position <- gets td_curpos
  elems <- gets td_elementmap
  let n = length elems
      m = case findIndex (\p -> fst p == position) elems of
               Nothing -> Nothing
               Just k | k == n-1 -> Just 0
                      | otherwise -> Just (k+1)
  whenJust m $ \i ->
      setPos (fst $ elems !! i)

movePrev :: TwoD a ()
movePrev = do
  position <- gets td_curpos
  elems <- gets td_elementmap
  let n = length elems
      m = case findIndex (\p -> fst p == position) elems of
               Nothing -> Nothing
               Just 0  -> Just (n-1)
               Just k  -> Just (k-1)
  whenJust m $ \i ->
      setPos (fst $ elems !! i)

-- | Apply a transformation function the current search string
transformSearchString :: (String -> String) -> TwoD a ()
transformSearchString f = do
          s <- get
          let oldSearchString = td_searchString s
              newSearchString = f oldSearchString
          when (newSearchString /= oldSearchString) $ do
            -- FIXME curpos might end up outside new bounds
            let s' = s { td_searchString = newSearchString }
            m <- liftX $ generateElementmap s'
            let s'' = s' { td_elementmap = m }
                oldLen = length $ td_elementmap s
                newLen = length $ td_elementmap s''
            -- All the elements in the previous element map should be
            -- grayed out, except for those which will be covered by
            -- elements in the new element map.
            when (newLen < oldLen) $ grayoutElements newLen
            put s''
            updateAllElements

-- | By default gridselect used the defaultNavigation action, which
-- binds left,right,up,down and vi-style h,l,j,k navigation. Return
-- quits gridselect, returning the selected element, while Escape
-- cancels the selection. Slash enters the substring search mode. In
-- substring search mode, every string-associated keystroke is
-- added to a search string, which narrows down the object
-- selection. Substring search mode comes back to regular navigation
-- via Return, while Escape cancels the search. If you want that
-- navigation style, add 'defaultNavigation' as 'gs_navigate' to your
-- 'GSConfig' object. This is done by 'buildDefaultGSConfig' automatically.
defaultNavigation :: TwoD a (Maybe a)
defaultNavigation = makeXEventhandler $ shadowWithKeymap navKeyMap navDefaultHandler
  where navKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_slash)      , substringSearch defaultNavigation)
          ,((0,xK_Left)       , move (-1,0) >> defaultNavigation)
          ,((0,xK_h)          , move (-1,0) >> defaultNavigation)
          ,((0,xK_Right)      , move (1,0) >> defaultNavigation)
          ,((0,xK_l)          , move (1,0) >> defaultNavigation)
          ,((0,xK_Down)       , move (0,1) >> defaultNavigation)
          ,((0,xK_j)          , move (0,1) >> defaultNavigation)
          ,((0,xK_Up)         , move (0,-1) >> defaultNavigation)
          ,((0,xK_k)          , move (0,-1) >> defaultNavigation)
          ,((0,xK_Tab)        , moveNext >> defaultNavigation)
          ,((0,xK_n)          , moveNext >> defaultNavigation)
          ,((shiftMask,xK_Tab), movePrev >> defaultNavigation)
          ,((0,xK_p)          , movePrev >> defaultNavigation)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navDefaultHandler = const defaultNavigation

-- | This navigation style combines navigation and search into one mode at the cost of losing vi style
-- navigation. With this style, there is no substring search submode,
-- but every typed character is added to the substring search.
navNSearch :: TwoD a (Maybe a)
navNSearch = makeXEventhandler $ shadowWithKeymap navNSearchKeyMap navNSearchDefaultHandler
  where navNSearchKeyMap = M.fromList [
           ((0,xK_Escape)     , cancel)
          ,((0,xK_Return)     , select)
          ,((0,xK_Left)       , move (-1,0) >> navNSearch)
          ,((0,xK_Right)      , move (1,0) >> navNSearch)
          ,((0,xK_Down)       , move (0,1) >> navNSearch)
          ,((0,xK_Up)         , move (0,-1) >> navNSearch)
          ,((0,xK_Tab)        , moveNext >> navNSearch)
          ,((shiftMask,xK_Tab), movePrev >> navNSearch)
          ,((0,xK_BackSpace), transformSearchString (\s -> if s == "" then "" else init s) >> navNSearch)
          ]
        -- The navigation handler ignores unknown key symbols, therefore we const
        navNSearchDefaultHandler (_,s,_) = do
          transformSearchString (++ s)
          navNSearch

-- | Navigation submode used for substring search. It returns to the
-- first argument navigation style when the user hits Return.
substringSearch :: TwoD a (Maybe a) -> TwoD a (Maybe a)
substringSearch returnNavigation = fix $ \me ->
  let searchKeyMap = M.fromList [
           ((0,xK_Escape)   , transformSearchString (const "") >> returnNavigation)
          ,((0,xK_Return)   , returnNavigation)
          ,((0,xK_BackSpace), transformSearchString (\s -> if s == "" then "" else init s) >> me)
          ]
      searchDefaultHandler (_,s,_) = do
          transformSearchString (++ s)
          me
  in makeXEventhandler $ shadowWithKeymap searchKeyMap searchDefaultHandler


-- FIXME probably move that into Utils?
-- Conversion scheme as in http://en.wikipedia.org/wiki/HSV_color_space
hsv2rgb :: Fractional a => (Integer,a,a) -> (a,a,a)
hsv2rgb (h,s,v) =
    let hi = div h 60 `mod` 6 :: Integer
        f = ((fromInteger h/60) - fromInteger hi) :: Fractional a => a
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
        (r,g,b) = hsv2rgb (seed 83 `mod` 360,
                           fromInteger (seed 191 `mod` 1000)/2500+0.4,
                           fromInteger (seed 121 `mod` 1000)/2500+0.4)
    in if active
         then return ("#faff69", "black")
         else return ("#" ++ concatMap (twodigitHex.(round :: Double -> Word8).(*256)) [r, g, b], "white")

-- | Colorize a window depending on it's className.
fromClassName :: Window -> Bool -> X (String, String)
fromClassName w active = runQuery className w >>= flip defaultColorizer active

twodigitHex :: Word8 -> String
twodigitHex = printf "%02x"

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
-- We create a random seed from the hash of all characters
-- in the string, and use it to generate a ratio between 0 and 1
stringToRatio :: String -> Double
stringToRatio "" = 0
stringToRatio s = let gen = mkStdGen $ foldl' (\t c -> t * 31 + fromEnum c) 0 s
                  in fst $ randomR (0, 1) gen

-- | Brings up a 2D grid of elements in the center of the screen, and one can
-- select an element with cursors keys. The selected element is returned.
gridselect :: GSConfig a -> [(String,a)] -> X (Maybe a)
gridselect _ [] = return Nothing
gridselect gsconfig elements =
 withDisplay $ \dpy -> do
    rootw <- asks theRoot
    scr <- gets $ screenRect . W.screenDetail . W.current . windowset
    win <- liftIO $ mkUnmanagedWindow dpy (defaultScreenOfDisplay dpy) rootw
                    (rect_x scr) (rect_y scr) (rect_width scr) (rect_height scr)
    liftIO $ mapWindow dpy win
    liftIO $ selectInput dpy win (exposureMask .|. keyPressMask .|. buttonReleaseMask)
    status <- io $ grabKeyboard dpy win True grabModeAsync grabModeAsync currentTime
    void $ io $ grabPointer dpy win True buttonReleaseMask grabModeAsync grabModeAsync none none currentTime
    font <- initXMF (gs_font gsconfig)
    let screenWidth = toInteger $ rect_width scr
        screenHeight = toInteger $ rect_height scr
    selectedElement <- if status == grabSuccess then do
                            let restriction ss cs = (fromInteger ss/fromInteger (cs gsconfig)-1)/2 :: Double
                                restrictX = floor $ restriction screenWidth gs_cellwidth
                                restrictY = floor $ restriction screenHeight gs_cellheight
                                originPosX = floor $ (gs_originFractX gsconfig - (1/2)) * 2 * fromIntegral restrictX
                                originPosY = floor $ (gs_originFractY gsconfig - (1/2)) * 2 * fromIntegral restrictY
                                coords = diamondRestrict restrictX restrictY originPosX originPosY
                                s = TwoDState { td_curpos = NE.head (notEmpty coords),
                                                td_availSlots = coords,
                                                td_elements = elements,
                                                td_gsconfig = gsconfig,
                                                td_font = font,
                                                td_paneX = screenWidth,
                                                td_paneY = screenHeight,
                                                td_drawingWin = win,
                                                td_searchString = "",
                                                td_elementmap = [] }
                            m <- generateElementmap s
                            evalTwoD (updateAllElements >> gs_navigate gsconfig)
                                     (s { td_elementmap = m })
                      else
                          return Nothing
    liftIO $ do
      unmapWindow dpy win
      destroyWindow dpy win
      ungrabPointer dpy currentTime
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
    for_ mbWindow callback

windowMap :: X [(String,Window)]
windowMap = do
    ws <- gets windowset
    mapM keyValuePair (W.allWindows ws)
 where keyValuePair w = (, w) <$> decorateName' w

decorateName' :: Window -> X String
decorateName' w = do
  show <$> getName w

-- | Builds a default gs config from a colorizer function.
buildDefaultGSConfig :: (a -> Bool -> X (String,String)) -> GSConfig a
buildDefaultGSConfig col = GSConfig 50 130 10 col "xft:Sans-8" defaultNavigation noRearranger (1/2) (1/2) "white" True

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

-- | Select a workspace and view it using the given function
-- (normally 'W.view' or 'W.greedyView')
--
-- Another option is to shift the current window to the selected workspace:
--
-- > gridselectWorkspace (\ws -> W.greedyView ws . W.shift ws)
gridselectWorkspace :: GSConfig WorkspaceId ->
                          (WorkspaceId -> WindowSet -> WindowSet) -> X ()
gridselectWorkspace conf viewFunc = gridselectWorkspace' conf (windows . viewFunc)

-- | Select a workspace and run an arbitrary action on it.
gridselectWorkspace' :: GSConfig WorkspaceId -> (WorkspaceId -> X ()) -> X ()
gridselectWorkspace' conf func = withWindowSet $ \ws -> do
    let wss = map W.tag $ W.hidden ws ++ map W.workspace (W.current ws : W.visible ws)
    gridselect conf (zip wss wss) >>= flip whenJust func

-- $rearrangers
--
-- Rearrangers allow for arbitrary post-filter rearranging of the grid
-- elements.
--
-- For example, to be able to switch to a new dynamic workspace by typing
-- in its name, you can use the following keybinding action:
--
-- > import XMonad.Actions.DynamicWorkspaces (addWorkspace)
-- >
-- > gridselectWorkspace' def
-- >                          { gs_navigate   = navNSearch
-- >                          , gs_rearranger = searchStringRearrangerGenerator id
-- >                          }
-- >                      addWorkspace

-- | A function taking the search string and a list of elements, and
-- returning a potentially rearranged list of elements.
type Rearranger a = String -> [(String, a)] -> X [(String, a)]

-- | A rearranger that leaves the elements unmodified.
noRearranger :: Rearranger a
noRearranger _ = return

-- | A generator for rearrangers that append a single element based on the
-- search string, if doing so would not be redundant (empty string or value
-- already present).
searchStringRearrangerGenerator :: (String -> a) -> Rearranger a
searchStringRearrangerGenerator f =
    let r "" xs                       = return xs
        r s  xs | s `elem` map fst xs = return xs
                | otherwise           = return $ xs ++ [(s, f s)]
    in r
