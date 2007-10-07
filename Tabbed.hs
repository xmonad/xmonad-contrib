{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.Tabbed
-- Copyright   :  (c) 2007 David Roundy, Andrea Rossato
-- License     :  BSD-style (see xmonad/LICENSE)
-- 
-- Maintainer  :  droundy@darcs.net, andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A tabbed layout for the Xmonad Window Manager 
--
-----------------------------------------------------------------------------

module XMonadContrib.Tabbed ( 
                             -- * Usage:
                             -- $usage
                             tabbed
                            , shrinkText
                            , TConf (..), defaultTConf
                            ) where

import Control.Monad.State ( gets )
import Control.Monad.Reader
import Data.Maybe
import Data.Bits
import Data.List

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import XMonad
import Operations
import qualified StackSet as W

import XMonadContrib.NamedWindows
import XMonadContrib.Invisible
import XMonadContrib.XUtils

-- $usage
-- You can use this module with the following in your configuration file:
--
-- > import XMonadContrib.Tabbed
--
-- > defaultLayouts :: [(String, SomeLayout Window)]
-- > defaultLayouts = [SomeLayout tiled
-- >                  ,SomeLayout $ Mirror tiled
-- >                  -- Extension-provided layouts
-- >                  ,SomeLayout $ tabbed shrinkText defaultTConf)
-- >                  , ... ]
--
-- You can also edit the default configuration options.
--
-- > myTabConfig = defaultTConf { inactiveBorderColor = "#FF0000"
-- >                            , activeTextColor = "#00FF00"}
--
-- and
--
-- > defaultLayouts = [ ...
-- >                  , tabbed shrinkText myTabConfig ]

-- %import XMonadContrib.Tabbed
-- %layout , tabbed shrinkText defaultTConf

tabbed :: Shrinker -> TConf -> Tabbed a
tabbed s t = Tabbed (I Nothing) (I (Just s)) t

data TConf = 
    TConf { activeColor         :: String
          , inactiveColor       :: String
          , activeBorderColor   :: String
          , inactiveTextColor   :: String
          , inactiveBorderColor :: String
          , activeTextColor     :: String
          , fontName            :: String
          , tabSize             :: Int
          } deriving (Show, Read)

defaultTConf :: TConf
defaultTConf = 
    TConf { activeColor         = "#999999"
          , inactiveColor       = "#666666"
          , activeBorderColor   = "#FFFFFF"
          , inactiveBorderColor = "#BBBBBB"
          , activeTextColor     = "#FFFFFF"
          , inactiveTextColor   = "#BFBFBF"
          , fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
          , tabSize             = 20
          }

data TabState = 
    TabState { tabsWindows :: [(Window,Window)]
             , scr         :: Rectangle
             , fontS       :: FontStruct -- FontSet
    }

data Tabbed a = 
    Tabbed (Invisible Maybe TabState) (Invisible Maybe Shrinker) TConf
    deriving (Show, Read)

instance LayoutClass Tabbed Window where
    doLayout (Tabbed ist ishr conf) = doLay ist ishr conf
    handleMessage                   = handleMess
    description _                   = "Tabbed"

doLay :: Invisible Maybe TabState -> Invisible Maybe Shrinker -> TConf 
      -> Rectangle -> W.Stack Window -> X ([(Window, Rectangle)], Maybe (Tabbed Window))
doLay ist ishr c sc (W.Stack w [] []) = do
  whenIJust ist $ \st -> mapM_ deleteWindow (map fst $ tabsWindows st)
  return ([(w,sc)], Just $ Tabbed (I Nothing) ishr c)
doLay ist ishr conf sc@(Rectangle _ _ wid _) s@(W.Stack w _ _) = do
  let ws = W.integrate s
      width = wid `div` fromIntegral (length ws)
      -- initialize state
  st <- case ist of
          (I Nothing  ) -> initState conf sc ws
          (I (Just ts)) -> if map snd (tabsWindows ts) == ws && scr ts == sc
                           then return ts
                           else do mapM_ deleteWindow (map fst $ tabsWindows ts)
                                   tws <- createTabs conf sc ws
                                   return (ts {scr = sc, tabsWindows = zip tws ws})
  mapM_ showWindow $ map fst $ tabsWindows st
  mapM_ (updateTab ishr conf (fontS st) width) $ tabsWindows st
  return ([(w,shrink conf sc)], Just (Tabbed (I (Just st)) ishr conf))

handleMess :: Tabbed Window -> SomeMessage -> X (Maybe (Tabbed Window))
handleMess (Tabbed (I (Just st@(TabState {tabsWindows = tws}))) ishr conf) m
    | Just e <- fromMessage m :: Maybe Event = handleEvent ishr conf st e     >> return Nothing
    | Just Hide             == fromMessage m = mapM_ hideWindow (map fst tws) >> return Nothing
    | Just ReleaseResources == fromMessage m = do mapM_ deleteWindow $ map fst tws
                                                  releaseFont (fontS st)
                                                  return $ Just $ Tabbed (I Nothing) (I Nothing) conf
handleMess _ _  = return Nothing

handleEvent :: Invisible Maybe Shrinker -> TConf -> TabState -> Event -> X ()
-- button press
handleEvent ishr conf (TabState    {tabsWindows = tws,   scr          = screen, fontS         = fs }) 
                      (ButtonEvent {ev_window   = thisw, ev_subwindow = thisbw, ev_event_type = t  })
    | t == buttonPress, tl <- map fst tws, thisw `elem` tl || thisbw `elem` tl  = do
  case lookup thisw tws of 
    Just x  -> do focus x
                  updateTab ishr conf fs width (thisw, x)
    Nothing -> return ()
    where width = rect_width screen `div` fromIntegral (length tws)
-- propertyNotify
handleEvent ishr conf (TabState      {tabsWindows = tws, scr = screen, fontS = fs }) 
                      (PropertyEvent {ev_window   = thisw })
    | thisw `elem` (map snd tws) = do
  let tabwin = (fst $ fromJust $ find ((== thisw) . snd) tws, thisw)
  updateTab ishr conf fs width tabwin
    where width = rect_width screen `div` fromIntegral (length tws)
-- expose
handleEvent ishr conf (TabState {tabsWindows = tws, scr = screen, fontS = fs }) 
                      (ExposeEvent {ev_window   = thisw })
    | thisw `elem` (map fst tws) = do
  updateTab ishr conf fs width (thisw, fromJust $ lookup thisw tws)
    where width = rect_width screen `div` fromIntegral (length tws)
handleEvent _ _ _ _ =  return ()

initState :: TConf -> Rectangle -> [Window] -> X TabState
initState conf sc ws = do
  fs  <- initFont (fontName conf)
  tws <- createTabs conf sc ws
  return $ TabState (zip tws ws) sc fs

createTabs :: TConf -> Rectangle -> [Window] -> X [Window]
createTabs _ _ [] = return []
createTabs c (Rectangle x y wh ht) owl@(ow:ows) = do
  let wid    = wh `div` (fromIntegral $ length owl)
      height = fromIntegral $ tabSize c
      mask   = Just (exposureMask .|. buttonPressMask)
  d  <- asks display
  w  <- createNewWindow (Rectangle x y wid height) mask
  io $ restackWindows d $ w : [ow]
  ws <- createTabs c (Rectangle (x + fromIntegral wid) y (wh - wid) ht) ows
  return (w:ws)

updateTab :: Invisible Maybe Shrinker -> TConf -> FontStruct -> Dimension -> (Window,Window) -> X ()
updateTab ishr c fs wh (tabw,ow) = do
  nw <- getName ow
  let ht                   = fromIntegral $ tabSize c :: Dimension
      focusColor win ic ac = (maybe ic (\focusw -> if focusw == win 
                                                   then ac else ic) . W.peek) 
                             `fmap` gets windowset
  (bc',borderc',tc') <- focusColor ow
                           (inactiveColor c, inactiveBorderColor c, inactiveTextColor c) 
                           (activeColor   c, activeBorderColor   c, activeTextColor   c)
  let s    = fromIMaybe shrinkText ishr 
      name = shrinkWhile s (\n -> textWidth fs n >
                            fromIntegral wh - fromIntegral (ht `div` 2)) (show nw)
  paintAndWrite tabw fs wh ht 1 bc' borderc' tc' bc' AlignCenter name

shrink :: TConf -> Rectangle -> Rectangle
shrink c (Rectangle x y w h) = 
    Rectangle x (y + fromIntegral (tabSize c)) w (h - fromIntegral (tabSize c))

type Shrinker = String -> [String]

shrinkWhile :: Shrinker -> (String -> Bool) -> String -> String
shrinkWhile sh p x = sw $ sh x
    where sw [n] = n
          sw [] = ""
          sw (n:ns) | p n = sw ns
                    | otherwise = n

shrinkText :: Shrinker
shrinkText "" = [""]
shrinkText cs = cs : shrinkText (init cs)
