{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Decoration
-- Copyright   :  (c) 2007 Andrea Rossato
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
    , Decoration
    , DecorationStyle (..)
    , DeConfig (..), defaultDeConfig, mkDefaultDeConfig
    , shrinkText, CustomShrink ( CustomShrink )
    , Shrinker (..), DefaultShrinker
    , module XMonad.Layout.LayoutModifier
    , fi
    ) where

import Data.Maybe
import Data.List

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.LayoutModifier
import XMonad.Layout.WindowArranger
import XMonad.Util.NamedWindows
import XMonad.Util.Invisible
import XMonad.Util.XUtils
import XMonad.Util.Font

-- $usage
-- For usage examples you can see "XMonad.Layout.SimpleDecoration",
-- "XMonad.Layout.Tabbed", "XMonad.Layout.DwmStyle",

decoration :: (DecorationStyle ds a, Shrinker s) => s -> DeConfig ds a
           -> l a -> ModifiedLayout (Decoration ds s) l a
decoration s c = ModifiedLayout (Decoration (I Nothing) s c)

data DeConfig ds a =
    DeConfig { activeColor         :: String
             , inactiveColor       :: String
             , urgentColor         :: String
             , activeBorderColor   :: String
             , inactiveBorderColor :: String
             , urgentBorderColor   :: String
             , activeTextColor     :: String
             , inactiveTextColor   :: String
             , urgentTextColor     :: String
             , fontName            :: String
             , decoWidth           :: Dimension
             , decoHeight          :: Dimension
             , style               :: ds a
             } deriving (Show, Read)

mkDefaultDeConfig :: DecorationStyle ds a => ds a -> DeConfig ds a
mkDefaultDeConfig ds =
    DeConfig { activeColor         = "#999999"
             , inactiveColor       = "#666666"
             , urgentColor         = "#FFFF00"
             , activeBorderColor   = "#FFFFFF"
             , inactiveBorderColor = "#BBBBBB"
             , urgentBorderColor   = "##00FF00"
             , activeTextColor     = "#FFFFFF"
             , inactiveTextColor   = "#BFBFBF"
             , urgentTextColor     = "#FF0000"
             , fontName            = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
             , decoWidth           = 200
             , decoHeight          = 20
             , style               = ds
             }

type DecoWin = (Window,Maybe Rectangle)
type OrigWin = (Window,Rectangle)
data DecorationState =
    DS { decos :: [(OrigWin,DecoWin)]
       , font  :: XMonadFont
       }

data Decoration ds s a =
    Decoration (Invisible Maybe DecorationState) s (DeConfig ds a)
    deriving (Show, Read)

class (Read (ds a), Show (ds a)) => DecorationStyle ds a where
    describeDeco :: ds a -> String
    describeDeco ds = show ds

    decorateFirst :: ds a -> Bool
    decorateFirst _ = True

    shrink :: ds a -> Rectangle -> Rectangle -> Rectangle
    shrink _ (Rectangle _ _ _ dh) (Rectangle x y w h) = Rectangle x (y + fi dh) w (h - dh)

    pureDecoration :: ds a -> Dimension -> Dimension -> Rectangle
                   -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> Maybe Rectangle
    pureDecoration _ _ h _ _ _ (_,Rectangle x y w _) = Just $ Rectangle x y w h

    decorate :: ds a -> Dimension -> Dimension -> Rectangle
             -> W.Stack a -> [(a,Rectangle)] -> (a,Rectangle) -> X (Maybe Rectangle)
    decorate ds w h r s ars ar = return $ pureDecoration ds w h r s ars ar

data DefaultStyle a = DefaultStyle deriving (Read, Show)
instance DecorationStyle DefaultStyle a

defaultDeConfig :: DeConfig DefaultStyle a
defaultDeConfig = mkDefaultDeConfig DefaultStyle

instance (DecorationStyle ds Window, Shrinker s) => LayoutModifier (Decoration ds s) Window where
    redoLayout (Decoration st sh c) sc stack wrs
        | decorate_first   = do whenIJust st $ \s -> deleteWindows (getDWs $ decos s)
                                return (wrs, Just $ Decoration (I Nothing) sh c)
        | I Nothing  <- st = initState c wrs >>= processState
        | I (Just s) <- st = do let dwrs  = decos s
                                    (d,a) = curry diff (get_ws dwrs) ws
                                    toDel = todel d dwrs
                                    toAdd = toadd a wrs
                                deleteWindows (getDWs toDel)
                                ndwrs <- createDecos c toAdd
                                processState (s {decos = ndwrs ++ del_dwrs d dwrs })
        | otherwise        = return (wrs, Nothing)

        where
          ws        = map fst wrs
          del_dwrs  = listFromList get_w notElem
          get_w     = fst . fst
          get_ws    = map get_w
          find_dw i = fst . snd . flip (!!) i
          todel   d = filter (flip elem d . get_w)
          toadd   a = filter (flip elem a . fst  )

          insert_dwr ((w,r),(dw,Just dr)) xs = (dw,dr):(w, shrink (style c) dr r):xs
          insert_dwr (x    ,(_ ,Nothing)) xs = x:xs

          resync _         [] = return []
          resync d ((w,r):xs) = case  w `elemIndex` get_ws d of
                                  Just i  -> do dr   <- decorate (style c) (decoWidth c) (decoHeight c) sc stack wrs (w,r)
                                                dwrs <- resync d xs
                                                return $ ((w,r),(find_dw i d, dr)) : dwrs
                                  Nothing -> resync d xs

          decorate_first = length wrs == 1 && (not . decorateFirst . style $ c)
          processState s = do ndwrs <- resync (decos s) wrs
                              showWindows (getDWs ndwrs)
                              updateDecos sh c (font s) ndwrs
                              return (foldr insert_dwr [] ndwrs, Just (Decoration (I (Just (s {decos = ndwrs}))) sh c))

    handleMess (Decoration (I (Just s@(DS {decos = dwrs}))) sh c) m
        | Just e <- fromMessage m :: Maybe Event = handleEvent sh c s e >> return Nothing
        | Just Hide             <- fromMessage m = hideWindows      dws >> return Nothing
        | Just ReleaseResources <- fromMessage m = do deleteWindows dws
                                                      releaseXMF (font s)
                                                      return $ Just $ Decoration (I Nothing) sh c
        where dws = getDWs dwrs

    handleMess _ _ = return Nothing

    emptyLayoutMod (Decoration (I (Just (DS dwrs _))) _ _) _ _ = deleteWindows (getDWs dwrs) >> return ([], Nothing)
    emptyLayoutMod _ _ _  = return ([], Nothing)

    modifierDescription (Decoration _ _ c) = describeDeco $ style c

handleEvent :: (DecorationStyle ds a, Shrinker s) => s -> DeConfig ds a -> DecorationState-> Event -> X ()
handleEvent sh c (DS dwrs fs) e
    | PropertyEvent {ev_window = w} <- e, w `elem` (map (fst . fst) dwrs) = updateDecos sh c fs dwrs
    | ExposeEvent   {ev_window = w} <- e, w `elem` (map (fst . snd) dwrs) = updateDecos sh c fs dwrs
handleEvent _ _ _ _ = return ()

getDWs :: [(OrigWin,DecoWin)] -> [Window]
getDWs = map (fst . snd)

initState ::  DecorationStyle ds a => DeConfig ds a -> [(Window,Rectangle)] -> X DecorationState
initState conf wrs = do
  fs   <- initXMF (fontName conf)
  dwrs <- createDecos conf wrs
  return $ DS dwrs fs

createDecos :: DecorationStyle ds a => DeConfig ds a -> [(Window,Rectangle)] -> X [(OrigWin,DecoWin)]
createDecos _ [] = return []
createDecos c (wr:wrs) = do
  let rect = Rectangle 0 0 1 1
      mask = Just (exposureMask .|. buttonPressMask)
  dw   <- createNewWindow rect mask (inactiveColor c) True
  dwrs <- createDecos c wrs
  return ((wr,(dw,Nothing)):dwrs)

updateDecos :: (DecorationStyle ds a, Shrinker s) => s -> DeConfig ds a -> XMonadFont -> [(OrigWin, DecoWin)] -> X ()
updateDecos s c f = mapM_ $ updateDeco s c f

updateDeco :: (DecorationStyle ds a, Shrinker s) => s -> DeConfig ds a -> XMonadFont -> (OrigWin, DecoWin) -> X ()
updateDeco sh c fs ((w,_),(dw,Just (Rectangle _ _ wh ht))) = do
  nw  <- getName w
  ur  <- readUrgents
  dpy <- asks display
  let focusColor win ic ac uc = (maybe ic (\focusw -> case () of
                                                       _ | focusw == win -> ac
                                                         | win `elem` ur -> uc
                                                         | otherwise     -> ic) . W.peek)
                                `fmap` gets windowset
  (bc,borderc,tc) <- focusColor w
                     (inactiveColor c, inactiveBorderColor c, inactiveTextColor c)
                     (activeColor   c, activeBorderColor   c, activeTextColor   c)
                     (urgentColor   c, urgentBorderColor   c, urgentTextColor   c)
  let s  = shrinkIt sh
  name <- shrinkWhile s (\n -> do
                                size <- io $ textWidthXMF dpy fs n
                                return $ size > fromIntegral wh - fromIntegral (ht `div` 2)) (show nw)
  paintAndWrite dw fs wh ht 1 bc borderc tc bc AlignCenter name
updateDeco _ _ _ (_,(w,Nothing)) = hideWindow w

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
