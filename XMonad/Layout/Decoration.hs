{-# OPTIONS_GHC -fglasgow-exts #-} -- for deriving Typeable
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
    , DecorationMsg (..)
    , Theme (..), defaultTheme
    , shrinkText, CustomShrink ( CustomShrink )
    , Shrinker (..), DefaultShrinker
    , module XMonad.Layout.LayoutModifier
    , isDecoration, fi
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

decoration :: (DecorationStyle ds a, Shrinker s) => s -> Theme -> ds a
           -> l a -> ModifiedLayout (Decoration ds s) l a
decoration s t ds = ModifiedLayout (Decoration (I Nothing) s t ds)

data Theme =
    Theme { activeColor         :: String
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
          } deriving (Show, Read)

defaultTheme :: Theme
defaultTheme =
    Theme { activeColor         = "#999999"
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
          }

data DecorationMsg = SetTheme Theme deriving ( Typeable )
instance Message DecorationMsg

type DecoWin = (Window,Maybe Rectangle)
type OrigWin = (Window,Rectangle)
data DecorationState =
    DS { decos :: [(OrigWin,DecoWin)]
       , font  :: XMonadFont
       }

data Decoration ds s a =
    Decoration (Invisible Maybe DecorationState) s Theme (ds a)
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

instance (DecorationStyle ds Window, Shrinker s) => LayoutModifier (Decoration ds s) Window where
    redoLayout (Decoration st sh c ds) sc stack wrs
        | decorate_first   = do whenIJust st releaseResources
                                return (wrs, Just $ Decoration (I Nothing) sh c ds)
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

          -- We drop any windows that are *precisely* stacked underneath
          -- another window: these must be intended to be tabbed!
          insert_dwr otherRs (((w,r),(dw,Just dr)):zzz)
              | r `elem` otherRs = (dw,dr):insert_dwr otherRs zzz
              | otherwise = (dw,dr):(w, shrink ds dr r):insert_dwr (r:otherRs)  zzz
          insert_dwr otherRs (((w,r),(_ ,Nothing)):zzz) = (w,r):insert_dwr (r:otherRs) zzz
          insert_dwr _ [] = []

          resync _         [] = return []
          resync d ((w,r):xs) = case  w `elemIndex` get_ws d of
                                  Just i  -> do dr   <- decorate ds (decoWidth c) (decoHeight c) sc stack wrs (w,r)
                                                dwrs <- resync d xs
                                                return $ ((w,r),(find_dw i d, dr)) : dwrs
                                  Nothing -> resync d xs

          decorate_first = length wrs == 1 && (not . decorateFirst $ ds)
          processState s = do ndwrs <- resync (decos s) wrs
                              showWindows (getDWs ndwrs)
                              updateDecos sh c (font s) ndwrs
                              return (insert_dwr [] ndwrs, Just (Decoration (I (Just (s {decos = ndwrs}))) sh c ds))

    handleMess (Decoration (I (Just s@(DS {decos = dwrs}))) sh c ds) m
        | Just e <- fromMessage m :: Maybe Event = handleEvent sh c s e      >> return Nothing
        | Just Hide             <- fromMessage m = hideWindows (getDWs dwrs) >> return Nothing
        | Just (SetTheme nc)    <- fromMessage m = do releaseResources s
                                                      return $ Just $ Decoration (I Nothing) sh nc ds
        | Just ReleaseResources <- fromMessage m = do releaseResources s
                                                      return $ Just $ Decoration (I Nothing) sh c  ds
    handleMess _ _ = return Nothing

    emptyLayoutMod (Decoration (I (Just (DS dwrs f))) sh c ds) _ _ = do
        deleteWindows (getDWs dwrs)
        releaseXMF f
        return ([], Just $ Decoration (I Nothing) sh c ds)
    emptyLayoutMod _ _ _  = return ([], Nothing)

    modifierDescription (Decoration _ _ _ ds) = describeDeco ds

handleEvent :: Shrinker s => s -> Theme -> DecorationState -> Event -> X ()
handleEvent sh c (DS dwrs fs) e
    | PropertyEvent {ev_window = w} <- e, w `elem` (map (fst . fst) dwrs) = updateDecos sh c fs dwrs
    | ExposeEvent   {ev_window = w} <- e, w `elem` (map (fst . snd) dwrs) = updateDecos sh c fs dwrs
    | ButtonEvent {ev_window = w,ev_event_type = t} <- e,
      t == buttonPress,
      Just ((mainw,_),_) <- lookFor w dwrs = focus mainw
handleEvent _ _ _ _ = return ()

lookFor :: Window -> [(OrigWin,DecoWin)] -> Maybe (OrigWin,DecoWin)
lookFor w ((x,(w',y)):zs) | w == w' = Just (x,(w',y))
                          | otherwise = lookFor w zs
lookFor _ [] = Nothing

getDWs :: [(OrigWin,DecoWin)] -> [Window]
getDWs = map (fst . snd)

initState ::  Theme -> [(Window,Rectangle)] -> X DecorationState
initState conf wrs = do
  fs   <- initXMF (fontName conf)
  dwrs <- createDecos conf wrs
  return $ DS dwrs fs

releaseResources :: DecorationState -> X ()
releaseResources s = do
  deleteWindows (getDWs $ decos s)
  releaseXMF    (font s)

createDecos :: Theme -> [(Window,Rectangle)] -> X [(OrigWin,DecoWin)]
createDecos _ [] = return []
createDecos c (wr:wrs) = do
  let rect = Rectangle 0 0 1 1
      mask = Just (exposureMask .|. buttonPressMask)
  dw   <- createNewWindow rect mask (inactiveColor c) True
  dwrs <- createDecos c wrs
  return ((wr,(dw,Nothing)):dwrs)

updateDecos :: Shrinker s => s -> Theme -> XMonadFont -> [(OrigWin, DecoWin)] -> X ()
updateDecos s c f = mapM_ $ updateDeco s c f

updateDeco :: Shrinker s => s -> Theme -> XMonadFont -> (OrigWin, DecoWin) -> X ()
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

isDecoration :: Window -> X Bool
isDecoration w = withDisplay (io . flip getWindowAttributes w) >>= return . wa_override_redirect

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
