{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.WorkspaceDir
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  David Roundy <droundy@darcs.net>
-- Stability   :  unstable
-- Portability :  unportable
--
-- WindowNavigation is an extension to allow easy navigation of a workspace.
--
-----------------------------------------------------------------------------

module XMonadContrib.WindowNavigation ( 
                                   -- * Usage
                                   -- $usage
                                   windowNavigation, configurableNavigation,
                                   Navigate(..), Direction(..),
                                   navigateColor, navigateBrightness,
                                   noNavigateBorders, defaultWNConfig
                                  ) where

import Graphics.X11.Xlib ( Rectangle(..), Window, Pixel, setWindowBorder )
import Control.Monad.Reader ( ask )
import Data.List ( nub, sortBy, (\\) )
import XMonad
import qualified StackSet as W
import Operations ( windows, focus, LayoutMessages(..) )
import XMonadContrib.LayoutModifier
import XMonadContrib.Invisible
import XMonadContrib.XUtils

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.WindowNavigation
-- >
-- > layoutHook = Layout $ windowNavigation $ Select ...
--
-- or perhaps
--
-- > layoutHook = Layout $ configurableNavigation (navigateBorder "green") $ Select ...
--
-- In keybindings:
--
-- >    , ((modMask, xK_Right), sendMessage $ Go R)
-- >    , ((modMask, xK_Left ), sendMessage $ Go L)
-- >    , ((modMask, xK_Up   ), sendMessage $ Go U)
-- >    , ((modMask, xK_Down ), sendMessage $ Go D)

-- %import XMonadContrib.WindowNavigation
-- %keybind , ((modMask,                 xK_Right), sendMessage $ Go R)
-- %keybind , ((modMask,                 xK_Left ), sendMessage $ Go L)
-- %keybind , ((modMask,                 xK_Up   ), sendMessage $ Go U)
-- %keybind , ((modMask,                 xK_Down ), sendMessage $ Go D)
-- %keybind , ((modMask .|. controlMask, xK_Right), sendMessage $ Swap R)
-- %keybind , ((modMask .|. controlMask, xK_Left ), sendMessage $ Swap L)
-- %keybind , ((modMask .|. controlMask, xK_Up   ), sendMessage $ Swap U)
-- %keybind , ((modMask .|. controlMask, xK_Down ), sendMessage $ Swap D)
-- %layout -- include 'windowNavigation' in layoutHook definition above.
-- %layout -- just before the list, like the following (don't uncomment next line):
-- %layout -- layoutHook = Layout $ windowNavigation $ ...
-- %layout -- or
-- %layout -- layoutHook = Layout $ configurableNavigation (navigateBorder "green") $ ...


data Navigate = Go Direction | Swap Direction deriving ( Read, Show, Typeable )
data Direction = U | D | R | L deriving ( Read, Show, Eq )
instance Message Navigate

data WNConfig = 
    WNC { brightness    :: Maybe Double -- Indicates a fraction of the focus color.
        , upColor       :: String
        , downColor     :: String
        , leftColor     :: String
        , rightColor    :: String
        } deriving (Show, Read)

noNavigateBorders :: WNConfig
noNavigateBorders = 
    defaultWNConfig {brightness = Just 0}

navigateColor :: String -> WNConfig
navigateColor c =
    WNC Nothing c c c c

navigateBrightness :: Double -> WNConfig
navigateBrightness f | f > 1 = navigateBrightness 1
                     | f < 0 = navigateBrightness 0
navigateBrightness f = defaultWNConfig { brightness = Just f }

defaultWNConfig :: WNConfig
defaultWNConfig = WNC (Just 0.5) "#0000FF" "#00FFFF" "#FF0000" "#FF00FF"

data NavigationState a = NS Point [(a,Rectangle)]

data WindowNavigation a = WindowNavigation WNConfig (Invisible Maybe (NavigationState a)) deriving ( Read, Show )

windowNavigation :: LayoutClass l a => l a -> ModifiedLayout WindowNavigation l a
windowNavigation = ModifiedLayout (WindowNavigation defaultWNConfig (I Nothing))

configurableNavigation :: LayoutClass l a => WNConfig -> l a -> ModifiedLayout WindowNavigation l a
configurableNavigation conf = ModifiedLayout (WindowNavigation conf (I Nothing))

instance LayoutModifier WindowNavigation Window where
    redoLayout (WindowNavigation conf (I state)) rscr s wrs =
        do XConf { normalBorder = nbc, focusedBorder = fbc } <- ask
           [uc,dc,lc,rc] <-
               case brightness conf of
               Just frac -> do myc <- averagePixels fbc nbc frac
                               return [myc,myc,myc,myc]
               Nothing -> mapM stringToPixel [upColor conf, downColor conf,
                                              leftColor conf, rightColor conf]
           let dirc U = uc
               dirc D = dc
               dirc L = lc
               dirc R = rc
           let w    = W.focus s
               r    = case filter ((==w).fst) wrs of ((_,x):_) -> x
                                                     []        -> rscr
               pt   = case state of Just (NS ptold _) | ptold `inrect` r -> ptold
                                    _ -> center r
               wrs' = filter ((/=w) . fst) wrs
               wnavigable = nub $ concatMap
                            (\d -> truncHead $ sortby d $ filter (inr d pt . snd) wrs') [U,D,R,L]
               wnavigablec = nub $ concatMap
                            (\d -> map (\(win,_) -> (win,dirc d)) $
                                   truncHead $ sortby d $ filter (inr d pt . snd) wrs') [U,D,R,L]
               wothers = case state of Just (NS _ wo) -> map fst wo
                                       _              -> []
           mapM_ (sc nbc) (wothers \\ map fst wnavigable)
           mapM_ (\(win,c) -> sc c win) wnavigablec
           return (wrs, Just $ WindowNavigation conf $ I $ Just $ NS pt wnavigable)

    handleMess (WindowNavigation conf (I (Just (NS pt wrs)))) m
        | Just (Go d) <- fromMessage m =
                         case sortby d $ filter (inr d pt . snd) wrs of
                         []        -> return Nothing
                         ((w,r):_) -> do focus w
                                         return $ Just $ WindowNavigation conf $ I $ Just $
                                                NS (centerd d pt r) wrs
        | Just (Swap d) <- fromMessage m =
             case sortby d $ filter (inr d pt . snd) wrs of
             []        -> return Nothing
             ((w,_):_) -> do let swap st = unint (W.focus st) $ map (swapw (W.focus st)) $ W.integrate st
                                 swapw y x | x == w = y
                                           | x == y = w
                                           | otherwise = x
                                 unint f xs = case span (/= f) xs of
                                              (u,_:dn) -> W.Stack { W.focus = f
                                                                  , W.up = reverse u
                                                                  , W.down = dn }
                                              _ -> W.Stack { W.focus = f
                                                           , W.down = xs
                                                           , W.up = [] }
                             windows $ W.modify' swap
                             return Nothing
        | Just Hide <- fromMessage m =
                       do XConf { normalBorder = nbc } <- ask
                          mapM_ (sc nbc . fst) wrs
                          return $ Just $ WindowNavigation conf $ I $ Just $ NS pt []
        | Just ReleaseResources <- fromMessage m =
               handleMess (WindowNavigation conf (I $ Just (NS pt wrs))) (SomeMessage Hide)
    handleMess _ _ = return Nothing

truncHead :: [a] -> [a]
truncHead (x:_) = [x]
truncHead [] = []

sc :: Pixel -> Window -> X ()
sc c win = withDisplay $ \dpy -> io $ setWindowBorder dpy win c

center :: Rectangle -> Point
center (Rectangle x y w h) = P (fromIntegral x + fromIntegral w/2)  (fromIntegral y + fromIntegral h/2)

centerd :: Direction -> Point -> Rectangle -> Point
centerd d (P xx yy) (Rectangle x y w h) | d == U || d == D = P xx (fromIntegral y + fromIntegral h/2)
                                        | otherwise = P (fromIntegral x + fromIntegral w/2) yy

inr :: Direction -> Point -> Rectangle -> Bool
inr D (P x y) (Rectangle l yr w h) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y <  fromIntegral yr + fromIntegral h
inr U (P x y) (Rectangle l yr w _) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y >  fromIntegral yr
inr R (P a x) (Rectangle b l _ w)  = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     a <  fromIntegral b
inr L (P a x) (Rectangle b l c w)  = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     a >  fromIntegral b + fromIntegral c

inrect :: Point -> Rectangle -> Bool
inrect (P x y) (Rectangle a b w h) = x >  fromIntegral a && x < fromIntegral a + fromIntegral w &&
                                     y >  fromIntegral b && y < fromIntegral b + fromIntegral h

sortby :: Direction -> [(a,Rectangle)] -> [(a,Rectangle)]
sortby U = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y' y)
sortby D = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y y')
sortby R = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x x')
sortby L = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x' x)

data Point = P Double Double
