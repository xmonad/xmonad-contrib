{-# OPTIONS_GHC -fglasgow-exts #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WindowNavigation
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- WindowNavigation is an extension to allow easy navigation of a workspace.
--
-----------------------------------------------------------------------------

module XMonad.Layout.WindowNavigation (
                                   -- * Usage
                                   -- $usage
                                   windowNavigation, configurableNavigation,
                                   Navigate(..), Direction(..),
                                   MoveWindowToWindow(..),
                                   navigateColor, navigateBrightness,
                                   noNavigateBorders, defaultWNConfig
                                  ) where

import Data.List ( nub, sortBy, (\\) )
import XMonad hiding (Point)
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier
import XMonad.Util.Invisible
import XMonad.Util.XUtils

import XMonad.Hooks.ManageDocks (Direction(..))

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WindowNavigation
--
-- Then edit your @layoutHook@ by adding the WindowNavigation layout modifier
-- to some layout:
--
-- > myLayouts = windowNavigation (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayouts }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- In keybindings:
--
-- >    , ((modMask x,                 xK_Right), sendMessage $ Go R)
-- >    , ((modMask x,                 xK_Left ), sendMessage $ Go L)
-- >    , ((modMask x,                 xK_Up   ), sendMessage $ Go U)
-- >    , ((modMask x,                 xK_Down ), sendMessage $ Go D)
-- >    , ((modMask x .|. controlMask, xK_Right), sendMessage $ Swap R)
-- >    , ((modMask x .|. controlMask, xK_Left ), sendMessage $ Swap L)
-- >    , ((modMask x .|. controlMask, xK_Up   ), sendMessage $ Swap U)
-- >    , ((modMask x .|. controlMask, xK_Down ), sendMessage $ Swap D)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".


data MoveWindowToWindow a = MoveWindowToWindow a a deriving ( Read, Show, Typeable )
instance Typeable a => Message (MoveWindowToWindow a)

data Navigate = Go Direction | Swap Direction | Move Direction deriving ( Read, Show, Typeable )
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
defaultWNConfig = WNC (Just 0.4) "#0000FF" "#00FFFF" "#FF0000" "#FF00FF"

data NavigationState a = NS Point [(a,Rectangle)]

data WindowNavigation a = WindowNavigation WNConfig (Invisible Maybe (NavigationState a)) deriving ( Read, Show )

windowNavigation :: LayoutClass l a => l a -> ModifiedLayout WindowNavigation l a
windowNavigation = ModifiedLayout (WindowNavigation defaultWNConfig (I Nothing))

configurableNavigation :: LayoutClass l a => WNConfig -> l a -> ModifiedLayout WindowNavigation l a
configurableNavigation conf = ModifiedLayout (WindowNavigation conf (I Nothing))

instance LayoutModifier WindowNavigation Window where
    redoLayout (WindowNavigation conf (I state)) rscr s origwrs =
        do XConf { normalBorder = nbc, focusedBorder = fbc, display = dpy } <- ask
           [uc,dc,lc,rc] <-
               case brightness conf of
               Just frac -> do myc <- averagePixels fbc nbc frac
                               return [myc,myc,myc,myc]
               Nothing -> mapM (stringToPixel dpy) [upColor conf, downColor conf,
                                                    leftColor conf, rightColor conf]
           let dirc U = uc
               dirc D = dc
               dirc L = lc
               dirc R = rc
           let w    = W.focus s
               r    = case filter ((==w).fst) origwrs of ((_,x):_) -> x
                                                         []        -> rscr
               pt   = case state of Just (NS ptold _) | ptold `inrect` r -> ptold
                                    _ -> center r
               existing_wins = W.integrate s
               wrs = filter ((`elem` existing_wins) . fst) $ filter ((/=r) . snd) $
                     filter ((/=w) . fst) origwrs
               wnavigable = nub $ concatMap
                            (\d -> truncHead $ navigable d pt wrs) [U,D,R,L]
               wnavigablec = nub $ concatMap
                            (\d -> map (\(win,_) -> (win,dirc d)) $
                                   truncHead $ navigable d pt wrs) [U,D,R,L]
               wothers = case state of Just (NS _ wo) -> map fst wo
                                       _              -> []
           mapM_ (sc nbc) (wothers \\ map fst wnavigable)
           mapM_ (\(win,c) -> sc c win) wnavigablec
           return (origwrs, Just $ WindowNavigation conf $ I $ Just $ NS pt wnavigable)

    handleMessOrMaybeModifyIt (WindowNavigation conf (I (Just (NS pt wrs)))) m
        | Just (Go d) <- fromMessage m =
                         case navigable d pt wrs of
                         []        -> return Nothing
                         ((w,r):_) -> do modify focusWindowHere
                                         return $ Just $ Left $ WindowNavigation conf $ I $ Just $
                                                NS (centerd d pt r) wrs
                             where focusWindowHere :: XState -> XState
                                   focusWindowHere s
                                       | Just w == W.peek (windowset s) = s
                                       | has w $ W.stack $ W.workspace $ W.current $ windowset s =
                                           s { windowset = until ((Just w ==) . W.peek)
                                                           W.focusUp $ windowset s }
                                       | otherwise = s
                                   has _ Nothing         = False
                                   has x (Just (W.Stack t l rr)) = x `elem` (t : l ++ rr)

        | Just (Swap d) <- fromMessage m =
             case navigable d pt wrs of
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
        | Just (Move d) <- fromMessage m =
             case navigable d pt wrs of
             []        -> return Nothing
             ((w,_):_) -> do mst <- gets (W.stack . W.workspace . W.current . windowset)
                             return $ do st <- mst
                                         Just $ Right $ SomeMessage $ MoveWindowToWindow (W.focus st) w
        | Just Hide <- fromMessage m =
                       do XConf { normalBorder = nbc } <- ask
                          mapM_ (sc nbc . fst) wrs
                          return $ Just $ Left $ WindowNavigation conf $ I $ Just $ NS pt []
        | Just ReleaseResources <- fromMessage m =
               handleMessOrMaybeModifyIt (WindowNavigation conf (I $ Just (NS pt wrs))) (SomeMessage Hide)
    handleMessOrMaybeModifyIt _ _ = return Nothing

navigable :: Direction -> Point -> [(Window, Rectangle)] -> [(Window, Rectangle)]
navigable d pt = sortby d . filter (inr d pt . snd)

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
