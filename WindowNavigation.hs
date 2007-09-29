{-# OPTIONS -fglasgow-exts #-}
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
                                   windowNavigation,
                                   Navigate(..), Direction(..)
                                  ) where

import Graphics.X11.Xlib ( Rectangle(..), Window, setWindowBorder )
import Control.Monad.Reader ( ask, asks )
import Data.List ( nub, sortBy, (\\) )
import XMonad
import qualified StackSet as W
import Operations ( focus, initColor, LayoutMessages(..) )
import XMonadContrib.LayoutModifier
import XMonadContrib.Invisible

-- $usage
-- You can use this module with the following in your Config.hs file:
--
-- > import XMonadContrib.WindowNavigation
-- >
-- > defaultLayout = SomeLayout $ windowNavigation $ LayoutSelection ...
--
-- In keybindings:
--
-- >    , ((modMask, xK_Right), sendMessage $ Go R)
-- >    , ((modMask, xK_Left), sendMessage $ Go L)
-- >    , ((modMask, xK_Up), sendMessage $ Go U)
-- >    , ((modMask, xK_Down), sendMessage $ Go D)

-- %import XMonadContrib.WindowNavigation
-- %keybind , ((modMask, xK_Right), sendMessage $ Go R)
-- %keybind , ((modMask, xK_Left), sendMessage $ Go L)
-- %keybind , ((modMask, xK_Up), sendMessage $ Go U)
-- %keybind , ((modMask, xK_Down), sendMessage $ Go D)
-- %layout -- include 'windowNavigation' in defaultLayout definition above.
-- %layout -- just before the list, like the following (don't uncomment next line):
-- %layout -- defaultLayout = SomeLayout $ windowNavigation $ ...


data Navigate = Go Direction deriving ( Read, Show, Typeable )
data Direction = U | D | R | L deriving ( Read, Show, Eq )
instance Message Navigate

data NavigationState a = NS Point [(a,Rectangle)]

data WindowNavigation a = WindowNavigation (Invisible Maybe (NavigationState a)) deriving ( Read, Show )

windowNavigation = ModifiedLayout (WindowNavigation (I Nothing))

instance LayoutModifier WindowNavigation Window where
    redoLayout (WindowNavigation (I state)) rscr s wrs =
        do XConf { display = dpy, normalBorder = nbc } <- ask
           navigableColor <- io $ (Just `fmap` initColor dpy "#0000FF") `catch` \_ -> return Nothing
           --uc <- io $ (Just `fmap` initColor dpy "#0000FF") `catch` \_ -> return Nothing
           --dc <- io $ (Just `fmap` initColor dpy "#00FFFF") `catch` \_ -> return Nothing
           --lc <- io $ (Just `fmap` initColor dpy "#FF0000") `catch` \_ -> return Nothing
           --rc <- io $ (Just `fmap` initColor dpy "#FF00FF") `catch` \_ -> return Nothing
           --let dirc U = uc
           --    dirc D = dc
           --    dirc L = lc
           --    dirc R = rc
           let w = W.focus s
               r = case filter ((==w).fst) wrs of ((_,x):_) -> x
                                                  [] -> rscr
               pt = case state of Just (NS ptold _) | ptold `inrect` r -> ptold
                                  _ -> center r
               wrs' = filter ((/=w) . fst) wrs
               wnavigable = nub $ concatMap
                            (\d -> truncHead $ sortby d $ filter (inr d pt . snd) wrs') [U,D,R,L]
               --wnavigablec = nub $ concatMap
               --             (\d -> map (\(w,_) -> (w,dirc d)) $
               --                    truncHead $ sortby d $ filter (inr d pt . snd) wrs') [U,D,R,L]
               wothers = case state of Just (NS _ wo) -> map fst wo
                                       _ -> []
           mapM_ (sc (Just nbc)) (wothers \\ map fst wnavigable)
           mapM_ (sc navigableColor) $ map fst wnavigable
           --mapM_ (\(w,c) -> sc c w) wnavigablec
           return (wrs, Just $ WindowNavigation $ I $ Just $ NS pt wnavigable)

    handleMess (WindowNavigation (I (Just (NS pt wrs)))) m
        | Just (Go d) <- fromMessage m =
                         case sortby d $ filter (inr d pt . snd) wrs of
                         [] -> return Nothing
                         ((w,r):_) -> do focus w
                                         return $ Just $ WindowNavigation $ I $ Just $
                                                NS (centerd d pt r) wrs
        | Just Hide <- fromMessage m =
                       do XConf { display = dpy, normalBorder = nbc } <- ask
                          mapM_ (sc (Just nbc) . fst) wrs
                          return $ Just $ WindowNavigation $ I $ Just $ NS pt []
        | Just ReleaseResources <- fromMessage m =
               handleMess (WindowNavigation (I $ Just (NS pt wrs))) (SomeMessage Hide)
    handleMess _ _ = return Nothing

truncHead (x:_) = [x]
truncHead [] = []

sc mc win = do dpy <- asks display
               case mc of Just c -> io $ setWindowBorder dpy win c
                          Nothing -> return ()

center (Rectangle x y w h) = P (fromIntegral x + fromIntegral w/2)  (fromIntegral y + fromIntegral h/2)
centerd d (P xx yy) (Rectangle x y w h) | d == U || d == D = P xx (fromIntegral y + fromIntegral h/2)
                                        | otherwise = P (fromIntegral x + fromIntegral w/2) yy
inr D (P x y) (Rectangle l yr w h) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y < fromIntegral yr + fromIntegral h
inr U (P x y) (Rectangle l yr w _) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                     y > fromIntegral yr
inr R (P a x) (Rectangle b l _ w) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                    a < fromIntegral b
inr L (P a x) (Rectangle b l c w) = x >= fromIntegral l && x < fromIntegral l + fromIntegral w &&
                                    a > fromIntegral b + fromIntegral c
inrect (P x y) (Rectangle a b w h) = x > fromIntegral a && x < fromIntegral a + fromIntegral w &&
                                     y > fromIntegral b && y < fromIntegral b + fromIntegral h

sortby U = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y' y)
sortby D = sortBy (\(_,Rectangle _ y _ _) (_,Rectangle _ y' _ _) -> compare y y')
sortby R = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x x')
sortby L = sortBy (\(_,Rectangle x _ _ _) (_,Rectangle x' _ _ _) -> compare x' x)

data Point = P Double Double
