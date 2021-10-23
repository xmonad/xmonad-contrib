-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.FloatSnap
-- Description :  Snap to other windows or the edge of the screen while moving or resizing.
-- Copyright   :  (c) 2009 Anders Engstrom <ankaan@gmail.com>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Anders Engstrom <ankaan@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Move and resize floating windows using other windows and the edge of the
-- screen as guidelines.
-----------------------------------------------------------------------------

module XMonad.Actions.FloatSnap (
                -- * Usage
                -- $usage
                Direction2D(..),
                snapMove,
                snapGrow,
                snapShrink,
                snapMagicMove,
                snapMagicResize,
                snapMagicMouseResize,
                afterDrag,
                ifClick,
                ifClick') where

import XMonad
import XMonad.Prelude (fromJust, isNothing, listToMaybe, sort, when)
import qualified XMonad.StackSet as W
import qualified Data.Set as S

import XMonad.Hooks.ManageDocks (calcGap)
import XMonad.Util.Types (Direction2D(..))
import XMonad.Actions.AfterDrag

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- >    import XMonad.Actions.FloatSnap
--
-- Then add appropriate key bindings, for example:
--
-- >        , ((modm,               xK_Left),  withFocused $ snapMove L Nothing)
-- >        , ((modm,               xK_Right), withFocused $ snapMove R Nothing)
-- >        , ((modm,               xK_Up),    withFocused $ snapMove U Nothing)
-- >        , ((modm,               xK_Down),  withFocused $ snapMove D Nothing)
-- >        , ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
-- >        , ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
-- >        , ((modm .|. shiftMask, xK_Up),    withFocused $ snapShrink D Nothing)
-- >        , ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".
--
-- And possibly add appropriate mouse bindings, for example:
--
-- >        , ((modm,               button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicMove (Just 50) (Just 50) w)))
-- >        , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> ifClick (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
-- >        , ((modm,               button3), (\w -> focus w >> mouseResizeWindow w >> ifClick (snapMagicResize [R,D] (Just 50) (Just 50) w)))
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".
--
-- Using these mouse bindings, it will not snap while moving, but allow you to click the window once after it has been moved or resized to snap it into place.
-- Note that the order in which the commands are applied in the mouse bindings are important. Snapping can also be used together with other window resizing
-- functions, such as those from "XMonad.Actions.FlexibleResize"
--
-- An alternative set of mouse bindings that will always snap after the drag is:
--
-- >        , ((modm,               button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicMove (Just 50) (Just 50) w)))
-- >        , ((modm .|. shiftMask, button1), (\w -> focus w >> mouseMoveWindow w >> afterDrag (snapMagicResize [L,R,U,D] (Just 50) (Just 50) w)))
-- >        , ((modm,               button3), (\w -> focus w >> mouseResizeWindow w >> afterDrag (snapMagicResize [R,D] (Just 50) (Just 50) w)))
--
-- Interesting values for the distance to look for window in the orthogonal axis are Nothing (to snap against every window), Just 0 (to only snap
-- against windows that we should collide with geometrically while moving) and Just 1 (to also snap against windows we brush against).
--
-- For 'snapMagicMove', 'snapMagicResize' and 'snapMagicMouseResize', try instead setting it to the same as the maximum snapping distance.
--
-- When a value is specified it can be geometrically conceived as adding a border with the specified width around the window and then checking which
-- windows it should collide with.

-- | Resize the window by each edge independently to snap against the closest part of other windows or the edge of the screen. Use the location of the
--   mouse over the window to decide which edges to snap. In corners, the two adjoining edges will be snapped, along the middle of an edge only that edge
--   will be snapped. In the center of the window all edges will snap. Intended to be used together with "XMonad.Actions.FlexibleResize" or
--   "XMonad.Actions.FlexibleManipulate".
snapMagicMouseResize
    :: Rational  -- ^ How big the middle snap area of each axis should be.
    -> Maybe Int -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Maybe Int -- ^ The maximum distance to snap. Use Nothing to not impose any boundary.
    -> Window    -- ^ The window to move and resize.
    -> X ()
snapMagicMouseResize middle collidedist snapdist w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    (_, _, _, px, py, _, _, _) <- io $ queryPointer d w
    let x = (fromIntegral px - wx wa)/ww wa
        y = (fromIntegral py - wy wa)/wh wa
        ml = [L | x <= (0.5 - middle/2)]
        mr = [R | x >  (0.5 + middle/2)]
        mu = [U | y <= (0.5 - middle/2)]
        md = [D | y >  (0.5 + middle/2)]
        mdir = ml++mr++mu++md
        dir = if null mdir
              then [L,R,U,D]
              else mdir
    snapMagicResize dir collidedist snapdist w
    where
        wx = fromIntegral.wa_x
        wy = fromIntegral.wa_y
        ww = fromIntegral.wa_width
        wh = fromIntegral.wa_height

-- | Resize the window by each edge independently to snap against the closest part of other windows or the edge of the screen.
snapMagicResize
    :: [Direction2D] -- ^ The edges to snap.
    -> Maybe Int   -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Maybe Int   -- ^ The maximum distance to snap. Use Nothing to not impose any boundary.
    -> Window      -- ^ The window to move and resize.
    -> X ()
snapMagicResize dir collidedist snapdist w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    (xbegin,xend) <- handleAxis True d wa
    (ybegin,yend) <- handleAxis False d wa

    let xbegin' = if L `elem` dir then xbegin else wx wa
        xend'   = if R `elem` dir then xend   else wx wa + ww wa
        ybegin' = if U `elem` dir then ybegin else wy wa
        yend'   = if D `elem` dir then yend   else wy wa + wh wa

    io $ moveWindow d w (fromIntegral xbegin') (fromIntegral ybegin')
    io $ resizeWindow d w (fromIntegral $ xend' - xbegin') (fromIntegral $ yend' - ybegin')
    float w
    where
        wx = fromIntegral.wa_x
        wy = fromIntegral.wa_y
        ww = fromIntegral.wa_width
        wh = fromIntegral.wa_height

        handleAxis horiz d wa = do
            ((mbl,mbr,bs),(mfl,mfr,fs)) <- getSnap horiz collidedist d w
            let begin = if bs
                        then wpos wa
                        else case (mbl,mbr) of
                            (Just bl,Just br) -> if wpos wa - bl < br - wpos wa then bl else br
                            (Just bl,Nothing) -> bl
                            (Nothing,Just br) -> br
                            (Nothing,Nothing) -> wpos wa
                end = if fs
                      then wpos wa + wdim wa
                      else case (if mfl==Just begin then Nothing else mfl,mfr) of
                          (Just fl,Just fr) -> if wpos wa + wdim wa - fl < fr - wpos wa - wdim wa then fl else fr
                          (Just fl,Nothing) -> fl
                          (Nothing,Just fr) -> fr
                          (Nothing,Nothing) -> wpos wa + wdim wa
                begin' = if isNothing snapdist || abs (begin - wpos wa) <= fromJust snapdist then begin else wpos wa
                end' = if isNothing snapdist || abs (end - wpos wa - wdim wa) <= fromJust snapdist then end else wpos wa + wdim wa
            return (begin',end')
            where
                (wpos, wdim, _, _) = constructors horiz


-- | Move a window by both axises in any direction to snap against the closest part of other windows or the edge of the screen.
snapMagicMove
    :: Maybe Int -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Maybe Int -- ^ The maximum distance to snap. Use Nothing to not impose any boundary.
    -> Window    -- ^ The window to move.
    -> X ()
snapMagicMove collidedist snapdist w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    nx <- handleAxis True d wa
    ny <- handleAxis False d wa

    io $ moveWindow d w (fromIntegral nx) (fromIntegral ny)
    float w
    where
        handleAxis horiz d wa = do
            ((mbl,mbr,bs),(mfl,mfr,fs)) <- getSnap horiz collidedist d w
            return $ if bs || fs
                     then wpos wa
                     else let b = case (mbl,mbr) of
                                    (Just bl,Just br) -> if wpos wa - bl < br - wpos wa then bl else br
                                    (Just bl,Nothing) -> bl
                                    (Nothing,Just br) -> br
                                    (Nothing,Nothing) -> wpos wa
                              f = case (mfl,mfr) of
                                    (Just fl,Just fr) -> if wpos wa + wdim wa - fl < fr - wpos wa - wdim wa then fl else fr
                                    (Just fl,Nothing) -> fl
                                    (Nothing,Just fr) -> fr
                                    (Nothing,Nothing) -> wpos wa
                              newpos = if abs (b - wpos wa) <= abs (f - wpos wa - wdim wa) then b else f - wdim wa
                          in if isNothing snapdist || abs (newpos - wpos wa) <= fromJust snapdist then newpos else wpos wa
            where
                (wpos, wdim, _, _) = constructors horiz

-- | Move a window in the specified direction until it snaps against another window or the edge of the screen.
snapMove
    :: Direction2D -- ^ What direction to move the window in.
    -> Maybe Int -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Window    -- ^ The window to move.
    -> X ()
snapMove L = doSnapMove True True
snapMove R = doSnapMove True False
snapMove U = doSnapMove False True
snapMove D = doSnapMove False False

doSnapMove :: Bool -> Bool -> Maybe Int -> Window -> X ()
doSnapMove horiz rev collidedist w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    ((bl,br,_),(fl,fr,_)) <- getSnap horiz collidedist d w

    let (mb,mf) = if rev then (bl,fl)
                         else (br,fr)

        newpos = fromIntegral $ case (mb,mf) of
                                    (Just b,Nothing) -> b
                                    (Nothing,Just f) -> f - wdim wa
                                    (Just b,Just f) -> if rev /= (b < f - wdim wa)
                                                       then b
                                                       else f - wdim wa
                                    _ -> wpos wa

    if horiz then io $ moveWindow d w newpos (fromIntegral $ wa_y wa)
             else io $ moveWindow d w (fromIntegral $ wa_x wa) newpos
    float w

    where
        (wpos, wdim, _, _) = constructors horiz

-- | Grow the specified edge of a window until it snaps against another window or the edge of the screen.
snapGrow
    :: Direction2D -- ^ What edge of the window to grow.
    -> Maybe Int -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Window    -- ^ The window to grow.
    -> X ()
snapGrow = snapResize True

-- | Shrink the specified edge of a window until it snaps against another window or the edge of the screen.
snapShrink
    :: Direction2D -- ^ What edge of the window to shrink.
    -> Maybe Int -- ^ The distance in the orthogonal axis to look for windows to snap against. Use Nothing to snap against every window.
    -> Window    -- ^ The window to shrink.
    -> X ()
snapShrink = snapResize False

snapResize :: Bool -> Direction2D -> Maybe Int -> Window -> X ()
snapResize grow dir collidedist w = whenX (isClient w) $ withDisplay $ \d ->
  withWindowAttributes d w $ \wa -> do
    mr <- case dir of
              L -> do ((mg,ms,_),(_,_,_)) <- getSnap True collidedist d w
                      return $ case (if grow then mg else ms) of
                                   Just v -> Just (v, wy wa, ww wa + wx wa - v, wh wa)
                                   _ -> Nothing
              R -> do ((_,_,_),(ms,mg,_)) <- getSnap True collidedist d w
                      return $ case (if grow then mg else ms) of
                                   Just v -> Just (wx wa, wy wa, v - wx wa, wh wa)
                                   _ -> Nothing
              U -> do ((mg,ms,_),(_,_,_)) <- getSnap False collidedist d w
                      return $ case (if grow then mg else ms) of
                                   Just v -> Just (wx wa, v, ww wa, wh wa + wy wa - v)
                                   _ -> Nothing
              D -> do ((_,_,_),(ms,mg,_)) <- getSnap False collidedist d w
                      return $ case (if grow then mg else ms) of
                                   Just v -> Just (wx wa, wy wa, ww wa, v - wy wa)
                                   _ -> Nothing

    case mr of
        Nothing -> return ()
        Just (nx,ny,nw,nh) -> when (nw>0 && nh>0) $ do io $ moveWindow d w (fromIntegral nx) (fromIntegral ny)
                                                       io $ resizeWindow d w (fromIntegral nw) (fromIntegral nh)
    float w
    where
        wx = fromIntegral.wa_x
        wy = fromIntegral.wa_y
        ww = fromIntegral.wa_width
        wh = fromIntegral.wa_height


getSnap :: Bool -> Maybe Int -> Display -> Window -> X ((Maybe Int,Maybe Int,Bool),(Maybe Int,Maybe Int,Bool))
getSnap horiz collidedist d w = do
    wa <- io $ getWindowAttributes d w
    screen <- W.current <$> gets windowset
    let sr = screenRect $ W.screenDetail screen
        wl = W.integrate' . W.stack $ W.workspace screen
    gr <- ($ sr) <$> calcGap (S.fromList [minBound .. maxBound])
    wla <- filter (collides wa) <$> io (mapM (getWindowAttributes d) $ filter (/=w) wl)

    return ( neighbours (back wa sr gr wla) (wpos wa)
           , neighbours (front wa sr gr wla) (wpos wa + wdim wa)
           )

    where
        wborder = fromIntegral.wa_border_width

        (wpos, wdim, rpos, rdim) = constructors horiz
        (refwpos, refwdim, _, _) = constructors $ not horiz

        back wa sr gr wla = dropWhile (< rpos sr) $
                            takeWhile (< rpos sr + rdim sr) $
                            sort $ rpos sr:rpos gr:(rpos gr + rdim gr):
                                   foldr (\a as -> wpos a:(wpos a + wdim a + wborder a + wborder wa):as) [] wla

        front wa sr gr wla = dropWhile (<= rpos sr) $
                             takeWhile (<= rpos sr + rdim sr) $
                             sort $ (rpos gr - 2*wborder wa):(rpos gr + rdim gr - 2*wborder wa):(rpos sr + rdim sr - 2*wborder wa):
                                    foldr (\a as -> (wpos a - wborder a - wborder wa):(wpos a + wdim a):as) [] wla

        neighbours l v = ( listToMaybe $ reverse $ takeWhile (< v) l
                         , listToMaybe $ dropWhile (<= v) l
                         , v `elem` l
                         )

        collides wa oa = case collidedist of
                             Nothing -> True
                             Just dist -> refwpos oa - wborder oa < refwpos wa + refwdim wa + wborder wa + dist
                                       && refwpos wa - wborder wa - dist < refwpos oa + refwdim oa + wborder oa


constructors :: Bool -> (WindowAttributes -> Int, WindowAttributes -> Int, Rectangle -> Int, Rectangle -> Int)
constructors True = ( fromIntegral.wa_x
                    , fromIntegral.wa_width
                    , fromIntegral.rect_x
                    , fromIntegral.rect_width
                    )
constructors False = ( fromIntegral.wa_y
                     , fromIntegral.wa_height
                     , fromIntegral.rect_y
                     , fromIntegral.rect_height
                     )
