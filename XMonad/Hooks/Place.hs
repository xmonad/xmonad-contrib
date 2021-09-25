-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.Place
-- Description :  Automatic placement of floating windows.
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Automatic placement of floating windows.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.Place   ( -- * Usage
                              -- $usage

                              -- * Placement actions
                              placeFocused
                            , placeHook

                              -- * Placement policies
                              -- $placements
                            , Placement
                            , smart
                            , simpleSmart
                            , fixed
                            , underMouse
                            , inBounds
                            , withGaps

                              -- * Others
                            , purePlaceWindow ) where


import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as S

import XMonad.Layout.WindowArranger
import XMonad.Actions.FloatKeys

import qualified Data.Map as M
import Data.Ratio ((%))
import Control.Monad.Trans (lift)

-- $usage
-- This module provides a 'ManageHook' that automatically places
-- floating windows at appropriate positions on the screen, as well
-- as an 'X' action to manually trigger repositioning.
--
-- You can use this module by including the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.Place
--
-- and adding 'placeHook' to your 'manageHook', for example:
--
-- > main = xmonad $ def { manageHook = placeHook simpleSmart
-- >                                    <+> manageHook def }
--
-- Note that 'placeHook' should be applied after most other hooks, especially hooks
-- such as 'doFloat' and 'doShift'. Since hooks combined with '<+>' are applied from
-- right to left, this means that 'placeHook' should be the /first/ hook in your chain.
--
-- You can also define a key to manually trigger repositioning with 'placeFocused' by
-- adding the following to your keys definition:
--
-- > , ((modm, xK_w), placeFocused simpleSmart)
--
-- Both 'placeHook' and 'placeFocused' take a 'Placement' parameter, which specifies
-- the placement policy to use (smart, under the mouse, fixed position, etc.). See
-- 'Placement' for a list of available policies.



{- Placement policies -}

-- $placements
-- Placement policies determine how windows will be placed by 'placeFocused' and 'placeHook'.
--
-- A few examples:
--
-- * Basic smart placement
--
-- > myPlacement = simpleSmart
--
-- * Under the mouse (pointer at the top-left corner), but constrained
--   inside of the screen area
--
-- > myPlacement = inBounds (underMouse (0, 0))
--
-- * Smart placement with a preference for putting windows near
-- the center of the screen, and with 16px gaps at the top and bottom
-- of the screen where no window will be placed
--
-- > myPlacement = withGaps (16,0,16,0) (smart (0.5,0.5))


-- | The type of placement policies
data Placement = Smart (Rational, Rational)
               | Fixed (Rational, Rational)
               | UnderMouse (Rational, Rational)
               | Bounds (Dimension, Dimension, Dimension, Dimension) Placement
                 deriving (Show, Read, Eq)


-- | Try to place windows with as little overlap as possible
smart :: (Rational, Rational) -- ^ Where the window should be placed inside
                              -- the available area. See 'fixed'.
      -> Placement
smart = Smart

simpleSmart :: Placement
simpleSmart = inBounds $ smart (0,0)


-- | Place windows at a fixed position
fixed :: (Rational, Rational) -- ^ Where windows should go.
                              --
                              --     * (0,0) -> top left of the screen
                              --
                              --     * (1,0) -> top right of the screen
                              --
                              --     * etc
      -> Placement
fixed = Fixed


-- | Place windows under the mouse
underMouse :: (Rational, Rational) -- ^ Where the pointer should be relative to
                                   -- the window's frame; see 'fixed'.
           -> Placement
underMouse = UnderMouse


-- | Apply the given placement policy, constraining the
-- placed windows inside the screen boundaries.
inBounds :: Placement -> Placement
inBounds = Bounds (0,0,0,0)


-- | Same as 'inBounds', but allows specifying gaps along the screen's edges
withGaps :: (Dimension, Dimension, Dimension, Dimension)
         -- ^ top, right, bottom and left gaps
         -> Placement -> Placement
withGaps = Bounds



{- Placement functions -}


-- | Repositions the focused window according to a placement policy. Works for
-- both \"real\" floating windows and windows in a 'WindowArranger'-based
-- layout.
placeFocused :: Placement -> X ()
placeFocused p = withFocused $ \window -> do
                   info <- gets $ screenInfo . S.current . windowset
                   floats <- gets $ M.keys . S.floating . windowset

                   r'@(Rectangle x' y' _ _) <- placeWindow p window info floats

                     -- use X.A.FloatKeys if the window is floating, send
                     -- a WindowArranger message otherwise.
                   if window `elem` floats
                     then keysMoveWindowTo (x', y') (0, 0) window
                     else sendMessage $ SetGeometry r'


-- | Hook to automatically place windows when they are created.
placeHook :: Placement -> ManageHook
placeHook p = do window <- ask
                 r <- Query $ lift $ getWindowRectangle window
                 allRs <- Query $ lift getAllRectangles
                 pointer <- Query $ lift $ getPointer window

                 return $ Endo $ \theWS -> fromMaybe theWS $
                   do let currentRect = screenRect $ S.screenDetail $ S.current theWS
                          floats = M.keys $ S.floating theWS

                      guard(window `elem` floats )

                        -- Look for the workspace(s) on which the window is to be
                        -- spawned. Each of them also needs an associated screen
                        -- rectangle; for hidden workspaces, we use the current
                        -- workspace's screen.
                      let infos = filter ((window `elem`) . stackContents . S.stack . fst)
                                     $ [screenInfo $ S.current theWS]
                                        ++ map screenInfo (S.visible theWS)
                                        ++ zip (S.hidden theWS) (repeat currentRect)

                      guard(not $ null infos)

                      let (workspace, screen) = head infos
                          rs = mapMaybe (`M.lookup` allRs)
                               $ organizeClients workspace window floats
                          r' = purePlaceWindow p screen rs pointer r
                          newRect = r2rr screen r'
                          newFloats = M.insert window newRect (S.floating theWS)

                      return $ theWS { S.floating = newFloats }


placeWindow :: Placement -> Window
            -> (S.Workspace WorkspaceId (Layout Window) Window, Rectangle)
                 -- ^ The workspace with reference to which the window should be placed,
                 -- and the screen's geometry.
            -> [Window]
                 -- ^ The list of floating windows.
            -> X Rectangle
placeWindow p window (ws, s) floats
  = do (r, rs, pointer) <- getNecessaryData window ws floats
       return $ purePlaceWindow p s rs pointer r


-- | Compute the new position of a window according to a placement policy.
purePlaceWindow :: Placement -- ^ The placement strategy
                -> Rectangle -- ^ The screen
                -> [Rectangle] -- ^ The other visible windows
                -> (Position, Position) -- ^ The pointer's position.
                -> Rectangle -- ^ The window to be placed
                -> Rectangle
purePlaceWindow (Bounds (t,r,b,l) p') (Rectangle sx sy sw sh) rs p w
  = let s' = Rectangle (sx + fi l) (sy + fi t) (sw - l - r) (sh - t - b)
    in checkBounds s' $ purePlaceWindow p' s' rs p w

purePlaceWindow (Fixed ratios) s _ _ w = placeRatio ratios s w

purePlaceWindow (UnderMouse (rx, ry)) _ _ (px, py) (Rectangle _ _ w h)
  = Rectangle (px - truncate (rx * fi w)) (py - truncate (ry * fi h)) w h

purePlaceWindow (Smart ratios) s rs _ w
  = placeSmart ratios s rs (rect_width w) (rect_height w)


-- | Helper: Places a Rectangle at a fixed position indicated by two Rationals
-- inside another,
placeRatio :: (Rational, Rational) -> Rectangle -> Rectangle -> Rectangle
placeRatio (rx, ry) (Rectangle x1 y1 w1 h1) (Rectangle _ _ w2 h2)
  = Rectangle (scale rx x1 (x1 + fi w1 - fi w2))
              (scale ry y1 (y1 + fi h1 - fi h2))
              w2 h2


-- | Helper: Ensures its second parameter is contained inside the first
-- by possibly moving it.
checkBounds :: Rectangle -> Rectangle -> Rectangle
checkBounds (Rectangle x1 y1 w1 h1) (Rectangle x2 y2 w2 h2)
  = Rectangle (max x1 (min (x1 + fi w1 - fi w2) x2))
              (max y1 (min (y1 + fi h1 - fi h2) y2))
              w2 h2





{- Utilities -}

scale :: (RealFrac a, Integral b) => a -> b -> b -> b
scale r n1 n2 = truncate $ r * fi n2 + (1 - r) * fi n1


r2rr :: Rectangle -> Rectangle -> S.RationalRect
r2rr (Rectangle x0 y0 w0 h0) (Rectangle x y w h)
  = S.RationalRect ((fi x-fi x0) % fi w0)
                   ((fi y-fi y0) % fi h0)
                   (fi w % fi w0)
                   (fi h % fi h0)



{- Querying stuff -}

stackContents :: Maybe (S.Stack w) -> [w]
stackContents = maybe [] S.integrate

screenInfo :: S.Screen i l a sid ScreenDetail -> (S.Workspace i l a, Rectangle)
screenInfo S.Screen{ S.workspace = ws, S.screenDetail = (SD s)} = (ws, s)

getWindowRectangle :: Window -> X Rectangle
getWindowRectangle window
  = do d <- asks display
       (_, x, y, w, h, _, _) <- io $ getGeometry d window

         -- We can't use the border width returned by
         -- getGeometry because it will be 0 if the
         -- window isn't mapped yet.
       b <- asks $ borderWidth . config

       return $ Rectangle x y (w + 2*b) (h + 2*b)

getAllRectangles :: X (M.Map Window Rectangle)
getAllRectangles = do ws <- gets windowset
                      let allWindows = join $ map (stackContents . S.stack)
                                         $ (S.workspace . S.current) ws
                                         : (map S.workspace . S.visible) ws
                                         ++ S.hidden ws
                      allRects <- mapM getWindowRectangle allWindows

                      return $ M.fromList $ zip allWindows allRects

organizeClients :: S.Workspace a b Window -> Window -> [Window] -> [Window]
organizeClients ws w floats
  = let (floatCs, layoutCs) = partition (`elem` floats) $ filter (/= w)
                              $ stackContents $ S.stack ws
    in reverse layoutCs ++ reverse floatCs
      -- About the ordering: the smart algorithm will overlap windows
      -- starting ith the head of the list. So:
      --  - we put the non-floating windows first since they'll
      --    probably be below the floating ones,
      --  - we reverse the lists, since the newer/more important
      --    windows are usually near the head.

getPointer :: Window -> X (Position, Position)
getPointer window = do d <- asks display
                       (_,_,_,x,y,_,_,_) <- io $ queryPointer d window
                       return (fi x,fi y)

-- | Return values are, in order: window's rectangle,
-- other windows' rectangles and pointer's coordinates.
getNecessaryData :: Window
                 -> S.Workspace WorkspaceId (Layout Window) Window
                 -> [Window]
                 -> X (Rectangle, [Rectangle], (Position, Position))
getNecessaryData window ws floats
  = do r <- getWindowRectangle window

       rs <- mapM getWindowRectangle (organizeClients ws window floats)

       pointer <- getPointer window

       return (r, rs, pointer)




{- Smart placement algorithm -}

-- | Alternate representation for rectangles.
data SmartRectangle a = SR
  { sr_x0, sr_y0 :: a -- ^ Top left coordinates, inclusive
  , sr_x1, sr_y1 :: a -- ^ Bottom right coorsinates, exclusive
  } deriving (Show, Eq)

r2sr :: Rectangle -> SmartRectangle Position
r2sr (Rectangle x y w h) = SR x y (x + fi w) (y + fi h)

sr2r :: SmartRectangle Position -> Rectangle
sr2r (SR x0 y0 x1 y1) = Rectangle x0 y0 (fi $ x1 - x0) (fi $ y1 - y0)

width :: Num a => SmartRectangle a -> a
width r = sr_x1 r - sr_x0 r

height :: Num a => SmartRectangle a -> a
height r = sr_y1 r - sr_y0 r

isEmpty :: Real a => SmartRectangle a -> Bool
isEmpty r = (width r <= 0) || (height r <= 0)

contains :: Real a => SmartRectangle a -> SmartRectangle a -> Bool
contains r1 r2 = sr_x0 r1 <= sr_x0 r2
                 && sr_y0 r1 <= sr_y0 r2
                 && sr_x1 r1 >= sr_x1 r2
                 && sr_y1 r1 >= sr_y1 r2


-- | Main placement function
placeSmart :: (Rational, Rational) -- ^ point of the screen where windows
                                   -- should be placed first, if possible.
           -> Rectangle -- ^ screen
           -> [Rectangle] -- ^ other clients
           -> Dimension -- ^ width
           -> Dimension -- ^ height
           -> Rectangle
placeSmart (rx, ry) s@(Rectangle sx sy sw sh) rs w h
  = let free = map sr2r $ findSpace (r2sr s) (map r2sr rs) (fi w) (fi h)
    in position free (scale rx sx (sx + fi sw - fi w))
                     (scale ry sy (sy + fi sh - fi h))
                     w h

-- | Second part of the algorithm:
-- Chooses the best position in which to place a window,
-- according to a list of free areas and an ideal position for
-- the top-left corner.
-- We can't use semi-open surfaces for this, so we go back to
-- X11 Rectangles/Positions/etc instead.
position :: [Rectangle] -- ^ Free areas
         -> Position -> Position -- ^ Ideal coordinates
         -> Dimension -> Dimension -- ^ Width and height of the window
         -> Rectangle
position rs x y w h = minimumBy distanceOrder $ map closest rs
  where distanceOrder r1 r2
          = compare (distance (rect_x r1,rect_y r1) (x,y) :: Dimension)
                    (distance (rect_x r2,rect_y r2) (x,y) :: Dimension)
        distance (x1,y1) (x2,y2) = truncate $ (sqrt :: Double -> Double)
                                   $ fi $ (x1 - x2)^(2::Int)
                                        + (y1 - y2)^(2::Int)
        closest r = checkBounds r (Rectangle x y w h)


-- | First part of the algorithm:
-- Tries to find an area in which to place a new
-- rectangle so that it overlaps as little as possible with
-- other rectangles already present. The first rectangles in
-- the list will be overlapped first.
findSpace :: Real a =>
             SmartRectangle a -- ^ The total available area
          -> [SmartRectangle a] -- ^ The parts already in use
          -> a -- ^ Width of the rectangle to place
          -> a -- ^ Height of the rectangle to place
          -> [SmartRectangle a]
findSpace total [] _ _ = [total]
findSpace total rs@(_:rs') w h
  = case filter largeEnough $ cleanup $ subtractRects total rs of
      [] -> findSpace total rs' w h
      as -> as
    where largeEnough r = width r >= w && height r >= h


-- | Subtracts smaller rectangles from a total rectangle
-- , returning a list of remaining rectangular areas.
subtractRects :: Real a => SmartRectangle a
               -> [SmartRectangle a] -> [SmartRectangle a]
subtractRects total [] = [total]
subtractRects total (r:rs)
  = do total' <- subtractRects total rs
       filter (not . isEmpty)
                [ total' {sr_y1 = min (sr_y1 total') (sr_y0 r)} -- Above
                , total' {sr_x0 = max (sr_x0 total') (sr_x1 r)} -- Right
                , total' {sr_y0 = max (sr_y0 total') (sr_y1 r)} -- Below
                , total' {sr_x1 = min (sr_x1 total') (sr_x0 r)} -- Left
                ]


-- | "Nubs" a list of rectangles, dropping all those that are
-- already contained in another rectangle of the list.
cleanup :: Real a => [SmartRectangle a] -> [SmartRectangle a]
cleanup rs = foldr dropIfContained [] $ sortBy sizeOrder rs

sizeOrder :: Real a => SmartRectangle a -> SmartRectangle a -> Ordering
sizeOrder r1 r2 | w1 < w2 = LT
                | w1 == w2 && h1 < h2 = LT
                | w1 == w2 && h1 == h2 = EQ
                | otherwise = GT
                where w1 = width r1
                      w2 = width r2
                      h1 = height r1
                      h2 = height r2

dropIfContained :: Real a => SmartRectangle a
                -> [SmartRectangle a] -> [SmartRectangle a]
dropIfContained r rs  = if any (`contains` r) rs
                        then rs
                        else r:rs
