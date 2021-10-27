{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.PhysicalScreens
-- Description  : Manipulate screens ordered by physical location instead of ID.
-- Copyright    : (c) Nelson Elhage <nelhage@mit.edu>
-- License      : BSD
--
-- Maintainer   : Nelson Elhage <nelhage@mit.edu>
-- Stability    : unstable
-- Portability  : unportable
--
-- Manipulate screens ordered by physical location instead of ID
-----------------------------------------------------------------------------

module XMonad.Actions.PhysicalScreens (
                                        -- * Usage
                                        -- $usage
                                        PhysicalScreen(..)
                                      , getScreen
                                      , viewScreen
                                      , sendToScreen
                                      , onNextNeighbour
                                      , onPrevNeighbour
                                      , horizontalScreenOrderer
                                      , verticalScreenOrderer
                                      , ScreenComparator(ScreenComparator)
                                      , getScreenIdAndRectangle
                                      , screenComparatorById
                                      , screenComparatorByRectangle
                                      ) where

import XMonad
import XMonad.Prelude (elemIndex, fromMaybe, on, sortBy)
import qualified XMonad.StackSet as W

{- $usage

This module allows you name Xinerama screens from XMonad using their
physical location relative to each other (as reported by Xinerama),
rather than their @ScreenID@ s, which are arbitrarily determined by
your X server and graphics hardware.

You can specify how to order the screen by giving a ScreenComparator.
To create a screen comparator you can use screenComparatorByRectangle or screenComparatorByScreenId.
The default ScreenComparator orders screens by the upper-left-most corner, from top-to-bottom
and then left-to-right.

Example usage in your @~\/.xmonad\/xmonad.hs@ file:

> import XMonad.Actions.PhysicalScreens
> import Data.Default

> , ((modMask, xK_a), onPrevNeighbour def W.view)
> , ((modMask, xK_o), onNextNeighbour def W.view)
> , ((modMask .|. shiftMask, xK_a), onPrevNeighbour def W.shift)
> , ((modMask .|. shiftMask, xK_o), onNextNeighbour def W.shift)

> --
> -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
> -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
> --
> [((modm .|. mask, key), f sc)
>     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
>     , (f, mask) <- [(viewScreen def, 0), (sendToScreen def, shiftMask)]]

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings".
 -}

-- | The type of the index of a screen by location
newtype PhysicalScreen = P Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

getScreenIdAndRectangle :: W.Screen i l a ScreenId ScreenDetail -> (ScreenId, Rectangle)
getScreenIdAndRectangle screen = (W.screen screen, rect) where
  rect = screenRect $ W.screenDetail screen

-- | Translate a physical screen index to a "ScreenId"
getScreen:: ScreenComparator -> PhysicalScreen -> X (Maybe ScreenId)
getScreen (ScreenComparator cmpScreen) (P i) = do w <- gets windowset
                                                  let screens = W.current w : W.visible w
                                                  if i<0 || i >= length screens
                                                    then return Nothing
                                                    else let ss = sortBy (cmpScreen `on` getScreenIdAndRectangle) screens
                                                    in return $ Just $ W.screen $ ss !! i

-- | Switch to a given physical screen
viewScreen :: ScreenComparator -> PhysicalScreen -> X ()
viewScreen sc p = do i <- getScreen sc p
                     whenJust i $ \s -> do
                         w <- screenWorkspace s
                         whenJust w $ windows . W.view

-- | Send the active window to a given physical screen
sendToScreen :: ScreenComparator -> PhysicalScreen -> X ()
sendToScreen sc p = do i <- getScreen sc p
                       whenJust i $ \s -> do
                         w <- screenWorkspace s
                         whenJust w $ windows . W.shift

-- | A ScreenComparator allow to compare two screen based on their coordonate and Xinerama Id
newtype ScreenComparator = ScreenComparator ((ScreenId, Rectangle) -> (ScreenId, Rectangle) -> Ordering)

-- | The default ScreenComparator orders screens by the upper-left-most corner, from top-to-bottom
instance Default ScreenComparator where
  def= verticalScreenOrderer

-- | Compare screen only by their coordonate
screenComparatorByRectangle :: (Rectangle -> Rectangle -> Ordering) -> ScreenComparator
screenComparatorByRectangle rectComparator = ScreenComparator comparator where
  comparator (_, rec1) (_, rec2) = rectComparator rec1 rec2

-- | Compare screen only by their Xinerama id
screenComparatorById :: (ScreenId -> ScreenId -> Ordering) -> ScreenComparator
screenComparatorById idComparator = ScreenComparator comparator where
  comparator (id1, _) (id2, _) = idComparator id1 id2

-- | orders screens by the upper-left-most corner, from top-to-bottom
verticalScreenOrderer :: ScreenComparator
verticalScreenOrderer = screenComparatorByRectangle comparator where
    comparator (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (y1, x1) (y2, x2)

-- | orders screens by the upper-left-most corner, from left-to-right
horizontalScreenOrderer :: ScreenComparator
horizontalScreenOrderer = screenComparatorByRectangle comparator where
    comparator (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (x1, y1) (x2, y2)

-- | Get ScreenId for neighbours of the current screen based on position offset.
getNeighbour :: ScreenComparator -> Int -> X ScreenId
getNeighbour (ScreenComparator cmpScreen) d =
  do w <- gets windowset
     let ss = map W.screen $ sortBy (cmpScreen `on` getScreenIdAndRectangle) $ W.current w : W.visible w
         curPos = fromMaybe 0 $ elemIndex (W.screen (W.current w)) ss
         pos = (curPos + d) `mod` length ss
     return $ ss !! pos

neighbourWindows :: ScreenComparator -> Int -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
neighbourWindows sc d f = do s <- getNeighbour sc d
                             w <- screenWorkspace s
                             whenJust w $ windows . f

-- | Apply operation on a WindowSet with the WorkspaceId of the next screen in the physical order as parameter.
onNextNeighbour :: ScreenComparator -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
onNextNeighbour sc = neighbourWindows sc 1

-- | Apply operation on a WindowSet with the WorkspaceId of the previous screen in the physical order as parameter.
onPrevNeighbour :: ScreenComparator -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
onPrevNeighbour sc = neighbourWindows sc (-1)
