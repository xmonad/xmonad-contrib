{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.PhysicalScreens
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
                                      ) where

import XMonad
import qualified XMonad.StackSet as W

import Data.List (sortBy,findIndex)
import Data.Function (on)

{- $usage

This module allows you name Xinerama screens from XMonad using their
physical location relative to each other (as reported by Xinerama),
rather than their @ScreenID@ s, which are arbitrarily determined by
your X server and graphics hardware.

Screens are ordered by the upper-left-most corner, from top-to-bottom
and then left-to-right.

Example usage in your @~\/.xmonad\/xmonad.hs@ file:

> import XMonad.Actions.PhysicalSCreens

> , ((modMask, xK_a), onPrevNeighbour W.view)
> , ((modMask, xK_o), onNextNeighbour W.view)
> , ((modMask .|. shiftMask, xK_a), onPrevNeighbour W.shift)
> , ((modMask .|. shiftMask, xK_o), onNextNeighbour W.shift)

> --
> -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
> -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
> --
> [((modm .|. mask, key), f sc)
>     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
>     , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]

For detailed instructions on editing your key bindings, see
"XMonad.Doc.Extending#Editing_key_bindings".
 -}

-- | The type of the index of a screen by location
newtype PhysicalScreen = P Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | Translate a physical screen index to a "ScreenId"
getScreen :: PhysicalScreen -> X (Maybe ScreenId)
getScreen (P i) = do w <- gets windowset
                     let screens = W.current w : W.visible w
                     if i<0 || i >= length screens
                      then return Nothing
                      else let ss = sortBy (cmpScreen `on` (screenRect . W.screenDetail)) screens
                           in return $ Just $ W.screen $ ss !! i

-- | Switch to a given physical screen
viewScreen :: PhysicalScreen -> X ()
viewScreen p = do i <- getScreen p
                  whenJust i $ \s -> do
                  w <- screenWorkspace s
                  whenJust w $ windows . W.view

-- | Send the active window to a given physical screen
sendToScreen :: PhysicalScreen -> X ()
sendToScreen p = do i <- getScreen p
                    whenJust i $ \s -> do
                      w <- screenWorkspace s
                      whenJust w $ windows . W.shift

-- | Compare two screens by their top-left corners, ordering
-- | top-to-bottom and then left-to-right.
cmpScreen :: Rectangle -> Rectangle -> Ordering
cmpScreen (Rectangle x1 y1 _ _) (Rectangle x2 y2 _ _) = compare (y1,x1) (y2,x2)


-- | Get ScreenId for neighbours of the current screen based on position offset.
getNeighbour :: Int -> X ScreenId
getNeighbour d = do w <- gets windowset
                    let ss = map W.screen $ sortBy (cmpScreen `on` (screenRect . W.screenDetail)) $ W.current w : W.visible w
                        curPos = maybe 0 id $ findIndex (== W.screen (W.current w)) ss
                        pos = (curPos + d) `mod` length ss
                    return $ ss !! pos

neighbourWindows :: Int -> (WorkspaceId -> WindowSet -> WindowSet) -> X ()
neighbourWindows d f = do s <- getNeighbour d
                          w <- screenWorkspace s
                          whenJust w $ windows . f

-- | Apply operation on a WindowSet with the WorkspaceId of the next screen in the physical order as parameter.
onNextNeighbour :: (WorkspaceId -> WindowSet -> WindowSet) -> X ()
onNextNeighbour = neighbourWindows 1

-- | Apply operation on a WindowSet with the WorkspaceId of the previous screen in the physical order as parameter.
onPrevNeighbour :: (WorkspaceId -> WindowSet -> WindowSet) -> X ()
onPrevNeighbour = neighbourWindows (-1)

