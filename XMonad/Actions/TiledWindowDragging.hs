-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.TiledWindowDragging
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides an action that allows you to change the position of windows by dragging them around.
--
-----------------------------------------------------------------------------

module XMonad.Actions.TiledWindowDragging
  (
  -- * Usage
  -- $usage
    dragWindow
  )
where
import           XMonad
import qualified XMonad.StackSet               as W
import           XMonad.Layout.DraggingVisualizer
import           Control.Monad


-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Actions.TiledWindowDragging
-- > import XMonad.Layout.DraggingVisualizer
--
-- then edit your 'layoutHook' by adding the draggingVisualizer to your layout:
-- 
-- > myLayout = draggingVisualizer $ layoutHook def
--
-- Then add a mouse binding for 'dragWindow':
--
-- > , ((modMask .|. shiftMask, button1), dragWindow)
--
-- For detailed instructions on editing your mouse bindings, see
-- "XMonad.Doc.Extending#Editing_mouse_bindings".



-- | Create a mouse binding for this to be able to drag your windows around.
-- You need "XMonad.Layout.DraggingVisualizer" for this to look good.
dragWindow :: Window -> X ()
dragWindow window = whenX (isClient window) $ do
  focus window
  (offsetX, offsetY)                <- getPointerOffset window
  (winX, winY, winWidth, winHeight) <- getWindowPlacement window

  mouseDrag
    (\posX posY ->
      let rect = Rectangle (fInt (fInt winX + (posX - fInt offsetX)))
                           (fInt (fInt winY + (posY - fInt offsetY)))
                           (fInt winWidth)
                           (fInt winHeight)
      in  sendMessage $ DraggingWindow window rect
    )
    (sendMessage DraggingStopped >> performWindowSwitching window)


-- | get the pointer offset relative to the given windows root coordinates
getPointerOffset :: Window -> X (Int, Int)
getPointerOffset win = do
  (_, _, _, oX, oY, _, _, _) <- withDisplay (\d -> io $ queryPointer d win)
  return (fInt oX, fInt oY)

-- | return a tuple of windowX, windowY, windowWidth, windowHeight
getWindowPlacement :: Window -> X (Int, Int, Int, Int)
getWindowPlacement window = do
  wa <- withDisplay (\d -> io $ getWindowAttributes d window)
  return
    (fInt $ wa_x wa, fInt $ wa_y wa, fInt $ wa_width wa, fInt $ wa_height wa)


performWindowSwitching :: Window -> X ()
performWindowSwitching win = do
  root                          <- asks theRoot
  (_, _, selWin, _, _, _, _, _) <- withDisplay (\d -> io $ queryPointer d root)
  ws                            <- gets windowset
  let allWindows = W.index ws
  when ((win `elem` allWindows) && (selWin `elem` allWindows)) $ do
    let allWindowsSwitched = map (switchEntries win selWin) allWindows
    let (ls, t : rs)       = break (== win) allWindowsSwitched
    let newStack           = W.Stack t (reverse ls) rs
    windows $ W.modify' $ const newStack
 where
  switchEntries a b x | x == a    = b
                      | x == b    = a
                      | otherwise = x



-- | shorthand for fromIntegral
fInt :: Integral a => Integral b => a -> b
fInt = fromIntegral


