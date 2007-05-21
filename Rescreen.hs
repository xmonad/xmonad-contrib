-- Grabs new screen information.  Useful for randr setups.
-- To use rescreen, add a keybinding in Config.hs. For example:
-- , ((modMask .|. shiftMask, xK_F12   ), rescreen)

-- TODO Get this code into xmonad when it is ready for randr support.
-- Make it happen automatically on randr events.  It's currently 20 loc, but I
-- believe it can be shrunk a bit.

module XMonadContrib.Rescreen (rescreen) where

import qualified StackSet as W
import XMonad
import Operations

import Graphics.X11.Xlib
import Graphics.X11.Xinerama

import Control.Monad.State
import Control.Monad.Reader
import Data.List (partition)

rescreen :: X ()
rescreen = do
    dpy    <- asks display
    xinesc <- io $ getScreenInfo dpy
    -- TODO: This stuff is necessary because Xlib apparently caches screen
    -- width/height.  Find a better solution later.  I hate Xlib.
    let sx = maximum $ map (\r -> rect_x r + fromIntegral (rect_width  r)) xinesc
        sy = maximum $ map (\r -> rect_y r + fromIntegral (rect_height r)) xinesc
    modify (\s -> s { xineScreens = xinesc, dimensions = (sx, sy) })
    ws <- gets windowset
    let s = W.current ws : W.visible ws
        t = zipWith const [0 :: ScreenId ..] xinesc
        (stay, hide) = partition (\x -> fromIntegral (W.screen x) < length t) s
        newsids = filter (\x -> fromIntegral x >= length s) t
        (newvis, newinvis) = splitAt (length newsids) (map W.workspace hide ++ W.hidden ws)
        (newcurr : xs) = stay ++ zipWith W.Screen newvis newsids
    windows $ const $ ws { W.current = newcurr
                         , W.visible = xs
                         , W.hidden  = newinvis
                         }
