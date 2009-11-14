-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Dzen
-- Copyright   :  (c) glasser@mit.edu
-- License     :  BSD
--
-- Maintainer  :  glasser@mit.edu
-- Stability   :  stable
-- Portability :  unportable
--
-- Handy wrapper for dzen. Requires dzen >= 0.2.4.
--
-----------------------------------------------------------------------------

module XMonad.Util.Dzen (
    -- * Flexible interface
    dzenConfig,
    timeout,
    font,
    xScreen,
    vCenter,
    hCenter,
    center,
    onCurr,
    x,
    y,
    addArgs,

    -- * Legacy interface
    dzen,
    dzenScreen,
    dzenWithArgs,

    -- * Miscellaneous
    seconds,
    chomp,
    (>=>)
  ) where

import Control.Monad
import Data.List
import XMonad
import XMonad.StackSet
import XMonad.Util.Run (runProcessWithInputAndWait, seconds)

type DzenConfig = (Int, [String]) -> X (Int, [String])

-- | @dzenConfig config s@ will display the string @s@ according to the
-- configuration @config@.  For example, to display the string @\"foobar\"@ with
-- all the default settings, you can simply call
--
-- > dzenConfig return "foobar"
--
-- Or, to set a longer timeout, you could use
--
-- > dzenConfig (timeout 10) "foobar"
--
-- You can combine configurations with the (>=>) operator.  To display
-- @\"foobar\"@ for 10 seconds on the first screen, you could use
--
-- > dzenConfig (timeout 10 >=> xScreen 0) "foobar"
--
-- As a final example, you could adapt the above to display @\"foobar\"@ for
-- 10 seconds on the current screen with
--
-- > dzenConfig (timeout 10 >=> onCurr xScreen) "foobar"
dzenConfig :: DzenConfig -> String -> X ()
dzenConfig conf s = do
    (t, args) <- conf (seconds 3, [])
    runProcessWithInputAndWait "dzen2" args (chomp s) t

-- | dzen wants exactly one newline at the end of its input, so this can be
-- used for your own invocations of dzen.  However, all functions in this
-- module will call this for you.
chomp :: String -> String
chomp = (++"\n") . reverse . dropWhile ('\n' ==) . reverse

-- | Set the timeout, in seconds.  This defaults to 3 seconds if not
-- specified.
timeout :: Rational -> DzenConfig
timeout = timeoutMicro . seconds

-- | Set the timeout, in microseconds.  Mostly here for the legacy
-- interface.
timeoutMicro :: Int -> DzenConfig
timeoutMicro n (_, ss) = return (n, ss)

-- | Add raw command-line arguments to the configuration.  These will be
-- passed on verbatim to dzen2.  The default includes no arguments.
addArgs :: [String] -> DzenConfig
addArgs ss (n, ss') = return (n, ss ++ ss')

-- | Start dzen2 on a particular screen.  Only works with versions of dzen
-- that support the "-xs" argument.
xScreen :: ScreenId -> DzenConfig
xScreen sc = addArgs ["-xs", show (fromIntegral sc + 1 :: Int)]

-- | Take a screen-specific configuration and supply it with the screen ID
-- of the currently focused screen, according to xmonad.  For example, show
-- a 100-pixel wide bar centered within the current screen, you could use
--
-- > dzenConfig (onCurr (hCenter 100)) "foobar"
--
-- Of course, you can still combine these with (>=>); for example, to center
-- the string @\"foobar\"@ both horizontally and vertically in a 100x14 box
-- using the lovely Terminus font, you could use
--
-- > terminus = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
-- > dzenConfig (onCurr (center 100 14) >=> font terminus) "foobar"
onCurr :: (ScreenId -> DzenConfig) -> DzenConfig
onCurr f conf = gets (screen . current . windowset) >>= flip f conf

-- | Put the top of the dzen bar at a particular pixel.
x :: Int -> DzenConfig
x n = addArgs ["-x", show n]
-- | Put the left of the dzen bar at a particular pixel.
y :: Int -> DzenConfig
y n = addArgs ["-y", show n]

-- | Specify the font.  Check out xfontsel to get the format of the String
-- right; if your dzen supports xft, then you can supply that here, too.
font :: String -> DzenConfig
font fn = addArgs ["-fn", fn]

-- | @vCenter height sc@ sets the configuration to have the dzen bar appear
-- on screen @sc@ with height @height@, vertically centered with respect to
-- the actual size of that screen.
vCenter :: Int -> ScreenId -> DzenConfig
vCenter = center' rect_height "-h" "-y"

-- | @hCenter width sc@ sets the configuration to have the dzen bar appear
-- on screen @sc@ with width @width@, horizontally centered with respect to
-- the actual size of that screen.
hCenter :: Int -> ScreenId -> DzenConfig
hCenter = center' rect_width  "-w" "-x"

-- | @center width height sc@ sets the configuration to have the dzen bar
-- appear on screen @sc@ with width @width@ and height @height@, centered
-- both horizontally and vertically with respect to the actual size of that
-- screen.
center :: Int -> Int -> ScreenId -> DzenConfig
center width height sc = hCenter width sc >=> vCenter height sc

-- Center things along a single dimension on a particular screen.
center' :: (Rectangle -> Dimension) -> String -> String -> Int -> ScreenId -> DzenConfig
center' selector extentName positionName extent sc conf = do
    rect <- gets (detailFromScreenId sc . windowset)
    case rect of
        Nothing -> return conf
        Just r  -> addArgs
            [extentName  , show extent,
             positionName, show ((fromIntegral (selector r) - extent) `div` 2),
             "-xs"       , show (fromIntegral sc + 1 :: Int)
            ] conf

-- Get the rectangle outlining a particular screen.
detailFromScreenId :: ScreenId -> WindowSet -> Maybe Rectangle
detailFromScreenId sc ws = fmap screenRect maybeSD where
    c       = current ws
    v       = visible ws
    mapping = map (\s -> (screen s, screenDetail s)) (c:v)
    maybeSD = lookup sc mapping

-- | @dzen str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds.
-- Example usage:
--
-- > dzen "Hi, mom!" (5 `seconds`)
dzen :: String -> Int -> X ()
dzen = flip (dzenConfig . timeoutMicro)

-- | @dzen str args timeout@ pipes @str@ to dzen2 for @timeout@ seconds, passing @args@ to dzen.
-- Example usage:
--
-- > dzenWithArgs "Hi, dons!" ["-ta", "r"] (5 `seconds`)
dzenWithArgs :: String -> [String] -> Int -> X ()
dzenWithArgs str args t = dzenConfig (timeoutMicro t >=> addArgs args) str

-- | @dzenScreen sc str timeout@ pipes @str@ to dzen2 for @timeout@ microseconds, and on screen @sc@.
-- Requires dzen to be compiled with Xinerama support.
dzenScreen :: ScreenId -> String -> Int -> X ()
dzenScreen sc str t = dzenConfig (timeoutMicro t >=> xScreen sc) str
