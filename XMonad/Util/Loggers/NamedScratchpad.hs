-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Loggers.NamedScratchpad
-- Description :  A collection of Loggers for "XMonad.Util.NamedScratchpad".
-- Copyright   :  (c) Brandon S Allbery <allbery.b@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Brandon S Allbery <allbery.b@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- 'XMonad.Util.Loggers' for 'XMonad.Util.NamedScratchpad'
--
-----------------------------------------------------------------------------

module XMonad.Util.Loggers.NamedScratchpad (-- * Usage
                                            -- $usage
                                            nspTrackStartup
                                           ,nspTrackHook
                                           ,nspActiveIcon
                                           ,nspActive
                                           ,nspActive') where

import XMonad.Core
import Graphics.X11.Xlib (Window)
import Graphics.X11.Xlib.Extras (Event(..))
import XMonad.Util.Loggers (Logger)
import XMonad.Util.NamedScratchpad (NamedScratchpad(..))
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Prelude (All (..), chr, foldM, forM)
import qualified Data.IntMap as M
import qualified XMonad.StackSet as W (allWindows)

-- $usage
-- This is a set of 'Logger's for 'NamedScratchpad's.
-- It provides a 'startupHook' and 'handleEventHook' to keep track of
-- 'NamedScratchpad's, and several possible 'Logger's for use in
-- 'XMonad.Hooks.StatusBar.PP.ppExtras'.
--
-- You must add 'nspTrackStartup' to your 'startupHook' to initialize
-- 'NamedScratchpad' tracking and to detect any currently running
-- 'NamedScratchpad's on restart, and 'nspTrackHook' to your 'handleEventHook'
-- to track the coming and going of 'NamedScratchpad's.
--
-- Why would you want to do this? If you aren't using 'EwmhDesktops', this
-- gives you a way to see what 'NamedScratchpad's are running. If you are
-- using 'EwmhDesktops' then you can get that from a taskbar... but you may
-- have noticed that selecting the window from the taskbar moves you to
-- the 'NSP' workspace instead of moving the window to the current workspace.
-- (This is difficult to change; "minimizing" by moving it back to 'NSP'
-- is even harder.)
-- I hide the 'NamedScratchpad's from the taskbar and use this to track
-- them instead (see 'XMonad.Util.NoTaskbar').

-- The extension data for tracking NSP windows
newtype NSPTrack = NSPTrack [Maybe Window]
instance ExtensionClass NSPTrack where
  initialValue = NSPTrack []

-- | 'startupHook' to initialize scratchpad activation tracking
--
-- > , startupHook = ... <+> nspTrackStartup scratchpads
--
-- If you kickstart the 'logHook', do it /after/ 'nspTrackStartup'!
nspTrackStartup :: [NamedScratchpad] -> X ()
nspTrackStartup ns = do
  let ns'i = M.fromList $ zip [0..] $ map (const Nothing) ns
  ns' <- withWindowSet $ foldM (isSp ns) ns'i . W.allWindows
  XS.put (NSPTrack (map snd $ M.toAscList ns'))

isSp :: [NamedScratchpad] -> M.IntMap (Maybe Window) -> Window -> X (M.IntMap (Maybe Window))
isSp ns ws w = do
  n <- runQuery (scratchpadWindow ns) w
  return $ case n of
            Nothing -> ws
            Just n' -> M.insert n' (Just w) ws

scratchpadWindow :: [NamedScratchpad] -> Query (Maybe Int)
scratchpadWindow ns = foldM sp' Nothing (zip [0..] ns)
  where sp' :: Maybe Int -> (Int,NamedScratchpad) -> Query (Maybe Int)
        sp' r@(Just _) _              = return r
        sp' Nothing    (n,NS _ _ q _) = q >>= \p -> return $ if p then Just n else Nothing

-- | 'handleEventHook' to track scratchpad activation/deactivation
--
-- > , handleEventHook = ... <+> nspTrackHook scratchpads
nspTrackHook :: [NamedScratchpad] -> Event -> X All
nspTrackHook _ DestroyWindowEvent{ev_window = w} = do
  XS.modify $ \(NSPTrack ws) -> NSPTrack $ map (\sw -> if sw == Just w then Nothing else sw) ws
  return (All True)
nspTrackHook ns ConfigureRequestEvent{ev_window = w} = do
  NSPTrack ws <- XS.get
  ws' <- forM (zip3 [0 :: Integer ..] ws ns) $ \(_,w',NS _ _ q _) -> do
    p <- runQuery q w
    return $ if p then Just w else w'
  XS.put $ NSPTrack ws'
  return (All True)
nspTrackHook _ _ = return (All True)

-- | 'Logger' for scratchpads' state, using Unicode characters as "icons".
--
-- > , ppExtras = [..., nspActive' iconChars showActive showInactive, ...]
nspActiveIcon :: [Char] -> (String -> String) -> (String -> String) -> Logger
nspActiveIcon icns act inact = do
  NSPTrack ws <- XS.get
  return $ if null ws
            then Nothing
            else let icon' n = if n < length icns then icns !! n else '\NUL'
                     icon  n = let c = icon' n
                                in [if c == '\NUL' then chr (0x2460 + n) else c]
                     ckact n w = let icn = icon n
                                  in case w of
                                      Nothing -> inact icn
                                      Just _  -> act   icn
                     s = unwords $ zipWith ckact [0..] ws
                  in Just s

-- | 'Logger' with String-s (and no defaults)
--
-- > , ppExtras = [..., nspActive iconStrs showActive showInactive, ...]
nspActive :: [String] -> (String -> String) -> (String -> String) -> Logger
nspActive icns act inact = do
  NSPTrack ws <- XS.get
  return $ if null ws
            then Nothing
            else let  ckact n w = let icn = icns !! n
                                    in case w of
                                        Nothing -> inact icn
                                        Just _  -> act   icn
                      s = unwords $ zipWith ckact [0..] ws
                  in Just s

-- | Variant of the above getting the String-s from the 'NamedScratchpad's
nspActive' :: [NamedScratchpad] -> (String -> String) -> (String -> String) -> Logger
nspActive' ns = nspActive (map name ns)
