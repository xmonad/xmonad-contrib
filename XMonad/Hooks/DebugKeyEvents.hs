{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.DebugKeyEvents
-- Description  : Track key events.
-- Copyright    : (c) 2011 Brandon S Allbery <allbery.b@gmail.com>
-- License      : BSD
--
-- Maintainer   : Brandon S Allbery <allbery.b@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- A debugging module to track key events, useful when you can't tell whether
-- xmonad is processing some or all key events.
-----------------------------------------------------------------------------

module XMonad.Hooks.DebugKeyEvents (-- * Usage
                                    -- $usage
                                    debugKeyEvents
                                   ) where

import           XMonad.Core
import           XMonad.Prelude
import           XMonad.Operations               (cleanMask)

import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras

import           Control.Monad.State             (gets)
import           Data.Bits
import           Numeric                         (showHex)
import           System.IO                       (hPutStrLn
                                                 ,stderr)

-- $usage
-- Add this to your handleEventHook to print received key events to the
-- log (the console if you use @startx@/@xinit@, otherwise usually
-- @~/.xsession-errors@).
--
-- >      , handleEventHook = debugKeyEvents
--
-- If you already have a handleEventHook then you should append it:
--
-- >      , handleEventHook = ... <+> debugKeyEvents
--
-- Logged key events look like:
--
-- @keycode 53 sym 120 (0x78, "x") mask 0x0 () clean 0x0 ()@
--
-- The @mask@ and @clean@ indicate the modifiers pressed along with
-- the key; @mask@ is raw, and @clean@ is what @xmonad@ sees after
-- sanitizing it (removing @numberLockMask@, etc.)
--
-- For more detailed instructions on editing the logHook see:
--
-- "XMonad.Doc.Extending#The_log_hook_and_external_status_bars"

-- | Print key events to stderr for debugging
debugKeyEvents :: Event -> X All
debugKeyEvents KeyEvent{ev_event_type = t, ev_state = m, ev_keycode = code}
  | t == keyPress =
      withDisplay $ \dpy -> do
        sym <- io $ keycodeToKeysym dpy code 0
        msk <- cleanMask m
        nl <- gets numberlockMask
        io $ hPutStrLn stderr $ unwords ["keycode"
                                        ,show code
                                        ,"sym"
                                        ,show sym
                                        ," ("
                                        ,hex sym
                                        ," \""
                                        ,keysymToString sym
                                        ,"\") mask"
                                        ,hex m
                                        ,"(" ++ vmask nl m ++ ")"
                                        ,"clean"
                                        ,hex msk
                                        ,"(" ++ vmask nl msk ++ ")"
                                        ]
        return (All True)
debugKeyEvents _ = return (All True)

-- | Convenient showHex variant
hex :: (Integral n, Show n) => n -> String
hex v = "0x" ++ showHex v ""

-- | Convert a modifier mask into a useful string
vmask                 :: KeyMask -> KeyMask -> String
vmask numLockMask msk =  unwords $
                         reverse $
                         fst $
                         foldr vmask' ([],msk) masks
    where
      masks = map (\m -> (m,show m)) [0..toEnum (finiteBitSize msk - 1)] ++
              [(numLockMask,"num"  )
              ,(   lockMask,"lock" )
              ,(controlMask,"ctrl" )
              ,(  shiftMask,"shift")
              ,(   mod5Mask,"mod5" )
              ,(   mod4Mask,"mod4" )
              ,(   mod3Mask,"mod3" )
              ,(   mod2Mask,"mod2" )
              ,(   mod1Mask,"mod1" )
              ]
      vmask'   _   a@( _,0)                = a
      vmask' (m,s)   (ss,v) | v .&. m == m = (s:ss,v .&. complement m)
      vmask'   _        r                  = r
