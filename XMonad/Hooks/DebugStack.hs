-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DebugStack
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2012
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Dump the state of the 'StackSet'.  A @logHook@ and @handleEventHook@ are
-- also provided.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DebugStack (debugStack
                               ,debugStackString
                               ,debugStackLogHook
                               ,debugStackEventHook
                               ) where

import           XMonad.Core
import qualified XMonad.StackSet         as W

import           XMonad.Util.DebugWindow

import           Graphics.X11.Types                  (Window)
import           Graphics.X11.Xlib.Extras            (Event)

import           Control.Monad                       (foldM)
import           Data.Map                            (toList)
import           Data.Monoid                         (All(..))

-- | Print the state of the current window stack to @stderr@, which for most
--   installations goes to @~/.xsession-errors@.  "XMonad.Util.DebugWindow"
--   is used to display the individual windows.
debugStack :: X ()
debugStack =  debugStackString >>= trace

-- | The above packaged as a 'logHook'.  (Currently this is identical.)
debugStackLogHook :: X ()
debugStackLogHook =  debugStack

-- | The above packaged as a 'handleEventHook'.  You almost certainly do not
--   want to use this unconditionally, as it will cause massive amounts of
--   output and possibly slow @xmonad@ down severely.

debugStackEventHook   :: Event -> X All
debugStackEventHook _ =  debugStack >> return (All True)

-- | Dump the state of the current 'StackSet' as a multiline 'String'.
--   @
--   stack [    mm
--         ,(*) ww
--         ,    ww
--         ]
--   float {    ww
--         ,    ww
--         }
--   @
--
-- One thing I'm not sure of is where the zipper is when focus is on a
-- floating window.
debugStackString :: X String
debugStackString =  withWindowSet $ \ws -> do
                      s <- emit "stack" ("[","]") (W.peek ws)                    $ W.index    ws
                      f <- emit "float" ("{","}") (W.peek ws) $ map fst $ toList $ W.floating ws
                      return $ s ++ f
  where
    emit :: String -> (String,String) -> Maybe Window -> [Window] -> X String
    emit title (lb,rb) _       [] = return $ title ++ " " ++ lb ++ rb ++ "]\n"
    emit title (lb,rb) focused ws = do
      (_,_,_,_,ss) <- foldM emit' (title,lb,rb,focused,"") ws
      return $ ss                               ++
               replicate (length title + 1) ' ' ++
               rb                               ++
               "\n"

    emit' :: (String,String,String,Maybe Window,String)
          -> Window
          -> X (String,String,String,Maybe Window,String)
    emit' (t,l,r,f,a) w = do
      w' <- emit'' f w
      return (replicate (length t) ' '
             ,',' : replicate (length l - 1) ' '
             ,r
             ,f
             ,a ++ t ++ " " ++ l ++ w' ++ "\n"
             )
    emit'' :: Maybe Window -> Window -> X String
    emit'' focus win =
      let fi f = if win == f then "(*) " else "    "
       in (maybe "    " fi focus ++) `fmap` debugWindow win
