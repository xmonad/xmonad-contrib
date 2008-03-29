{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.ScratchWorkspace
-- Copyright   :  (c) Braden Shepherdson, David Roundy 2008
-- License     :  BSD-style (as xmonad)
--
-- Maintainer  :  Braden.Shepherdson@gmail.com
-- Stability   :  unstable
-- Portability :  unportable

module XMonad.Layout.ScratchWorkspace ( toggleScratchWorkspace ) where

import Data.List ( partition )
import Control.Monad ( guard )

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W

hiddenRect :: Rectangle
hiddenRect = Rectangle (-1) (-1) 0 0

scratchName :: String
scratchName = "*scratch*"

-- This module uses an ugly hack, which is to create a special screen for
-- the scratch workspace.  This screen is then moved onto a visible area or
-- away when you ask for the scratch workspace to be shown or hidden.

-- This is a workaround for the fact that we don't have anything like
-- proper support for hierarchical workspaces, so I use the only hierarchy
-- we've got, which is at the screen level.

toggleScratchWorkspace :: LayoutClass l Int => l Int -> X ()
toggleScratchWorkspace l =
    do s <- gets windowset
       defaultl <- asks (layoutHook . config)
       srs <- withDisplay getCleanedScreenInfo
       if length srs == 1 + length (W.visible s)
         then -- we don't yet have a scratch screen!
             if scratchName `W.tagMember` s
             then return () -- We'll just bail out of scratchName already exists...
             else do let scratchscreen = W.Screen scratch (-1) (SD hiddenRect)
                         scratch = W.Workspace scratchName defaultl Nothing
                         s' = s { W.visible = scratchscreen: W.visible s }
                     modify $ \st -> st { windowset = s' }
                     refresh
         else -- We've already got a scratch (we think)
           if length srs /= length (W.visible s)
           then -- Something is odd... too many screens are visible! Do nothing.
                return ()
           else -- Yes, it does seem there's a scratch screen already
              case partition ((/= -1) . W.screen) $ W.current s : W.visible s of
              (others@(c:vs),[scratchscreen]) ->
                  if screenRect (W.screenDetail scratchscreen) == hiddenRect
                  then -- we're hidden now, so let's display ourselves
                      do let r = screenRect $ W.screenDetail c
                         (rs,_) <- runLayout (W.Workspace "" l (Just $ W.Stack 0 [1] [])) r
                         let (r0, r1) = case rs of
                                        [(0,ra),(1,rb)] -> (ra,rb)
                                        [(1,ra),(0,rb)] -> (rb,ra)
                                        [(1,ra)] -> (r,ra)
                                        [(0,ra)] -> (ra,r)
                                        _ -> (r,r)
                             s' = s { W.current = setrect r0 scratchscreen,
                                      W.visible = setrect r1 c : vs }
                         modify $ \st -> st { windowset = s' }
                         refresh
                  else -- we're visible, so now we want to hide
                       do ml <- handleMessage (W.layout $ W.workspace scratchscreen) (SomeMessage Hide)
                          let scratchscreen' = case ml of
                                               Nothing -> scratchscreen
                                               Just l' -> scratchscreen
                                                          { W.workspace =
                                                            (W.workspace scratchscreen) { W.layout = l' } }
                          mapM_ hide $ W.integrate' $ W.stack $ W.workspace scratchscreen
                          let modscr scr = do guard $ scratchName /= W.tag (W.workspace scr)
                                              r' <- pickRect (W.screen scr) srs
                                              Just $ setrect r' scr
                              pickRect _ [z] = Just z
                              pickRect i (z:zs) | i < 1 = Just z
                                                | otherwise = pickRect (i-1) zs
                              pickRect _ [] = Nothing
                          case mapM modscr others of
                            Just (c':vs') ->
                                do let s' = s { W.current = c',
                                                W.visible = setrect hiddenRect scratchscreen' : vs' }
                                   modify $ \st -> st { windowset = s' }
                                   refresh
                            _ -> return () -- weird error!
              _ -> -- Something is odd... there doesn't seem to *really* be a scratch screen...
                   return ()
    where setrect :: Rectangle -> W.Screen i l a sid ScreenDetail -> W.Screen i l a sid ScreenDetail
          setrect x scr = scr {W.screenDetail = (W.screenDetail scr) {screenRect = x}}
