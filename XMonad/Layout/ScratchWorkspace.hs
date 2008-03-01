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

import Data.Maybe ( listToMaybe, catMaybes )
import Control.Monad ( guard, when )

import XMonad
import XMonad.Core
import qualified XMonad.StackSet as W

toggleScratchWorkspace :: LayoutClass l Int => l Int -> X ()
toggleScratchWorkspace l =
    do s <- gets windowset
       when (scratchName `W.tagMember` s) $
         case visibleScratch s of
         Just oldscratch ->
             do srs <- withDisplay getCleanedScreenInfo
                when (length srs == length (W.visible s)) $ do
                  ml <- handleMessage (W.layout $ W.workspace oldscratch) (SomeMessage Hide)
                  let scratch = case ml of
                                Nothing -> oldscratch
                                Just l' -> oldscratch { W.workspace =
                                                        (W.workspace oldscratch) { W.layout = l' } }
                  mapM_ hide $ W.integrate' $ W.stack $ W.workspace scratch
                  let modscr scr = do guard $ scratchName /= W.tag (W.workspace scr)
                                      Just $ scr { W.screenDetail = newDetail }
                          where newDetail = (W.screenDetail scr)
                                            { screenRect = pickRect (W.screen scr) srs }
                                pickRect _ [z] = z
                                pickRect i (z:zs) | i < 1 = z
                                                  | otherwise = pickRect (i-1) zs
                                pickRect _ [] = error "XMonad.Layout.ScratchWorkspace.toggleScratchWorkspace: internal error"
                      s' = case catMaybes $ map modscr $ W.current s : W.visible s of
                           newc:newv -> s { W.current = newc, W.visible = newv,
                                            W.hidden = W.workspace scratch : W.hidden s}
                           [] -> error "XMonad.Layout.ScratchWorkspace.toggleScratchWorkspace: internal error"
                  modify $ \st -> st { windowset = s' }
                  refresh
         Nothing ->
             case hiddenScratch s of
             Nothing -> return ()
             Just hs -> do r <- gets (screenRect . W.screenDetail . W.current . windowset)
                           (rs,_) <- doLayout l r (W.Stack 0 [1] [])
                           let (r0, r1) = case rs of
                                          [(0,ra),(1,rb)] -> (ra,rb)
                                          [(1,ra),(0,rb)] -> (rb,ra)
                                          [(1,ra)] -> (r,ra)
                                          [(0,ra)] -> (ra,r)
                                          _ -> (r,r)
                               c' = (W.current s) { W.screenDetail =
                                                    (W.screenDetail (W.current s)) { screenRect = r1 }}
                           let s' = s { W.current = W.Screen hs (-1) (SD r0 (0,0,0,0)),
                                        W.visible = c': W.visible s,
                                        W.hidden = filter (not . isScratchW) $ W.hidden s }
                           modify $ \st -> st { windowset = s' }
                           refresh
    where visibleScratch s = listToMaybe $ filter isScratch $ W.current s : W.visible s
          hiddenScratch s = listToMaybe $ filter isScratchW $ W.hidden s
          isScratchW w = scratchName == W.tag w
          isScratch scr = scratchName == W.tag (W.workspace scr)
--          notScratch scr = scratchName /= W.tag (W.workspace scr)


scratchName :: String
scratchName = "*scratch*"

-- isScratchVisible :: X Bool
-- isScratchVisible = gets (elem scratchName . map (W.tag . W.workspace) . W.visible . windowset)
