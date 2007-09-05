-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.DynamicLog
-- Copyright   :  (c) Don Stewart <dons@cse.unsw.edu.au>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Don Stewart <dons@cse.unsw.edu.au>
-- Stability   :  unstable
-- Portability :  unportable
--
-- DynamicLog
--
-- Log events in:
--
-- >     1 2 [3] 4 8
--
-- format. suitable to pipe into dzen.
--
-----------------------------------------------------------------------------

module XMonadContrib.DynamicLog (
    -- * Usage
    -- $usage 
    dynamicLog, dynamicLogXinerama, pprWindowSet, pprWindowSetXinerama
  ) where

-- 
-- Useful imports
--
import XMonad
import Data.Maybe ( isJust )
import Data.List
import Data.Ord ( comparing )
import qualified StackSet as S

-- $usage 
--
-- To use, set:
--
-- >    import XMonadContrib.DynamicLog
-- >    logHook = dynamicLog

-- %import XMonadContrib.DynamicLog
-- %def -- comment out default logHook definition above if you uncomment this:
-- %def logHook = dynamicLog


-- |
-- Perform an arbitrary action on each state change.
-- Examples include:
--      * do nothing
--      * log the state to stdout
--
-- An example logger, print a status bar output to dzen, in the form:
--
-- > 1 2 [3] 4 7
--  

dynamicLog :: X ()
dynamicLog = withWindowSet $ io . putStrLn . pprWindowSet

pprWindowSet :: WindowSet -> String
pprWindowSet s =  concatMap fmt $ sortBy (comparing S.tag)
            (map S.workspace (S.current s : S.visible s) ++ S.hidden s)
   where this     = S.tag (S.workspace (S.current s))
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w | S.tag w == this         = "[" ++ pprTag w ++ "]"
               | S.tag w `elem` visibles = "<" ++ pprTag w ++ ">"
               | isJust (S.stack w)      = " " ++ pprTag w ++ " "
               | otherwise               = ""

-- |
-- Workspace logger with a format designed for Xinerama:
--
-- > [1 9 3] 2 7
--
-- where 1, 9, and 3 are the workspaces on screens 1, 2 and 3, respectively,
-- and 2 and 7 are non-visible, non-empty workspaces
--
dynamicLogXinerama :: X ()
dynamicLogXinerama = withWindowSet $ io . putStrLn . pprWindowSetXinerama

pprWindowSetXinerama :: WindowSet -> String
pprWindowSetXinerama ws = "[" ++ unwords onscreen ++ "] " ++ unwords offscreen
  where onscreen  = map (pprTag . S.workspace)
                        . sortBy (comparing S.screen) $ S.current ws : S.visible ws
        offscreen = map pprTag . filter (isJust . S.stack)
                        . sortBy (comparing S.tag) $ S.hidden ws

-- util functions
pprTag :: Integral i => S.Workspace i a -> String
pprTag = show . (+(1::Int)) . fromIntegral . S.tag
