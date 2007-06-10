-- 
-- DynamicLog
--
-- Log events in:
--
--      1 2 [3] 4 8
--
-- format. suitable to pipe into dzen.
--
-- To use, set:
--
--      import XMonadContrib.DynamicLog
--      logHook = dynamicLog
--
-- Don Stewart

module XMonadContrib.DynamicLog where

-- 
-- Useful imports
--
import XMonad
import Data.List
import qualified StackSet as S

--
-- Perform an arbitrary action on each state change.
-- Examples include:
--      * do nothing
--      * log the state to stdout

--
-- An example logger, print a status bar output to dzen, in the form:
--
--  1 2 [3] 4 7
--  

dynamicLog :: X ()
dynamicLog = withWindowSet $ io . putStrLn . ppr
  where
    ppr s =  concatMap fmt $ sortBy tags
                (map S.workspace (S.current s : S.visible s) ++ S.hidden s)

       where tags a b = S.tag a `compare` S.tag b
             this     = S.tag (S.workspace (S.current s))
             pprTag   = show . (+(1::Int)) . fromIntegral . S.tag
             fmt w | S.tag w == this      = "[" ++ pprTag w ++ "]"
                   | S.stack w /= S.Empty = " " ++ pprTag w ++ " "
                   | otherwise            = ""
