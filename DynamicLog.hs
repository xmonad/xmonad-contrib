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
-- format. Suitable to pipe into dzen.
--
-----------------------------------------------------------------------------

module XMonadContrib.DynamicLog (
    -- * Usage
    -- $usage 
    dynamicLog,
    dynamicLogWithPP,
    dynamicLogXinerama,

    pprWindowSet,
    pprWindowSetXinerama,

    PP(..), defaultPP, sjanssenPP,
    wrap, xmobarColor
  ) where

-- 
-- Useful imports
--
import XMonad
import {-# SOURCE #-} Config (workspaces)
import Operations () -- for ReadableSomeLayout instance
import Data.Maybe ( isJust )
import Data.List
import Data.Ord ( comparing )
import qualified StackSet as S
import Data.Monoid
import XMonadContrib.NamedWindows

-- $usage 
--
-- To use, set:
--
-- >    import XMonadContrib.DynamicLog
-- >    logHook = dynamicLog

-- %import XMonadContrib.DynamicLog
-- %def -- comment out default logHook definition above if you uncomment any of these:
-- %def logHook = dynamicLog


-- |
-- An example log hook, print a status bar output to stdout, in the form:
--
-- > 1 2 [3] 4 7 : full : title
--
-- That is, the currently populated workspaces, the current
-- workspace layout, and the title of the focused window.
--
dynamicLog :: X ()
dynamicLog = dynamicLogWithPP defaultPP

-- |
-- A log function that uses the 'PP' hooks to customize output.
dynamicLogWithPP :: PP -> X ()
dynamicLogWithPP pp = do
    -- layout description
    ld <- withWindowSet $ return . description . S.layout . S.workspace . S.current
    -- workspace list
    ws <- withWindowSet $ return . pprWindowSet pp
    -- window title
    wt <- withWindowSet $ maybe (return "") (fmap show . getName) . S.peek

    io . putStrLn . sepBy (ppSep pp) . ppOrder pp $
                        [ ws
                        , ppLayout pp ld
                        , ppTitle  pp wt
                        ]

pprWindowSet :: PP -> WindowSet -> String
pprWindowSet pp s =  unwords' $ map fmt $ sortBy cmp
            (map S.workspace (S.current s : S.visible s) ++ S.hidden s)
   where f Nothing Nothing   = EQ
         f (Just _) Nothing  = LT
         f Nothing (Just _)  = GT
         f (Just x) (Just y) = compare x y

         wsIndex = flip elemIndex workspaces . S.tag

         cmp a b = f (wsIndex a) (wsIndex b) `mappend` compare (S.tag a) (S.tag b)

         this     = S.tag (S.workspace (S.current s))
         visibles = map (S.tag . S.workspace) (S.visible s)

         fmt w = printer pp (S.tag w)
          where printer | S.tag w == this         = ppCurrent
                        | S.tag w `elem` visibles = ppVisible
                        | isJust (S.stack w)      = ppHidden
                        | otherwise               = ppHiddenNoWindows

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
  where onscreen  = map (S.tag . S.workspace)
                        . sortBy (comparing S.screen) $ S.current ws : S.visible ws
        offscreen = map S.tag . filter (isJust . S.stack)
                        . sortBy (comparing S.tag) $ S.hidden ws

wrap :: String -> String -> String -> String
wrap l r "" = ""
wrap l r m  = l ++ m ++ r

-- | Intersperse spaces, filtering empty words.
unwords' :: [String] -> String
unwords' = sepBy " "

sepBy :: String -> [String] -> String
sepBy sep = concat . intersperse sep . filter (not . null)

-- TODO dzenColor
xmobarColor :: String -> String -> String -> String
xmobarColor fg bg = wrap t "</fc>"
 where t = concat ["<fc=", fg, if null bg then "" else "," ++ bg, ">"]

-- | The 'PP' type allows the user to customize various behaviors of
-- dynamicLogPP
data PP = PP { ppCurrent, ppVisible
             , ppHidden, ppHiddenNoWindows :: WorkspaceId -> String
             , ppSep :: String
             , ppTitle :: String -> String
             , ppLayout :: String -> String
             , ppOrder :: [String] -> [String] }

-- | The default pretty printing options, as seen in dynamicLog
defaultPP :: PP
defaultPP = PP { ppCurrent         = wrap "[" "]"
               , ppVisible         = wrap "<" ">"
               , ppHidden          = id
               , ppHiddenNoWindows = const ""
               , ppSep             = " : "
               , ppTitle           = const ""
               , ppLayout          = id
               , ppOrder           = id }

-- | The options that sjanssen likes to use, as an example.  Note the use of
-- 'xmobarColor' and the record update on defaultPP
sjanssenPP :: PP
sjanssenPP = defaultPP { ppCurrent = xmobarColor "white" "#ff000000"
                       , ppTitle = xmobarColor "#00ee00" ""
                       , ppOrder = reverse
                       }
