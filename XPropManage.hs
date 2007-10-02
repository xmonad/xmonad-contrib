-----------------------------------------------------------------------------
-- |
-- Module       : XMonadContrib.XPropManage
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- A ManageHook matching on XProperties.
-----------------------------------------------------------------------------

module XMonadContrib.XPropManage (
                 -- * Usage
                 -- $usage
                 xPropManageHook, XPropMatch, pmX, pmP
                 ) where

import Data.Char (chr)
import Data.List (concat)

import Control.Monad.State
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import XMonad

-- $usage
-- 
-- Add something like the following lines to Config.hs to use this module
-- > import XMonadContrib.XPropManage
-- 
-- > manageHook = xPropManageHook xPropMatches 
-- >
-- > xPropMatches :: [XPropMatch]
-- > xPropMatches = [ ([ (wM_CLASS, any ("gimp"==)))], (\w -> float w >> return (W.shift "2")))
-- >                , ([ (wM_COMMAND, any ("screen" ==)), (wM_CLASS, any ("xterm" ==))], pmX (addTag "screen"))
-- >                , ([ (wM_NAME, any ("Iceweasel" `isInfixOf`))], pmP (W.shift "3"))
-- >                ]
--
-- Properties known to work: wM_CLASS, wM_NAME, wM_COMMAND
--
-- A XPropMatch consists of a list of conditions and function telling what to do.
--
-- The list entries are pairs of an XProperty to match on (like wM_CLASS, wM_NAME)^1,
-- and an function which matches onto the value of the property (represented as a List
-- of Strings).
--
-- If a match succeeds the function is called immediately, can perform any action and then return
-- a function to apply in 'windows' (see Operations.hs). So if the action does only work on the
-- WindowSet use just 'pmP function'.
--
-- *1 You can get the available properties of an application with the xprop utility. STRING properties
-- should work fine. Others might not work.
--

type XPropMatch = ([(Atom, [String] -> Bool)], (Window -> X (WindowSet -> WindowSet)))

pmX :: (Window -> X ()) -> Window -> X (WindowSet -> WindowSet)
pmX f w = f w >> return id

pmP :: (WindowSet -> WindowSet) -> Window -> X (WindowSet -> WindowSet)
pmP f _ = return f

xPropManageHook :: [XPropMatch] -> Window -> X (WindowSet -> WindowSet)
xPropManageHook tms w = withDisplay $ \d -> do
    fs <- mapM (matchProp d w `uncurry`) tms
    return (foldr (.) id fs)

matchProp :: Display -> Window -> [(Atom, [String] -> Bool)] -> (Window -> X (WindowSet -> WindowSet)) -> X (WindowSet -> WindowSet)
matchProp d w tm tf = do
    m <- and `liftM` sequence (map (\(k,f) -> f `liftM` getProp d w k) tm)
    case m of
        True   -> tf w
        False  -> return id

getProp :: Display -> Window -> Atom -> X ([String])
getProp d w p = do
    prop <- io $ catch (getTextProperty d w p >>= wcTextPropertyToTextList d) (\_ -> return [[]])
    let filt q | q == wM_COMMAND = concat . map splitAtNull
               | otherwise       = id
    return (filt p prop)

splitAtNull :: String -> [String]
splitAtNull s = case dropWhile (== (chr 0)) s of
    "" -> []
    s' -> w : splitAtNull s''
          where (w, s'') = break (== (chr 0)) s'

