{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module       : XMonad.Hooks.XPropManage
-- Copyright    : (c) Karsten Schoelzel <kuser@gmx.de>
-- License      : BSD
--
-- Maintainer   : Karsten Schoelzel <kuser@gmx.de>
-- Stability    : unstable
-- Portability  : unportable
--
-- A ManageHook matching on XProperties.
-----------------------------------------------------------------------------

module XMonad.Hooks.XPropManage (
                 -- * Usage
                 -- $usage
                 xPropManageHook, XPropMatch, pmX, pmP
                 ) where

import Control.Exception as E
import Data.Char (chr)
import Data.Monoid (Endo(..))

import Control.Monad.Trans (lift)

import XMonad

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Hooks.XPropManage
-- > import qualified XMonad.StackSet as W
-- > import XMonad.Actions.TagWindows
-- > import Data.List
--
-- > manageHook = xPropManageHook xPropMatches
-- >
-- > xPropMatches :: [XPropMatch]
-- > xPropMatches = [ ([ (wM_CLASS, any ("gimp"==))], (\w -> float w >> return (W.shift "2")))
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
-- \*1 You can get the available properties of an application with the xprop utility. STRING properties
-- should work fine. Others might not work.
--

type XPropMatch = ([(Atom, [String] -> Bool)], (Window -> X (WindowSet -> WindowSet)))

pmX :: (Window -> X ()) -> Window -> X (WindowSet -> WindowSet)
pmX f w = f w >> return id

pmP :: (WindowSet -> WindowSet) -> Window -> X (WindowSet -> WindowSet)
pmP f _ = return f

xPropManageHook :: [XPropMatch] -> ManageHook
xPropManageHook tms = mconcat $ map propToHook tms
    where
      propToHook (ms, f) = fmap and (mapM mkQuery ms) --> mkHook f
      mkQuery (a, tf)    = fmap tf (getQuery a)
      mkHook func        = ask >>= Query . lift . fmap Endo . func

getProp :: Display -> Window -> Atom -> X ([String])
getProp d w p = do
    prop <- io $ E.catch (getTextProperty d w p >>= wcTextPropertyToTextList d) (\(_ :: IOException) -> return [[]])
    let filt q | q == wM_COMMAND = concat . map splitAtNull
               | otherwise       = id
    return (filt p prop)

getQuery ::  Atom -> Query [String]
getQuery p = ask >>= \w ->  Query . lift $ withDisplay $ \d -> getProp d w p

splitAtNull :: String -> [String]
splitAtNull s = case dropWhile (== (chr 0)) s of
    "" -> []
    s' -> w : splitAtNull s''
          where (w, s'') = break (== (chr 0)) s'
