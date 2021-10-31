{-# LANGUAGE ViewPatterns #-}

--------------------------------------------------------------------------------
-- |
-- Module       : XMonad.Actions.CycleWindows
-- Description  : Cycle windows while maintaining focus in place.
-- Copyright    : (c) Wirt Wolff <wirtwolff@gmail.com>
-- License      : BSD3-style (see LICENSE)
--
-- Maintainer   : Wirt Wolff <wirtwolff@gmail.com>
-- Stability    : unstable
-- Portability  : unportable
--
-- Provides bindings to cycle windows up or down on the current workspace
-- stack while maintaining focus in place.
--
-- Bindings are available to:
--
-- * Cycle nearby or nth windows into the focused frame
--
-- * Cycle a window halfway around the stack
--
-- * Cycle windows through the focused position.
--
-- * Cycle unfocused windows.
--
-- These bindings are especially useful with layouts that hide some of
-- the windows in the stack, such as Full, "XMonad.Layout.TwoPane" or
-- when using "XMonad.Layout.LimitWindows" to only show three or four
-- panes. See also "XMonad.Actions.RotSlaves" for related actions.
-----------------------------------------------------------------------------
module XMonad.Actions.CycleWindows (
        -- * Usage
        -- $usage

        -- * Cycling nearby or nth window into current frame
        -- $cycle
        cycleRecentWindows,
        cycleStacks',
        -- * Cycling half the stack to get rid of a boring window
        -- $opposite
        rotOpposite', rotOpposite,
        -- * Cycling windows through the current frame
        -- $focused
        rotFocused', rotFocusedUp, rotFocusedDown, shiftToFocus',
        -- * Cycling windows through other frames
        -- $unfocused
        rotUnfocused', rotUnfocusedUp, rotUnfocusedDown,
        -- * Updating the mouse pointer
        -- $pointer

        -- * Generic list rotations
        -- $generic
        rotUp, rotDown
) where

import XMonad
import XMonad.Prelude
import qualified XMonad.StackSet as W
import qualified Data.List.NonEmpty as NE
import XMonad.Actions.RotSlaves

import Control.Arrow (second)

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@ file:
--
-- > import XMonad.Actions.CycleWindows
-- >    -- config
-- >    -- other key bindings with x here your config
-- >
-- >              -- make sure mod matches keysym
-- >  , ((mod4Mask,  xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
-- >  , ((modm, xK_z), rotOpposite)
-- >  , ((modm                , xK_i), rotUnfocusedUp)
-- >  , ((modm                , xK_u), rotUnfocusedDown)
-- >  , ((modm .|. controlMask, xK_i), rotFocusedUp)
-- >  , ((modm .|. controlMask, xK_u), rotFocusedDown)
--
-- Also, if you use focus follows mouse, you will want to read the section
-- on updating the mouse pointer below.  For detailed instructions on
-- editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
{- $pointer
With FocusFollowsMouse == True, the focus is updated after binding
actions, possibly focusing a window you didn't intend to focus. Most
people using TwoPane probably already have a logHook causing the mouse
to follow focus. (See "XMonad.Actions.UpdatePointer", or "XMonad.Actions.Warp")

If you want this built into the key binding instead, use the appropriate
action from one of those modules to also have your bindings move the pointer
to the point of your choice on the current window:

> import XMonad.Actions.UpdatePointer -- or Actions.Warp

and either

> -- modify the window rotation bindings
> , ((modm .|. controlMask, xK_i   ), rotFocusedUp
>                                            >> updatePointer (Relative 1 1))
> , ((modm .|. controlMask, xK_u   ), rotFocusedDown
>                                            >> updatePointer (Relative 1 1))
>
>    -- or add to xmonad's logHook
>    , logHook = dynamicLogWithPP xmobarPP
>                    >> updatePointer Nearest -- or your preference

-}

-- $cycle
-- Cycle windows into focus from below or above the focused pane by pressing
-- a key while one or more modifier keys is held down. The window order isn't
-- changed until a modifier is released, leaving the previously focused window
-- just below the new one, (or above if the window just above is chosen.) For
-- best results use the same modifier + key combination as the one used to invoke
-- the \"bring from below\" action.  Also, once cycling, pressing a number key n
-- will focus the nth window, with 0 being the one originally focused.
cycleRecentWindows :: [KeySym] -- ^ A list of modifier keys used when invoking this action.
                               --   As soon as one of them is released, the final switch is made.
                    -> KeySym  -- ^ Key used to shift windows from below the current choice into the current frame.
                    -> KeySym  -- ^ Key used to shift windows from above the current choice into the current frame.
                               --   If it's the same as the first key, it is effectively ignored.
                    -> X ()
cycleRecentWindows = cycleStacks' stacks where
    stacks s = map (`shiftToFocus'` s) (wins s)
    wins (W.Stack t l r) = t : r ++ reverse l


-- | Cycle through a /finite/ list of window stacks with repeated presses
--   of a key while a modifier key is held down. For best results use the same
--   mod key + key combination as the one used to invoke the \"bring from below\"
--   action. You could use cycleStacks' with a different stack permutations
--   function to, for example, cycle from one below to one above to two below,
--   etc. instead of in order. You are responsible for having it generate a
--   finite list, though, or xmonad may hang seeking its length.
cycleStacks' :: (W.Stack Window -> [W.Stack Window]) -- ^ A function to a finite list of permutations of a given stack.
                                    -> [KeySym]  -- ^ A list of modifier keys used to invoke 'cycleStacks''.
                                                 --   As soon as any is released, we're no longer cycling on the [Stack Window]
                                    -> KeySym    -- ^ Key used to select a \"next\" stack.
                                    -> KeySym    -- ^ Key used to select a \"previous\" stack.
                                    -> X ()
cycleStacks' filteredPerms mods keyNext keyPrev = do
    XConf {theRoot = root, display = d} <- ask
    stacks <- gets $ maybe [] filteredPerms . W.stack . W.workspace . W.current . windowset

    let evt = allocaXEvent $
                  \p -> do maskEvent d (keyPressMask .|. keyReleaseMask) p
                           KeyEvent {ev_event_type = t, ev_keycode = c} <- getEvent p
                           s <- keycodeToKeysym d c 0
                           return (t, s)
        choose n (t, s)
              | t == keyPress   && s == keyNext  = io evt >>= choose (n+1)
              | t == keyPress   && s == keyPrev  = io evt >>= choose (n-1)
              | t == keyPress   && s `elem` [xK_0..xK_9] = io evt >>= choose (numKeyToN s)
              | t == keyRelease && s `elem` mods = return ()
              | otherwise                        = doStack n >> io evt >>= choose n
        doStack n = windows . W.modify' . const $ stacks `cycref` n

    io $ grabKeyboard d root False grabModeAsync grabModeAsync currentTime
    io evt >>= choose 1
    io $ ungrabKeyboard d currentTime
  where cycref l i = l !! (i `mod` length l) -- modify' ensures l is never [], but must also be finite
        numKeyToN = subtract 48 . read . show

-- | Given a stack element and a stack, shift or insert the element (window)
--   at the currently focused position.
shiftToFocus' :: (Eq a, Show a, Read a) => a -> W.Stack a -> W.Stack a
shiftToFocus' w s@(W.Stack _ ls _) = W.Stack w (reverse revls') rs'
  where (revls', rs') = splitAt (length ls) . filter (/= w) $ W.integrate s


-- $opposite
-- Shifts the focused window as far as possible from the current focus,
-- i.e. halfway around the stack. Windows above the focus up to the \"opposite\"
-- position remain in place, while those above the insertion shift toward
-- the current focus. This is useful for people who use lots of windows in Full,
-- TwoPane, etc., to get rid of boring windows while cycling and swapping
-- near the focus.
rotOpposite :: X()
rotOpposite = windows $ W.modify' rotOpposite'

-- | The opposite rotation on a Stack.
rotOpposite' :: W.Stack a -> W.Stack a
rotOpposite' (W.Stack t l r) = W.Stack t' l' r'
  where rrvl = r ++ reverse l
        part = (length rrvl + 1) `div` 2
        (l', notEmpty -> t' :| r') = second reverse . splitAt (length l) $
                                reverse (take part rrvl ++ t : drop part rrvl)


-- $focused
-- Most people will want the @rotAllUp@ or @rotAllDown@ actions from
-- "XMonad.Actions.RotSlaves" to cycle all windows in the stack.
--
-- The following actions keep the \"next\" window stable, which is
-- mostly useful in two window layouts, or when you have a log viewer or
-- buffer window you want to keep next to the cycled window.

-- | Rotate windows through the focused frame, excluding the \"next\" window.
-- With, e.g. TwoPane, this allows cycling windows through either the
-- master or slave pane, without changing the other frame. When the master
-- is focused, the window below is skipped, when a non-master window is
-- focused, the master is skipped.
rotFocusedUp :: X ()
rotFocusedUp = windows . W.modify' $ rotFocused' rotUp
rotFocusedDown :: X ()
rotFocusedDown = windows . W.modify' $ rotFocused' rotDown

-- | The focused rotation on a stack.
rotFocused' :: ([a] -> [a]) -> W.Stack a -> W.Stack a
rotFocused' _ s@(W.Stack _ [] []) = s
rotFocused' f   (W.Stack t [] (r:rs)) = W.Stack t' [] (r:rs') -- Master has focus
    where (notEmpty -> t' :| rs') = f (t:rs)
rotFocused' f s@W.Stack{} = rotSlaves' f s                    -- otherwise


-- $unfocused
-- Rotate windows through the unfocused frames. This is similar to
-- @rotSlaves@, from "XMonad.Actions.RotSlaves", but excludes the current
-- frame rather than master.
rotUnfocusedUp :: X ()
rotUnfocusedUp = windows . W.modify' $ rotUnfocused' rotUp
rotUnfocusedDown :: X ()
rotUnfocusedDown = windows . W.modify' $ rotUnfocused' rotDown

-- | The unfocused rotation on a stack.
rotUnfocused' :: ([a] -> [a]) -> W.Stack a -> W.Stack a
rotUnfocused' _ s@(W.Stack _ [] []) = s
rotUnfocused' f s@(W.Stack _ [] _ ) = rotSlaves' f s                 -- Master has focus
rotUnfocused' f   (W.Stack t ls rs) = W.Stack t (reverse revls') rs' -- otherwise
    where  (master :| revls) = NE.reverse (let l:ll = ls in l :| ll)
           (revls',rs') = splitAt (length ls) (f $ master:revls ++ rs)

-- $generic
-- Generic list rotations such that @rotUp [1..4]@ is equivalent to
-- @[2,3,4,1]@ and @rotDown [1..4]@ to @[4,1,2,3]@. They both are
-- @id@ for null or singleton lists.
rotUp :: [a] -> [a]
rotUp   l = drop 1 l ++ take 1 l
rotDown :: [a] -> [a]
rotDown = reverse . rotUp . reverse
