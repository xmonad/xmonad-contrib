-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.BorderPerWindow
-- Description :  Set border width for a window in a ManageHook.
-- Copyright   :  (c) 2021 Xiaokui Shu
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  subbyte@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Want to customize border width, for each window on all layouts? Want
-- specific window have no border on all layouts? Try this.
-----------------------------------------------------------------------------

module XMonad.Hooks.BorderPerWindow ( -- * Usage
                                      -- $usage
                                      defineBorderWidth
                                    , actionQueue

                                      -- * Design Considerations
                                      -- $design
                                    ) where


import XMonad
import XMonad.Util.ActionQueue (enqueue, actionQueue)

-- $usage
--
-- To use this module, first import it
--
-- > import XMonad.Hooks.BorderPerWindow (defineBorderWidth, actionQueue)
--
-- Then specify which window to customize the border of in your
-- @manageHook@:
--
-- > myManageHook :: ManageHook
-- > myManageHook = composeAll
-- >     [ className =? "firefox"  --> defineBorderWidth 0
-- >     , className =? "Chromium" --> defineBorderWidth 0
-- >     , isDialog                --> defineBorderWidth 8
-- >     ]
--
-- Finally, add the 'actionQueue' combinator and @myManageHook@ to your
-- config:
--
-- > main = xmonad $ actionQueue $ def
-- >     { ...
-- >     , manageHook = myManageHook
-- >     , ...
-- >     }
--
-- Note that this module is incompatible with other ways of changing
-- borders, like "XMonad.Layout.NoBorders".  This is because we are
-- changing the border exactly /once/ (when the window first appears)
-- and not every time some condition is satisfied.

-- $design
--
-- 1. Keep it simple. Since the extension does not aim to change border setting
--    when layout changes, only execute the border setting function once to
--    avoid potential window flashing/jumping/scaling.
--
-- 2. The 'ManageHook' eDSL is a nice language for specifying windows. Let's
--    build on top of it and use it to specify window to define border.

defineBorderWidth :: Dimension -> ManageHook
defineBorderWidth bw = do
    w <- ask
    liftX . enqueue $ updateBorderWidth w bw
    idHook

updateBorderWidth :: Window -> Dimension -> X ()
updateBorderWidth w bw = do
    withDisplay $ \d -> io $ setWindowBorderWidth d w bw
    refresh
