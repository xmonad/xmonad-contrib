{-# LANGUAGE LambdaCase #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExclusiveScratchpads
-- Description :  Named scratchpads that can be mutually exclusive.
-- Copyright   :  Bruce Forte (2017)
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Bruce Forte
-- Stability   :  unstable
-- Portability :  unportable
--
-- Named scratchpads that can be mutually exclusive.
--
-----------------------------------------------------------------------------

module XMonad.Util.ExclusiveScratchpads (
  -- * Usage
  -- $usage
  mkXScratchpads,
  xScratchpadsManageHook,
  -- * Keyboard related
  scratchpadAction,
  hideAll,
  resetExclusiveSp,
  -- * Mouse related
  setNoexclusive,
  resizeNoexclusive,
  floatMoveNoexclusive,
  -- * Types
  ExclusiveScratchpad(..),
  ExclusiveScratchpads,
  -- * Hooks
  nonFloating,
  defaultFloating,
  customFloating
  ) where

import XMonad.Prelude (appEndo, filterM, liftA2, (<=<))
import XMonad
import XMonad.Actions.Minimize
import XMonad.Actions.TagWindows (addTag,delTag)
import XMonad.Hooks.ManageHelpers (doRectFloat,isInProperty)

import qualified XMonad.StackSet as W

-- $usage
--
-- For this module to work properly, you need to use "XMonad.Layout.BoringWindows" and
-- "XMonad.Layout.Minimize", please refer to the documentation of these modules for more
-- information on how to configure them.
--
-- To use this module, put the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Utils.ExclusiveScratchpads
-- > import XMonad.ManageHook (title,appName)
-- > import qualified XMonad.StackSet as W
--
-- Add exclusive scratchpads, for example:
--
-- > exclusiveSps = mkXScratchpads [ ("htop",   "urxvt -name htop -e htop", title =? "htop")
-- >                               , ("xclock", "xclock", appName =? "xclock")
-- >                               ] $ customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
--
-- The scratchpads don\'t have to be exclusive, you can create them like this (see 'ExclusiveScratchpad'):
--
-- > regularSps   = [ XSP "term" "urxvt -name scratchpad" (appName =? "scratchpad") defaultFloating [] ]
--
-- Create a list that contains all your scratchpads like this:
--
-- > scratchpads = exclusiveSps ++ regularSps
--
-- Add the hooks to your managehook (see "XMonad.Doc.Extending#Editing_the_manage_hook"), eg.:
--
-- > manageHook = myManageHook <+> xScratchpadsManageHook scratchpads
--
-- And finally add some keybindings (see "XMonad.Doc.Extending#Editing_key_bindings"):
--
-- > , ((modMask, xK_h), scratchpadAction scratchpads "htop")
-- > , ((modMask, xK_c), scratchpadAction scratchpads "xclock")
-- > , ((modMask, xK_t), scratchpadAction scratchpads "term")
-- > , ((modMask, xK_h), hideAll scratchpads)
--
-- Now you can get your scratchpads by pressing the corresponding keys, if you
-- have the @htop@ scratchpad on your current screen and you fetch the @xclock@
-- scratchpad then @htop@ gets hidden.
--
-- If you move a scratchpad it still gets hidden when you fetch a scratchpad of
-- the same family, to change that behaviour and make windows not exclusive
-- anymore when they get resized or moved add these mouse bindings
-- (see "XMonad.Doc.Extending#Editing_mouse_bindings"):
--
-- >     , ((mod4Mask, button1), floatMoveNoexclusive scratchpads)
-- >     , ((mod4Mask, button3), resizeNoexclusive scratchpads)
--
-- To reset a moved scratchpad to the original position that you set with its hook,
-- call @resetExclusiveSp@ when it is in focus. For example if you want to extend
-- Mod-Return to reset the placement when a scratchpad is in focus but keep the
-- default behaviour for tiled windows, set these key bindings:
--
-- > , ((modMask, xK_Return), windows W.swapMaster >> resetExclusiveSp scratchpads)
--
-- __Note:__ This is just an example, in general you can add more than two
-- exclusive scratchpads and multiple families of such.

data ExclusiveScratchpad = XSP { name   :: String       -- ^ Name of the scratchpad
                               , cmd    :: String       -- ^ Command to spawn the scratchpad
                               , query  :: Query Bool   -- ^ Query to match the scratchpad
                               , hook   :: ManageHook   -- ^ Hook to specify the placement policy
                               , exclusive :: [String]  -- ^ Names of exclusive scratchpads
                               }

type ExclusiveScratchpads = [ExclusiveScratchpad]

-- -----------------------------------------------------------------------------------

-- | Create 'ExclusiveScratchpads' from @[(name,cmd,query)]@ with a common @hook@
mkXScratchpads :: [(String,String,Query Bool)]  -- ^ List of @(name,cmd,query)@ of the
                                                --   exclusive scratchpads
               -> ManageHook                    -- ^ The common @hook@ that they use
               -> ExclusiveScratchpads
mkXScratchpads xs h = foldl accumulate [] xs
  where
    accumulate a (n,c,q) = XSP n c q h (filter (n/=) names) : a
    names = map (\(n,_,_) -> n) xs

-- | Create 'ManageHook' from 'ExclusiveScratchpads'
xScratchpadsManageHook :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads from
                                                --   which a 'ManageHook' should be generated
                       -> ManageHook
xScratchpadsManageHook = composeAll . fmap (\sp -> query sp --> hook sp)

-- | Pop up/hide the scratchpad by name and possibly hide its exclusive
scratchpadAction :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                 -> String                -- ^ Name of the scratchpad to toggle
                 -> X ()
scratchpadAction xs n =
  let ys = filter ((n==).name) xs in

  case ys of []     -> return ()
             (sp:_) -> let q = query sp in withWindowSet $ \s -> do
                       ws <- filterM (runQuery q) $ W.allWindows s

                       case ws of []    -> do spawn (cmd sp)
                                              hideOthers xs n
                                              windows W.shiftMaster

                                  (w:_) -> do toggleWindow w
                                              whenX (runQuery isExclusive w) (hideOthers xs n)
  where
    toggleWindow w = liftA2 (&&) (runQuery isMaximized w) (onCurrentScreen w) >>= \case
      True  -> whenX (onCurrentScreen w) (minimizeWindow w)
      False -> do windows (flip W.shiftWin w =<< W.currentTag)
                  maximizeWindowAndFocus w
                  windows W.shiftMaster

    onCurrentScreen w = withWindowSet (return . elem w . currentWindows)

-- | Hide all 'ExclusiveScratchpads' on the current screen
hideAll :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
        -> X ()
hideAll xs = mapWithCurrentScreen q minimizeWindow
  where q = joinQueries (map query xs) <&&> isExclusive <&&> isMaximized

-- | If the focused window is a scratchpad, the scratchpad gets reset to the original
-- placement specified with the hook and becomes exclusive again
resetExclusiveSp :: ExclusiveScratchpads -- ^ List of exclusive scratchpads
                 -> X ()
resetExclusiveSp xs = withFocused $ \w -> whenX (isScratchpad xs w) $ do
  let ys = filterM (flip runQuery w . query) xs

  unlessX (null <$> ys) $ do
    mh <- head . map hook <$> ys  -- ys /= [], so `head` is fine
    n  <- head . map name <$> ys  -- same

    (windows . appEndo <=< runQuery mh) w
    hideOthers xs n
    delTag "_XSP_NOEXCLUSIVE" w

  where unlessX = whenX . fmap not

-- -----------------------------------------------------------------------------------

-- | Hide the scratchpad of the same family by name if it's on the focused workspace
hideOthers :: ExclusiveScratchpads -> String -> X ()
hideOthers xs n =
  let ys = concatMap exclusive $ filter ((n==).name) xs
      qs = map query $ filter ((`elem` ys).name) xs
      q  = joinQueries qs <&&> isExclusive <&&> isMaximized in

  mapWithCurrentScreen q minimizeWindow

-- | Conditionally map a function on all windows of the current screen
mapWithCurrentScreen :: Query Bool -> (Window -> X ()) -> X ()
mapWithCurrentScreen q f = withWindowSet $ \s -> do
  ws <- filterM (runQuery q) $ currentWindows s
  mapM_ f ws

-- | Extract all windows on the current screen from a StackSet
currentWindows :: W.StackSet i l a sid sd -> [a]
currentWindows = W.integrate' . W.stack . W.workspace . W.current

-- | Check if given window is a scratchpad
isScratchpad :: ExclusiveScratchpads -> Window -> X Bool
isScratchpad xs w = withWindowSet $ \s -> do
  let q = joinQueries $ map query xs

  ws <- filterM (runQuery q) $ W.allWindows s
  return $ elem w ws

-- | Build a disjunction from a list of clauses
joinQueries :: [Query Bool] -> Query Bool
joinQueries = foldl (<||>) (liftX $ return False)

-- | Useful queries
isExclusive, isMaximized :: Query Bool
isExclusive = notElem "_XSP_NOEXCLUSIVE" . words <$> stringProperty "_XMONAD_TAGS"
isMaximized = not <$> isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

-- -----------------------------------------------------------------------------------

-- | Make a window not exclusive anymore
setNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
               -> Window                -- ^ Window which should be made not
                                        --   exclusive anymore
               -> X ()
setNoexclusive xs w = whenX (isScratchpad xs w) $ addTag "_XSP_NOEXCLUSIVE" w

-- | Float and drag the window, make it not exclusive anymore
floatMoveNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                     -> Window                -- ^ Window which should be moved
                     -> X ()
floatMoveNoexclusive xs w = setNoexclusive xs w
  >> focus w
  >> mouseMoveWindow w
  >> windows W.shiftMaster

-- | Resize window, make it not exclusive anymore
resizeNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                  -> Window                -- ^ Window which should be resized
                  -> X ()
resizeNoexclusive xs w = setNoexclusive xs w
  >> focus w
  >> mouseResizeWindow w
  >> windows W.shiftMaster

-- -----------------------------------------------------------------------------------

-- | Manage hook that makes the window non-floating
nonFloating :: ManageHook
nonFloating = idHook

-- | Manage hook that makes the window floating with the default placement
defaultFloating :: ManageHook
defaultFloating = doFloat

-- | Manage hook that makes the window floating with custom placement
customFloating :: W.RationalRect  -- ^ @RationalRect x y w h@ that specifies relative position,
                                  --   height and width (see "XMonad.StackSet#RationalRect")
               -> ManageHook
customFloating = doRectFloat
