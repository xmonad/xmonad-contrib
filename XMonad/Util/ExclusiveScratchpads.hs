
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.ExclusiveScratchpads
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

import Control.Applicative ((<$>))
import Control.Monad (filterM,unless,void,(<=<))
import Data.Monoid (appEndo)
import XMonad hiding (hide)
import XMonad.Actions.DynamicWorkspaces (addHiddenWorkspace)
import XMonad.Actions.TagWindows (addTag,delTag)
import XMonad.Hooks.ManageHelpers (doRectFloat)

import qualified XMonad.StackSet as W

-- $usage
-- To use this module, put the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Utils.ExclusiveScratchpads
-- > import XMonad.ManageHook
-- > import qualified XMonad.StackSet as W
--
-- Add exclusive scratchpads, for example:
--
-- > exclusiveSps = mkXScratchpads [ ("htop",   "urxvt -e htop", title =? "htop")
-- >                               , ("xclock", "xclock", appName =? "xclock")
-- >                               ] $ customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)
--
-- The scratchpads don\'t have to be exclusive, you can create them like this (see 'ExclusiveScratchpad'):
--
-- > regularSps   = [ EXS "term" "urxvt -name scratchpad" (appName =? "scratchpad") defaultFloating [] ]
--
-- Create a list that contains all your scratchpads like this:
--
-- > scratchpads = exclusiveSps ++ regularSps
--
-- Add the hooks to your managehook, eg. (see "XMonad.Doc.Extending#Editing_the_manage_hook"):
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
-- If you are annoyed by the @NSP@ workspace in your statusbar and are using
-- @dynamicLogWithPP@ add this to your PP:
--
-- > , ppHidden        = \x -> if x == "NSP" then "" else x
--
-- __Note:__ This is just an example, in general you can add more than two
-- exclusive scratchpads and multiple families of such.


-- | ExclusiveScratchpad record specifies its properties
data ExclusiveScratchpad = EXS { name   :: String   -- ^ Name of the scratchpad
                           , cmd    :: String       -- ^ Command to spawn the scratchpad
                           , query  :: Query Bool   -- ^ Query to match the scratchpad
                           , hook   :: ManageHook   -- ^ Hook to specify the placement policy
                           , exclusive :: [String]  -- ^ Names of exclusive scratchpads
                           }

type ExclusiveScratchpads = [ExclusiveScratchpad]

-- | Name of the hidden workspace
nsp :: String
nsp = "NSP"

-- | Create 'ExclusiveScratchpads' from @[(name,cmd,query)]@ with a common @hook@
mkXScratchpads :: [(String,String,Query Bool)]  -- ^ List of @(name,cmd,query)@ of the
                                                --   exclusive scratchpads
               -> ManageHook                    -- ^ The common @hook@ that they use
               -> ExclusiveScratchpads
mkXScratchpads sps h = foldl accumulate [] sps
  where
    accumulate a (n,c,q) = EXS n c q h (filter (n/=) names) : a
    names = map (\(n,_,_) -> n) sps

-- | Create 'ManageHook' from 'ExclusiveScratchpads'
xScratchpadsManageHook :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads from
                                                --   which a 'ManageHook' should be generated
                       -> ManageHook
xScratchpadsManageHook = composeAll . fmap (\sp -> query sp --> hook sp)

-- | Pop up/hide the scratchpad by name and possibly hide its exclusive
scratchpadAction :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                 -> String                -- ^ Name of the scratchpad to toggle
                 -> X ()
scratchpadAction sps n = popOrHide sps n >> whenX (isExclusive sps n) (hideOthers sps n)

-- | Toggles the scratchpad, if 'nsp' is not present create it on a spawn
popOrHide :: ExclusiveScratchpads -> String -> X ()
popOrHide sps n = do
  let sp = filter ((n==).name) sps
      q  = joinQueries $ map query sp

  unlessX (mapWithCurrentScreen q hide) $ withWindowSet $ \s -> do
    ws <- filterM (runQuery q) $ W.allWindows s
    case ws of [] -> unless (null sp)
                       $ spawnSp s $ cmd $ head sp  -- sp /= [], so `head` is fine
               _  -> mapW (fetchWindow s) ws
  where
    spawnSp s c = do
      unless (any ((nsp==).W.tag) $ W.workspaces s) (addHiddenWorkspace nsp)
      spawn c

    fetchWindow s = (W.shiftMaster .) . (W.shiftWin $ W.currentTag s)

-- | Move the window to the hidden workspace
hide :: Window -> WindowSet -> WindowSet
hide = W.shiftWin nsp

-- | Hide all 'ExclusiveScratchpads' on the current screen
hideAll :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
        -> X ()
hideAll sps = void $ mapWithCurrentScreen q hide
  where q = joinQueries $ map query sps

-- | Hide the scratchpad of the same family by name if it's on the focused workspace
hideOthers :: ExclusiveScratchpads -> String -> X ()
hideOthers sps n =
  let others = concatMap exclusive $ filter ((n==).name) sps
      otherQueries = map query $ filter ((`elem` others).name) sps
      joinedQuery = joinQueries otherQueries
      qry = joinedQuery <&&> isExclusiveQuery in

  void $ mapWithCurrentScreen qry hide

-- | Conditionally map a function on all windows of the current screen
mapWithCurrentScreen :: Query Bool -> (Window -> WindowSet -> WindowSet) -> X Bool
mapWithCurrentScreen q f = withWindowSet $ \s -> do
  ws <- filterM (runQuery q) $ W.integrate' $ W.stack $ W.workspace $ W.current s
  mapW f ws

  return $ ws /= []

-- | Check if scratchpad is exclusive
isExclusive :: ExclusiveScratchpads -> String -> X Bool
isExclusive sps n = withWindowSet $ \s -> do
  let q = isExclusiveQuery <&&> joinQueries (map query $ filter ((n==).name) sps)

  ws <- filterM (runQuery q) $ W.allWindows s
  return $ ws /= []

-- | Check if given window is a scratchpad
isScratchpad :: ExclusiveScratchpads -> Window -> X Bool
isScratchpad sps w = withWindowSet $ \s -> do
  let q = joinQueries $ map query sps

  ws <- filterM (runQuery q) $ W.allWindows s
  return $ elem w ws

-- | Query that matches if the window is exclusive
isExclusiveQuery :: Query Bool
isExclusiveQuery = (notElem "CS_NOEXCLUSIVE" . words) <$> stringProperty "_XMONAD_TAGS"

-- | Build a disjunction from a list of clauses
joinQueries :: [Query Bool] -> Query Bool
joinQueries = foldl (<||>) (liftX $ return False)

-- | Useful for "mapping a function over a list of windows"
mapW :: (a -> WindowSet -> WindowSet) -> [a] -> X ()
mapW f ws = windows $ foldl (.) id (map f ws)

-- | Counterpart to whenX
unlessX :: X Bool -> X () -> X ()
unlessX = whenX . fmap not

-- | If the focused window is a scratchpad, the scratchpad gets reset to the original
-- placement specified with the hook and becomes exclusive again
resetExclusiveSp :: ExclusiveScratchpads -- ^ List of exclusive scratchpads
                 -> X ()
resetExclusiveSp sps = withFocused $ \w -> whenX (isScratchpad sps w) $ do
  let msp = filterM (flip runQuery w . query) sps

  unlessX (null <$> msp) $ do
    mh <- (head . map hook) <$> msp  -- msp /= [], so `head` is fine
    n  <- (head . map name) <$> msp  -- same

    (windows . appEndo <=< runQuery mh) w
    hideOthers sps n
    delTag "CS_NOEXCLUSIVE" w

-- | Make a window not exclusive anymore
setNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
               -> Window                -- ^ Window which should be made not
                                        --   exclusive anymore
               -> X ()
setNoexclusive sps w = whenX (isScratchpad sps w) $ addTag "CS_NOEXCLUSIVE" w

-- | Float and drag the window, make it not exclusive anymore
floatMoveNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                     -> Window                -- ^ Window which should be moved
                     -> X ()
floatMoveNoexclusive sps w = setNoexclusive sps w
  >> focus w
  >> mouseMoveWindow w
  >> windows W.shiftMaster

-- | Resize window, make it not exclusive anymore
resizeNoexclusive :: ExclusiveScratchpads  -- ^ List of exclusive scratchpads
                  -> Window                    -- ^ Window which should be resized
                  -> X ()
resizeNoexclusive sps w = setNoexclusive sps w
  >> focus w
  >> mouseResizeWindow w
  >> windows W.shiftMaster

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
