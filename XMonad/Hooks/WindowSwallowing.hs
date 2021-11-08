{-# LANGUAGE NamedFieldPuns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.WindowSwallowing
-- Description :  Temporarily hide parent windows when opening other programs.
-- Copyright   :  (c) 2020 Leon Kowarschick
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Leon Kowarschick. <thereal.elkowar@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides a handleEventHook that implements window swallowing.
--
-- If you open a GUI-window (i.e. feh) from the terminal,
-- the terminal will normally still be shown on screen, unnecessarily
-- taking up space on the screen.
-- With window swallowing, can detect that you opened a window from within another
-- window, and allows you "swallow" that parent window for the time the new
-- window is running.
--
-- __NOTE__: This module depends on @pstree@ to analyze the process hierarchy, so make
-- sure that is on your @$PATH@.
--
-- __NOTE__ that this does not always work perfectly:
--
-- - Because window swallowing needs to check the process hierarchy, it requires
--   both the child and the parent to be distinct processes. This means that
--   applications which implement instance sharing cannot be supported by window swallowing.
--   Most notably, this excludes some terminal emulators as well as tmux
--   from functioning as the parent process. It also excludes a good amount of
--   child programs, because many graphical applications do implement instance sharing.
--   For example, window swallowing will probably not work with your browser.
--
-- - To check the process hierarchy, we need to be able to get the process ID
--   by looking at the window. This requires the @_NET_WM_PID@ X-property to be set.
--   If any application you want to use this with does not provide the @_NET_WM_PID@,
--   there is not much you can do except for reaching out to the author of that
--   application and asking them to set that property.
-----------------------------------------------------------------------------
module XMonad.Hooks.WindowSwallowing
  ( -- * Usage
    -- $usage
    swallowEventHook, swallowEventHookSub
  )
where
import           XMonad
import           XMonad.Prelude
import qualified XMonad.StackSet               as W
import           XMonad.Layout.SubLayouts
import qualified XMonad.Util.ExtensibleState   as XS
import           XMonad.Util.WindowProperties
import           XMonad.Util.Run                ( runProcessWithInput )
import qualified Data.Map.Strict               as M

-- $usage
-- You can use this module by including  the following in your @~\/.xmonad/xmonad.hs@:
--
-- > import XMonad.Hooks.WindowSwallowing
--
-- and using 'swallowEventHook' somewhere in your 'handleEventHook', for example:
--
-- > myHandleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "Termite") (return True)
--
-- The variant 'swallowEventHookSub' can be used if a layout from "XMonad.Layouts.SubLayouts" is used;
-- instead of swallowing the window it will merge the child window with the parent. (this does not work with floating windows)
--
-- For more information on editing your handleEventHook and key bindings,
-- see "XMonad.Doc.Extending".

-- | Run @action@ iff both parent- and child queries match and the child
-- is a child by PID.
--
-- A 'MapRequestEvent' is called right before a window gets opened. We
-- intercept that call to possibly open the window ourselves, swapping
-- out it's parent processes window for the new window in the stack.
handleMapRequestEvent :: Query Bool -> Query Bool -> Window -> (Window -> X ()) -> X ()
handleMapRequestEvent parentQ childQ childWindow action =
  -- For a window to be opened from within another window, that other window
  -- must be focused. Thus the parent window that would be swallowed has to be
  -- the currently focused window.
  withFocused $ \parentWindow -> do
    -- First verify that both windows match the given queries
    parentMatches <- runQuery parentQ parentWindow
    childMatches  <- runQuery childQ childWindow
    when (parentMatches && childMatches) $ do
      -- read the windows _NET_WM_PID properties
      childWindowPid  <- getProp32s "_NET_WM_PID" childWindow
      parentWindowPid <- getProp32s "_NET_WM_PID" parentWindow
      case (parentWindowPid, childWindowPid) of
        (Just (parentPid : _), Just (childPid : _)) -> do
          -- check if the new window is a child process of the last focused window
          -- using the process ids.
          isChild <- liftIO $ fi childPid `isChildOf` fi parentPid
          when isChild $ do
            action parentWindow
        _ -> return ()
      return ()

-- | handleEventHook that will merge child windows via
-- "XMonad.Layouts.SubLayouts" when they are opened from another window.
swallowEventHookSub
  :: Query Bool -- ^ query the parent window has to match for window swallowing to occur.
                --   Set this to @return True@ to run swallowing for every parent.
  -> Query Bool -- ^ query the child window has to match for window swallowing to occur.
                --   Set this to @return True@ to run swallowing for every child
  -> Event      -- ^ The event to handle.
  -> X All
swallowEventHookSub parentQ childQ event =
  All True <$ case event of
    MapRequestEvent{ev_window=childWindow} ->
      handleMapRequestEvent parentQ childQ childWindow $ \parentWindow -> do
        manage childWindow
        sendMessage (Merge parentWindow childWindow)
    _ -> pure ()

-- | handleEventHook that will swallow child windows when they are
-- opened from another window.
swallowEventHook
  :: Query Bool -- ^ query the parent window has to match for window swallowing to occur.
                --   Set this to @return True@ to run swallowing for every parent.
  -> Query Bool -- ^ query the child window has to match for window swallowing to occur.
                --   Set this to @return True@ to run swallowing for every child
  -> Event      -- ^ The event to handle.
  -> X All
swallowEventHook parentQ childQ event = do
  case event of
    MapRequestEvent{ev_window=childWindow} ->
      handleMapRequestEvent parentQ childQ childWindow $ \parentWindow -> do
        -- We set the newly opened window as the focused window, replacing the parent window.
        -- If the parent window was floating, we transfer that data to the child,
        -- such that it shows up at the same position, with the same dimensions.
        windows
          ( W.modify' (\x -> x { W.focus = childWindow })
          . moveFloatingState parentWindow childWindow
          )
        XS.modify (addSwallowedParent parentWindow childWindow)

    -- This is called in many circumstances, most notably for us:
    -- right before a window gets closed. We store the current
    -- state of the window stack here, such that we know where the
    -- child window was on the screen when restoring the swallowed parent process.
    ConfigureEvent{} -> withWindowSet $ \ws -> do
      XS.modify . setStackBeforeWindowClosing . currentStack $ ws
      XS.modify . setFloatingBeforeWindowClosing . W.floating $ ws

    -- This is called right after any window closes.
    DestroyWindowEvent { ev_event = eventId, ev_window = childWindow } ->
      -- Because DestroyWindowEvent is emitted a lot more often then you think,
      -- this check verifies that the event is /actually/ about closing a window.
      when (eventId == childWindow) $ do
        -- we get some data from the extensible state, most notably we ask for
        -- the \"parent\" window of the now closed window.
        maybeSwallowedParent <- XS.gets (getSwallowedParent childWindow)
        maybeOldStack        <- XS.gets stackBeforeWindowClosing
        oldFloating          <- XS.gets floatingBeforeClosing
        case (maybeSwallowedParent, maybeOldStack) of
          -- If there actually is a corresponding swallowed parent window for this window,
          -- we will try to restore it.
          -- Because there are some cases where the stack-state is not stored correctly in the ConfigureEvent hook,
          -- we have to first check if the stack-state is valid.
          -- If it is, we can restore the parent exactly where the child window was before being closed.
          -- If the stored stack-state is invalid however, we still restore the window
          -- by just inserting it as the focused window in the stack.
          --
          -- After restoring, we remove the information about the swallowing from the state.
          (Just parent, Nothing) -> do
            windows (insertIntoStack parent)
            deleteState childWindow
          (Just parent, Just oldStack) -> do
            stackStoredCorrectly <- do
              curStack <- withWindowSet (return . currentStack)
              let oldLen = length (W.integrate oldStack)
              let curLen = length (W.integrate' curStack)
              return (oldLen - 1 == curLen && childWindow == W.focus oldStack)

            if stackStoredCorrectly
              then windows
                (\ws ->
                  updateCurrentStack
                      (const $ Just $ oldStack { W.focus = parent })
                    $ moveFloatingState childWindow parent
                    $ ws { W.floating = oldFloating }
                )
              else windows (insertIntoStack parent)
            deleteState childWindow
          _ -> return ()
    _ -> return ()
  return $ All True
 where
  deleteState :: Window -> X ()
  deleteState childWindow = do
    XS.modify $ removeSwallowed childWindow
    XS.modify $ setStackBeforeWindowClosing Nothing

-- | insert a window as focused into the current stack, moving the previously focused window down the stack
insertIntoStack :: a -> W.StackSet i l a sid sd -> W.StackSet i l a sid sd
insertIntoStack win = W.modify
  (Just $ W.Stack win [] [])
  (\s -> Just $ s { W.focus = win, W.down = W.focus s : W.down s })

-- | run a pure transformation on the Stack of the currently focused workspace.
updateCurrentStack
  :: (Maybe (W.Stack a) -> Maybe (W.Stack a))
  -> W.StackSet i l a sid sd
  -> W.StackSet i l a sid sd
updateCurrentStack f = W.modify (f Nothing) (f . Just)

currentStack :: W.StackSet i l a sid sd -> Maybe (W.Stack a)
currentStack = W.stack . W.workspace . W.current


-- | move the floating state from one window to another, sinking the original window
moveFloatingState
  :: Ord a
  => a -- ^ window to move from
  -> a -- ^ window to move to
  -> W.StackSet i l a s sd
  -> W.StackSet i l a s sd
moveFloatingState from to ws = ws
  { W.floating = M.delete from $ maybe (M.delete to (W.floating ws))
                                       (\r -> M.insert to r (W.floating ws))
                                       (M.lookup from (W.floating ws))
  }

-- | check if a given process is a child of another process. This depends on "pstree" being in the PATH
-- NOTE: this does not work if the child process does any kind of process-sharing.
isChildOf
  :: Int -- ^ child PID
  -> Int -- ^ parent PID
  -> IO Bool
isChildOf child parent = do
  output <- runProcessWithInput "pstree" ["-T", "-p", show parent] ""
  return $ any (show child `isInfixOf`) $ lines output

data SwallowingState =
  SwallowingState
    { currentlySwallowed       :: M.Map Window Window         -- ^ mapping from child window window to the currently swallowed parent window
    , stackBeforeWindowClosing :: Maybe (W.Stack Window)      -- ^ current stack state right before DestroyWindowEvent is sent
    , floatingBeforeClosing    :: M.Map Window W.RationalRect -- ^ floating map of the stackset right before DestroyWindowEvent is sent
    } deriving (Show)

getSwallowedParent :: Window -> SwallowingState -> Maybe Window
getSwallowedParent win SwallowingState { currentlySwallowed } =
  M.lookup win currentlySwallowed

addSwallowedParent :: Window -> Window -> SwallowingState -> SwallowingState
addSwallowedParent parent child s@SwallowingState { currentlySwallowed } =
  s { currentlySwallowed = M.insert child parent currentlySwallowed }

removeSwallowed :: Window -> SwallowingState -> SwallowingState
removeSwallowed child s@SwallowingState { currentlySwallowed } =
  s { currentlySwallowed = M.delete child currentlySwallowed }

setStackBeforeWindowClosing
  :: Maybe (W.Stack Window) -> SwallowingState -> SwallowingState
setStackBeforeWindowClosing stack s = s { stackBeforeWindowClosing = stack }

setFloatingBeforeWindowClosing
  :: M.Map Window W.RationalRect -> SwallowingState -> SwallowingState
setFloatingBeforeWindowClosing x s = s { floatingBeforeClosing = x }

instance ExtensionClass SwallowingState where
  initialValue = SwallowingState { currentlySwallowed       = mempty
                                 , stackBeforeWindowClosing = Nothing
                                 , floatingBeforeClosing    = mempty
                                 }
