-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.SetWMName
-- Description :  Set the WM name to a given string.
-- Copyright   :  © 2007 Ivan Tarasov <Ivan.Tarasov@gmail.com>
-- License     :  BSD
--
-- Maintainer  :  Ivan.Tarasov@gmail.com
-- Stability   :  experimental
-- Portability :  unportable
--
-- Sets the WM name to a given string, so that it could be detected using
-- _NET_SUPPORTING_WM_CHECK protocol.
--
-- May be useful for making Java GUI programs work, just set WM name to "LG3D"
-- and use Java 1.6u1 (1.6.0_01-ea-b03 works for me) or later.
--
-- To your @~\/.xmonad\/xmonad.hs@ file, add the following line:
--
-- > import XMonad.Hooks.SetWMName
--
-- Then edit your @startupHook@:
--
-- > startupHook = setWMName "LG3D"
--
-- For details on the problems with running Java GUI programs in non-reparenting
-- WMs, see <http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=6429775> and
-- related bugs.
--
-- Setting WM name to "compiz" does not solve the problem, because of yet
-- another bug in AWT code (related to insets). For LG3D insets are explicitly
-- set to 0, while for other WMs the insets are \"guessed\" and the algorithm
-- fails miserably by guessing absolutely bogus values.
--
-- For detailed instructions on editing your hooks, see
-- "XMonad.Doc.Extending#4".
-----------------------------------------------------------------------------

module XMonad.Hooks.SetWMName (
      setWMName
    , getWMName
    )
  where

import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (alloca)

import XMonad
import XMonad.Prelude (fromJust, join, listToMaybe, maybeToList, nub, ord)

-- | sets WM name
setWMName :: String -> X ()
setWMName name = do
    atom_NET_SUPPORTING_WM_CHECK <- netSupportingWMCheckAtom
    atom_NET_WM_NAME <- getAtom "_NET_WM_NAME"
    atom_NET_SUPPORTED_ATOM <- getAtom "_NET_SUPPORTED"
    atom_UTF8_STRING <- getAtom "UTF8_STRING"

    root <- asks theRoot
    supportWindow <- getSupportWindow
    dpy <- asks display
    io $ do
        -- _NET_SUPPORTING_WM_CHECK atom of root and support windows refers to the support window
        mapM_ (\w -> changeProperty32 dpy w atom_NET_SUPPORTING_WM_CHECK wINDOW propModeReplace [fromIntegral supportWindow]) [root, supportWindow]
        -- set WM_NAME in supportWindow (now only accepts latin1 names to eliminate dependency on utf8 encoder)
        changeProperty8 dpy supportWindow atom_NET_WM_NAME atom_UTF8_STRING propModeReplace (latin1StringToCCharList name)
        -- declare which _NET protocols are supported (append to the list if it exists)
        supportedList <- join . maybeToList <$> getWindowProperty32 dpy atom_NET_SUPPORTED_ATOM root
        changeProperty32 dpy root atom_NET_SUPPORTED_ATOM aTOM propModeReplace (nub $ fromIntegral atom_NET_SUPPORTING_WM_CHECK : fromIntegral atom_NET_WM_NAME : supportedList)
  where
    latin1StringToCCharList :: String -> [CChar]
    latin1StringToCCharList = map (fromIntegral . ord)

netSupportingWMCheckAtom :: X Atom
netSupportingWMCheckAtom = getAtom "_NET_SUPPORTING_WM_CHECK"

getSupportWindow :: X Window
getSupportWindow = withDisplay $ \dpy -> do
    atom_NET_SUPPORTING_WM_CHECK <- netSupportingWMCheckAtom
    root <- asks theRoot
    supportWindow <- (listToMaybe =<<) <$> io (getWindowProperty32 dpy atom_NET_SUPPORTING_WM_CHECK root)
    validateWindow (fmap fromIntegral supportWindow)
  where
    validateWindow :: Maybe Window -> X Window
    validateWindow w = do
        valid <- maybe (return False) isValidWindow w
        if valid then
            return $ fromJust w
          else
            createSupportWindow

    -- is there a better way to check the validity of the window?
    isValidWindow :: Window -> X Bool
    isValidWindow w = withDisplay $ \dpy -> io $ alloca $ \p -> do
        status <- xGetWindowAttributes dpy w p
        return (status /= 0)

    -- this code was translated from C (see OpenBox WM, screen.c)
    createSupportWindow :: X Window
    createSupportWindow = withDisplay $ \dpy -> do
        root <- asks theRoot
        let visual = defaultVisual dpy (defaultScreen dpy)  -- should be CopyFromParent (=0), but the constructor is hidden in X11.XLib
        window <- io $ allocaSetWindowAttributes $ \winAttrs -> do
            set_override_redirect winAttrs True         -- WM cannot decorate/move/close this window
            set_event_mask winAttrs propertyChangeMask  -- not sure if this is needed
            let bogusX = -100
                bogusY = -100
              in
                createWindow dpy root bogusX bogusY 1 1 0 0 inputOutput visual (cWEventMask .|. cWOverrideRedirect) winAttrs
        io $ mapWindow dpy window   -- not sure if this is needed
        io $ lowerWindow dpy window -- not sure if this is needed
        return window

-- | Get WM name.
getWMName :: X String
getWMName = getSupportWindow >>= runQuery title
