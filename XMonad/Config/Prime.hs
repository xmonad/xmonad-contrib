{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, KindSignatures, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Prime
-- Description :  Draft of a brand new config syntax for xmonad.
-- Copyright   :  Devin Mullins <devin.mullins@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <devin.mullins@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This is a draft of a brand new config syntax for xmonad. It aims to be:
--
--  * easier to copy/paste snippets from the docs
--
--  * easier to get the gist for what's going on, for you imperative programmers
--
-- It's brand new, so it's pretty much guaranteed to break or change syntax.
-- But what's the worst that could happen? Xmonad crashes and logs you out?
-- It probably won't do that. Give it a try.
--
-----------------------------------------------------------------------------

module XMonad.Config.Prime {-# DEPRECATED "This module is a perpetual draft and will therefore be removed from xmonad-contrib in the near future." #-} (
-- Note: The identifiers here are listed in the order that makes the most sense
-- for a user, while the definitions below are listed in the order that makes
-- the most sense for a developer.

-- * Start here
-- $start_here
xmonad,
nothing,
-- * Attributes you can set
-- $settables
normalBorderColor,
focusedBorderColor,
terminal,
modMask,
borderWidth,
focusFollowsMouse,
clickJustFocuses,
SettableClass(..),
UpdateableClass(..),

-- * Attributes you can add to
-- $summables
manageHook,
handleEventHook,
workspaces,
logHook,
startupHook,
clientMask,
rootMask,
SummableClass(..),

-- * Attributes you can add to or remove from
-- $removables
keys,
mouseBindings,
RemovableClass(..),

-- * Modifying the list of workspaces
-- $workspaces
withWorkspaces,
wsNames,
wsKeys,
wsActions,
wsSetName,

-- * Modifying the screen keybindings
-- $screens
withScreens,
sKeys,
sActions,
onScreens,

-- * Modifying the layoutHook
-- $layout
addLayout,
resetLayout,
modifyLayout,

-- * Updating the XConfig en masse
-- $update
startWith,
apply,
applyIO,

-- * The rest of the world
-- | Everything you know and love from the core "XMonad" module is available
-- for use in your config file, too.
module XMonad,
-- | (Almost) everything you know and love from the Haskell "Prelude" is
-- available for use in your config file. Note that '>>' has been overriden, so
-- if you want to create do-blocks for normal monads, you'll need some let
-- statements or a separate module. (See the Troubleshooting section.)
module Prelude,

-- * Core
-- | These are the building blocks on which the config language is built.
-- Regular people shouldn't need to know about these.
Prime,
Arr,
(>>),
ifThenElse,

-- * Example config
-- $example

-- * Troubleshooting
-- $troubleshooting
) where

import Prelude hiding ((>>), mod)
import qualified Prelude as P ((>>=), (>>))

import XMonad.Prelude (All)

import XMonad hiding (xmonad, XConfig(..))
import XMonad (XConfig(XConfig))
import qualified XMonad.StackSet as W
import qualified XMonad as X (xmonad, XConfig(..))

import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap, removeKeysP, removeMouseBindings)

-- $start_here
-- To start with, create a @~\/.xmonad\/xmonad.hs@ that looks like this:
--
-- > {-# LANGUAGE RebindableSyntax #-}
-- > import XMonad.Config.Prime
-- >
-- > -- Imports go here.
-- >
-- > main = xmonad $ do
-- >   nothing
-- >   -- Configs go here.
--
-- This will give you a default xmonad install, with room to grow. The lines
-- starting with double dashes are comments. You may delete them. Note that
-- Haskell is a bit precise about indentation. Make sure all the statements in
-- your do-block start at the same column, and make sure that any multi-line
-- statements are formatted with a hanging indent. (For an example, see the
-- 'keys =+' statement in the /Example config/ section, below.)
--
-- After changing your config file, restart xmonad with mod-q (where, by
-- default, "mod" == "alt").

--
-- The Prime "Monad"
--

-- | A Prime is a function that transforms an XConfig. It's not a monad, but we
-- turn on RebindableSyntax so we can abuse the pretty do notation.
type Prime l l' = Arr (XConfig l) (XConfig l')

-- | An Arr is a generalization of Prime. Don't reference the type, if you can
-- avoid it. It might go away in the future.
type Arr x y = x -> IO y

-- | Composes two Arrs using 'Prelude.>>=' from "Prelude".
(>>) :: Arr x y -> Arr y z -> Arr x z
(>>) x y c = (P.>>=) (x c) y

-- | Because of RebindableSyntax, this is necessary to enable you to use
-- if-then-else expressions. No need to call it directly.
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  a _ = a
ifThenElse False _ b = b

-- | This is the xmonad main function. It passes 'XMonad.Config.def' (the
-- default 'XConfig') into your do-block, takes the modified config out of your
-- do-block, and then runs xmonad.
--
-- The do-block is a 'Prime'. Advanced readers can skip right to that
-- definition.

xmonad :: (Default a, Read (l Window), LayoutClass l Window) =>
          (a -> IO (XConfig l)) -> IO ()
xmonad prime = (P.>>=) (prime def) X.xmonad

-- | This doesn't modify the config in any way. It's just here for your initial
-- config because Haskell doesn't allow empty do-blocks. Feel free to delete it
-- once you've added other stuff.
nothing :: Prime l l
nothing = return

-- $settables
-- These are a bunch of attributes that you can set. Syntax looks like this:
--
-- >   terminal =: "urxvt"
--
-- Strings are double quoted, Dimensions are unquoted integers, booleans are
-- 'True' or 'False' (case-sensitive), and 'modMask' is usually 'mod1Mask' or
-- 'mod4Mask'.

class UpdateableClass s x y | s -> x y where
  -- | This lets you apply a function to an attribute (i.e. read, modify, write).
  (=.) :: s c -> (x -> y) -> Arr c c

class SettableClass s x y | s -> x y where
  -- | This lets you modify an attribute.
  (=:) :: s c -> y -> Arr c c

-- Undecideable instance. But it's nice to leave open the possibility to write
-- fields you can't read (e.g. `wmName =: ...`).
instance UpdateableClass s x y => SettableClass s x y where
  s =: y = s =. const y

data Settable x c = Settable (c -> x)      -- getter
                             (x -> c -> c) -- setter

instance UpdateableClass (Settable x) x x where
  (Settable g s =. f) c = return $ s (f $ g c) c

-- | Non-focused windows border color. Default: @\"#dddddd\"@
normalBorderColor :: Settable String (XConfig l)
normalBorderColor = Settable X.normalBorderColor (\x c -> c { X.normalBorderColor = x })

-- | Focused windows border color. Default: @\"#ff0000\"@
focusedBorderColor :: Settable String (XConfig l)
focusedBorderColor = Settable X.focusedBorderColor (\x c -> c { X.focusedBorderColor = x })

-- | The preferred terminal application. Default: @\"xterm\"@
terminal :: Settable String (XConfig l)
terminal = Settable X.terminal (\x c -> c { X.terminal = x })

-- | The mod modifier, as used by key bindings. Default: @mod1Mask@ (which is
-- probably alt on your computer).
modMask :: Settable KeyMask (XConfig l)
modMask = Settable X.modMask (\x c -> c { X.modMask = x })

-- | The border width (in pixels). Default: @1@
borderWidth :: Settable Dimension (XConfig l)
borderWidth = Settable X.borderWidth (\x c -> c { X.borderWidth = x })

-- | Whether window focus follows the mouse cursor on move, or requires a mouse
-- click. (Mouse? What's that?) Default: @True@
focusFollowsMouse :: Settable Bool (XConfig l)
focusFollowsMouse = Settable X.focusFollowsMouse (\x c -> c { X.focusFollowsMouse = x })

-- | If True, a mouse click on an inactive window focuses it, but the click is
-- not passed to the window. If False, the click is also passed to the window.
-- Default @True@
clickJustFocuses :: Settable Bool (XConfig l)
clickJustFocuses = Settable X.clickJustFocuses (\x c -> c { X.clickJustFocuses = x })

-- $summables
-- In addition to being able to set these attributes, they have a special
-- syntax for being able to add to them. The operator is @=+@ (the plus comes
-- /after/ the equals), but each attribute has a different syntax for what
-- comes after the operator.

class SummableClass s y | s -> y where
  -- | This lets you add to an attribute.
  (=+) :: s c -> y -> Arr c c
  infix 0 =+

data Summable x y c = Summable (c -> x)      -- getter
                               (x -> c -> c) -- setter
                               (x -> y -> x) -- accumulator

instance UpdateableClass (Summable x y) x x where
  (Summable g s _ =. f) c = return $ s (f $ g c) c

instance SummableClass (Summable x y) y where
  (Summable g s a =+ y) c = return $ s (g c `a` y) c

-- | The action to run when a new window is opened. Default:
--
-- >   manageHook =: composeAll [className =? "MPlayer" --> doFloat, className =? "Gimp" --> doFloat]
--
-- To add more rules to this list, you can say, for instance:
--
-- > import XMonad.StackSet
-- > ...
-- >   manageHook =+ (className =? "Emacs" --> doF kill)
-- >   manageHook =+ (className =? "Vim" --> doF shiftMaster)
--
-- Note that operator precedence mandates the parentheses here.
manageHook :: Summable ManageHook ManageHook (XConfig l)
manageHook = Summable X.manageHook (\x c -> c { X.manageHook = x }) (<+>)

-- | Custom X event handler. Return @All True@ if the default handler should
-- also be run afterwards. Default does nothing. To add an event handler:
--
-- > import XMonad.Hooks.ServerMode
-- > ...
-- >   handleEventHook =+ serverModeEventHook
handleEventHook :: Summable (Event -> X All) (Event -> X All) (XConfig l)
handleEventHook = Summable X.handleEventHook (\x c -> c { X.handleEventHook = x }) (<+>)

-- | List of workspaces' names. Default: @map show [1 .. 9 :: Int]@. Adding
-- appends to the end:
--
-- >   workspaces =+ ["0"]
--
-- This is useless unless you also create keybindings for this.
workspaces :: Summable [String] [String] (XConfig l)
workspaces = Summable X.workspaces (\x c -> c { X.workspaces = x }) (++)

-- | The action to perform when the windows set is changed. This happens
-- whenever focus change, a window is moved, etc. @logHook =+@ takes an @X ()@
-- and appends it via '(>>)'. For instance:
--
-- > import XMonad.Hooks.ICCCMFocus
-- > ...
-- >   logHook =+ takeTopFocus
--
-- Note that if your expression is parametrically typed (e.g. of type
-- @MonadIO m => m ()@), you'll need to explicitly annotate it, like so:
--
-- >   logHook =+ (io $ putStrLn "Hello, world!" :: X ())
logHook :: Summable (X ()) (X ()) (XConfig l)
logHook = Summable X.logHook (\x c -> c { X.logHook = x }) (P.>>)

-- | The action to perform on startup. @startupHook =+@ takes an @X ()@ and
-- appends it via '(>>)'. For instance:
--
-- > import XMonad.Hooks.SetWMName
-- > ...
-- >   startupHook =+ setWMName "LG3D"
--
-- Note that if your expression is parametrically typed (e.g. of type
-- @MonadIO m => m ()@), you'll need to explicitly annotate it, as documented
-- in 'logHook'.
startupHook :: Summable (X ()) (X ()) (XConfig l)
startupHook = Summable X.startupHook (\x c -> c { X.startupHook = x }) (P.>>)

-- | The client events that xmonad is interested in. This is useful in
-- combination with handleEventHook. Default: @structureNotifyMask .|.
-- enterWindowMask .|. propertyChangeMask@
--
-- >   clientMask =+ keyPressMask .|. keyReleaseMask
clientMask :: Summable EventMask EventMask (XConfig l)
clientMask = Summable X.clientMask (\x c -> c { X.clientMask = x }) (.|.)

-- | The root events that xmonad is interested in. This is useful in
-- combination with handleEventHook. Default: @substructureRedirectMask .|.
-- substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|.
-- structureNotifyMask .|. buttonPressMask@
rootMask :: Summable EventMask EventMask (XConfig l)
rootMask = Summable X.rootMask (\x c -> c { X.rootMask = x }) (.|.)

-- $removables
-- The following support the the @=+@ for adding items and the @=-@ operator
-- for removing items.

class RemovableClass r y | r -> y where
  -- | This lets you remove from an attribute.
  (=-) :: r c -> y -> Arr c c
  infix 0 =-

data Keys c = Keys { kAdd :: [(String, X ())] -> c -> c,
                     kRemove :: [String] -> c -> c }

instance SummableClass Keys [(String, X ())] where
  Keys { kAdd = a } =+ newKeys = return . a newKeys

instance RemovableClass Keys [String] where
  Keys { kRemove = r } =- sadKeys = return . r sadKeys

-- | Key bindings to 'X' actions. Default: see @`man xmonad`@. 'keys'
-- takes a list of keybindings specified emacs-style, as documented in
-- 'XMonad.Util.EZConfig.mkKeyMap'. For example, to change the "kill window"
-- key:
--
-- >   keys =- ["M-S-c"]
-- >   keys =+ [("M-M1-x", kill)]
keys :: Keys (XConfig l)
keys = Keys {
  -- Note that since checkKeymap happens on newKeys, it doesn't check for
  -- duplicates between repeated applications. Probably OK. (Especially since
  -- overriding defaults is a common behavior.) Also note that there's no
  -- reference cycle here. Yay!
  kAdd = \newKeys c -> (c `additionalKeysP` newKeys) { X.startupHook = (P.>>) (X.startupHook c) (checkKeymap c newKeys) },
  kRemove = flip removeKeysP
}

data MouseBindings c = MouseBindings { mAdd :: [((ButtonMask, Button), Window -> X ())] -> c -> c,
                                       mRemove :: [(ButtonMask, Button)] -> c -> c }

instance SummableClass MouseBindings [((ButtonMask, Button), Window -> X ())] where
  MouseBindings { mAdd = a } =+ newBindings = return . a newBindings

instance RemovableClass MouseBindings [(ButtonMask, Button)] where
  MouseBindings { mRemove = r } =- sadBindings = return . r sadBindings

-- | Mouse button bindings to an 'X' actions on a window. Default: see @`man
-- xmonad`@. To make mod-<scrollwheel> switch workspaces:
--
-- > import XMonad.Actions.CycleWS (nextWS, prevWS)
-- > ...
-- >   mouseBindings =+ [((mod4Mask, button4), const prevWS),
-- >                     ((mod4Mask, button5), const nextWS)]
--
-- Note that you need to specify the numbered mod-mask e.g. 'mod4Mask' instead
-- of just 'modMask'.
mouseBindings :: MouseBindings (XConfig l)
mouseBindings = MouseBindings {
  mAdd = flip additionalMouseBindings,
  mRemove = flip removeMouseBindings
}

-- $workspaces
-- Workspaces can be configured through 'workspaces', but then the 'keys' need
-- to be set, and this can be a bit laborious. 'withWorkspaces' provides a
-- convenient mechanism for common workspace updates.

-- | Configure workspaces through a Prime-like interface. Example:
--
-- >   withWorkspaces $ do
-- >     wsKeys =+ ["0"]
-- >     wsActions =+ [("M-M1-", windows . swapWithCurrent)]
-- >     wsSetName 1 "mail"
--
-- This will set 'workspaces' and add the necessary keybindings to 'keys'. Note
-- that it won't remove old keybindings; it's just not that clever.
withWorkspaces :: Arr WorkspaceConfig WorkspaceConfig -> Prime l l
withWorkspaces wsarr xconf = (P.>>=) (wsarr def) $ \wsconf -> wsprime wsconf xconf
  where wsprime :: WorkspaceConfig -> Prime l l
        wsprime wsconf =
          (workspaces =: allNames) >>
          (keys =+ [(mod ++ key, action name) | (name, key) <- zip allNames (wsKeys_ wsconf),
                                                (mod, action) <- wsActions_ wsconf])
          where allNames = zipWith chooseName (wsNames_ wsconf) (wsKeys_ wsconf)
                chooseName name keyspec = if not (null name) then name else keyspec

data WorkspaceConfig = WorkspaceConfig {
  wsNames_ :: [String],
  wsKeys_ :: [String],
  wsActions_ :: [(String, String -> X ())]
}

instance Default WorkspaceConfig where
  def = WorkspaceConfig {
    wsNames_ = repeat "",
    wsKeys_ = map (:[]) ['1'..'9'], -- The hungry monkey eats dots and turns them into numbers.
    wsActions_ = [("M-", windows . W.greedyView),
                  ("M-S-", windows . W.shift)]
  }

-- | The list of workspace names, like 'workspaces' but with two differences:
--
--   1. If any entry is the empty string, it'll be replaced with the
--      corresponding entry in 'wsKeys'.
--   2. The list is truncated to the size of 'wsKeys'.
--
-- The default value is @'repeat' ""@.
--
-- If you'd like to create workspaces without associated keyspecs, you can do
-- that afterwards, outside the 'withWorkspaces' block, with @'workspaces' =+@.
wsNames :: Settable [String] WorkspaceConfig
wsNames = Settable wsNames_ (\x c -> c { wsNames_ = x })

-- | The list of workspace keys. These are combined with the modifiers in
-- 'wsActions' to form the keybindings for navigating to workspaces. Default:
-- @["1","2",...,"9"]@.
wsKeys :: Summable [String] [String] WorkspaceConfig
wsKeys = Summable wsKeys_ (\x c -> c { wsKeys_ = x }) (++)

-- | Mapping from key prefix to command. Its type is @[(String, String ->
-- X())]@. The key prefix may be a modifier such as @\"M-\"@, or a submap
-- prefix such as @\"M-a \"@, or both, as in @\"M-a M-\"@. The command is a
-- function that takes a workspace name and returns an @X ()@. 'withWorkspaces'
-- creates keybindings for the cartesian product of 'wsKeys' and 'wsActions'.
--
-- Default:
--
-- > [("M-", windows . W.greedyView),
-- >  ("M-S-", windows . W.shift)]
wsActions :: Summable [(String, String -> X ())] [(String, String -> X ())] WorkspaceConfig
wsActions = Summable wsActions_ (\x c -> c { wsActions_ = x }) (++)

-- | A convenience for just modifying one entry in 'wsNames', in case you only
-- want a few named workspaces. Example:
--
-- >     wsSetName 1 "mail"
-- >     wsSetName 2 "web"
wsSetName :: Int -> String -> Arr WorkspaceConfig WorkspaceConfig
wsSetName index newName = wsNames =. zipWith (curry maybeSet) [0..]
  where maybeSet (i, oldName) | i == (index - 1) = newName
                              | otherwise = oldName

-- $screens
-- 'withScreens' provides a convenient mechanism to set keybindings for moving
-- between screens, much like 'withWorkspaces'.

-- | Configure screen keys through a Prime-like interface:
--
-- >   withScreens $ do
-- >     sKeys =: ["e", "r"]
--
-- This will add the necessary keybindings to 'keys'. Note that it won't remove
-- old keybindings; it's just not that clever.
withScreens :: Arr ScreenConfig ScreenConfig -> Prime l l
withScreens sarr xconf = (P.>>=) (sarr def) $ \sconf -> sprime sconf xconf
  where sprime :: ScreenConfig -> Prime l l
        sprime sconf =
          keys =+ [(mod ++ key, action sid) | (sid, key) <- zip [0..] (sKeys_ sconf),
                                              (mod, action) <- sActions_ sconf]

data ScreenConfig = ScreenConfig {
  sKeys_ :: [String],
  sActions_ :: [(String, ScreenId -> X ())]
}

instance Default ScreenConfig where
  def = ScreenConfig {
    sKeys_ = ["w", "e", "r"],
    sActions_ = [("M-", windows . onScreens W.view),
                 ("M-S-", windows . onScreens W.shift)]
  }


-- | The list of screen keys. These are combined with the modifiers in
-- 'sActions' to form the keybindings for navigating to workspaces. Default:
-- @["w","e","r"]@.
sKeys :: Summable [String] [String] ScreenConfig
sKeys = Summable sKeys_ (\x c -> c { sKeys_ = x }) (++)

-- | Mapping from key prefix to command. Its type is @[(String, ScreenId ->
-- X())]@. Works the same as 'wsActions' except for a different function type.
--
-- Default:
--
-- > [("M-", windows . onScreens W.view),
-- >  ("M-S-", windows . onScreens W.shift)]
sActions :: Summable [(String, ScreenId -> X ())] [(String, ScreenId -> X ())] ScreenConfig
sActions = Summable sActions_ (\x c -> c { sActions_ = x }) (++)

-- | Converts a stackset transformer parameterized on the workspace type into one
-- parameterized on the screen type. For example, you can use @onScreens W.view
-- 0@ to navigate to the workspace on the 0th screen. If the screen id is not
-- recognized, the returned transformer acts as an identity function.
onScreens :: Eq s => (i -> W.StackSet i l a s sd -> W.StackSet i l a s sd) ->
                     s -> W.StackSet i l a s sd -> W.StackSet i l a s sd
onScreens f sc ws = maybe id f (W.lookupWorkspace sc ws) ws

-- $layout
-- Layouts are special. You can't modify them using the @=:@ or @=.@ operator.
-- You need to use the following functions.

-- | Add a layout to the list of layouts choosable with mod-space. For instance:
--
-- > import XMonad.Layout.Tabbed
-- > ...
-- >   addLayout simpleTabbed
addLayout :: (LayoutClass l Window, LayoutClass r Window) => r Window -> Prime l (Choose l r)
addLayout r c = return c { X.layoutHook = X.layoutHook c ||| r }

-- | Reset the layoutHook from scratch. For instance, to get rid of the wide
-- layout:
--
-- >   resetLayout $ Tall 1 (3/100) (1/2) ||| Full
--
-- (The dollar is like an auto-closing parenthesis, so all the stuff to the
-- right of it is treated like an argument to resetLayout.)
resetLayout :: (LayoutClass r Window) => r Window -> Prime l r
resetLayout r c = return c { X.layoutHook = r }

-- | Modify your 'layoutHook' with some wrapper function. You probably want to call
-- this after you're done calling 'addLayout'. Example:
--
-- > import XMonad.Layout.NoBorders
-- > ...
-- >   modifyLayout smartBorders
modifyLayout :: (LayoutClass r Window) => (l Window -> r Window) -> Prime l r
modifyLayout f c = return c { X.layoutHook = f $ X.layoutHook c }

-- $update
-- Finally, there are a few contrib modules that bundle multiple attribute
-- updates together. There are three types: 1) wholesale replacements for the
-- default config, 2) pure functions on the config, and 3) IO actions on the
-- config. The syntax for each is different. Examples:
--
-- 1) To start with a 'XMonad.Config.Gnome.gnomeConfig' instead of the default,
-- we use 'startWith':
--
-- > import XMonad.Config.Gnome
-- > ...
-- >   startWith gnomeConfig
--
-- 2) 'XMonad.Hooks.UrgencyHook.withUrgencyHook' is a pure function, so we need
-- to use 'apply':
--
-- > import XMonad.Hooks.UrgencyHook
-- > ...
-- >   apply $ withUrgencyHook dzenUrgencyHook
--
-- 3) 'XMonad.Hooks.DynamicLog.xmobar' returns an @IO (XConfig l)@, so we need
-- to use 'applyIO':
--
-- > import XMonad.Hooks.DynamicLog
-- > ...
-- >   applyIO xmobar

-- | Replace the current 'XConfig' with the given one. If you use this, you
-- probably want it to be the first line of your config.
startWith :: XConfig l' -> Prime l l'
startWith = const . return

-- | Turns a pure function on 'XConfig' into a 'Prime'.
apply :: (XConfig l -> XConfig l') -> Prime l l'
apply f = return . f

-- | Turns an IO function on 'XConfig' into a 'Prime'.
applyIO :: (XConfig l -> IO (XConfig l')) -> Prime l l'
applyIO = id  -- This is here in case we want to change the Prime type later.

-- $example
-- As an example, I've included below a subset of my current config. Note that
-- my import statements specify individual identifiers in parentheticals.
-- That's optional. The default is to import the entire module. I just find it
-- helpful to remind me where things came from.
--
-- > {-# LANGUAGE RebindableSyntax #-}
-- > import XMonad.Config.Prime
-- >
-- > import XMonad.Actions.CycleWS (prevWS, nextWS)
-- > import XMonad.Actions.SwapWorkspaces (swapWithCurrent)
-- > import XMonad.Actions.WindowNavigation (withWindowNavigation)
-- > import XMonad.Layout.Fullscreen (fullscreenSupport)
-- > import XMonad.Layout.NoBorders (smartBorders)
-- > import XMonad.Layout.Tabbed (simpleTabbed)
-- >
-- > main = xmonad $ do
-- >   modMask =: mod4Mask
-- >   normalBorderColor =: "#222222"
-- >   terminal =: "urxvt"
-- >   focusFollowsMouse =: False
-- >   resetLayout $ Tall 1 (3/100) (1/2) ||| simpleTabbed
-- >   modifyLayout smartBorders
-- >   apply fullscreenSupport
-- >   applyIO $ withWindowNavigation (xK_w, xK_a, xK_s, xK_d)
-- >   withWorkspaces $ do
-- >     wsKeys =+ ["0"]
-- >     wsActions =+ [("M-M1-", windows . swapWithCurrent)]
-- >   keys =+ [
-- >       ("M-,",                      sendMessage $ IncMasterN (-1)),
-- >       ("M-.",                      sendMessage $ IncMasterN 1),
-- >       ("M-M1-d",                   spawn "date | dzen2 -fg '#eeeeee' -p 2"),
-- >       ("C-S-q",                    return ()),
-- >       ("<XF86AudioLowerVolume>",   spawn "amixer set Master 5%-"),
-- >       ("<XF86AudioRaiseVolume>",   spawn "amixer set Master 5%+"),
-- >       ("M-M1-x",                   kill),
-- >       ("M-i",                      prevWS),
-- >       ("M-o",                      nextWS)
-- >     ]

-- $troubleshooting
-- === Only the last line of my config seems to take effect. What gives?
-- You're missing the @{-\# LANGUAGE RebindableSyntax \#-}@ line at the top.
--
-- === How do I do use normal monads like 'X' or 'IO'?
-- Here are a couple of ways:
--
-- > import qualified Prelude as P
-- > ...
-- > test1, test2 :: X ()
-- > test1 = spawn "echo Hi" P.>> spawn "echo Bye"
-- > test2 = do spawn "echo Hi"
-- >            spawn "echo Bye"
-- >   where (>>) = (P.>>)
--
-- === How do I use the old keyboard syntax?
-- You can use 'apply' and supply your own Haskell function. For instance:
--
-- > apply $ flip additionalKeys $ [((mod1Mask, xK_z), spawn "date | dzen2 -fg '#eeeeee' -p 2")]
--
-- === How do I run a command before xmonad starts (like 'spawnPipe')?
-- If you're using it for a status bar, see if 'XMonad.Hooks.DynamicLog.dzen'
-- or 'XMonad.Hooks.DynamicLog.xmobar' does what you want. If so, you can apply
-- it with 'applyIO'.
--
-- If not, you can write your own @XConfig l -> IO (XConfig l)@ and apply it
-- with 'applyIO'. When writing this function, see the above tip about using
-- normal monads.
--
-- Alternatively, you could do something like this this:
--
-- > import qualified Prelude as P (>>)
-- >
-- > main =
-- >   openFile ".xmonad.log" AppendMode >>= \log ->
-- >   hSetBuffering log LineBuffering P.>>
-- >   (xmonad $ do
-- >      nothing -- Prime config here.
-- >   )
