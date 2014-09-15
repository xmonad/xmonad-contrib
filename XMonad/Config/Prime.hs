{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies, KindSignatures, MultiParamTypeClasses, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.Prime
-- Copyright   :  Devin Mullins <devinmullins@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <devinmullins@gmail.com>
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

module XMonad.Config.Prime (
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

-- * Modifying the layoutHook
-- $layout
addLayout,
resetLayout,
modifyLayout,

-- * Update entire XConfig
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
(>>),

-- * Example config
-- $example

-- * Troubleshooting
-- $troubleshooting
) where

import Prelude hiding ((>>))
import qualified Prelude as P ((>>=), (>>))

import qualified Data.Map as M
import Data.Monoid (All)

import XMonad hiding (xmonad, XConfig(..))
import XMonad (XConfig(XConfig))
import qualified XMonad as X (xmonad, XConfig(..))

import XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings, checkKeymap, mkKeymap, removeKeysP, removeMouseBindings)

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
-- The Prime "Monad"
--

-- | A Prime is a function that transforms an XConfig. It's not a monad, but we
-- turn on RebindableSyntax so we can abuse the pretty do notation.
type Prime l l' = XConfig l -> IO (XConfig l')

-- | Composes two Primes using 'Prelude.>>=' from "Prelude".
(>>) :: Prime l l' -> Prime l' l'' -> Prime l l''
(>>) x y c = (P.>>=) (x c) y

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
  (=.) :: s l -> (x -> y) -> Prime l l

class SettableClass s x y | s -> x y where
  -- | This lets you modify an attribute.
  (=:) :: s l -> y -> Prime l l

-- Undecideable instance. But it's nice to leave open the possibility to write
-- fields you can't read (e.g. `wmName =: ...`).
instance UpdateableClass s x y => SettableClass s x y where
  s =: y = s =. const y

data Settable x l = Settable (XConfig l -> x)              -- getter
                             (x -> XConfig l -> XConfig l) -- setter

instance UpdateableClass (Settable x) x x where
  (Settable g s =. f) c = return $ s (f $ g c) c

-- | Non-focused windows border color. Default: @\"#dddddd\"@
normalBorderColor  :: Settable String l
normalBorderColor = Settable X.normalBorderColor (\x c -> c { X.normalBorderColor = x })

-- | Focused windows border color. Default: @\"#ff0000\"@
focusedBorderColor :: Settable String l
focusedBorderColor = Settable X.focusedBorderColor (\x c -> c { X.focusedBorderColor = x })

-- | The preferred terminal application. Default: @\"xterm\"@
terminal :: Settable String l
terminal = Settable X.terminal (\x c -> c { X.terminal = x })

-- | The mod modifier, as used by key bindings. Default: @mod1Mask@ (which is
-- probably alt on your computer).
modMask :: Settable KeyMask l
modMask = Settable X.modMask (\x c -> c { X.modMask = x })

-- | The border width (in pixels). Default: @1@
borderWidth :: Settable Dimension l
borderWidth = Settable X.borderWidth (\x c -> c { X.borderWidth = x })

-- | Whether window focus follows the mouse cursor on move, or requires a mouse
-- click. (Mouse? What's that?) Default: @True@
focusFollowsMouse :: Settable Bool l
focusFollowsMouse = Settable X.focusFollowsMouse (\x c -> c { X.focusFollowsMouse = x })

-- | If True, a mouse click on an inactive window focuses it, but the click is
-- not passed to the window. If False, the click is also passed to the window.
-- Default @True@
clickJustFocuses :: Settable Bool l
clickJustFocuses = Settable X.clickJustFocuses (\x c -> c { X.clickJustFocuses = x })

-- $summables
-- In addition to being able to set these attributes, they have a special
-- syntax for being able to add to them. The operator is @=+@ (the plus comes
-- /after/ the equals), but each attribute has a different syntax for what
-- comes after the operator.

class SummableClass s y | s -> y where
  -- | This lets you add to an attribute.
  (=+) :: s l -> y -> Prime l l
  infix 0 =+

data Summable x y l = Summable (XConfig l -> x)              -- getter
                               (x -> XConfig l -> XConfig l) -- setter
                               (x -> y -> x)                 -- accumulator

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
manageHook :: Summable ManageHook ManageHook l
manageHook = Summable X.manageHook (\x c -> c { X.manageHook = x }) (<+>)

-- | Custom X event handler. Return @All True@ if the default handler should
-- also be run afterwards. Default does nothing. To add an event handler:
--
-- > import XMonad.Hooks.ServerMode
-- > ...
-- >   handleEventHook =+ serverModeEventHook
handleEventHook :: Summable (Event -> X All) (Event -> X All) l
handleEventHook = Summable X.handleEventHook (\x c -> c { X.handleEventHook = x }) (<+>)

-- | List of workspaces' names. Default: @map show [1 .. 9 :: Int]@. Adding
-- appends to the end:
--
-- >   workspaces =+ ["0"]
--
-- This is useless unless you also create keybindings for this.
workspaces :: Summable [String] [String] l
workspaces = Summable X.workspaces (\x c -> c { X.workspaces = x }) (++)

-- TODO: Rework the workspaces thing to pair names with keybindings.

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
logHook :: Summable (X ()) (X ()) l
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
startupHook :: Summable (X ()) (X ()) l
startupHook = Summable X.startupHook (\x c -> c { X.startupHook = x }) (P.>>)

-- | The client events that xmonad is interested in. This is useful in
-- combination with handleEventHook. Default: @structureNotifyMask .|.
-- enterWindowMask .|. propertyChangeMask@
--
-- >   clientMask =+ keyPressMask .|. keyReleaseMask
clientMask :: Summable EventMask EventMask l
clientMask = Summable X.clientMask (\x c -> c { X.clientMask = x }) (.|.)

-- | The root events that xmonad is interested in. This is useful in
-- combination with handleEventHook. Default: @substructureRedirectMask .|.
-- substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|.
-- structureNotifyMask .|. buttonPressMask@
rootMask :: Summable EventMask EventMask l
rootMask = Summable X.rootMask (\x c -> c { X.rootMask = x }) (.|.)

-- $removables
-- The following support the the @=+@ for adding items and the @=-@ operator
-- for removing items.

class RemovableClass r y | r -> y where
  -- | This lets you remove from an attribute.
  (=-) :: r l -> y -> Prime l l
  infix 0 =-

data Keys (l :: * -> *) = Keys

-- Note that since checkKeymap happens on newKeys, it doesn't check for
-- duplicates between repeated applications. Probably OK. (Especially since
-- overriding defaults is a common behavior.) Also note that there's no
-- reference cycle here. Yay!

instance UpdateableClass Keys (XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())) [(String, X ())] where
  (_ =. f) c = return c { X.keys = \c' -> mkKeymap c' newKeys,
                          X.startupHook = (P.>>) (X.startupHook c) (checkKeymap c newKeys) }
    where newKeys = f $ X.keys c

instance SummableClass Keys [(String, X ())] where
  (_ =+ newKeys) c = return (c `additionalKeysP` newKeys) { X.startupHook = (P.>>) (X.startupHook c) (checkKeymap c newKeys) }

instance RemovableClass Keys [String] where
  (_ =- sadKeys) c = return (c `removeKeysP` sadKeys)

-- | Key bindings to 'X' actions. Default: see @`man xmonad`@. 'keys'
-- takes a list of keybindings specified emacs-style, as documented in
-- 'XMonad.Util.EZConfig.mkKeyMap'. For example, to change the "kill window"
-- key:
--
-- >   keys =- ["M-S-c"]
-- >   keys =+ [("M-M1-x", kill)]
keys :: Keys l
keys = Keys

data MouseBindings (l :: * -> *) = MouseBindings

instance SummableClass MouseBindings [((ButtonMask, Button), Window -> X ())] where
  (_ =+ newBindings) c = return (c `additionalMouseBindings` newBindings)

instance RemovableClass MouseBindings [(ButtonMask, Button)] where
  (_ =- sadBindings) c = return (c `removeMouseBindings` sadBindings)

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
mouseBindings :: MouseBindings l
mouseBindings = MouseBindings

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
