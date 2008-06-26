--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.EZConfig
-- Copyright   :  Devin Mullins <me@twifkak.com>
--                Brent Yorgey <byorgey@gmail.com> (key parsing)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
--
-- Useful helper functions for amending the defaultConfig, and for
-- parsing keybindings specified in a special (emacs-like) format.
--
-- (See also "XMonad.Util.CustomKeys" in xmonad-contrib.)
--
--------------------------------------------------------------------

module XMonad.Util.EZConfig (
                             -- * Usage
                             -- $usage

                             -- * Adding or removing keybindings

                             additionalKeys, additionalKeysP,
                             removeKeys, removeKeysP,
                             additionalMouseBindings, removeMouseBindings,

                             -- * Emacs-style keybinding specifications

                             mkKeymap, checkKeymap,
                            ) where

import XMonad
import XMonad.Actions.Submap

import qualified Data.Map as M
import Data.List (foldl', intersperse, sortBy, groupBy, nub)
import Data.Ord (comparing)
import Data.Maybe
import Control.Arrow (first, (&&&))

import Text.ParserCombinators.ReadP

-- $usage
-- To use this module, first import it into your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Util.EZConfig
--
-- Then, use one of the provided functions to modify your
-- configuration.  You can use 'additionalKeys', 'removeKeys',
-- 'additionalMouseBindings', and 'removeMouseBindings' to easily add
-- and remove keybindings or mouse bindings.  You can use 'mkKeymap'
-- to create a keymap using emacs-style keybinding specifications
-- like @\"M-x\"@ instead of @(modMask, xK_x)@, or 'additionalKeysP'
-- and 'removeKeysP' to easily add or remove emacs-style keybindings.
-- If you use emacs-style keybindings, the 'checkKeymap' function is
-- provided, suitable for adding to your 'startupHook', which can warn
-- you of any parse errors or duplicate bindings in your keymap.
--
-- For more information and usage eamples, see the documentation
-- provided with each exported function, and check the xmonad config
-- archive (<http://haskell.org/haskellwiki/Xmonad/Config_archive>)
-- for some real examples of use.

-- |
-- Add or override keybindings from the existing set. Example use:
--
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `additionalKeys`
-- >                 [ ((mod1Mask, xK_m        ), spawn "echo 'Hi, mom!' | dzen2 -p 4")
-- >                 , ((mod1Mask, xK_BackSpace), withFocused hide) -- N.B. this is an absurd thing to do
-- >                 ]
--
-- This overrides the previous definition of mod-m.
--
-- Note that, unlike in xmonad 0.4 and previous, you can't use modMask to refer
-- to the modMask you configured earlier. You must specify mod1Mask (or
-- whichever), or add your own @myModMask = mod1Mask@ line.
additionalKeys :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a
additionalKeys conf keyList =
    conf { keys = \cnf -> M.union (M.fromList keyList) (keys conf cnf) }

-- | Like 'additionalKeys', except using short @String@ key
--   descriptors like @\"M-m\"@ instead of @(modMask, xK_m)@, as
--   described in the documentation for 'mkKeymap'.  For example:
--
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `additionalKeysP`
-- >                 [ ("M-m", spawn "echo 'Hi, mom!' | dzen2 -p 4")
-- >                 , ("M-<Backspace>", withFocused hide) -- N.B. this is an absurd thing to do
-- >                 ]

additionalKeysP :: XConfig l -> [(String, X ())] -> XConfig l
additionalKeysP conf keyList =
    conf { keys = \cnf -> M.union (mkKeymap cnf keyList) (keys conf cnf) }

-- |
-- Remove standard keybindings you're not using. Example use:
--
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `removeKeys` [(mod1Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
removeKeys :: XConfig a -> [(ButtonMask, KeySym)] -> XConfig a
removeKeys conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` M.fromList (zip keyList $ return ()) }

-- | Like 'removeKeys', except using short @String@ key descriptors
--   like @\"M-m\"@ instead of @(modMask, xK_m)@, as described in the
--   documentation for 'mkKeymap'. For example:
--
-- > main = xmonad $ defaultConfig { terminal = "urxvt" }
-- >                 `removeKeysP` ["M-S-" ++ [n] | n <- ['1'..'9']]

removeKeysP :: XConfig l -> [String] -> XConfig l
removeKeysP conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` mkKeymap cnf (zip keyList $ repeat (return ())) }

-- | Like 'additionalKeys', but for mouse bindings.
additionalMouseBindings :: XConfig a -> [((ButtonMask, Button), Window -> X ())] -> XConfig a
additionalMouseBindings conf mouseBindingsList =
    conf { mouseBindings = \cnf -> M.union (M.fromList mouseBindingsList) (mouseBindings conf cnf) }

-- | Like 'removeKeys', but for mouse bindings.
removeMouseBindings :: XConfig a -> [(ButtonMask, Button)] -> XConfig a
removeMouseBindings conf mouseBindingList =
    conf { mouseBindings = \cnf -> mouseBindings conf cnf `M.difference`
                                   M.fromList (zip mouseBindingList $ return ()) }


--------------------------------------------------------------
--  Keybinding parsing  ---------------------------------------
--------------------------------------------------------------

-- | Given a config (used to determine the proper modifier key to use)
--   and a list of @(String, X ())@ pairs, create a key map by parsing
--   the key sequence descriptions contained in the Strings.  The key
--   sequence descriptions are \"emacs-style\": @M-@, @C-@, @S-@, and
--   @M\#-@ denote mod, control, shift, and mod1-mod5 (where @\#@ is
--   replaced by the appropriate number) respectively; some special
--   keys can be specified by enclosing their name in angle brackets.
--
--   For example, @\"M-C-x\"@ denotes mod+ctrl+x; @\"S-\<Escape\>\"@ denotes
--   shift-escape.
--
--   Sequences of keys can also be specified by separating the key
--   descriptions with spaces. For example, @\"M-x y \<Down\>\"@ denotes the
--   sequence of keys mod+x, y, down.  Submaps (see
--   "XMonad.Actions.Submap") will be automatically generated to
--   correctly handle these cases.
--
--   So, for example, a complete key map might be specified as
--
-- > keys = \c -> mkKeymap c $
-- >     [ ("M-S-<Return>", spawn $ terminal c)
-- >     , ("M-x w", spawn "xmessage 'woohoo!'")  -- type mod+x then w to pop up 'woohoo!'
-- >     , ("M-x y", spawn "xmessage 'yay!'")     -- type mod+x then y to pop up 'yay!'
-- >     , ("M-S-c", kill)
-- >     ]
--
-- Alternatively, you can use 'additionalKeysP' to automatically
-- create a keymap and add it to your config.
--
-- Here is a complete list of supported special keys.  Note that a few
-- keys, such as the arrow keys, have synonyms:
--
-- > <Backspace>
-- > <Tab>
-- > <Return>
-- > <Pause>
-- > <Scroll_lock>
-- > <Sys_Req>
-- > <Escape>, <Esc>
-- > <Delete>
-- > <Home>
-- > <Left>, <L>
-- > <Up>, <U>
-- > <Right>, <R>
-- > <Down>, <D>
-- > <Page_Up>
-- > <Page_Down>
-- > <End>
-- > <Insert>
-- > <Break>
-- > <Space>
-- > <F1>-<F24>
-- > <KP_Space>
-- > <KP_Tab>
-- > <KP_Enter>
-- > <KP_F1>
-- > <KP_F2>
-- > <KP_F3>
-- > <KP_F4>
-- > <KP_Home>
-- > <KP_Left>
-- > <KP_Up>
-- > <KP_Right>
-- > <KP_Down>
-- > <KP_Prior>
-- > <KP_Page_Up>
-- > <KP_Next>
-- > <KP_Page_Down>
-- > <KP_End>
-- > <KP_Begin>
-- > <KP_Insert>
-- > <KP_Delete>
-- > <KP_Equal>
-- > <KP_Multiply>
-- > <KP_Add>
-- > <KP_Separator>
-- > <KP_Subtract>
-- > <KP_Decimal>
-- > <KP_Divide>
-- > <KP_0>
-- > <KP_1>
-- > <KP_2>
-- > <KP_3>
-- > <KP_4>
-- > <KP_5>
-- > <KP_6>
-- > <KP_7>
-- > <KP_8>
-- > <KP_9>

mkKeymap :: XConfig l -> [(String, X ())] -> M.Map (KeyMask, KeySym) (X ())
mkKeymap c = M.fromList . mkSubmaps . readKeymap c

-- | Given a list of pairs of parsed key sequences and actions,
--   group them into submaps in the appropriate way.
mkSubmaps :: [ ([(KeyMask,KeySym)], X ()) ] -> [((KeyMask, KeySym), X ())]
mkSubmaps binds = map combine gathered
  where gathered = groupBy fstKey
                 . sortBy (comparing fst)
                 $ binds
        combine [([k],act)] = (k,act)
        combine ks = (head . fst . head $ ks,
                      submap . M.fromList . mkSubmaps $ map (first tail) ks)
        fstKey = (==) `on` (head . fst)

on :: (a -> a -> b) -> (c -> a) -> c -> c -> b
op `on` f = \x y -> f x `op` f y

-- | Given a configuration record and a list of (key sequence
--   description, action) pairs, parse the key sequences into lists of
--   @(KeyMask,KeySym)@ pairs.  Key sequences which fail to parse will
--   be ignored.
readKeymap :: XConfig l -> [(String, X())] -> [([(KeyMask,KeySym)], X())]
readKeymap c = mapMaybe (maybeKeys . first (readKeySequence c))
  where maybeKeys (Nothing,_) = Nothing
        maybeKeys (Just k, act) = Just (k, act)

-- | Parse a sequence of keys, returning Nothing if there is
--   a parse failure (no parse, or ambiguous parse).
readKeySequence :: XConfig l -> String -> Maybe [(KeyMask, KeySym)]
readKeySequence c = listToMaybe . parses
  where parses = map fst . filter (null.snd) . readP_to_S (parseKeySequence c)

-- | Parse a sequence of key combinations separated by spaces, e.g.
--   @\"M-c x C-S-2\"@ (mod+c, x, ctrl+shift+2).
parseKeySequence :: XConfig l -> ReadP [(KeyMask, KeySym)]
parseKeySequence c = sepBy1 (parseKeyCombo c) (many1 $ char ' ')

-- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
parseKeyCombo :: XConfig l -> ReadP (KeyMask, KeySym)
parseKeyCombo c = do mods <- many (parseModifier c)
                     k <- parseKey
                     return (foldl' (.|.) 0 mods, k)

-- | Parse a modifier: either M- (user-defined mod-key),
--   C- (control), S- (shift), or M#- where # is an integer
--   from 1 to 5 (mod1Mask through mod5Mask).
parseModifier :: XConfig l -> ReadP KeyMask
parseModifier c =  (string "M-" >> return (modMask c))
               +++ (string "C-" >> return controlMask)
               +++ (string "S-" >> return shiftMask)
               +++ do char 'M'
                      n <- satisfy (`elem` ['1'..'5'])
                      char '-'
                      return (mod1Mask + (read [n]) - 1)

-- | Parse an unmodified basic key, like @\"x\"@, @\"<F1>\"@, etc.
parseKey :: ReadP KeySym
parseKey = parseRegular +++ parseSpecial

-- | Parse a regular key name (represented by itself).
parseRegular :: ReadP KeySym
parseRegular = choice [ char s >> return k
                      | (s,k) <- zip ['!'..'~'] [xK_exclam..xK_asciitilde]
                      ]

-- | Parse a special key name (one enclosed in angle brackets).
parseSpecial :: ReadP KeySym
parseSpecial = do char '<'
                  key <- choice [ string name >> return k
                                | (name,k) <- keyNames
                                ]
                  char '>'
                  return key

-- | A list of all special key names and their associated KeySyms.
keyNames :: [(String, KeySym)]
keyNames = functionKeys ++ specialKeys

-- | A list pairing function key descriptor strings (e.g. @\"<F2>\"@) with
--   the associated KeySyms.
functionKeys :: [(String, KeySym)]
functionKeys = [ ("F" ++ show n, k)
               | (n,k) <- zip ([1..24] :: [Int]) [xK_F1..] ]

-- | A list of special key names and their corresponding KeySyms.
specialKeys :: [(String, KeySym)]
specialKeys = [ ("Backspace"  , xK_BackSpace)
              , ("Tab"        , xK_Tab)
              , ("Return"     , xK_Return)
              , ("Pause"      , xK_Pause)
              , ("Scroll_lock", xK_Scroll_Lock)
              , ("Sys_Req"    , xK_Sys_Req)
              , ("Escape"     , xK_Escape)
              , ("Esc"        , xK_Escape)
              , ("Delete"     , xK_Delete)
              , ("Home"       , xK_Home)
              , ("Left"       , xK_Left)
              , ("Up"         , xK_Up)
              , ("Right"      , xK_Right)
              , ("Down"       , xK_Down)
              , ("L"          , xK_Left)
              , ("U"          , xK_Up)
              , ("R"          , xK_Right)
              , ("D"          , xK_Down)
              , ("Page_Up"    , xK_Page_Up)
              , ("Page_Down"  , xK_Page_Down)
              , ("End"        , xK_End)
              , ("Insert"     , xK_Insert)
              , ("Break"      , xK_Break)
              , ("Space"      , xK_space)
              , ("KP_Space"   , xK_KP_Space)
              , ("KP_Tab"     , xK_KP_Tab)
              , ("KP_Enter"   , xK_KP_Enter)
              , ("KP_F1"      , xK_KP_F1)
              , ("KP_F2"      , xK_KP_F2)
              , ("KP_F3"      , xK_KP_F3)
              , ("KP_F4"      , xK_KP_F4)
              , ("KP_Home"    , xK_KP_Home)
              , ("KP_Left"    , xK_KP_Left)
              , ("KP_Up"      , xK_KP_Up)
              , ("KP_Right"   , xK_KP_Right)
              , ("KP_Down"    , xK_KP_Down)
              , ("KP_Prior"   , xK_KP_Prior)
              , ("KP_Page_Up" , xK_KP_Page_Up)
              , ("KP_Next"    , xK_KP_Next)
              , ("KP_Page_Down", xK_KP_Page_Down)
              , ("KP_End"     , xK_KP_End)
              , ("KP_Begin"   , xK_KP_Begin)
              , ("KP_Insert"  , xK_KP_Insert)
              , ("KP_Delete"  , xK_KP_Delete)
              , ("KP_Equal"   , xK_KP_Equal)
              , ("KP_Multiply", xK_KP_Multiply)
              , ("KP_Add"     , xK_KP_Add)
              , ("KP_Separator", xK_KP_Separator)
              , ("KP_Subtract", xK_KP_Subtract)
              , ("KP_Decimal" , xK_KP_Decimal)
              , ("KP_Divide"  , xK_KP_Divide)
              , ("KP_0"       , xK_KP_0)
              , ("KP_1"       , xK_KP_1)
              , ("KP_2"       , xK_KP_2)
              , ("KP_3"       , xK_KP_3)
              , ("KP_4"       , xK_KP_4)
              , ("KP_5"       , xK_KP_5)
              , ("KP_6"       , xK_KP_6)
              , ("KP_7"       , xK_KP_7)
              , ("KP_8"       , xK_KP_8)
              , ("KP_9"       , xK_KP_9)
              ]

-- | Given a configuration record and a list of (key sequence
--   description, action) pairs, check the key sequence descriptions
--   for validity, and warn the user (via a popup xmessage window) of
--   any unparseable or duplicate key sequences.  This function is
--   appropriate for adding to your @startupHook@, and you are highly
--   encouraged to do so; otherwise, duplicate or unparseable
--   keybindings will be silently ignored.
--
--   For example, you might do something like this:
--
-- > main = xmonad $ myConfig
-- >
-- > myKeymap = [("S-M-c", kill), ...]
-- > myConfig = defaultConfig {
-- >     ...
-- >     keys = \c -> mkKeymap c myKeymap
-- >     startupHook = return () >> checkKeymap myConfig myKeymap
-- >     ...
-- > }
--
-- NOTE: the @return ()@ in the example above is very important!
-- Otherwise, you might run into problems with infinite mutual
-- recursion: the definition of myConfig depends on the definition of
-- startupHook, which depends on the definition of myConfig, ... and
-- so on.  Actually, it's likely that the above example in particular
-- would be OK without the @return ()@, but making @myKeymap@ take
-- @myConfig@ as a parameter would definitely lead to
-- problems. Believe me.  It, uh, happened to my friend. In... a
-- dream. Yeah. In any event, the @return () >>@ introduces enough
-- laziness to break the deadlock.
--
checkKeymap :: XConfig l -> [(String, a)] -> X ()
checkKeymap conf km = warn (doKeymapCheck conf km)
  where warn ([],[])   = return ()
        warn (bad,dup) = spawn $ "xmessage 'Warning:\n"
                            ++ msg "bad" bad ++ "\n"
                            ++ msg "duplicate" dup ++ "'"
        msg _ [] = ""
        msg m xs = m ++ " keybindings detected: " ++ showBindings xs
        showBindings = concat . intersperse " " . map ((++"\"") . ("\""++))

-- | Given a config and a list of (key sequence description, action)
--   pairs, check the key sequence descriptions for validity,
--   returning a list of unparseable key sequences, and a list of
--   duplicate key sequences.
doKeymapCheck :: XConfig l -> [(String,a)] -> ([String], [String])
doKeymapCheck conf km = (bad,dups)
  where ks = map ((readKeySequence conf &&& id) . fst) km
        bad = nub . map snd . filter (isNothing . fst) $ ks
        dups = map (snd . head)
             . filter ((>1) . length)
             . groupBy ((==) `on` fst)
             . sortBy (comparing fst)
             . map (first fromJust)
             . filter (isJust . fst)
             $ ks
