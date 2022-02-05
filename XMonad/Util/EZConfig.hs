{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.EZConfig
-- Description :  Configure key bindings easily in Emacs style.
-- Copyright   :  Devin Mullins <me@twifkak.com>
--                Brent Yorgey <byorgey@gmail.com> (key parsing)
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Devin Mullins <me@twifkak.com>
--
-- Useful helper functions for amending the default configuration, and for
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
                             mkNamedKeymap,

                             -- * Parsers

                             parseKey, -- used by XMonad.Util.Paste
                             parseKeyCombo,
                             parseKeySequence, readKeySequence,
#ifdef TESTING
                             parseModifier,
#endif
                            ) where

import XMonad
import XMonad.Actions.Submap
import XMonad.Prelude

import XMonad.Util.NamedActions
import XMonad.Util.Parser

import Control.Arrow (first, (&&&))
import qualified Data.Map as M
import Data.Ord (comparing)

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
-- For more information and usage examples, see the documentation
-- provided with each exported function, and check the xmonad config
-- archive (<http://haskell.org/haskellwiki/Xmonad/Config_archive>)
-- for some real examples of use.

-- |
-- Add or override keybindings from the existing set. Example use:
--
-- > main = xmonad $ def { terminal = "urxvt" }
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
additionalKeys :: XConfig a -> [((KeyMask, KeySym), X ())] -> XConfig a
additionalKeys conf keyList =
    conf { keys = M.union (M.fromList keyList) . keys conf }

-- | Like 'additionalKeys', except using short @String@ key
--   descriptors like @\"M-m\"@ instead of @(modMask, xK_m)@, as
--   described in the documentation for 'mkKeymap'.  For example:
--
-- > main = xmonad $ def { terminal = "urxvt" }
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
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `removeKeys` [(mod1Mask .|. shiftMask, n) | n <- [xK_1 .. xK_9]]
removeKeys :: XConfig a -> [(KeyMask, KeySym)] -> XConfig a
removeKeys conf keyList =
    conf { keys = \cnf -> foldr M.delete (keys conf cnf) keyList }

-- | Like 'removeKeys', except using short @String@ key descriptors
--   like @\"M-m\"@ instead of @(modMask, xK_m)@, as described in the
--   documentation for 'mkKeymap'. For example:
--
-- > main = xmonad $ def { terminal = "urxvt" }
-- >                 `removeKeysP` ["M-S-" ++ [n] | n <- ['1'..'9']]

removeKeysP :: XConfig l -> [String] -> XConfig l
removeKeysP conf keyList =
    conf { keys = \cnf -> keys conf cnf `M.difference` mkKeymap cnf (zip keyList $ repeat (return ())) }

-- | Like 'additionalKeys', but for mouse bindings.
additionalMouseBindings :: XConfig a -> [((ButtonMask, Button), Window -> X ())] -> XConfig a
additionalMouseBindings conf mouseBindingsList =
    conf { mouseBindings = M.union (M.fromList mouseBindingsList) . mouseBindings conf }

-- | Like 'removeKeys', but for mouse bindings.
removeMouseBindings :: XConfig a -> [(ButtonMask, Button)] -> XConfig a
removeMouseBindings conf mouseBindingList =
    conf { mouseBindings = \cnf -> foldr M.delete (mouseBindings conf cnf) mouseBindingList }


--------------------------------------------------------------
--  Keybinding parsing  ---------------------------------------
--------------------------------------------------------------

-- | Given a config (used to determine the proper modifier key to use)
--   and a list of @(String, X ())@ pairs, create a key map by parsing
--   the key sequence descriptions contained in the Strings.  The key
--   sequence descriptions are \"emacs-style\": @M-@, @C-@, @S-@, and
--   @M\#-@ denote mod, control, shift, and mod1-mod5 (where @\#@ is
--   replaced by the appropriate number) respectively.  Note that if
--   you want to make a keybinding using \'alt\' even though you use a
--   different key (like the \'windows\' key) for \'mod\', you can use
--   something like @\"M1-x\"@ for alt+x (check the output of @xmodmap@
--   to see which mod key \'alt\' is bound to). Some special keys can
--   also be specified by enclosing their name in angle brackets.
--
--   For example, @\"M-C-x\"@ denotes mod+ctrl+x; @\"S-\<Escape\>\"@
--   denotes shift-escape; @\"M1-C-\<Delete\>\"@ denotes alt+ctrl+delete
--   (assuming alt is bound to mod1, which is common).
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
-- keys, such as the arrow keys, have synonyms.  If there are other
-- special keys you would like to see supported, feel free to submit a
-- patch, or ask on the xmonad mailing list; adding special keys is
-- quite simple.
--
-- > <Backspace>
-- > <Tab>
-- > <Return>
-- > <Pause>
-- > <Num_Lock>
-- > <Caps_Lock>
-- > <Scroll_lock>
-- > <Sys_Req>
-- > <Print>
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
-- > <Control_L>
-- > <Control_R>
-- > <Shift_L>
-- > <Shift_R>
-- > <Alt_L>
-- > <Alt_R>
-- > <Meta_L>
-- > <Meta_R>
-- > <Super_L>
-- > <Super_R>
-- > <Hyper_L>
-- > <Hyper_R>
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
-- > <KP_0>-<KP_9>
--
-- Long list of multimedia keys. Please note that not all keys may be
-- present in your particular setup although most likely they will do.
--
-- > <XF86ModeLock>
-- > <XF86MonBrightnessUp>
-- > <XF86MonBrightnessDown>
-- > <XF86KbdLightOnOff>
-- > <XF86KbdBrightnessUp>
-- > <XF86KbdBrightnessDown>
-- > <XF86Standby>
-- > <XF86AudioLowerVolume>
-- > <XF86AudioMute>
-- > <XF86AudioRaiseVolume>
-- > <XF86AudioPlay>
-- > <XF86AudioStop>
-- > <XF86AudioPrev>
-- > <XF86AudioNext>
-- > <XF86HomePage>
-- > <XF86Mail>
-- > <XF86Start>
-- > <XF86Search>
-- > <XF86AudioRecord>
-- > <XF86Calculator>
-- > <XF86Memo>
-- > <XF86ToDoList>
-- > <XF86Calendar>
-- > <XF86PowerDown>
-- > <XF86ContrastAdjust>
-- > <XF86RockerUp>
-- > <XF86RockerDown>
-- > <XF86RockerEnter>
-- > <XF86Back>
-- > <XF86Forward>
-- > <XF86Stop>
-- > <XF86Refresh>
-- > <XF86PowerOff>
-- > <XF86WakeUp>
-- > <XF86Eject>
-- > <XF86ScreenSaver>
-- > <XF86WWW>
-- > <XF86Sleep>
-- > <XF86Favorites>
-- > <XF86AudioPause>
-- > <XF86AudioMedia>
-- > <XF86MyComputer>
-- > <XF86VendorHome>
-- > <XF86LightBulb>
-- > <XF86Shop>
-- > <XF86History>
-- > <XF86OpenURL>
-- > <XF86AddFavorite>
-- > <XF86HotLinks>
-- > <XF86BrightnessAdjust>
-- > <XF86Finance>
-- > <XF86Community>
-- > <XF86AudioRewind>
-- > <XF86XF86BackForward>
-- > <XF86Launch0>-<XF86Launch9>, <XF86LaunchA>-<XF86LaunchF>
-- > <XF86ApplicationLeft>
-- > <XF86ApplicationRight>
-- > <XF86Book>
-- > <XF86CD>
-- > <XF86Calculater>
-- > <XF86Clear>
-- > <XF86Close>
-- > <XF86Copy>
-- > <XF86Cut>
-- > <XF86Display>
-- > <XF86DOS>
-- > <XF86Documents>
-- > <XF86Excel>
-- > <XF86Explorer>
-- > <XF86Game>
-- > <XF86Go>
-- > <XF86iTouch>
-- > <XF86LogOff>
-- > <XF86Market>
-- > <XF86Meeting>
-- > <XF86MenuKB>
-- > <XF86MenuPB>
-- > <XF86MySites>
-- > <XF86New>
-- > <XF86News>
-- > <XF86OfficeHome>
-- > <XF86Open>
-- > <XF86Option>
-- > <XF86Paste>
-- > <XF86Phone>
-- > <XF86Q>
-- > <XF86Reply>
-- > <XF86Reload>
-- > <XF86RotateWindows>
-- > <XF86RotationPB>
-- > <XF86RotationKB>
-- > <XF86Save>
-- > <XF86ScrollUp>
-- > <XF86ScrollDown>
-- > <XF86ScrollClick>
-- > <XF86Send>
-- > <XF86Spell>
-- > <XF86SplitScreen>
-- > <XF86Support>
-- > <XF86TaskPane>
-- > <XF86Terminal>
-- > <XF86Tools>
-- > <XF86Travel>
-- > <XF86UserPB>
-- > <XF86User1KB>
-- > <XF86User2KB>
-- > <XF86Video>
-- > <XF86WheelButton>
-- > <XF86Word>
-- > <XF86Xfer>
-- > <XF86ZoomIn>
-- > <XF86ZoomOut>
-- > <XF86Away>
-- > <XF86Messenger>
-- > <XF86WebCam>
-- > <XF86MailForward>
-- > <XF86Pictures>
-- > <XF86Music>
-- > <XF86TouchpadToggle>
-- > <XF86AudioMicMute>
-- > <XF86_Switch_VT_1>-<XF86_Switch_VT_12>
-- > <XF86_Ungrab>
-- > <XF86_ClearGrab>
-- > <XF86_Next_VMode>
-- > <XF86_Prev_VMode>
-- > <XF86Bluetooth>

mkKeymap :: XConfig l -> [(String, X ())] -> M.Map (KeyMask, KeySym) (X ())
mkKeymap c = M.fromList . mkSubmaps . readKeymap c

mkNamedKeymap :: XConfig l -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
mkNamedKeymap c = mkNamedSubmaps . readKeymap c

-- | Given a list of pairs of parsed key sequences and actions,
--   group them into submaps in the appropriate way.

mkNamedSubmaps :: [([(KeyMask, KeySym)], NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
mkNamedSubmaps = mkSubmaps' submapName

mkSubmaps :: [ ([(KeyMask,KeySym)], X ()) ] -> [((KeyMask, KeySym), X ())]
mkSubmaps = mkSubmaps' $ submap . M.fromList

mkSubmaps' ::  (Ord a) => ([(a, c)] -> c) -> [([a], c)] -> [(a, c)]
mkSubmaps' subm binds = map combine gathered
  where gathered = groupBy fstKey
                 . sortBy (comparing fst)
                 $ binds
        combine [([k],act)] = (k,act)
        combine ks = (head . fst . head $ ks,
                      subm . mkSubmaps' subm $ map (first tail) ks)
        fstKey = (==) `on` (head . fst)

-- | Given a configuration record and a list of (key sequence
--   description, action) pairs, parse the key sequences into lists of
--   @(KeyMask,KeySym)@ pairs.  Key sequences which fail to parse will
--   be ignored.
readKeymap :: XConfig l -> [(String, t)] -> [([(KeyMask, KeySym)], t)]
readKeymap c = mapMaybe (maybeKeys . first (readKeySequence c))
  where maybeKeys (Nothing,_) = Nothing
        maybeKeys (Just k, act) = Just (k, act)

-- | Parse a sequence of keys, returning Nothing if there is
--   a parse failure (no parse, or ambiguous parse).
readKeySequence :: XConfig l -> String -> Maybe [(KeyMask, KeySym)]
readKeySequence c = runParser (parseKeySequence c <* eof)

-- | Parse a sequence of key combinations separated by spaces, e.g.
--   @\"M-c x C-S-2\"@ (mod+c, x, ctrl+shift+2).
parseKeySequence :: XConfig l -> Parser [(KeyMask, KeySym)]
parseKeySequence c = parseKeyCombo c `sepBy1` many1 (char ' ')

-- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
parseKeyCombo :: XConfig l -> Parser (KeyMask, KeySym)
parseKeyCombo c = do mods <- many (parseModifier c)
                     k <- parseKey
                     return (foldl' (.|.) 0 mods, k)

-- | Parse a modifier: either M- (user-defined mod-key),
--   C- (control), S- (shift), or M#- where # is an integer
--   from 1 to 5 (mod1Mask through mod5Mask).
parseModifier :: XConfig l -> Parser KeyMask
parseModifier c = (string "M-" $> modMask c)
               <> (string "C-" $> controlMask)
               <> (string "S-" $> shiftMask)
               <> do _ <- char 'M'
                     n <- satisfy (`elem` ['1'..'5'])
                     _ <- char '-'
                     return $ indexMod (read [n] - 1)
    where indexMod = (!!) [mod1Mask,mod2Mask,mod3Mask,mod4Mask,mod5Mask]

-- | Parse an unmodified basic key, like @\"x\"@, @\"<F1>\"@, etc.
parseKey :: Parser KeySym
parseKey = parseSpecial <> parseRegular

-- | Parse a regular key name (represented by itself).
parseRegular :: Parser KeySym
parseRegular = choice [ string s $> k | (s, k) <- regularKeys ]

-- | Parse a special key name (one enclosed in angle brackets).
parseSpecial :: Parser KeySym
parseSpecial = do _ <- char '<'
                  choice [ k <$ string name <* char '>'
                         | (name, k) <- allSpecialKeys
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
-- > myConfig = def {
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
        warn (bad,dup) = xmessage $ "Warning:\n"
                            ++ msg "bad" bad ++ "\n"
                            ++ msg "duplicate" dup
        msg _ [] = ""
        msg m xs = m ++ " keybindings detected: " ++ showBindings xs
        showBindings = unwords . map (("\""++) . (++"\""))

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
