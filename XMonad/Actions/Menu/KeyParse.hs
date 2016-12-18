module XMonad.Actions.Menu.KeyParse where

{-|

Module      : XMonad.Menu.Actions.KeyParse
Description : A set of tools to run and render interactive menus

Maintainer  : David Janssen <janssen.dhj@gmail.com>
Stability   : unstable
Portability : unportable

This code is taken almost entirely from the key-string parsing capabilities of
EZConfig, written by Brent Yorgey. EZConfig does not export any of these
functions and I did not want to modify EZConfig itself, so I extracted the code,
modified it slightly and put it here. I don't think I can put myself in the
Copyright field of this header, I don't know what to do with this.

The code here differs from EZConfigs by:

1. No passing around of XConfig from XMonad. Only depends on X11 (but loses
   the ability to know what the user's own Modkey is)
2. Some renaming of functions to make them follow the same scheme

Note that the definition of keys is slightly weird. If you specify the shiftmask
in the keystroke, you must also capitalize the letter. This should be fixed in
the future. For clarity:
right: "a"
wrong: "S-a"
right: "S-A"
wrong: "S-a"

The two wrong definitions above will currently *never* register.
-}


import           Control.Arrow                ((&&&))
import           Data.Bits                    (complement, (.&.), (.|.))
import           Data.List                    (foldl')
import           Data.Maybe                   (listToMaybe)
import           Graphics.X11
import           Text.ParserCombinators.ReadP

type KeyStroke = (KeyMask, KeySym)

-- | Parse a string to an Emacs-style "M1-S-A" keystroke
readStroke :: String -> Maybe KeyStroke
readStroke = stringToKeystroke

-- | Show a keystroke in an Emacs-style "M1-a" string
showStroke :: KeyStroke -> String
showStroke = keystrokeToString

keystrokeToString :: KeyStroke -> String
keystrokeToString (mask, sym) = keymaskToString mask ++ keysymToString sym

stringToKeystroke :: String -> Maybe KeyStroke
stringToKeystroke = listToMaybe . map fst . filter (null . snd) . readP_to_S parseKeyCombo

keysequenceToString :: [KeyStroke] -> String
keysequenceToString = concatMap (\ks -> keystrokeToString ks ++ " ")


stringToKeySequence :: String -> Maybe [KeyStroke]
stringToKeySequence = listToMaybe . parses
  where parses = map fst . filter (null.snd) . readP_to_S parseKeySequence

keymaskToString :: KeyMask -> String
keymaskToString mask = concatMap (++"-") $ filter (not . null)
                $ map pick
                [(mod1Mask, "M1") ,(mod2Mask, "M2") ,(mod3Mask, "M3") ,(mod4Mask, "M4")
                ,(mod5Mask, "M5") ,(controlMask, "C") ,(shiftMask,"S")]
    where pick (m, str) = if m .&. complement mask == 0 then str else ""


-- | Parse a sequence of key combinations separated by spaces, e.g.
--   @\"M-c x C-S-2\"@ (mod+c, x, ctrl+shift+2).
parseKeySequence :: ReadP [(KeyMask, KeySym)]
parseKeySequence = sepBy1 parseKeyCombo (many1 $ char ' ')

-- | Parse a modifier-key combination such as "M-C-s" (mod+ctrl+s).
parseKeyCombo :: ReadP (KeyMask, KeySym)
parseKeyCombo = do mods <- many parseModifier
                   k    <- parseKey
                   return (foldl' (.|.) 0 mods, k)

-- | Parse a modifier: either M- (user-defined mod-key),
--   C- (control), S- (shift), or M#- where # is an integer
--   from 1 to 5 (mod1Mask through mod5Mask).
parseModifier :: ReadP KeyMask
parseModifier = (string "M-" >> return mod4Mask)
            +++ (string "C-" >> return controlMask)
            +++ (string "S-" >> return shiftMask)
            +++ do _ <- char 'M'
                   n <- satisfy (`elem` ['1'..'5'])
                   _ <- char '-'
                   return $ indexMod (read [n] - 1)
  where indexMod = (!!) [mod1Mask, mod2Mask, mod3Mask, mod4Mask, mod5Mask]

-- | Parse an unmodified basic key, like @\"x\"@, @\"<F1>\"@, etc.
parseKey :: ReadP KeySym
parseKey = parseRegular +++ parseSpecial

-- | Parse a regular key name (represented by itself).
parseRegular :: ReadP KeySym
parseRegular = choice [ char s >> return k
                      | (s,k) <- zip ['!'..'~'] [xK_exclam..xK_asciitilde]]

-- | Parse a special key name (one enclosed in angle brackets).
parseSpecial :: ReadP KeySym
parseSpecial = do _   <- char '<'
                  key <- choice [ string name >> return k
                                | (name,k) <- keyNames
                                ]
                  _   <- char '>'
                  return key

-- | A list of all special key names and their associated KeySyms.
keyNames :: [(String, KeySym)]
keyNames = functionKeys ++ specialKeys ++ multimediaKeys

-- | A list pairing function key descriptor strings (e.g. @\"<F2>\"@) with
--   the associated KeySyms.
functionKeys :: [(String, KeySym)]
functionKeys = [ ('F' : show n, k)
               | (n,k) <- zip ([1..24] :: [Int]) [xK_F1..] ]

-- | A list of special key names and their corresponding KeySyms.
specialKeys :: [(String, KeySym)]
specialKeys = [ ("Backspace"  , xK_BackSpace)
              , ("Tab"        , xK_Tab)
              , ("Return"     , xK_Return)
              , ("Pause"      , xK_Pause)
              , ("Scroll_lock", xK_Scroll_Lock)
              , ("Sys_Req"    , xK_Sys_Req)
              , ("Print"      , xK_Print)
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

-- | List of multimedia keys. If X server does not know about some
-- | keysym it's omitted from list. (stringToKeysym returns noSymbol in this case)
multimediaKeys :: [(String, KeySym)]
multimediaKeys = filter ((/= noSymbol) . snd) . map (id &&& stringToKeysym) $
                 [ "XF86ModeLock"
                 , "XF86MonBrightnessUp"
                 , "XF86MonBrightnessDown"
                 , "XF86KbdLightOnOff"
                 , "XF86KbdBrightnessUp"
                 , "XF86KbdBrightnessDown"
                 , "XF86Standby"
                 , "XF86AudioLowerVolume"
                 , "XF86AudioMute"
                 , "XF86AudioRaiseVolume"
                 , "XF86AudioPlay"
                 , "XF86AudioStop"
                 , "XF86AudioPrev"
                 , "XF86AudioNext"
                 , "XF86HomePage"
                 , "XF86Mail"
                 , "XF86Start"
                 , "XF86Search"
                 , "XF86AudioRecord"
                 , "XF86Calculator"
                 , "XF86Memo"
                 , "XF86ToDoList"
                 , "XF86Calendar"
                 , "XF86PowerDown"
                 , "XF86ContrastAdjust"
                 , "XF86RockerUp"
                 , "XF86RockerDown"
                 , "XF86RockerEnter"
                 , "XF86Back"
                 , "XF86Forward"
                 , "XF86Stop"
                 , "XF86Refresh"
                 , "XF86PowerOff"
                 , "XF86WakeUp"
                 , "XF86Eject"
                 , "XF86ScreenSaver"
                 , "XF86WWW"
                 , "XF86Sleep"
                 , "XF86Favorites"
                 , "XF86AudioPause"
                 , "XF86AudioMedia"
                 , "XF86MyComputer"
                 , "XF86VendorHome"
                 , "XF86LightBulb"
                 , "XF86Shop"
                 , "XF86History"
                 , "XF86OpenURL"
                 , "XF86AddFavorite"
                 , "XF86HotLinks"
                 , "XF86BrightnessAdjust"
                 , "XF86Finance"
                 , "XF86Community"
                 , "XF86AudioRewind"
                 , "XF86BackForward"
                 , "XF86Launch0"
                 , "XF86Launch1"
                 , "XF86Launch2"
                 , "XF86Launch3"
                 , "XF86Launch4"
                 , "XF86Launch5"
                 , "XF86Launch6"
                 , "XF86Launch7"
                 , "XF86Launch8"
                 , "XF86Launch9"
                 , "XF86LaunchA"
                 , "XF86LaunchB"
                 , "XF86LaunchC"
                 , "XF86LaunchD"
                 , "XF86LaunchE"
                 , "XF86LaunchF"
                 , "XF86ApplicationLeft"
                 , "XF86ApplicationRight"
                 , "XF86Book"
                 , "XF86CD"
                 , "XF86Calculater"
                 , "XF86Clear"
                 , "XF86Close"
                 , "XF86Copy"
                 , "XF86Cut"
                 , "XF86Display"
                 , "XF86DOS"
                 , "XF86Documents"
                 , "XF86Excel"
                 , "XF86Explorer"
                 , "XF86Game"
                 , "XF86Go"
                 , "XF86iTouch"
                 , "XF86LogOff"
                 , "XF86Market"
                 , "XF86Meeting"
                 , "XF86MenuKB"
                 , "XF86MenuPB"
                 , "XF86MySites"
                 , "XF86New"
                 , "XF86News"
                 , "XF86OfficeHome"
                 , "XF86Open"
                 , "XF86Option"
                 , "XF86Paste"
                 , "XF86Phone"
                 , "XF86Q"
                 , "XF86Reply"
                 , "XF86Reload"
                 , "XF86RotateWindows"
                 , "XF86RotationPB"
                 , "XF86RotationKB"
                 , "XF86Save"
                 , "XF86ScrollUp"
                 , "XF86ScrollDown"
                 , "XF86ScrollClick"
                 , "XF86Send"
                 , "XF86Spell"
                 , "XF86SplitScreen"
                 , "XF86Support"
                 , "XF86TaskPane"
                 , "XF86Terminal"
                 , "XF86Tools"
                 , "XF86Travel"
                 , "XF86UserPB"
                 , "XF86User1KB"
                 , "XF86User2KB"
                 , "XF86Video"
                 , "XF86WheelButton"
                 , "XF86Word"
                 , "XF86Xfer"
                 , "XF86ZoomIn"
                 , "XF86ZoomOut"
                 , "XF86Away"
                 , "XF86Messenger"
                 , "XF86WebCam"
                 , "XF86MailForward"
                 , "XF86Pictures"
                 , "XF86Music"
                 , "XF86TouchpadToggle"
                 , "XF86AudioMicMute"
                 , "XF86_Switch_VT_1"
                 , "XF86_Switch_VT_2"
                 , "XF86_Switch_VT_3"
                 , "XF86_Switch_VT_4"
                 , "XF86_Switch_VT_5"
                 , "XF86_Switch_VT_6"
                 , "XF86_Switch_VT_7"
                 , "XF86_Switch_VT_8"
                 , "XF86_Switch_VT_9"
                 , "XF86_Switch_VT_10"
                 , "XF86_Switch_VT_11"
                 , "XF86_Switch_VT_12"
                 , "XF86_Ungrab"
                 , "XF86_ClearGrab"
                 , "XF86_Next_VMode"
                 , "XF86_Prev_VMode" ]
