{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}
--------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prelude
-- Description :  Utility functions and re-exports.
-- Copyright   :  slotThe <soliditsallgood@mailbox.org>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  slotThe <soliditsallgood@mailbox.org>
--
-- Utility functions and re-exports for a more ergonomic developing
-- experience.  Users themselves will not find much use here.
--
--------------------------------------------------------------------
module XMonad.Prelude (
    module Exports,
    fi,
    chunksOf,
    (.:),
    (!?),
    NonEmpty((:|)),
    notEmpty,
    safeGetWindowAttributes,

    -- * Keys
    keyToString,
    keymaskToString,
    cleanKeyMask,
    regularKeys,
    allSpecialKeys,
    specialKeys,
    multimediaKeys,
    functionKeys,
    WindowScreen,
) where

import Foreign (alloca, peek)
import XMonad

import Control.Applicative as Exports
import Control.Monad       as Exports
import Data.Bool           as Exports
import Data.Char           as Exports
import Data.Foldable       as Exports
import Data.Function       as Exports
import Data.Functor        as Exports
import Data.List           as Exports
import Data.Maybe          as Exports
import Data.Monoid         as Exports
import Data.Traversable    as Exports

import qualified Data.Map.Strict as Map

import Control.Arrow ((&&&), first)
import Data.Bifunctor (bimap)
import Data.Bits
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Tuple (swap)
import GHC.Stack
import qualified XMonad.StackSet as W

-- | Short for 'fromIntegral'.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

-- | Given a maximum length, splits a list into sublists
--
-- >>> chunksOf 5 (take 30 $ repeat 'a')
-- ["aaaaa","aaaaa","aaaaa","aaaaa","aaaaa","aaaaa"]
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = chunk : chunksOf i rest
  where !(chunk, rest) = splitAt i xs

-- | Safe version of '(!!)'.
(!?) :: [a] -> Int -> Maybe a
(!?) xs n | n < 0 = Nothing
          | otherwise = listToMaybe $ drop n xs

-- | Multivariant composition.
--
-- > f .: g ≡ (f .) . g ≡ \c d -> f (g c d)
(.:) :: (a -> b) -> (c -> d -> a) -> c -> d -> b
(.:) = (.) . (.)

-- | 'Data.List.NonEmpty.fromList' with a better error message. Useful to
-- silence GHC's Pattern match(es) are non-exhaustive warning in places where
-- the programmer knows it's always non-empty, but it's infeasible to express
-- that in the type system.
notEmpty :: HasCallStack => [a] -> NonEmpty a
notEmpty [] = error "unexpected empty list"
notEmpty (x:xs) = x :| xs

-- | A safe version of 'Graphics.X11.Extras.getWindowAttributes'.
safeGetWindowAttributes :: Window -> X (Maybe WindowAttributes)
safeGetWindowAttributes w = withDisplay $ \dpy -> io . alloca $ \p ->
  xGetWindowAttributes dpy w p >>= \case
    0 -> pure Nothing
    _ -> Just <$> peek p

-----------------------------------------------------------------------
-- Keys

-- | Convert a modifier mask into a useful string.
keymaskToString :: KeyMask -- ^ Num lock mask
                -> KeyMask -- ^ Modifier mask
                -> String
keymaskToString numLockMask msk =
  concat . reverse . fst . foldr go ([], msk) $ masks
 where
  masks :: [(KeyMask, String)]
  masks = map (\m -> (m, show m))
              [0 .. toEnum (finiteBitSize msk - 1)]
       ++ [ (numLockMask, "num-" )
          , (lockMask,    "lock-")
          , (controlMask, "C-"   )
          , (shiftMask,   "S-"   )
          , (mod5Mask,    "M5-"  )
          , (mod4Mask,    "M4-"  )
          , (mod3Mask,    "M3-"  )
          , (mod2Mask,    "M2-"  )
          , (mod1Mask,    "M1-"  )
          ]

  go :: (KeyMask, String) -> ([String], KeyMask) -> ([String], KeyMask)
  go (m, s) a@(ss, v)
    | v == 0       = a
    | v .&. m == m = (s : ss, v .&. complement m)
    | otherwise    = a

-- | Convert a full key combination; i.e., a 'KeyMask' and 'KeySym'
-- pair, into a string.
keyToString :: (KeyMask, KeySym) -> String
keyToString = uncurry (++) . bimap (keymaskToString 0) ppKeysym
 where
  ppKeysym :: KeySym -> String
  ppKeysym x = case specialMap Map.!? x of
    Just s  -> "<" <> s <> ">"
    Nothing -> case regularMap Map.!? x of
      Nothing -> keysymToString x
      Just s  -> s

  regularMap = Map.fromList (map swap regularKeys)
  specialMap = Map.fromList (map swap allSpecialKeys)

-- | Strip numlock, capslock, mouse buttons and XKB group from a 'KeyMask',
-- leaving only modifier keys like Shift, Control, Super, Hyper in the mask
-- (hence the \"Key\" in \"cleanKeyMask\").
--
-- Core's 'cleanMask' only strips the first two because key events from
-- passive grabs (key bindings) are stripped of mouse buttons and XKB group by
-- the X server already for compatibility reasons. For more info, see:
-- <https://www.x.org/releases/X11R7.7/doc/kbproto/xkbproto.html#Delivering_a_Key_or_Button_Event_to_a_Client>
cleanKeyMask :: X (KeyMask -> KeyMask)
cleanKeyMask = cleanKeyMask' <$> gets numberlockMask

cleanKeyMask' :: KeyMask -> KeyMask -> KeyMask
cleanKeyMask' numLockMask mask =
    mask .&. complement (numLockMask .|. lockMask) .&. (button1Mask - 1)

-- | A list of "regular" (extended ASCII) keys.
regularKeys :: [(String, KeySym)]
regularKeys = map (first (:[]))
            $ zip ['!'             .. '~'          ] -- ASCII
                  [xK_exclam       .. xK_asciitilde]
           <> zip ['\xa0'          .. '\xff'       ] -- Latin1
                  [xK_nobreakspace .. xK_ydiaeresis]

-- | A list of all special key names and their associated KeySyms.
allSpecialKeys :: [(String, KeySym)]
allSpecialKeys = functionKeys <> specialKeys <> multimediaKeys

-- | A list pairing function key descriptor strings (e.g. @\"<F2>\"@)
-- with the associated KeySyms.
functionKeys :: [(String, KeySym)]
functionKeys = [ ('F' : show n, k)
               | (n,k) <- zip ([1..24] :: [Int]) [xK_F1..]
               ]

-- | A list of special key names and their corresponding KeySyms.
specialKeys :: [(String, KeySym)]
specialKeys =
  [ ("Backspace"  , xK_BackSpace)
  , ("Tab"        , xK_Tab)
  , ("Return"     , xK_Return)
  , ("Pause"      , xK_Pause)
  , ("Num_Lock"   , xK_Num_Lock)
  , ("Caps_Lock"  , xK_Caps_Lock)
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
  , ("Control_L"  , xK_Control_L)
  , ("Control_R"  , xK_Control_R)
  , ("Shift_L"    , xK_Shift_L)
  , ("Shift_R"    , xK_Shift_R)
  , ("Alt_L"      , xK_Alt_L)
  , ("Alt_R"      , xK_Alt_R)
  , ("Meta_L"     , xK_Meta_L)
  , ("Meta_R"     , xK_Meta_R)
  , ("Super_L"    , xK_Super_L)
  , ("Super_R"    , xK_Super_R)
  , ("Hyper_L"    , xK_Hyper_L)
  , ("Hyper_R"    , xK_Hyper_R)
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

-- | List of multimedia keys. If Xlib does not know about some keysym
-- it's omitted from the list ('stringToKeysym' returns 'noSymbol' in
-- this case).
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
  , "XF86_Prev_VMode"
  , "XF86Bluetooth"
  ]

-- | The specialized 'W.Screen' derived from 'WindowSet'.
type WindowScreen -- FIXME move to core
    = W.Screen WorkspaceId (Layout Window) Window ScreenId ScreenDetail
