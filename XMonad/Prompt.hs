{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt
-- Copyright   :  (C) 2007 Andrea Rossato, 2015 Evgeny Kurnevsky
--                    2015 Sibi Prabakaran, 2018 Yclept Nemo
-- License     :  BSD3
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for writing graphical prompts for XMonad
--
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- Bugs:
-- if 'alwaysHighlight' is True, and
--  1 type several characters
--  2 tab-complete past several entries
--  3 backspace back to the several characters
--  4 tab-complete once (results in the entry past the one in [2])
--  5 tab-complete against this shorter list of completions
-- then the prompt will freeze (XMonad continues however).
-----------------------------------------------------------------------------

module XMonad.Prompt
    ( -- * Usage
      -- $usage
      mkXPrompt
    , mkXPromptWithReturn
    , mkXPromptWithModes
    , def
    , amberXPConfig
    , greenXPConfig
    , XPMode
    , XPType (..)
    , XPColor (..)
    , XPPosition (..)
    , XPConfig (..)
    , XPrompt (..)
    , XP
    , defaultXPKeymap, defaultXPKeymap'
    , emacsLikeXPKeymap, emacsLikeXPKeymap'
    , vimLikeXPKeymap, vimLikeXPKeymap'
    , quit
    , promptSubmap, promptBuffer, toHeadChar, bufferOne
    , killBefore, killAfter, startOfLine, endOfLine
    , insertString, pasteString, pasteString'
    , clipCursor, moveCursor, moveCursorClip
    , setInput, getInput, getOffset
    , defaultColor, modifyColor, setColor
    , resetColor, setBorderColor
    , modifyPrompter, setPrompter, resetPrompter
    , moveWord, moveWord', killWord, killWord'
    , changeWord, deleteString
    , moveHistory, setSuccess, setDone, setModeDone
    , Direction1D(..)
    , ComplFunction
    , ComplCaseSensitivity(..)
    -- * X Utilities
    -- $xutils
    , mkUnmanagedWindow
    , fillDrawable
    -- * Other Utilities
    -- $utils
    , mkComplFunFromList
    , mkComplFunFromList'
    -- * @nextCompletion@ implementations
    , getNextOfLastWord
    , getNextCompletion
    -- * List utilities
    , getLastWord
    , skipLastWord
    , splitInSubListsAt
    , breakAtSpace
    , uniqSort
    , historyCompletion
    , historyCompletionP
    -- * History filters
    , deleteAllDuplicates
    , deleteConsecutive
    , HistoryMatches
    , initMatches
    , historyUpMatching
    , historyDownMatching
    -- * Types
    , XPState
    ) where

import           XMonad                       hiding (cleanMask, config)
import qualified XMonad                       as X (numberlockMask)
import qualified XMonad.StackSet              as W
import           XMonad.Util.Font
import           XMonad.Util.Types
import           XMonad.Util.XSelection       (getSelection)

import           Codec.Binary.UTF8.String     (decodeString,isUTF8Encoded)
import           Control.Arrow                (first, (&&&), (***))
import           Control.Concurrent           (threadDelay)
import           Control.Exception            as E hiding (handle)
import           Control.Monad.State
import           Data.Bifunctor               (bimap)
import           Data.Bits
import           Data.Char                    (isSpace)
import           Data.IORef
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe                   (fromMaybe, listToMaybe)
import           Data.Set                     (fromList, toList)
import           System.IO
import           System.IO.Unsafe             (unsafePerformIO)
import           System.Posix.Files

-- $usage
-- For usage examples see "XMonad.Prompt.Shell",
-- "XMonad.Prompt.XMonad" or "XMonad.Prompt.Ssh"
--
-- TODO:
--
-- * scrolling the completions that don't fit in the window (?)

type XP = StateT XPState IO

data XPState =
    XPS { dpy                   :: Display
        , rootw                 :: !Window
        , win                   :: !Window
        , screen                :: !Rectangle
        , complWin              :: Maybe Window
        , complWinDim           :: Maybe ComplWindowDim
        , complIndex            :: !(Int,Int)
        -- | This IORef should always have the same value as
        -- complWin. Its purpose is to enable removal of the
        -- completion window if an exception occurs, since the most
        -- recent value of complWin is not available when handling
        -- exceptions.
        , complWinRef           :: IORef (Maybe Window)
        , showComplWin          :: Bool
        , operationMode         :: XPOperationMode
        , highlightedCompl      :: Maybe String
        , gcon                  :: !GC
        , fontS                 :: !XMonadFont
        , commandHistory        :: W.Stack String
        , offset                :: !Int
        , config                :: XPConfig
        , successful            :: Bool
        , numlockMask           :: KeyMask
        , done                  :: Bool
        , modeDone              :: Bool
        , color                 :: XPColor
        , prompter              :: String -> String
        , eventBuffer           :: [(KeySym, String, Event)]
        , inputBuffer           :: String
        , currentCompletions    :: Maybe [String]
        }

data XPConfig =
    XPC { font                  :: String       -- ^ Font. For TrueType fonts, use something like
                                                -- @"xft:Hack:pixelsize=1"@. Alternatively, use X Logical Font
                                                -- Description, i.e. something like
                                                -- @"-*-dejavu sans mono-medium-r-normal--*-80-*-*-*-*-iso10646-1"@.
        , bgColor               :: String       -- ^ Background color
        , fgColor               :: String       -- ^ Font color
        , bgHLight              :: String       -- ^ Background color of a highlighted completion entry
        , fgHLight              :: String       -- ^ Font color of a highlighted completion entry
        , borderColor           :: String       -- ^ Border color
        , promptBorderWidth     :: !Dimension   -- ^ Border width
        , position              :: XPPosition   -- ^ Position: 'Top', 'Bottom', or 'CenteredAt'
        , alwaysHighlight       :: !Bool        -- ^ Always highlight an item, overriden to True with multiple modes. This implies having *one* column of autocompletions only.
        , height                :: !Dimension   -- ^ Window height
        , maxComplRows          :: Maybe Dimension
                                                -- ^ Just x: maximum number of rows to show in completion window
        , historySize           :: !Int         -- ^ The number of history entries to be saved
        , historyFilter         :: [String] -> [String]
                                                -- ^ a filter to determine which
                                                -- history entries to remember
        , promptKeymap          :: M.Map (KeyMask,KeySym) (XP ())
                                                -- ^ Mapping from key combinations to actions
        , completionKey         :: (KeyMask, KeySym)     -- ^ Key that should trigger completion
        , changeModeKey         :: KeySym       -- ^ Key to change mode (when the prompt has multiple modes)
        , defaultText           :: String       -- ^ The text by default in the prompt line
        , autoComplete          :: Maybe Int    -- ^ Just x: if only one completion remains, auto-select it,
                                                --   and delay by x microseconds
        , showCompletionOnTab   :: Bool         -- ^ Only show list of completions when Tab was pressed
        , complCaseSensitivity  :: ComplCaseSensitivity
                                                -- ^ Perform completion in a case-sensitive manner
        , searchPredicate       :: String -> String -> Bool
                                                -- ^ Given the typed string and a possible
                                                --   completion, is the completion valid?
        , defaultPrompter       :: String -> String
                                                -- ^ Modifies the prompt given by 'showXPrompt'
        , sorter                :: String -> [String] -> [String]
                                                -- ^ Used to sort the possible completions by how well they
                                                --   match the search string (see X.P.FuzzyMatch for an
                                                --   example).
        }

data XPType = forall p . XPrompt p => XPT p
type ComplFunction = String -> IO [String]
type XPMode = XPType
data XPOperationMode = XPSingleMode ComplFunction XPType | XPMultipleModes (W.Stack XPType)

newtype ComplCaseSensitivity = ComplCaseSensitive Bool

instance Show XPType where
    show (XPT p) = showXPrompt p

instance XPrompt XPType where
    showXPrompt                 = show
    nextCompletion      (XPT t) = nextCompletion      t
    commandToComplete   (XPT t) = commandToComplete   t
    completionToCommand (XPT t) = completionToCommand t
    completionFunction  (XPT t) = completionFunction  t
    modeAction          (XPT t) = modeAction          t

-- | The class prompt types must be an instance of. In order to
-- create a prompt you need to create a data type, without parameters,
-- and make it an instance of this class, by implementing a simple
-- method, 'showXPrompt', which will be used to print the string to be
-- displayed in the command line window.
--
-- This is an example of a XPrompt instance definition:
--
-- >     instance XPrompt Shell where
-- >          showXPrompt Shell = "Run: "
class XPrompt t where

    -- | This method is used to print the string to be
    -- displayed in the command line window.
    showXPrompt :: t -> String

    -- | This method is used to generate the next completion to be
    -- printed in the command line when tab is pressed, given the
    -- string presently in the command line and the list of
    -- completion.
    -- This function is not used when in multiple modes (because alwaysHighlight in XPConfig is True)
    nextCompletion :: t -> String -> [String] -> String
    nextCompletion = getNextOfLastWord

    -- | This method is used to generate the string to be passed to
    -- the completion function.
    commandToComplete :: t -> String -> String
    commandToComplete _ = getLastWord

    -- | This method is used to process each completion in order to
    -- generate the string that will be compared with the command
    -- presently displayed in the command line. If the prompt is using
    -- 'getNextOfLastWord' for implementing 'nextCompletion' (the
    -- default implementation), this method is also used to generate,
    -- from the returned completion, the string that will form the
    -- next command line when tab is pressed.
    completionToCommand :: t -> String -> String
    completionToCommand _ c = c

    -- | When the prompt has multiple modes, this is the function
    -- used to generate the autocompletion list.
    -- The argument passed to this function is given by `commandToComplete`
    -- The default implementation shows an error message.
    completionFunction :: t -> ComplFunction
    completionFunction t = \_ -> return ["Completions for " ++ showXPrompt t ++ " could not be loaded"]

    -- | When the prompt has multiple modes (created with mkXPromptWithModes), this function is called
    -- when the user picks an item from the autocompletion list.
    -- The first argument is the prompt (or mode) on which the item was picked
    -- The first string argument is the autocompleted item's text.
    -- The second string argument is the query made by the user (written in the prompt's buffer).
    -- See XMonad/Actions/Launcher.hs for a usage example.
    modeAction :: t -> String -> String -> X ()
    modeAction _ _ _ = return ()

data XPPosition = Top
                | Bottom
                -- | Prompt will be placed in the center horizontally and
                --   in the certain place of screen vertically. If it's in the upper
                --   part of the screen, completion window will be placed below(like
                --   in 'Top') and otherwise above(like in 'Bottom')
                | CenteredAt { xpCenterY :: Rational
                             -- ^ Rational between 0 and 1, giving
                             -- y coordinate of center of the prompt relative to the screen height.
                             , xpWidth  :: Rational
                             -- ^ Rational between 0 and 1, giving
                             -- width of the prompt relatave to the screen width.
                             }
                  deriving (Show,Read)

data XPColor =
    XPColor { bgNormal      :: String   -- ^ Background color
            , fgNormal      :: String   -- ^ Font color
            , bgHighlight   :: String   -- ^ Background color of a highlighted completion entry
            , fgHighlight   :: String   -- ^ Font color of a highlighted completion entry
            , border        :: String   -- ^ Border color
            }

amberXPConfig, greenXPConfig :: XPConfig

instance Default XPColor where
    def =
        XPColor { bgNormal    = "grey22"
                , fgNormal    = "grey80"
                , bgHighlight = "grey"
                , fgHighlight = "black"
                , border      = "white"
                }

instance Default XPConfig where
  def =
    XPC { font                  = "-misc-fixed-*-*-*-*-12-*-*-*-*-*-*-*"
        , bgColor               = bgNormal def
        , fgColor               = fgNormal def
        , bgHLight              = bgHighlight def
        , fgHLight              = fgHighlight def
        , borderColor           = border def
        , promptBorderWidth     = 1
        , promptKeymap          = defaultXPKeymap
        , completionKey         = (0,xK_Tab)
        , changeModeKey         = xK_grave
        , position              = Bottom
        , height                = 18
        , maxComplRows          = Nothing
        , historySize           = 256
        , historyFilter         = id
        , defaultText           = []
        , autoComplete          = Nothing
        , showCompletionOnTab   = False
        , complCaseSensitivity  = ComplCaseSensitive True
        , searchPredicate       = isPrefixOf
        , alwaysHighlight       = False
        , defaultPrompter       = id
        , sorter                = const id
        }
greenXPConfig = def { bgColor           = "black"
                    , fgColor           = "green"
                    , promptBorderWidth = 0
                    }
amberXPConfig = def { bgColor   = "black"
                    , fgColor   = "#ca8f2d"
                    , fgHLight  = "#eaaf4c"
                    }

initState :: Display -> Window -> Window -> Rectangle -> XPOperationMode
          -> GC -> XMonadFont -> [String] -> XPConfig -> KeyMask -> XPState
initState d rw w s opMode gc fonts h c nm =
    XPS { dpy                   = d
        , rootw                 = rw
        , win                   = w
        , screen                = s
        , complWin              = Nothing
        , complWinDim           = Nothing
        , complWinRef        = unsafePerformIO (newIORef Nothing)
        , showComplWin          = not (showCompletionOnTab c)
        , operationMode         = opMode
        , highlightedCompl      = Nothing
        , gcon                  = gc
        , fontS                 = fonts
        , commandHistory        = W.Stack { W.focus = defaultText c
                                          , W.up    = []
                                          , W.down  = h
                                          }
        , complIndex            = (0,0) --(column index, row index), used when `alwaysHighlight` in XPConfig is True
        , offset                = length (defaultText c)
        , config                = c
        , successful            = False
        , done                  = False
        , modeDone              = False
        , numlockMask           = nm
        , prompter              = defaultPrompter c
        , color                 = defaultColor c
        , eventBuffer           = []
        , inputBuffer           = ""
        , currentCompletions    = Nothing
        }

-- Returns the current XPType
currentXPMode :: XPState -> XPType
currentXPMode st = case operationMode st of
  XPMultipleModes modes -> W.focus modes
  XPSingleMode _ xptype -> xptype

-- When in multiple modes, this function sets the next mode
-- in the list of modes as active
setNextMode :: XPState -> XPState
setNextMode st = case operationMode st of
  XPMultipleModes modes -> case W.down modes of
    [] -> st -- there is no next mode, return same state
    (m:ms) -> let
      currentMode = W.focus modes
      in st { operationMode = XPMultipleModes W.Stack { W.up = [], W.focus = m, W.down = ms ++ [currentMode]}} --set next and move previous current mode to the of the stack
  _ -> st --nothing to do, the prompt's operation has only one mode

-- Returns the highlighted item
highlightedItem :: XPState -> [String] -> Maybe String
highlightedItem st' completions = case complWinDim st' of
  Nothing -> Nothing -- when there isn't any compl win, we can't say how many cols,rows there are
  Just winDim ->
    let
      (_,_,_,_,xx,yy) = winDim
      complMatrix = splitInSubListsAt (length yy) (take (length xx * length yy) completions)
      (col_index,row_index) = complIndex st'
    in case completions of
      [] -> Nothing
      _  -> complMatrix !? col_index >>= (!? row_index)
 where
  -- | Safe version of '(!!)'.
  (!?) :: [a] -> Int -> Maybe a
  (!?) xs n = listToMaybe $ drop n xs

-- | Return the selected completion, i.e. the 'String' we actually act
-- upon after the user confirmed their selection (by pressing @Enter@).
selectedCompletion :: XPState -> String
selectedCompletion st
    -- If 'alwaysHighlight' is used, look at the currently selected item (if any)
  | alwaysHighlight (config st) = fromMaybe (command st) $ highlightedCompl st
    -- Otherwise, look at what the user actually wrote so far
  | otherwise                   = command st

-- this would be much easier with functional references
command :: XPState -> String
command = W.focus . commandHistory

setCommand :: String -> XPState -> XPState
setCommand xs s = s { commandHistory = (commandHistory s) { W.focus = xs }}

-- | Sets the input string to the given value.
setInput :: String -> XP ()
setInput = modify . setCommand

-- | Returns the current input string. Intented for use in custom keymaps
-- where 'get' or similar can't be used to retrieve it.
getInput :: XP String
getInput = gets command

-- | Returns the offset of the current input string. Intended for use in custom
-- keys where 'get' or similar can't be used to retrieve it.
getOffset :: XP Int
getOffset = gets offset

-- | Accessor encapsulating disparate color fields of 'XPConfig' into an
-- 'XPColor' (the configuration provides default values).
defaultColor :: XPConfig -> XPColor
defaultColor c = XPColor { bgNormal     = bgColor c
                         , fgNormal     = fgColor c
                         , bgHighlight  = bgHLight c
                         , fgHighlight  = fgHLight c
                         , border       = borderColor c
                         }

-- | Modify the prompt colors.
modifyColor :: (XPColor -> XPColor) -> XP ()
modifyColor c = modify $ \s -> s { color = c $ color s }

-- | Set the prompt colors.
setColor :: XPColor -> XP ()
setColor = modifyColor . const

-- | Reset the prompt colors to those from 'XPConfig'.
resetColor :: XP ()
resetColor = gets (defaultColor . config) >>= setColor

-- | Set the prompt border color.
setBorderColor :: String -> XPColor -> XPColor
setBorderColor bc xpc = xpc { border = bc }

-- | Modify the prompter, i.e. for chaining prompters.
modifyPrompter :: ((String -> String) -> (String -> String)) -> XP ()
modifyPrompter p = modify $ \s -> s { prompter = p $ prompter s }

-- | Set the prompter.
setPrompter :: (String -> String) -> XP ()
setPrompter = modifyPrompter . const

-- | Reset the prompter to the one from 'XPConfig'.
resetPrompter :: XP ()
resetPrompter = gets (defaultPrompter . config) >>= setPrompter

-- | Set the current completion list, or 'Nothing' to invalidate the current
-- completions.
setCurrentCompletions :: Maybe [String] -> XP ()
setCurrentCompletions cs = modify $ \s -> s { currentCompletions = cs }

-- | Get the current completion list.
getCurrentCompletions :: XP (Maybe [String])
getCurrentCompletions = gets currentCompletions

-- | Same as 'mkXPrompt', except that the action function can have
--   type @String -> X a@, for any @a@, and the final action returned
--   by 'mkXPromptWithReturn' will have type @X (Maybe a)@.  @Nothing@
--   is yielded if the user cancels the prompt (by e.g. hitting Esc or
--   Ctrl-G).  For an example of use, see the 'XMonad.Prompt.Input'
--   module.
mkXPromptWithReturn :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X a)  -> X (Maybe a)
mkXPromptWithReturn t conf compl action = do
  st' <- mkXPromptImplementation (showXPrompt t) conf (XPSingleMode compl (XPT t))
  if successful st'
    then Just <$> action (selectedCompletion st')
    else return Nothing

-- | Creates a prompt given:
--
-- * a prompt type, instance of the 'XPrompt' class.
--
-- * a prompt configuration ('def' can be used as a starting point)
--
-- * a completion function ('mkComplFunFromList' can be used to
-- create a completions function given a list of possible completions)
--
-- * an action to be run: the action must take a string and return 'XMonad.X' ()
mkXPrompt :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X ()) -> X ()
mkXPrompt t conf compl action = void $ mkXPromptWithReturn t conf compl action

-- | Creates a prompt with multiple modes given:
--
-- * A non-empty list of modes
-- * A prompt configuration
--
-- The created prompt allows to switch between modes with `changeModeKey` in `conf`. The modes are
-- instances of XPrompt. See XMonad.Actions.Launcher for more details
--
-- The argument supplied to the action to execute is always the current highlighted item,
-- that means that this prompt overrides the value `alwaysHighlight` for its configuration to True.
mkXPromptWithModes :: [XPType] -> XPConfig -> X ()
mkXPromptWithModes modes conf = do
  let defaultMode = head modes
      modeStack = W.Stack { W.focus = defaultMode -- Current mode
                          , W.up = []
                          , W.down = tail modes -- Other modes
                          }
      om = XPMultipleModes modeStack
  st' <- mkXPromptImplementation (showXPrompt defaultMode) conf { alwaysHighlight = True } om
  when (successful st') $
    case operationMode st' of
      XPMultipleModes ms -> let
        action = modeAction $ W.focus ms
        in action (command st') $ fromMaybe "" (highlightedCompl st')
      _ -> error "The impossible occurred: This prompt runs with multiple modes but they could not be found." --we are creating a prompt with multiple modes, so its operationMode should have been constructed with XPMultipleMode

-- Internal function used to implement 'mkXPromptWithReturn' and
-- 'mkXPromptWithModes'.
mkXPromptImplementation :: String -> XPConfig -> XPOperationMode -> X XPState
mkXPromptImplementation historyKey conf om = do
  XConf { display = d, theRoot = rw } <- ask
  s <- gets $ screenRect . W.screenDetail . W.current . windowset
  numlock <- gets X.numberlockMask
  cachedir <- getXMonadCacheDir
  hist <- io $ readHistory cachedir
  fs <- initXMF (font conf)
  st' <- io $
    bracket
      (createWin d rw conf s)
      (destroyWindow d)
      (\w ->
        bracket
          (createGC d w)
          (freeGC d)
          (\gc -> do
            selectInput d w $ exposureMask .|. keyPressMask
            setGraphicsExposures d gc False
            let hs = fromMaybe [] $ M.lookup historyKey hist
                st = initState d rw w s om gc fs hs conf numlock
            runXP st))
  releaseXMF fs
  when (successful st') $ do
    let prune = take (historySize conf)
    io $ writeHistory cachedir $
      M.insertWith
      (\xs ys -> prune . historyFilter conf $ xs ++ ys)
      historyKey
      -- We need to apply historyFilter before as well, since
      -- otherwise the filter would not be applied if there is no
      -- history
      (prune $ historyFilter conf [selectedCompletion st'])
      hist
  return st'

-- | Removes numlock and capslock from a keymask.
-- Duplicate of cleanMask from core, but in the
-- XP monad instead of X.
cleanMask :: KeyMask -> XP KeyMask
cleanMask msk = do
  numlock <- gets numlockMask
  let highMasks = 1 `shiftL` 12 - 1
  return (complement (numlock .|. lockMask) .&. msk .&. highMasks)

-- | Inverse of 'Codec.Binary.UTF8.String.utf8Encode', that is, a convenience
-- function that checks to see if the input string is UTF8 encoded before
-- decoding.
utf8Decode :: String -> String
utf8Decode str
    | isUTF8Encoded str = decodeString str
    | otherwise         = str

runXP :: XPState -> IO XPState
runXP st = do
  let d = dpy st
      w = win st
  bracket
    (grabKeyboard d w True grabModeAsync grabModeAsync currentTime)
    (\_ -> ungrabKeyboard d currentTime)
    (\status ->
      execStateT
        (when (status == grabSuccess) $ do
          updateWindows
          eventLoop handleMain evDefaultStop)
        st
      `finally` (mapM_ (destroyWindow d) =<< readIORef (complWinRef st))
      `finally` sync d False)

type KeyStroke = (KeySym, String)

-- | Main event "loop". Gives priority to events from the state's event buffer.
eventLoop :: (KeyStroke -> Event -> XP ())
          -> XP Bool
          -> XP ()
eventLoop handle stopAction = do
    b <- gets eventBuffer
    (keysym,keystr,event) <- case b of
        []  -> do
                d <- gets dpy
                io $ allocaXEvent $ \e -> do
                    -- Also capture @buttonPressMask@, see Note [Allow ButtonEvents]
                    maskEvent d (exposureMask .|. keyPressMask .|. buttonPressMask) e
                    ev <- getEvent e
                    (ks,s) <- if ev_event_type ev == keyPress
                              then lookupString $ asKeyEvent e
                              else return (Nothing, "")
                    return (fromMaybe xK_VoidSymbol ks,s,ev)
        l   -> do
                modify $ \s -> s { eventBuffer = tail l }
                return $ head l
    handle (keysym,keystr) event
    stopAction >>= flip unless (eventLoop handle stopAction)

-- | Default event loop stop condition.
evDefaultStop :: XP Bool
evDefaultStop = (||) <$> gets modeDone <*> gets done

-- | Common patterns shared by all event handlers.
handleOther :: KeyStroke -> Event -> XP ()
handleOther _ ExposeEvent{ev_window = w} = do
    -- Expose events can be triggered by switching virtual consoles.
    st <- get
    when (win st == w) updateWindows
handleOther _ ButtonEvent{ev_event_type = t} = do
    -- See Note [Allow ButtonEvents]
    when (t == buttonPress) $ do
        d <- gets dpy
        io $ allowEvents d replayPointer currentTime
handleOther _ _ = return ()

{- Note [Allow ButtonEvents]

Some settings (like @clickJustFocuses = False@) set up the passive
pointer grabs that xmonad makes to intercept clicks to unfocused windows
with @pointer_mode = grabModeSync@ and @keyboard_mode = grabModeSync@.
This means that any click in an unfocused window leads to a
pointer/keyboard grab that freezes both devices until 'allowEvents' is
called. But "XMonad.Prompt" has its own X event loop, so 'allowEvents'
is never called and everything remains frozen indefinitely.

This does not happen when the grabs are made with @grabModeAsync@, as
pointer events processing is not frozen and the grab only lasts as long
as the mouse button is pressed.

Hence, in this situation we call 'allowEvents' in the prompts event loop
whenever a button event is received, releasing the pointer grab. In this
case, 'replayPointer' takes care of the fact that these events are not
merely discarded, but passed to the respective application window.
-}

-- | Prompt event handler for the main loop. Dispatches to input, completion
-- and mode switching handlers.
handleMain :: KeyStroke -> Event -> XP ()
handleMain stroke@(keysym,_) KeyEvent{ev_event_type = t, ev_state = m} = do
    (compKey,modeKey) <- gets $ (completionKey &&& changeModeKey) . config
    keymask <- cleanMask m
    -- haven't subscribed to keyRelease, so just in case
    when (t == keyPress) $
        if (keymask,keysym) == compKey
           then getCurrentCompletions >>= handleCompletionMain
           else do
                setCurrentCompletions Nothing
                if keysym == modeKey
                   then modify setNextMode >> updateWindows
                   else handleInputMain keymask stroke
handleMain stroke event = handleOther stroke event

-- | Prompt input handler for the main loop.
handleInputMain :: KeyMask -> KeyStroke -> XP ()
handleInputMain keymask (keysym,keystr) = do
    keymap <- gets (promptKeymap . config)
    case M.lookup (keymask,keysym) keymap of
        -- 'null keystr' i.e. when only a modifier was pressed
        Just action -> action >> updateWindows
        Nothing     -> unless (null keystr) $
            when (keymask .&. controlMask == 0) $ do
                insertString $ utf8Decode keystr
                updateWindows
                updateHighlightedCompl
                complete <- tryAutoComplete
                when complete $ setSuccess True >> setDone True

-- There are two options to store the completion list during the main loop:
-- * Use the State monad, with 'Nothing' as the initial state.
-- * Join the output of the event loop handler to the input of the (same)
--   subsequent handler, using 'Nothing' as the initial input.
-- Both approaches are, under the hood, equivalent.
--
-- | Prompt completion handler for the main loop. Given 'Nothing', generate the
-- current completion list. With the current list, trigger a completion.
handleCompletionMain :: Maybe [String] -> XP ()
handleCompletionMain Nothing   = do
    cs <- getCompletions
    when (length cs > 1) $
        modify $ \s -> s { showComplWin = True }
    setCurrentCompletions $ Just cs
    handleCompletion cs
handleCompletionMain (Just cs) = handleCompletion cs

handleCompletion :: [String] -> XP ()
handleCompletion cs = do
    alwaysHlight <- gets $ alwaysHighlight . config
    st <- get

    let updateWins  l = redrawWindows l
        updateState l = if alwaysHlight
            then hlComplete (getLastWord $ command st) l st
            else simpleComplete                        l st

    case cs of
      []  -> updateWindows
      [x] -> do updateState [x]
                cs' <- getCompletions
                updateWins cs'
                setCurrentCompletions $ Just cs'
      l   -> updateState l   >> updateWins l
    where
        -- When alwaysHighlight is off, just complete based on what the
        -- user has typed so far.
        simpleComplete :: [String] -> XPState -> XP ()
        simpleComplete l st = do
          let newCommand = nextCompletion (currentXPMode st) (command st) l
          modify $ \s -> setCommand newCommand $
                         s { offset = length newCommand
                           , highlightedCompl = Just newCommand
                           }

        -- If alwaysHighlight is on, and the user wants the next
        -- completion, move to the next completion item and update the
        -- buffer to reflect that.
        --
        --TODO: Scroll or paginate results
        hlComplete :: String -> [String] -> XPState -> XP ()
        hlComplete prevCompl l st =
          if | -- The current suggestion matches the command and is a
               -- proper suffix of the last suggestion, so replace it.
               isSuffixOfCmd && isProperSuffixOfLast -> replaceCompletion

               -- The current suggestion matches the command, so advance
               -- to the next completion and try again.
             | isSuffixOfCmd ->
                 hlComplete hlCompl l $ st{ complIndex = complIndex'
                                          , highlightedCompl = nextHlCompl
                                          }

               -- If nothing matches at all, delete the suggestion and
               -- highlight the next one.
             | otherwise -> replaceCompletion
         where
          hlCompl     :: String       = fromMaybe (command st) $ highlightedItem st l
          complIndex' :: (Int, Int)   = nextComplIndex st (length l)
          nextHlCompl :: Maybe String = highlightedItem st{ complIndex = complIndex' } cs

          isSuffixOfCmd        :: Bool = hlCompl `isSuffixOf` command st
          isProperSuffixOfLast :: Bool =      hlCompl   `isSuffixOf` prevCompl
                                      && not (prevCompl `isSuffixOf` hlCompl)

          replaceCompletion :: XP () = do
              put st
              replicateM_ (length $ words prevCompl) $ killWord Prev
              insertString' hlCompl
              endOfLine

-- | Initiate a prompt sub-map event loop. Submaps are intended to provide
-- alternate keybindings. Accepts a default action and a mapping from key
-- combinations to actions. If no entry matches, the default action is run.
promptSubmap :: XP ()
             -> M.Map (KeyMask, KeySym) (XP ())
             -> XP ()
promptSubmap defaultAction keymap = do
    md <- gets modeDone
    setModeDone False
    updateWindows
    eventLoop (handleSubmap defaultAction keymap) evDefaultStop
    setModeDone md

handleSubmap :: XP ()
             -> M.Map (KeyMask, KeySym) (XP ())
             -> KeyStroke
             -> Event
             -> XP ()
handleSubmap defaultAction keymap stroke KeyEvent{ev_event_type = t, ev_state = m} = do
    keymask <- cleanMask m
    when (t == keyPress) $ handleInputSubmap defaultAction keymap keymask stroke
handleSubmap _ _ stroke event = handleOther stroke event

handleInputSubmap :: XP ()
                  -> M.Map (KeyMask, KeySym) (XP ())
                  -> KeyMask
                  -> KeyStroke
                  -> XP ()
handleInputSubmap defaultAction keymap keymask (keysym,keystr) =
    case M.lookup (keymask,keysym) keymap of
        Just action -> action >> updateWindows
        Nothing     -> unless (null keystr) $ defaultAction >> updateWindows

-- | Initiate a prompt input buffer event loop. Input is sent to a buffer and
-- bypasses the prompt. The provided function is given the existing buffer and
-- the input keystring. The first field of the result determines whether the
-- input loop continues (if @True@). The second field determines whether the
-- input is appended to the buffer, or dropped (if @False@). If the loop is to
-- stop without keeping input - that is, @(False,False)@ - the event is
-- prepended to the event buffer to be processed by the parent loop. This
-- allows loop to process both fixed and indeterminate inputs.
--
-- Result given @(continue,keep)@:
--
-- * cont and keep
--
--      * grow input buffer
--
-- * stop and keep
--
--      * grow input buffer
--      * stop loop
--
-- * stop and drop
--
--      * buffer event
--      * stop loop
--
-- * cont and drop
--
--      * do nothing
promptBuffer :: (String -> String -> (Bool,Bool)) -> XP String
promptBuffer f = do
    md <- gets modeDone
    setModeDone False
    eventLoop (handleBuffer f) evDefaultStop
    buff <- gets inputBuffer
    modify $ \s -> s { inputBuffer = "" }
    setModeDone md
    return buff

handleBuffer :: (String -> String -> (Bool,Bool))
             -> KeyStroke
             -> Event
             -> XP ()
handleBuffer f stroke event@KeyEvent{ev_event_type = t, ev_state = m} = do
    keymask <- cleanMask m
    when (t == keyPress) $ handleInputBuffer f keymask stroke event
handleBuffer _ stroke event = handleOther stroke event

handleInputBuffer :: (String -> String -> (Bool,Bool))
                  -> KeyMask
                  -> KeyStroke
                  -> Event
                  -> XP ()
handleInputBuffer f keymask (keysym,keystr) event =
    unless (null keystr || keymask .&. controlMask /= 0) $ do
        (evB,inB) <- gets (eventBuffer &&& inputBuffer)
        let keystr' = utf8Decode keystr
        let (cont,keep) = f inB keystr'
        when keep $
            modify $ \s -> s { inputBuffer = inB ++ keystr' }
        unless cont $
            setModeDone True
        unless (cont || keep) $
            modify $ \s -> s { eventBuffer = (keysym,keystr,event) : evB }

-- | Predicate instructing 'promptBuffer' to get (and keep) a single non-empty
-- 'KeyEvent'.
bufferOne :: String -> String -> (Bool,Bool)
bufferOne xs x = (null xs && null x,True)

--Receives an state of the prompt, the size of the autocompletion list and returns the column,row
--which should be highlighted next
nextComplIndex :: XPState -> Int -> (Int,Int)
nextComplIndex st nitems = case complWinDim st of
  Nothing -> (0,0) --no window dims (just destroyed or not created)
  Just (_,_,_,_,xx,yy) -> let
    (ncols,nrows) = (length xx, length yy)
    (currentcol,currentrow) = complIndex st
    in if currentcol + 1 >= ncols then --hlight is in the last column
         if currentrow + 1 < nrows then --hlight is still not at the last row
           (currentcol, currentrow + 1)
         else
           (0,0)
       else if currentrow + 1 < nrows then --hlight not at the last row
              (currentcol, currentrow + 1)
            else
              (currentcol + 1, 0)

tryAutoComplete :: XP Bool
tryAutoComplete = do
    ac <- gets (autoComplete . config)
    case ac of
        Just d -> do cs <- getCompletions
                     case cs of
                         [c] -> runCompleted c d >> return True
                         _   -> return False
        Nothing    -> return False
  where runCompleted cmd delay = do
            st <- get
            let new_command = nextCompletion (currentXPMode st) (command st) [cmd]
            modify $ setCommand "autocompleting..."
            updateWindows
            io $ threadDelay delay
            modify $ setCommand new_command
            return True

-- KeyPresses

-- | Default key bindings for prompts.  Click on the \"Source\" link
--   to the right to see the complete list.  See also 'defaultXPKeymap''.
defaultXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
defaultXPKeymap = defaultXPKeymap' isSpace

-- | A variant of 'defaultXPKeymap' which lets you specify a custom
--   predicate for identifying non-word characters, which affects all
--   the word-oriented commands (move\/kill word).  The default is
--   'isSpace'.  For example, by default a path like @foo\/bar\/baz@
--   would be considered as a single word.  You could use a predicate
--   like @(\\c -> isSpace c || c == \'\/\')@ to move through or
--   delete components of the path one at a time.
defaultXPKeymap' :: (Char -> Bool) -> M.Map (KeyMask,KeySym) (XP ())
defaultXPKeymap' p = M.fromList $
  map (first $ (,) controlMask) -- control + <key>
  [ (xK_u, killBefore)
  , (xK_k, killAfter)
  , (xK_a, startOfLine)
  , (xK_e, endOfLine)
  , (xK_y, pasteString)
  -- Retain the pre-0.14 moveWord' behavior:
  , (xK_Right, moveWord' p Next >> moveCursor Next)
  , (xK_Left, moveCursor Prev >> moveWord' p Prev)
  , (xK_Delete, killWord' p Next)
  , (xK_BackSpace, killWord' p Prev)
  , (xK_w, killWord' p Prev)
  , (xK_g, quit)
  , (xK_bracketleft, quit)
  ] ++
  map (first $ (,) 0)
  [ (xK_Return, setSuccess True >> setDone True)
  , (xK_KP_Enter, setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete, deleteString Next)
  , (xK_Left, moveCursor Prev)
  , (xK_Right, moveCursor Next)
  , (xK_Home, startOfLine)
  , (xK_End, endOfLine)
  , (xK_Down, moveHistory W.focusUp')
  , (xK_Up, moveHistory W.focusDown')
  , (xK_Escape, quit)
  ]

-- | A keymap with many emacs-like key bindings.  Click on the
--   \"Source\" link to the right to see the complete list.
--   See also 'emacsLikeXPKeymap''.
emacsLikeXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
emacsLikeXPKeymap = emacsLikeXPKeymap' isSpace

-- | A variant of 'emacsLikeXPKeymap' which lets you specify a custom
--   predicate for identifying non-word characters, which affects all
--   the word-oriented commands (move\/kill word).  The default is
--   'isSpace'.  For example, by default a path like @foo\/bar\/baz@
--   would be considered as a single word.  You could use a predicate
--   like @(\\c -> isSpace c || c == \'\/\')@ to move through or
--   delete components of the path one at a time.
emacsLikeXPKeymap' :: (Char -> Bool) -> M.Map (KeyMask,KeySym) (XP ())
emacsLikeXPKeymap' p = M.fromList $
  map (first $ (,) controlMask) -- control + <key>
  [ (xK_z, killBefore) --kill line backwards
  , (xK_k, killAfter) -- kill line fowards
  , (xK_a, startOfLine) --move to the beginning of the line
  , (xK_e, endOfLine) -- move to the end of the line
  , (xK_d, deleteString Next) -- delete a character foward
  , (xK_b, moveCursor Prev) -- move cursor forward
  , (xK_f, moveCursor Next) -- move cursor backward
  , (xK_BackSpace, killWord' p Prev) -- kill the previous word
  , (xK_y, pasteString)
  , (xK_g, quit)
  , (xK_bracketleft, quit)
  ] ++
  map (first $ (,) mod1Mask) -- meta key + <key>
  [ (xK_BackSpace, killWord' p Prev)
  -- Retain the pre-0.14 moveWord' behavior:
  , (xK_f, moveWord' p Next >> moveCursor Next) -- move a word forward
  , (xK_b, moveCursor Prev >> moveWord' p Prev) -- move a word backward
  , (xK_d, killWord' p Next) -- kill the next word
  , (xK_n, moveHistory W.focusUp')
  , (xK_p, moveHistory W.focusDown')
  ]
  ++
  map (first $ (,) 0) -- <key>
  [ (xK_Return, setSuccess True >> setDone True)
  , (xK_KP_Enter, setSuccess True >> setDone True)
  , (xK_BackSpace, deleteString Prev)
  , (xK_Delete, deleteString Next)
  , (xK_Left, moveCursor Prev)
  , (xK_Right, moveCursor Next)
  , (xK_Home, startOfLine)
  , (xK_End, endOfLine)
  , (xK_Down, moveHistory W.focusUp')
  , (xK_Up, moveHistory W.focusDown')
  , (xK_Escape, quit)
  ]

-- | Vim-ish key bindings. Click on the \"Source\" link to the right to see the
-- complete list. See also 'vimLikeXPKeymap''.
vimLikeXPKeymap :: M.Map (KeyMask,KeySym) (XP ())
vimLikeXPKeymap = vimLikeXPKeymap' (setBorderColor "grey22") id id isSpace

-- | A variant of 'vimLikeXPKeymap' with customizable aspects:
vimLikeXPKeymap' :: (XPColor -> XPColor)
                    -- ^ Modifies the prompt color when entering normal mode.
                    -- The default is @setBorderColor "grey22"@ - same color as
                    -- the default background color.
                 -> (String -> String)
                    -- ^ Prompter to use in normal mode. The default of 'id'
                    -- balances 'defaultPrompter' but @("[n] " ++)@ is a good
                    -- alternate with 'defaultPrompter' as @("[i] " ++)@.
                 -> (String -> String)
                    -- ^ Filter applied to the X Selection before pasting. The
                    -- default is 'id' but @filter isPrint@ is a good
                    -- alternate.
                 -> (Char -> Bool)
                    -- ^ Predicate identifying non-word characters. The default
                    -- is 'isSpace'. See the documentation of other keymaps for
                    -- alternates.
                 -> M.Map (KeyMask,KeySym) (XP ())
vimLikeXPKeymap' fromColor promptF pasteFilter notWord = M.fromList $
    map (first $ (,) 0)
    [ (xK_Return,       setSuccess True >> setDone True)
    , (xK_KP_Enter,     setSuccess True >> setDone True)
    , (xK_BackSpace,    deleteString Prev)
    , (xK_Delete,       deleteString Next)
    , (xK_Left,         moveCursor Prev)
    , (xK_Right,        moveCursor Next)
    , (xK_Home,         startOfLine)
    , (xK_End,          endOfLine)
    , (xK_Down,         moveHistory W.focusUp')
    , (xK_Up,           moveHistory W.focusDown')
    , (xK_Escape,       moveCursor Prev
                            >> modifyColor fromColor
                            >> setPrompter promptF
                            >> promptSubmap (return ()) normalVimXPKeymap
                            >> resetColor
                            >> resetPrompter
      )
    ] where
    normalVimXPKeymap = M.fromList $
        map (first $ (,) 0)
        [ (xK_i,            setModeDone True)
        , (xK_a,            moveCursor Next >> setModeDone True)
        , (xK_s,            deleteString Next >> setModeDone True)
        , (xK_x,            deleteString Next >> clipCursor)
        , (xK_Delete,       deleteString Next >> clipCursor)
        , (xK_p,            moveCursor Next
                                >> pasteString' pasteFilter
                                >> moveCursor Prev
          )
        , (xK_0,            startOfLine)
        , (xK_Escape,       quit)
        , (xK_Down,         moveHistory W.focusUp')
        , (xK_j,            moveHistory W.focusUp')
        , (xK_Up,           moveHistory W.focusDown')
        , (xK_k,            moveHistory W.focusDown')
        , (xK_Right,        moveCursorClip Next)
        , (xK_l,            moveCursorClip Next)
        , (xK_h,            moveCursorClip Prev)
        , (xK_Left,         moveCursorClip Prev)
        , (xK_BackSpace,    moveCursorClip Prev)
        -- Implementation using the original 'moveWord'':
        --, (xK_e,            moveCursor Next >> moveWord' notWord Next >> moveCursor Prev)
        --, (xK_b,            moveWord' notWord Prev)
        --, (xK_w,            moveWord' (not . notWord) Next >> clipCursor)
        , (xK_e,            moveCursorClip Next >> moveWord' notWord Next)
        , (xK_b,            moveCursorClip Prev >> moveWord' notWord Prev)
        , (xK_w,            moveWord' (not . notWord) Next >> moveCursorClip Next)
        , (xK_f,            promptBuffer bufferOne >>= toHeadChar Next)
        , (xK_d,            promptSubmap (setModeDone True) deleteVimXPKeymap)
        , (xK_c,            promptSubmap (setModeDone True) changeVimXPKeymap
                                >> setModeDone True
          )
        ] ++
        map (first $ (,) shiftMask)
        [ (xK_dollar,       endOfLine >> moveCursor Prev)
        , (xK_D,            killAfter >> moveCursor Prev)
        , (xK_C,            killAfter >> setModeDone True)
        , (xK_P,            pasteString' pasteFilter >> moveCursor Prev)
        , (xK_A,            endOfLine >> setModeDone True)
        , (xK_I,            startOfLine >> setModeDone True)
        , (xK_F,            promptBuffer bufferOne >>= toHeadChar Prev)
        ]
    deleteVimXPKeymap = M.fromList $
        map (bimap (0 ,) (>> setModeDone True))
        [ (xK_e,            deleteString Next >> killWord' notWord Next >> clipCursor)
        , (xK_w,            killWord' (not . notWord) Next >> clipCursor)
        , (xK_0,            killBefore)
        , (xK_b,            killWord' notWord Prev)
        , (xK_d,            setInput "")
        ] ++
        map (bimap (shiftMask ,) (>> setModeDone True))
        [ (xK_dollar,       killAfter >> moveCursor Prev)
        ]
    changeVimXPKeymap = M.fromList $
        map (bimap (0 ,) (>> setModeDone True))
        [ (xK_e,            deleteString Next >> killWord' notWord Next)
        , (xK_0,            killBefore)
        , (xK_b,            killWord' notWord Prev)
        , (xK_c,            setInput "")
        , (xK_w,            changeWord notWord)
        ] ++
        map (bimap (shiftMask, ) (>> setModeDone True))
        [ (xK_dollar,       killAfter)
        ]

-- Useful for exploring off-by-one issues.
--testOffset :: XP ()
--testOffset = do
--    off <- getOffset
--    str <- getInput
--    setInput $ str ++ "|" ++ (show off) ++ ":" ++ (show $ length str)

-- | Set @True@ to save the prompt's entry to history and run it via the
-- provided action.
setSuccess :: Bool -> XP ()
setSuccess b = modify $ \s -> s { successful = b }

-- | Set @True@ to leave all event loops, no matter how nested.
setDone :: Bool -> XP ()
setDone b = modify $ \s -> s { done = b }

-- | Set @True@ to leave the current event loop, i.e. submaps.
setModeDone :: Bool -> XP ()
setModeDone b = modify $ \s -> s { modeDone = b }

-- KeyPress and State

-- | Quit.
quit :: XP ()
quit = flushString >> setSuccess False >> setDone True >> setModeDone True

-- | Kill the portion of the command before the cursor
killBefore :: XP ()
killBefore =
  modify $ \s -> setCommand (drop (offset s) (command s)) $ s { offset  = 0 }

-- | Kill the portion of the command including and after the cursor
killAfter :: XP ()
killAfter =
  modify $ \s -> setCommand (take (offset s) (command s)) s

-- | Kill the next\/previous word, using 'isSpace' as the default
--   predicate for non-word characters.  See 'killWord''.
killWord :: Direction1D -> XP ()
killWord = killWord' isSpace

-- | Kill the next\/previous word, given a predicate to identify
--   non-word characters. First delete any consecutive non-word
--   characters; then delete consecutive word characters, stopping
--   just before the next non-word character.
--
--   For example, by default (using 'killWord') a path like
--   @foo\/bar\/baz@ would be deleted in its entirety.  Instead you can
--   use something like @killWord' (\\c -> isSpace c || c == \'\/\')@ to
--   delete the path one component at a time.
killWord' :: (Char -> Bool) -> Direction1D -> XP ()
killWord' p d = do
  o <- gets offset
  c <- gets command
  let (f,ss)        = splitAt o c
      delNextWord   = dropWhile (not . p) . dropWhile p
      delPrevWord   = reverse . delNextWord . reverse
      (ncom,noff)   =
          case d of
            Next -> (f ++ delNextWord ss, o)
            Prev -> (delPrevWord f ++ ss, length $ delPrevWord f) -- laziness!!
  modify $ \s -> setCommand ncom $ s { offset = noff}

-- | From Vim's @:help cw@:
--
-- * Special case: When the cursor is in a word, "cw" and "cW" do not include
--   the white space after a word, they only change up to the end of the word.
changeWord :: (Char -> Bool) -> XP ()
changeWord p = join $ f <$> getInput <*> getOffset <*> pure p
    where
        f :: String -> Int -> (Char -> Bool) -> XP ()
        f str off _ | length str <= off ||
                      length str <= 0       = return ()
        f str off p'| p' $ str !! off       = killWord' (not . p') Next
                    | otherwise             = killWord' p' Next

-- | Put the cursor at the end of line
endOfLine :: XP ()
endOfLine  =
    modify $ \s -> s { offset = length (command s)}

-- | Put the cursor at the start of line
startOfLine :: XP ()
startOfLine  =
    modify $ \s -> s { offset = 0 }

-- |  Flush the command string and reset the offset
flushString :: XP ()
flushString = modify $ \s -> setCommand "" $ s { offset = 0}

--reset index if config has `alwaysHighlight`. The inserted char could imply fewer autocompletions.
--If the current index was column 2, row 1 and now there are only 4 autocompletion rows with 1 column, what should we highlight? Set it to the first and start navigation again
resetComplIndex :: XPState -> XPState
resetComplIndex st = if alwaysHighlight (config st) then st { complIndex = (0,0) } else st

-- | Insert a character at the cursor position
insertString :: String -> XP ()
insertString str = do
  insertString' str
  modify resetComplIndex

insertString' :: String -> XP ()
insertString' str =
  modify $ \s -> let
    cmd = c (command s) (offset s)
    st = s { offset = o (offset s)}
    in setCommand cmd st
  where o oo = oo + length str
        c oc oo | oo >= length oc = oc ++ str
                | otherwise = f ++ str ++ ss
                where (f,ss) = splitAt oo oc

-- | Insert the current X selection string at the cursor position. The X
-- selection is not modified.
pasteString :: XP ()
pasteString = pasteString' id

-- | A variant of 'pasteString' which allows modifying the X selection before
-- pasting.
pasteString' :: (String -> String) -> XP ()
pasteString' f = insertString . f =<< getSelection

-- | Remove a character at the cursor position
deleteString :: Direction1D -> XP ()
deleteString d =
  modify $ \s -> setCommand (c (command s) (offset s)) $ s { offset = o (offset s)}
  where o oo = if d == Prev then max 0 (oo - 1) else oo
        c oc oo
            | oo >= length oc && d == Prev = take (oo - 1) oc
            | oo <  length oc && d == Prev = take (oo - 1) f ++ ss
            | oo <  length oc && d == Next = f ++ tail ss
            | otherwise = oc
            where (f,ss) = splitAt oo oc

-- | Ensure the cursor remains over the command by shifting left if necessary.
clipCursor :: XP ()
clipCursor = modify $ \s -> s { offset = o (offset s) (command s)}
    where o oo c = min (max 0 $ length c - 1) oo

-- | Move the cursor one position.
moveCursor :: Direction1D -> XP ()
moveCursor d =
  modify $ \s -> s { offset = o (offset s) (command s)}
  where o oo c = if d == Prev then max 0 (oo - 1) else min (length c) (oo + 1)

-- | Move the cursor one position, but not beyond the command.
moveCursorClip :: Direction1D -> XP ()
moveCursorClip = (>> clipCursor) . moveCursor
--  modify $ \s -> s { offset = o (offset s) (command s)}
--  where o oo c = if d == Prev then max 0 (oo - 1) else min (max 0 $ length c - 1) (oo + 1)

-- | Move the cursor one word, using 'isSpace' as the default
--   predicate for non-word characters.  See 'moveWord''.
moveWord :: Direction1D -> XP ()
moveWord = moveWord' isSpace

-- | Given a direction, move the cursor to just before the next
-- (predicate,not-predicate) character transition. This means a (not-word,word)
-- transition should be followed by a 'moveCursorClip' action. Always considers
-- the character under the current cursor position.  This means a
-- (word,not-word) transition should be preceded by a 'moveCursorClip' action.
-- Calculated as the length of consecutive non-predicate characters starting
-- from the cursor position, plus the length of subsequent consecutive
-- predicate characters, plus when moving backwards the distance of the cursor
-- beyond the input. Reduced by one to avoid jumping off either end of the
-- input, when present.
--
-- Use these identities to retain the pre-0.14 behavior:
--
-- @
--     (oldMoveWord' p Prev) = (moveCursor Prev >> moveWord' p Prev)
-- @
--
-- @
--     (oldMoveWord' p Next) = (moveWord' p Next >> moveCursor Next)
-- @
moveWord' :: (Char -> Bool) -> Direction1D -> XP ()
moveWord' p d = do
  c <- gets command
  o <- gets offset
  let (f,ss) = splitOn o c
      splitOn n xs = (take (n+1) xs, drop n xs)
      gap = case d of
                Prev -> max 0 $ (o + 1) - length c
                Next -> 0
      len = max 0 . flip (-) 1 . (gap +)
          . uncurry (+)
          . (length *** (length . takeWhile (not . p)))
          . span p
      newoff = case d of
                Prev -> o - len (reverse f)
                Next -> o + len ss
  modify $ \s -> s { offset = newoff }

-- | Set the prompt's input to an entry further up or further down the history
-- stack. Use 'Stack' functions from 'XMonad.StackSet', i.e. 'focusUp'' or
-- 'focusDown''.
moveHistory :: (W.Stack String -> W.Stack String) -> XP ()
moveHistory f = do
  modify $ \s -> let ch = f $ commandHistory s
                 in s { commandHistory = ch
                      , offset         = length $ W.focus ch
                      , complIndex     = (0,0) }
  updateWindows
  updateHighlightedCompl

-- | Move the cursor in the given direction to the first instance of the first
-- character of the given string, assuming the string is not empty. The
-- starting cursor character is not considered, and the cursor is placed over
-- the matching character.
toHeadChar :: Direction1D -> String -> XP ()
toHeadChar d s = unless (null s) $ do
    cmd <- gets command
    off <- gets offset
    let c = head s
        off' = (if d == Prev then negate . fst else snd)
             . join (***) (maybe 0 (+1) . elemIndex c)
             . (reverse *** drop 1)
             $ splitAt off cmd
    modify $ \st -> st { offset = offset st + off' }

updateHighlightedCompl :: XP ()
updateHighlightedCompl = do
  st <- get
  cs <- getCompletions
  alwaysHighlight' <- gets $ alwaysHighlight . config
  when alwaysHighlight' $ modify $ \s -> s {highlightedCompl = highlightedItem st cs}

-- X Stuff

updateWindows :: XP ()
updateWindows = do
  d <- gets dpy
  drawWin
  c <- getCompletions
  case c  of
    [] -> void destroyComplWin
    l  -> redrawComplWin l
  io $ sync d False

redrawWindows :: [String] -> XP ()
redrawWindows c = do
  d <- gets dpy
  drawWin
  case c of
    [] -> return ()
    l  -> redrawComplWin l
  io $ sync d False

createWin :: Display -> Window -> XPConfig -> Rectangle -> IO Window
createWin d rw c s = do
  let (x,y) = case position c of
                Top -> (0,0)
                Bottom -> (0, rect_height s - height c)
                CenteredAt py w -> (floor $ fi (rect_width s) * ((1 - w) / 2), floor $ py * fi (rect_height s) - (fi (height c) / 2))
      width = case position c of
                CenteredAt _ w -> floor $ fi (rect_width s) * w
                _              -> rect_width s
  w <- mkUnmanagedWindow d (defaultScreenOfDisplay d) rw
                      (rect_x s + x) (rect_y s + fi y) width (height c)
  mapWindow d w
  return w

drawWin :: XP ()
drawWin = do
  st <- get
  let (c,(cr,(d,(w,gc)))) = (config &&& color &&& dpy &&& win &&& gcon) st
      scr = defaultScreenOfDisplay d
      wh = case position c of
             CenteredAt _ wd -> floor $ wd * fi (widthOfScreen scr)
             _               -> widthOfScreen scr
      ht = height c
      bw = promptBorderWidth c
  Just bgcolor <- io $ initColor d (bgNormal cr)
  Just borderC <- io $ initColor d (border cr)
  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc borderC bgcolor (fi bw) wh ht
  printPrompt p
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

printPrompt :: Drawable -> XP ()
printPrompt drw = do
  st <- get
  let (pr,(cr,gc)) = (prompter &&& color &&& gcon) st
      (c,(d,fs)) = (config &&& dpy &&& fontS) st
      (prt,(com,off)) = (pr . show . currentXPMode &&& command &&& offset) st
      str = prt ++ com
      -- break the string in 3 parts: till the cursor, the cursor and the rest
      (f,p,ss) = if off >= length com
                 then (str, " ","") -- add a space: it will be our cursor ;-)
                 else let (a,b) = splitAt off com
                      in (prt ++ a, [head b], tail b)
      ht = height c
  fsl <- io $ textWidthXMF (dpy st) fs f
  psl <- io $ textWidthXMF (dpy st) fs p
  (asc,desc) <- io $ textExtentsXMF fs str
  let y = fi $ ((ht - fi (asc + desc)) `div` 2) + fi asc
      x = (asc + desc) `div` 2

  let draw = printStringXMF d drw fs gc
  -- print the first part
  draw (fgNormal cr) (bgNormal cr) x y f
  -- reverse the colors and print the "cursor" ;-)
  draw (bgNormal cr) (fgNormal cr) (x + fromIntegral fsl) y p
  -- reverse the colors and print the rest of the string
  draw (fgNormal cr) (bgNormal cr) (x + fromIntegral (fsl + psl)) y ss

-- get the current completion function depending on the active mode
getCompletionFunction :: XPState -> ComplFunction
getCompletionFunction st = case operationMode st of
  XPSingleMode compl _ -> compl
  XPMultipleModes modes -> completionFunction $ W.focus modes

-- Completions
getCompletions :: XP [String]
getCompletions = do
  s <- get
  let q     = commandToComplete (currentXPMode s) (command s)
      compl = getCompletionFunction s
      srt   = sorter (config s)
  io $ (srt q <$> compl q) `E.catch` \(SomeException _) -> return []

setComplWin :: Window -> ComplWindowDim -> XP ()
setComplWin w wi = do
  wr <- gets complWinRef
  io $ writeIORef wr (Just w)
  modify (\s -> s { complWin = Just w, complWinDim = Just wi })

destroyComplWin :: XP ()
destroyComplWin = do
  d  <- gets dpy
  cw <- gets complWin
  wr <- gets complWinRef
  case cw of
    Just w -> do io $ destroyWindow d w
                 io $ writeIORef wr Nothing
                 modify (\s -> s { complWin = Nothing, complWinDim = Nothing })
    Nothing -> return ()

type ComplWindowDim = (Position,Position,Dimension,Dimension,Columns,Rows)
type Rows = [Position]
type Columns = [Position]

createComplWin :: ComplWindowDim -> XP Window
createComplWin wi@(x,y,wh,ht,_,_) = do
  st <- get
  let d = dpy st
      scr = defaultScreenOfDisplay d
  w <- io $ mkUnmanagedWindow d scr (rootw st)
                      x y wh ht
  io $ mapWindow d w
  setComplWin w wi
  return w

getComplWinDim :: [String] -> XP ComplWindowDim
getComplWinDim compl = do
  st <- get
  let (c,(scr,fs)) = (config &&& screen &&& fontS) st
      wh = case position c of
             CenteredAt _ w -> floor $ fi (rect_width scr) * w
             _ -> rect_width scr
      ht = height c
      bw = promptBorderWidth c

  tws <- mapM (textWidthXMF (dpy st) fs) compl
  let max_compl_len =  fromIntegral ((fi ht `div` 2) + maximum tws)
      columns = max 1 $ wh `div` fi max_compl_len
      rem_height =  rect_height scr - ht
      (rows,r) = length compl `divMod` fi columns
      needed_rows = max 1 (rows + if r == 0 then 0 else 1)
      limit_max_number = maybe id min (maxComplRows c)
      actual_max_number_of_rows = limit_max_number $ rem_height `div` ht
      actual_rows = min actual_max_number_of_rows (fi needed_rows)
      actual_height = actual_rows * ht
      (x,y) = case position c of
                Top -> (0,ht - bw)
                Bottom -> (0, 0 + rem_height - actual_height + bw)
                CenteredAt py w
                  | py <= 1/2 -> (floor $ fi (rect_width scr) * ((1 - w) / 2), floor (py * fi (rect_height scr) + (fi ht)/2) - bw)
                  | otherwise -> (floor $ fi (rect_width scr) * ((1 - w) / 2), floor (py * fi (rect_height scr) - (fi ht)/2) - actual_height + bw)
  (asc,desc) <- io $ textExtentsXMF fs $ head compl
  let yp = fi $ (ht + fi (asc - desc)) `div` 2
      xp = (asc + desc) `div` 2
      yy = map fi . take (fi actual_rows) $ [yp,(yp + ht)..]
      xx = take (fi columns) [xp,(xp + max_compl_len)..]

  return (rect_x scr + x, rect_y scr + fi y, wh, actual_height, xx, yy)

drawComplWin :: Window -> [String] -> XP ()
drawComplWin w compl = do
  st <- get
  let c   = config st
      cr  = color st
      d   = dpy st
      scr = defaultScreenOfDisplay d
      bw  = promptBorderWidth c
      gc  = gcon st
  Just bgcolor <- io $ initColor d (bgNormal cr)
  Just borderC <- io $ initColor d (border cr)

  (_,_,wh,ht,xx,yy) <- getComplWinDim compl

  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc borderC bgcolor (fi bw) wh ht
  let ac = splitInSubListsAt (length yy) (take (length xx * length yy) compl)

  printComplList d p gc (fgNormal cr) (bgNormal cr) xx yy ac
  --lift $ spawn $ "xmessage " ++ " ac: " ++ show ac  ++ " xx: " ++ show xx ++ " length xx: " ++ show (length xx) ++ " yy: " ++ show (length yy)
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

redrawComplWin ::  [String] -> XP ()
redrawComplWin compl = do
  st <- get
  nwi <- getComplWinDim compl
  let recreate = do destroyComplWin
                    w <- createComplWin nwi
                    drawComplWin w compl
  if compl /= [] && showComplWin st
     then case complWin st of
            Just w -> case complWinDim st of
                        Just wi -> if nwi == wi -- complWinDim did not change
                                   then drawComplWin w compl -- so update
                                   else recreate
                        Nothing -> recreate
            Nothing -> recreate
     else destroyComplWin

-- Finds the column and row indexes in which a string appears.
-- if the string is not in the matrix, the indexes default to (0,0)
findComplIndex :: String -> [[String]] -> (Int,Int)
findComplIndex x xss = let
  colIndex = fromMaybe 0 $ findIndex (\cols -> x `elem` cols) xss
  rowIndex = fromMaybe 0 $ elemIndex x $ (!!) xss colIndex
  in (colIndex,rowIndex)

printComplList :: Display -> Drawable -> GC -> String -> String
               -> [Position] -> [Position] -> [[String]] -> XP ()
printComplList d drw gc fc bc xs ys sss =
    zipWithM_ (\x ss ->
        zipWithM_ (\y item -> do
            st <- get
            alwaysHlight <- gets $ alwaysHighlight . config
            let (f,b)
                  | alwaysHlight -- default to the first item, the one in (0,0)
                  = let (colIndex,rowIndex) = findComplIndex item sss
                    in -- assign some colors
                     if complIndex st == (colIndex,rowIndex)
                     then (fgHighlight $ color st,bgHighlight $ color st)
                     else (fc,bc)
                  | -- compare item with buffer's value
                    completionToCommand (currentXPMode st) item == commandToComplete (currentXPMode st) (command st)
                  = (fgHighlight $ color st,bgHighlight $ color st)
                  | otherwise = (fc,bc)
            printStringXMF d drw (fontS st) gc f b x y item)
        ys ss) xs sss

-- History

type History = M.Map String [String]

emptyHistory :: History
emptyHistory = M.empty

getHistoryFile :: FilePath -> FilePath
getHistoryFile cachedir = cachedir ++ "/prompt-history"

readHistory :: FilePath -> IO History
readHistory cachedir = readHist `E.catch` \(SomeException _) -> return emptyHistory
 where
    readHist = do
        let path = getHistoryFile cachedir
        xs <- withFile path ReadMode hGetLine
        readIO xs

writeHistory :: FilePath -> History -> IO ()
writeHistory cachedir hist = do
  let path = getHistoryFile cachedir
      filtered = M.filter (not . null) hist
  writeFile path (show filtered) `E.catch` \(SomeException e) ->
                          hPutStrLn stderr ("error writing history: "++show e)
  setFileMode path mode
    where mode = ownerReadMode .|. ownerWriteMode

-- $xutils

-- | Fills a 'Drawable' with a rectangle and a border
fillDrawable :: Display -> Drawable -> GC -> Pixel -> Pixel
             -> Dimension -> Dimension -> Dimension -> IO ()
fillDrawable d drw gc borderC bgcolor bw wh ht = do
  -- we start with the border
  setForeground d gc borderC
  fillRectangle d drw gc 0 0 wh ht
  -- here foreground means the background of the text
  setForeground d gc bgcolor
  fillRectangle d drw gc (fi bw) (fi bw) (wh - (bw * 2)) (ht - (bw * 2))

-- | Creates a window with the attribute override_redirect set to True.
-- Windows Managers should not touch this kind of windows.
mkUnmanagedWindow :: Display -> Screen -> Window -> Position
                  -> Position -> Dimension -> Dimension -> IO Window
mkUnmanagedWindow d s rw x y w h = do
  let visual = defaultVisualOfScreen s
      attrmask = cWOverrideRedirect
  allocaSetWindowAttributes $
         \attributes -> do
           set_override_redirect attributes True
           createWindow d rw x y w h 0 (defaultDepthOfScreen s)
                        inputOutput visual attrmask attributes

-- $utils

-- | This function takes a list of possible completions and returns a
-- completions function to be used with 'mkXPrompt'
mkComplFunFromList :: XPConfig -> [String] -> String -> IO [String]
mkComplFunFromList _ _ [] = return []
mkComplFunFromList c l s =
  pure $ filter (searchPredicate c s) l

-- | This function takes a list of possible completions and returns a
-- completions function to be used with 'mkXPrompt'. If the string is
-- null it will return all completions.
mkComplFunFromList' :: XPConfig -> [String] -> String -> IO [String]
mkComplFunFromList' _ l [] = return l
mkComplFunFromList' c l s =
  pure $ filter (searchPredicate c s) l

-- | Given the prompt type, the command line and the completion list,
-- return the next completion in the list for the last word of the
-- command line. This is the default 'nextCompletion' implementation.
getNextOfLastWord :: XPrompt t => t -> String -> [String] -> String
getNextOfLastWord t c l = skipLastWord c ++ completionToCommand t (l !! ni)
    where ni = case commandToComplete t c `elemIndex` map (completionToCommand t) l of
                 Just i -> if i >= length l - 1 then 0 else i + 1
                 Nothing -> 0

-- | An alternative 'nextCompletion' implementation: given a command
-- and a completion list, get the next completion in the list matching
-- the whole command line.
getNextCompletion :: String -> [String] -> String
getNextCompletion c l = l !! idx
    where idx = case c `elemIndex` l of
                  Just i  -> if i >= length l - 1 then 0 else i + 1
                  Nothing -> 0

-- | Given a maximum length, splits a list into sublists
splitInSubListsAt :: Int -> [a] -> [[a]]
splitInSubListsAt _ [] = []
splitInSubListsAt i x = f : splitInSubListsAt i rest
    where (f,rest) = splitAt i x

-- | Gets the last word of a string or the whole string if formed by
-- only one word
getLastWord :: String -> String
getLastWord = reverse . fst . breakAtSpace . reverse

-- | Skips the last word of the string, if the string is composed by
-- more then one word. Otherwise returns the string.
skipLastWord :: String -> String
skipLastWord = reverse . snd . breakAtSpace . reverse

breakAtSpace :: String -> (String, String)
breakAtSpace s
    | " \\" `isPrefixOf` s2 = (s1 ++ " " ++ s1', s2')
    | otherwise = (s1, s2)
      where (s1, s2 ) = break isSpace s
            (s1',s2') = breakAtSpace $ tail s2

-- | 'historyCompletion' provides a canned completion function much like
--   'getShellCompl'; you pass it to mkXPrompt, and it will make completions work
--   from the query history stored in the XMonad cache directory.
historyCompletion :: X ComplFunction
historyCompletion = historyCompletionP (const True)

-- | Like 'historyCompletion' but only uses history data from Prompts whose
-- name satisfies the given predicate.
historyCompletionP :: (String -> Bool) -> X ComplFunction
historyCompletionP p = do
    cd <- getXMonadCacheDir
    pure $ \x ->
        let toComplList = deleteConsecutive . filter (isInfixOf x) . M.foldr (++) []
         in toComplList . M.filterWithKey (const . p) <$> readHistory cd

-- | Sort a list and remove duplicates. Like 'deleteAllDuplicates', but trades off
--   laziness and stability for efficiency.
uniqSort :: Ord a => [a] -> [a]
uniqSort = toList . fromList

-- | Functions to be used with the 'historyFilter' setting.
-- 'deleteAllDuplicates' will remove all duplicate entries.
-- 'deleteConsecutive' will only remove duplicate elements
-- immediately next to each other.
deleteAllDuplicates, deleteConsecutive :: [String] -> [String]
deleteAllDuplicates = nub
deleteConsecutive = map head . group

newtype HistoryMatches = HistoryMatches (IORef ([String],Maybe (W.Stack String)))

-- | Initializes a new HistoryMatches structure to be passed
-- to historyUpMatching
initMatches :: (Functor m, MonadIO m) => m HistoryMatches
initMatches = HistoryMatches <$> liftIO (newIORef ([],Nothing))

historyNextMatching :: HistoryMatches -> (W.Stack String -> W.Stack String) -> XP ()
historyNextMatching hm@(HistoryMatches ref) next = do
  (completed,completions) <- io $ readIORef ref
  input <- getInput
  if input `elem` completed
     then case completions of
            Just cs -> do
                let cmd = W.focus cs
                modify $ setCommand cmd
                modify $ \s -> s { offset = length cmd }
                io $ writeIORef ref (cmd:completed,Just $ next cs)
            Nothing -> return ()
     else do -- the user typed something new, recompute completions
       io . writeIORef ref . ([input] ,) . filterMatching input =<< gets commandHistory
       historyNextMatching hm next
    where filterMatching :: String -> W.Stack String -> Maybe (W.Stack String)
          filterMatching prefix = W.filter (prefix `isPrefixOf`) . next

-- | Retrieve the next history element that starts with
-- the current input. Pass it the result of initMatches
-- when creating the prompt. Example:
--
-- > ..
-- > ((modMask,xK_p), shellPrompt . myPrompt =<< initMatches)
-- > ..
-- > myPrompt ref = def
-- >   { promptKeymap = M.union [((0,xK_Up), historyUpMatching ref)
-- >                            ,((0,xK_Down), historyDownMatching ref)]
-- >                            (promptKeymap def)
-- >   , .. }
--
historyUpMatching, historyDownMatching :: HistoryMatches -> XP ()
historyUpMatching hm = historyNextMatching hm W.focusDown'
historyDownMatching hm = historyNextMatching hm W.focusUp'
