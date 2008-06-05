{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt
-- Copyright   :  (C) 2007 Andrea Rossato
-- License     :  BSD3
--
-- Maintainer  :  andrea.rossato@unibz.it
-- Stability   :  unstable
-- Portability :  unportable
--
-- A module for writing graphical prompts for XMonad
--
-----------------------------------------------------------------------------

module XMonad.Prompt
    ( -- * Usage
      -- $usage
      mkXPrompt
    , mkXPromptWithReturn
    , defaultXPConfig
    , XPType (..)
    , XPPosition (..)
    , XPConfig (..)
    , XPrompt (..)
    , ComplFunction
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
    , decodeInput
    , encodeOutput
    ) where

import XMonad  hiding (config, io)
import qualified XMonad.StackSet as W
import XMonad.Util.Font
import XMonad.Util.XSelection (getSelection)

import Control.Arrow ((&&&))
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<$>))
import Data.Char
import Data.Maybe
import Data.List
import Data.Set (fromList, toList)
import System.Environment (getEnv)
import System.IO
import System.Posix.Files

-- $usage
-- For usage examples see "XMonad.Prompt.Shell",
-- "XMonad.Prompt.XMonad" or "XMonad.Prompt.Ssh"
--
-- TODO:
--
-- * scrolling the completions that don't fit in the window (?)

type XP = StateT XPState IO

data XPState =
    XPS { dpy                :: Display
        , rootw              :: !Window
        , win                :: !Window
        , screen             :: !Rectangle
        , complWin           :: Maybe Window
        , complWinDim        :: Maybe ComplWindowDim
        , completionFunction :: String -> IO [String]
        , gcon               :: !GC
        , fontS              :: !XMonadFont
        , xptype             :: !XPType
        , command            :: String
        , offset             :: !Int
        , history            :: [History]
        , config             :: XPConfig
        }

data XPConfig =
    XPC { font              :: String     -- ^ Font
        , bgColor           :: String     -- ^ Background color
        , fgColor           :: String     -- ^ Font color
        , fgHLight          :: String     -- ^ Font color of a highlighted completion entry
        , bgHLight          :: String     -- ^ Background color of a highlighted completion entry
        , borderColor       :: String     -- ^ Border color
        , promptBorderWidth :: !Dimension  -- ^ Border width
        , position          :: XPPosition -- ^ Position: 'Top' or 'Bottom'
        , height            :: !Dimension  -- ^ Window height
        , historySize       :: !Int        -- ^ The number of history entries to be saved
        , defaultText       :: String     -- ^ The text by default in the prompt line
        } deriving (Show, Read)

data XPType = forall p . XPrompt p => XPT p

instance Show XPType where
    show (XPT p) = showXPrompt p

instance XPrompt XPType where
    showXPrompt                 = show
    nextCompletion      (XPT t) = nextCompletion      t
    commandToComplete   (XPT t) = commandToComplete   t
    completionToCommand (XPT t) = completionToCommand t

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
    nextCompletion :: t -> String -> [String] -> String
    nextCompletion t c l = getNextOfLastWord t c l

    -- | This method is used to generate the string to be passed to
    -- the completion function.
    commandToComplete :: t -> String -> String
    commandToComplete _ c = getLastWord c

    -- | This method is used to process each completion in order to
    -- generate the string that will be compared with the command
    -- presently displayed in the command line. If the prompt is using
    -- 'getNextOfLastWord' for implementing 'nextCompletion' (the
    -- default implementation), this method is also used to generate,
    -- from the returned completion, the string that will form the
    -- next command line when tab is pressed.
    completionToCommand :: t -> String -> String
    completionToCommand _ c = c

data XPPosition = Top
                | Bottom
                  deriving (Show,Read)

defaultXPConfig :: XPConfig
defaultXPConfig =
    XPC { font              = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , bgColor           = "#333333"
        , fgColor           = "#FFFFFF"
        , fgHLight          = "#000000"
        , bgHLight          = "#BBBBBB"
        , borderColor       = "#FFFFFF"
        , promptBorderWidth = 1
        , position          = Bottom
        , height            = 18
        , historySize       = 256
        , defaultText       = []
        }

type ComplFunction = String -> IO [String]

initState :: XPrompt p => Display -> Window -> Window -> Rectangle -> ComplFunction
          -> GC -> XMonadFont -> p -> [History] -> XPConfig -> XPState
initState d rw w s compl gc fonts pt h c =
    XPS { dpy                = d
        , rootw              = rw
        , win                = w
        , screen             = s
        , complWin           = Nothing
        , complWinDim        = Nothing
        , completionFunction = compl
        , gcon               = gc
        , fontS              = fonts
        , xptype             = XPT pt
        , command            = defaultText c
        , offset             = length (defaultText c)
        , history            = h
        , config             = c
        }

-- | Same as 'mkXPrompt', except that the action function can have
--   type @String -> X a@, for any @a@, and the final action returned
--   by 'mkXPromptWithReturn' will have type @X (Maybe a)@.  @Nothing@
--   is yielded if the user cancels the prompt (by e.g. hitting Esc or
--   Ctrl-G).  For an example of use, see the 'XMonad.Prompt.Input'
--   module.
mkXPromptWithReturn :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X a)  -> X (Maybe a)
mkXPromptWithReturn t conf compl action = do
  c <- ask
  let d = display c
      rw = theRoot c
  s <- gets $ screenRect . W.screenDetail . W.current . windowset
  w <- liftIO $ createWin d rw conf s
  liftIO $ selectInput d w $ exposureMask .|. keyPressMask
  gc <- liftIO $ createGC d w
  liftIO $ setGraphicsExposures d gc False
  (hist,h) <- liftIO $ readHistory
  fs <- initXMF (font conf)
  let st = initState d rw w s compl gc fs (XPT t) hist conf
  st' <- liftIO $ execStateT runXP st

  releaseXMF fs
  liftIO $ freeGC d gc
  liftIO $ hClose h
  if (command st' /= "")
    then do
      let htw = take (historySize conf) (history st')
      liftIO $ writeHistory htw
      Just <$> action (command st')
    else
      return Nothing

-- | Creates a prompt given:
--
-- * a prompt type, instance of the 'XPrompt' class.
--
-- * a prompt configuration ('defaultXPConfig' can be used as a
-- starting point)
--
-- * a completion function ('mkComplFunFromList' can be used to
-- create a completions function given a list of possible completions)
--
-- * an action to be run: the action must take a string and return 'XMonad.X' ()
mkXPrompt :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X ()) -> X ()
mkXPrompt t conf compl action = mkXPromptWithReturn t conf compl action >> return ()

runXP :: XP ()
runXP = do
  st <- get
  let (d,w) = (dpy &&& win) st
  status <- io $ grabKeyboard d w True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
          updateWindows
          eventLoop handle
          io $ ungrabKeyboard d currentTime
  io $ destroyWindow d w
  destroyComplWin
  io $ sync d False

type KeyStroke = (KeySym, String)

eventLoop :: (KeyStroke -> Event -> XP ()) -> XP ()
eventLoop action = do
  d <- gets dpy
  (keysym,string,event) <- io $
            allocaXEvent $ \e -> do
              maskEvent d (exposureMask .|. keyPressMask) e
              ev <- getEvent e
              (ks,s) <- if ev_event_type ev == keyPress
                        then lookupString $ asKeyEvent e
                        else return (Nothing, "")
              return (ks,s,ev)
  action (fromMaybe xK_VoidSymbol keysym,string) event

-- Main event handler
handle :: KeyStroke -> Event -> XP ()
handle k@(ks,_) e@(KeyEvent {ev_event_type = t})
    | t == keyPress && ks == xK_Tab    = do
  c <- getCompletions
  completionHandle c k e
handle ks (KeyEvent {ev_event_type = t, ev_state = m})
    | t == keyPress = keyPressHandle m ks
handle _ (ExposeEvent {ev_window = w}) = do
  st <- get
  when (win st == w) updateWindows
  eventLoop handle
handle _  _ = eventLoop handle

-- completion event handler
completionHandle ::  [String] -> KeyStroke -> Event -> XP ()
completionHandle c (ks,_) (KeyEvent {ev_event_type = t})
    | t == keyPress && ks == xK_Tab = do
  st <- get
  let updateState l = do let new_command = nextCompletion (xptype st) (command st) l
                         modify $ \s ->  s { command = new_command, offset = length new_command }
      updateWins  l = do redrawWindows l
                         eventLoop (completionHandle l)
  case c of
    []  -> updateWindows   >> eventLoop handle
    [x] -> updateState [x] >> getCompletions >>= updateWins
    l   -> updateState l   >> updateWins l
-- key release
    | t == keyRelease && ks == xK_Tab = eventLoop (completionHandle c)
-- other keys
completionHandle _ ks (KeyEvent {ev_event_type = t, ev_state = m})
    | t == keyPress = keyPressHandle m ks
-- some other event: go back to main loop
completionHandle _ k e = handle k e

-- KeyPresses

data Direction = Prev | Next deriving (Eq,Show,Read)

keyPressHandle :: KeyMask -> KeyStroke -> XP ()
-- commands: ctrl + ... todo
keyPressHandle mask (ks,_)
    | mask == controlMask =
        -- control sequences
        case () of
          _ | ks == xK_u               -> killBefore    >> go
            | ks == xK_k               -> killAfter     >> go
            | ks == xK_a               -> startOfLine   >> go
            | ks == xK_e               -> endOfLine     >> go
            | ks == xK_y               -> pasteString   >> go
            | ks == xK_Right           -> moveWord Next >> go
            | ks == xK_Left            -> moveWord Prev >> go
            | ks == xK_Delete          -> killWord Next >> go
            | ks == xK_BackSpace       -> killWord Prev >> go
            | ks == xK_w               -> killWord Prev >> go
            | ks == xK_g || ks == xK_c -> quit
            | otherwise  -> eventLoop handle -- unhandled control sequence
    | ks == xK_Return    = historyPush       >> return ()
    | ks == xK_BackSpace = deleteString Prev >> go
    | ks == xK_Delete    = deleteString Next >> go
    | ks == xK_Left      = moveCursor   Prev >> go
    | ks == xK_Right     = moveCursor   Next >> go
    | ks == xK_Up        = moveHistory  Prev >> go
    | ks == xK_Down      = moveHistory  Next >> go
    | ks == xK_Home      = startOfLine       >> go
    | ks == xK_End       = endOfLine         >> go
    | ks == xK_Escape    = quit
    where
      go   = updateWindows >> eventLoop handle
      quit = flushString   >> return () -- quit and discard everything
-- insert a character
keyPressHandle _ (_,s)
    | s == "" = eventLoop handle
    | otherwise = do insertString (decodeInput s)
                     updateWindows
                     eventLoop handle

-- KeyPress and State

-- | Kill the portion of the command before the cursor
killBefore :: XP ()
killBefore =
  modify $ \s -> s { command = drop (offset s) (command s)
                   , offset  = 0 }

-- | Kill the portion of the command including and after the cursor
killAfter :: XP ()
killAfter =
  modify $ \s -> s { command = take (offset s) (command s) }

-- | Kill the next\/previous word
killWord :: Direction -> XP ()
killWord d = do
  XPS { command = c, offset = o } <- get
  let (f,ss)        = splitAt o c
      delNextWord w =
          case w of
            ' ':x -> x
            word  -> snd . break isSpace $ word
      delPrevWord   = reverse . delNextWord . reverse
      (ncom,noff)   =
          case d of
            Next -> (f ++ delNextWord ss, o)
            Prev -> (delPrevWord f ++ ss, length $ delPrevWord f) -- laziness!!
  modify $ \s -> s { command = ncom, offset = noff}

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
flushString = do
  modify $ \s -> s { command = "", offset = 0}

-- | Insert a character at the cursor position
insertString :: String -> XP ()
insertString str =
  modify $ \s -> s { command = c (command s) (offset s), offset = o (offset s)}
  where o oo = oo + length str
        c oc oo | oo >= length oc = oc ++ str
                | otherwise = f ++ str ++ ss
                where (f,ss) = splitAt oo oc

-- | Insert the current X selection string at the cursor position.
pasteString :: XP ()
pasteString = join $ io $ liftM insertString $ getSelection

-- | Remove a character at the cursor position
deleteString :: Direction -> XP ()
deleteString d =
  modify $ \s -> s { command = c (command s) (offset s), offset = o (offset s)}
  where o oo = if d == Prev then max 0 (oo - 1) else oo
        c oc oo
            | oo >= length oc && d == Prev = take (oo - 1) oc
            | oo <  length oc && d == Prev = take (oo - 1) f ++ ss
            | oo <  length oc && d == Next = f ++ tail ss
            | otherwise = oc
            where (f,ss) = splitAt oo oc

-- | move the cursor one position
moveCursor :: Direction -> XP ()
moveCursor d =
  modify $ \s -> s { offset = o (offset s) (command s)}
  where o oo c = if d == Prev then max 0 (oo - 1) else min (length c) (oo + 1)

-- | move the cursor one word
moveWord :: Direction -> XP ()
moveWord d = do
  c <- gets command
  o <- gets offset
  let (f,ss) = splitAt o c
      lenToS = length . fst . break isSpace
      ln p s = case p s of
                 ' ':x -> 1 + lenToS x
                 x     -> lenToS x
      newoff = case d of
                 Prev -> o - (ln reverse f )
                 Next -> o + (ln id      ss)
  modify $ \s -> s { offset = newoff }

moveHistory :: Direction -> XP ()
moveHistory d = do
  h <- getHistory
  c <- gets command
  let str = if h /= [] then head h else c
  let nc = case elemIndex c h of
             Just i -> case d of
                         Prev -> h !! (if (i + 1) > (length h - 1) then 0 else i + 1)
                         Next -> h !! (max (i - 1) 0)
             Nothing -> str
  modify $ \s -> s { command = nc, offset = length nc}

-- X Stuff

updateWindows :: XP ()
updateWindows = do
  d <- gets dpy
  drawWin
  c <- getCompletions
  case c  of
    [] -> destroyComplWin >> return ()
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
  w <- mkUnmanagedWindow d (defaultScreenOfDisplay d) rw
                      (rect_x s + x) (rect_y s + fi y) (rect_width s) (height c)
  mapWindow d w
  return w

drawWin :: XP ()
drawWin = do
  st <- get
  let (c,(d,(w,gc))) = (config &&& dpy &&& win &&& gcon) st
      scr = defaultScreenOfDisplay d
      wh = widthOfScreen scr
      ht = height c
      bw = promptBorderWidth c
  Just bgcolor <- io $ initColor d (bgColor c)
  Just border  <- io $ initColor d (borderColor c)
  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc border bgcolor (fi bw) wh ht
  printPrompt p
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

printPrompt :: Drawable -> XP ()
printPrompt drw = do
  st <- get
  let (gc,(c,(d,fs))) = (gcon &&& config &&& dpy &&& fontS) st
      (prt,(com,off)) = (show . xptype &&& command &&& offset) st
      str = prt ++ com
      -- break the string in 3 parts: till the cursor, the cursor and the rest
      (f,p,ss) = if off >= length com
                 then (str, " ","") -- add a space: it will be our cursor ;-)
                 else let (a,b) = (splitAt off com)
                      in (prt ++ a, [head b], tail b)
      ht = height c
  fsl <- io $ textWidthXMF (dpy st) fs f
  psl <- io $ textWidthXMF (dpy st) fs p
  (asc,desc) <- io $ textExtentsXMF fs str
  let y = fi $ ((ht - fi (asc + desc)) `div` 2) + fi asc
      x = (asc + desc) `div` 2

  let draw = printStringXMF d drw fs gc
  -- print the first part
  draw (fgColor c) (bgColor c) x y f
  -- reverse the colors and print the "cursor" ;-)
  draw (bgColor c) (fgColor c) (x + fromIntegral fsl) y p
  -- reverse the colors and print the rest of the string
  draw (fgColor c) (bgColor c) (x + fromIntegral (fsl + psl)) y ss

-- Completions

getCompletions :: XP [String]
getCompletions = do
  s <- get
  io $ (completionFunction s) (commandToComplete (xptype s) (command s))
       `catch` \_ -> return []

setComplWin :: Window -> ComplWindowDim -> XP ()
setComplWin w wi =
  modify (\s -> s { complWin = Just w, complWinDim = Just wi })

destroyComplWin :: XP ()
destroyComplWin = do
  d  <- gets dpy
  cw <- gets complWin
  case cw of
    Just w -> do io $ destroyWindow d w
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
      wh = rect_width scr
      ht = height c

  tws <- mapM (textWidthXMF (dpy st) fs) compl
  let max_compl_len =  fromIntegral ((fi ht `div` 2) + maximum tws)
      columns = max 1 $ wh `div` (fi max_compl_len)
      rem_height =  rect_height scr - ht
      (rows,r) = (length compl) `divMod` fi columns
      needed_rows = max 1 (rows + if r == 0 then 0 else 1)
      actual_max_number_of_rows = rem_height `div` ht
      actual_rows = min actual_max_number_of_rows (fi needed_rows)
      actual_height = actual_rows * ht
      (x,y) = case position c of
                Top -> (0,ht)
                Bottom -> (0, (0 + rem_height - actual_height))
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
      d   = dpy st
      scr = defaultScreenOfDisplay d
      bw  = promptBorderWidth c
      gc  = gcon st
  Just bgcolor <- io $ initColor d (bgColor c)
  Just border  <- io $ initColor d (borderColor c)

  (_,_,wh,ht,xx,yy) <- getComplWinDim compl

  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc border bgcolor (fi bw) wh ht
  let ac = splitInSubListsAt (length yy) (take ((length xx) * (length yy)) compl)
  printComplList d p gc (fgColor c) (bgColor c) xx yy ac
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

redrawComplWin ::  [String] -> XP ()
redrawComplWin compl = do
  st <- get
  nwi <- getComplWinDim compl
  let recreate = do destroyComplWin
                    w <- createComplWin nwi
                    drawComplWin w compl
  if (compl /= [] )
     then case complWin st of
            Just w -> case complWinDim st of
                        Just wi -> if nwi == wi -- complWinDim did not change
                                   then drawComplWin w compl -- so update
                                   else recreate
                        Nothing -> recreate
            Nothing -> recreate
     else destroyComplWin

printComplList :: Display -> Drawable -> GC -> String -> String
               -> [Position] -> [Position] -> [[String]] -> XP ()
printComplList _ _ _ _ _ _ _ [] = return ()
printComplList _ _ _ _ _ [] _ _ = return ()
printComplList d drw gc fc bc (x:xs) y (s:ss) = do
  printComplColumn d drw gc fc bc x y s
  printComplList d drw gc fc bc xs y ss

printComplColumn :: Display -> Drawable -> GC -> String -> String
                 -> Position -> [Position] -> [String] -> XP ()
printComplColumn _ _ _ _ _ _ _ [] = return ()
printComplColumn _ _ _ _ _ _ [] _ = return ()
printComplColumn d drw gc fc bc x (y:yy) (s:ss) = do
  printComplString d drw gc fc bc x y s
  printComplColumn d drw gc fc bc x yy ss

printComplString :: Display -> Drawable -> GC -> String -> String
                 -> Position -> Position -> String  -> XP ()
printComplString d drw gc fc bc x y s = do
  st <- get
  if completionToCommand (xptype st) s == commandToComplete (xptype st) (command st)
     then printStringXMF d drw (fontS st) gc
                            (fgHLight $ config st) (bgHLight $ config st) x y s
     else printStringXMF d drw (fontS st) gc fc bc x y s

-- History

data History =
    H { prompt :: String
      , command_history :: String
      } deriving (Show, Read, Eq)

historyPush :: XP ()
historyPush = do
  c <- gets command
  when (c /= []) $ modify (\s -> s { history = nub $ H (showXPrompt (xptype s)) c : history s })

getHistory :: XP [String]
getHistory = do
  hist <- gets history
  pt <- gets xptype
  return $ map command_history . filter (\h -> prompt h == showXPrompt pt) $ hist

readHistory :: IO ([History],Handle)
readHistory = do
  home <- getEnv "HOME"
  let path = home ++ "/.xmonad/history"
  f <- fileExist path
  if f then do h <- openFile path ReadMode
               str <- hGetContents h
               case (reads str) of
                 [(hist,_)] -> return (hist,h)
                 [] -> return ([],h)
                 _ -> return ([],h)
       else do h <- openFile path WriteMode
               return ([],h)

writeHistory :: [History] -> IO ()
writeHistory hist = do
  home <- getEnv "HOME"
  let path = home ++ "/.xmonad/history"
  catch (writeFile path (show hist)) (\_ -> do putStrLn "error in writing"; return ())

-- $xutils

-- | Fills a 'Drawable' with a rectangle and a border
fillDrawable :: Display -> Drawable -> GC -> Pixel -> Pixel
             -> Dimension -> Dimension -> Dimension -> IO ()
fillDrawable d drw gc border bgcolor bw wh ht = do
  -- we start with the border
  setForeground d gc border
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
mkComplFunFromList :: [String] -> String -> IO [String]
mkComplFunFromList _ [] = return []
mkComplFunFromList l s =
  return $ filter (\x -> take (length s) x == s) l

-- | This function takes a list of possible completions and returns a
-- completions function to be used with 'mkXPrompt'. If the string is
-- null it will return all completions.
mkComplFunFromList' :: [String] -> String -> IO [String]
mkComplFunFromList' l [] = return l
mkComplFunFromList' l s =
  return $ filter (\x -> take (length s) x == s) l


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

-- Lift an IO action into the XP
io :: IO a -> XP a
io = liftIO

-- Shorthand for fromIntegral
fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

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

-- | Sort a list and remove duplicates.
uniqSort :: Ord a => [a] -> [a]
uniqSort = toList . fromList
