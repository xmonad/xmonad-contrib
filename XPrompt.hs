-----------------------------------------------------------------------------
-- |
-- Module      :  XMonadContrib.XPrompt
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

module XMonadContrib.XPrompt (
                             -- * Usage
                             -- $usage
                             mkXPrompt
                             , defaultPromptConfig
                             , mkComplFunFromList
                             , XPType (..)
                             , XPPosition (..)
                             , XPConfig (..)
                             , XPrompt (..)
                             ) where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import XMonad  hiding (io)
import Operations

import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Maybe
import Data.List


-- $usage:
--
-- For example usage see XMonadContrib.ShellPrompt or
-- XMonadContrib.XMonadPrompt


type XP = StateT XPState IO

data XPState =
    XPS { dpy :: Display    
        , rootw :: Window   
        , win :: Window
        , complWin :: Maybe Window
        , complWinDim :: Maybe ComplWindowDim
        , completionFunction :: String -> IO [String]
        , compList :: Maybe [String]
        , gcon :: GC
        , fs :: FontStruct 
        , xptype :: XPType
        , command :: String 
        , offset :: Int     
        , config :: XPConfig
        }

data XPConfig =  
    XPC { font           :: String   -- ^ Font
        , bgColor        :: String   -- ^ Backgroud color
        , fgColor        :: String   -- ^ Default font color
        , hLight         :: String   -- ^ Default font color
        , borderColor    :: String   -- ^ 
        , borderWidth    :: Dimension
        , position       :: XPPosition
        , height         :: Dimension      -- ^ Window height
        } deriving (Show, Read)

data XPType = forall p . XPrompt p => XPT p 

instance Show XPType where
    show (XPT p) = showXPrompt p

instance XPrompt XPType where
    showXPrompt = show

class XPrompt t where
    showXPrompt :: t -> String

data XPPosition = Top 
                | Bottom
                  deriving (Show,Read)

defaultPromptConfig :: XPConfig
defaultPromptConfig =
    XPC { font = "-misc-fixed-*-*-*-*-10-*-*-*-*-*-*-*"
        , bgColor = "#666666"
        , fgColor = "#FFFFFF"
        , hLight = "#999999"
        , borderColor = "#FFFFFF"
        , borderWidth = 1
        , position = Bottom
        , height = 18
        }

type ComplFunction = String -> IO [String]

initState :: XPrompt p => Display -> Window -> Window -> ComplFunction
          -> GC -> FontStruct -> p -> XPConfig -> XPState
initState d rw w compl gc f pt c =
    XPS d rw w Nothing Nothing compl Nothing gc f (XPT pt) "" 0 c

mkXPrompt :: XPrompt p => p -> XPConfig -> ComplFunction -> (String -> X ())  -> X ()
mkXPrompt t conf compl action = do
  c <- ask
  let d = display c
      rw = theRoot c
  w <- liftIO $ createWin d rw conf
  liftIO $ selectInput d w $ exposureMask .|. keyPressMask
  gc <- liftIO $ createGC d w
  liftIO $ setGraphicsExposures d gc False
  fontS <- liftIO $ loadQueryFont d (font conf)

  let st = initState d rw w compl gc fontS (XPT t) conf
  st' <- liftIO $ execStateT runXP st

  liftIO $ freeGC d gc
  liftIO $ freeFont d fontS
  action (command st')

runXP :: XP ()
runXP = do
  st <- get
  let d = dpy st
      w = win st
  status <- io $ grabKeyboard d w True grabModeAsync grabModeAsync currentTime
  when (status == grabSuccess) $ do
            updateWin
            io $ ungrabKeyboard d currentTime
  io $ destroyWindow d w
  destroyComplWin
  io $ sync d False

eventLoop :: XP ()
eventLoop = do
  d <- gets dpy
  (keysym,string,event) <- io $ 
            allocaXEvent $ \e -> do 
              nextEvent d e
              ev <- getEvent e
              (ks,s) <- lookupString $ asKeyEvent e
              return (ks,s,ev)
  handle (fromMaybe xK_VoidSymbol keysym,string) event

type KeyStroke = (KeySym, String)

-- Main event handler
handle :: KeyStroke -> Event -> XP ()
handle ks (KeyEvent {ev_event_type = t, ev_state = m}) 
    | t == keyPress = do
  keyPressHandle m ks
handle _ (AnyEvent {ev_event_type = t, ev_window = w}) 
    | t == expose = do 
  st <- get
  when (win st == w) updateWin                 
handle _  _ = eventLoop

-- KeyPresses

data Direction = Prev | Next deriving (Eq,Show,Read)

keyPressHandle :: KeyMask -> KeyStroke -> XP ()
-- commands: ctrl + ... todo
keyPressHandle mask (ks,s)
    | mask == controlMask = do
  -- TODO
  eventLoop

keyPressHandle _ (ks,_)
-- exit
    | ks == xK_Return = do
  return ()
-- backspace
    | ks == xK_BackSpace = do
  deleteString Prev
  updateWin
-- delete
    | ks == xK_Delete = do
  deleteString Next
  updateWin
-- left
    | ks == xK_Left = do
  moveCursor Prev
  updateWin
-- right
    | ks == xK_Right = do
  moveCursor Next
  updateWin
-- exscape: exit and discard everything
    | ks  == xK_Escape = do
  flushString
  return ()
-- tab -> completion loop
    | ks  == xK_Tab = do
  completionLoop
  --eventLoop

-- insert a character
keyPressHandle _ (_,s)
    | s == "" = eventLoop
    | otherwise = do
  insertString s
  updateWin

-- KeyPress and State

-- |  Flush the command string and reset the offest
flushString :: XP ()
flushString =
  modify (\s -> s { command = "", offset = 0} )  
 
-- | Insert a character at the cursor position
insertString :: String -> XP ()
insertString str = 
  modify (\s -> s { command = c (command s) (offset s), offset = o (offset s)} )
  where o oo = oo + length str
        c oc oo
            | oo >= length oc = oc ++ str
            | otherwise = f ++ str ++ ss
            where (f,ss) = splitAt oo oc

-- | Remove a character at the cursor position
deleteString :: Direction -> XP ()
deleteString d =
  modify (\s -> s { command = c (command s) (offset s), offset = o (offset s)} )
  where o oo = if d == Prev then max 0 (oo - 1) else oo
        c oc oo
            | oo >= length oc && d == Prev = take (oo - 1) oc
            | oo < length oc && d == Prev = take (oo - 1) f ++ ss
            | oo < length oc && d == Next = f ++ tail ss
            | otherwise = oc
            where (f,ss) = splitAt oo oc

-- | move the cursor one position
moveCursor :: Direction -> XP ()
moveCursor d =
  modify (\s -> s { offset = o (offset s) (command s)} )
  where o oo c = if d == Prev then max 0 (oo - 1) else min (length c) (oo + 1)


-- X Stuff

createWin :: Display -> Window -> XPConfig -> IO Window
createWin d rw c = do
  let scr = defaultScreenOfDisplay d
      wh = widthOfScreen scr
      (x,y) = case position c of
                Top -> (0,0)
                Bottom -> (0,heightOfScreen scr - (height c))
  w <- mkUnmanagedWindow d scr rw 
                      x (fi y) wh (height c)
  mapWindow d w
  return w

updateWin :: XP ()
updateWin = do
  st <- get
  drawWin
  compl <- getCompletions (command st)
  nwi <- getComplWinDim compl
  let recreate = do destroyComplWin
                    w <- createComplWin nwi
                    drawComplWin w compl
  -- check if we have to recreate the completion window
  if (compl /= [] )
     then case complWin st of
            Just w -> case complWinDim st of
                        Just wi -> if nwi == wi -- complWinDim did not change
                                   then drawComplWin w compl -- so update
                                   else recreate
                        Nothing -> recreate
            Nothing -> recreate
     else destroyComplWin
  io $ sync (dpy st) False
  eventLoop

drawWin :: XP ()
drawWin = do
  st <- get
  let c = config st
      d = dpy st
      scr = defaultScreenOfDisplay d
      w = win st
      wh = widthOfScreen scr
      ht = height c
      bw = borderWidth c
      gc = gcon st
      fontStruc = fs st
  bgcolor <- io $ initColor d (bgColor c)
  border <- io $ initColor d (borderColor c)
  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc border bgcolor (fi bw) wh ht
  printPrompt p gc fontStruc
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

printPrompt :: Drawable -> GC -> FontStruct -> XP ()
printPrompt drw gc fontst = do
  c <- gets config
  st <- get
  let d = dpy st
      (prt,com,off) = (show (xptype st), command st, offset st)
      str = prt ++ com
      -- scompose the string in 3 part: till the cursor, the cursor and the rest
      (f,p,ss) = if off >= length com
                 then (str, " ","") -- add a space: it will be our cursor ;-)
                 else let (a,b) = (splitAt off com) 
                      in (prt ++ a, [head b], tail b)
      ht = height c
      (fsl,psl) = (textWidth fontst f, textWidth fontst p)
      (_,asc,desc,_) = textExtents fontst str
      y = fi $ (ht + fi (asc + desc)) `div` 2
      x = (asc + desc) `div` 2
  fgcolor <- io $ initColor d $ fgColor c
  bgcolor <- io $ initColor d $ bgColor c
  -- print the first part
  io $ printString d drw gc fgcolor bgcolor x y f
  -- reverse the colors and print the "cursor" ;-)
  io $ printString d drw gc bgcolor fgcolor (x + fsl) y p
  -- reverse the colors and print the rest of the string
  io $ printString d drw gc fgcolor bgcolor (x + fsl + psl) y ss


-- Completions

getCompletions :: String -> XP [String]
getCompletions s = do
  cf <- gets completionFunction 
  c <- io $ cf s
  setComplList c
  return c

setComplWin :: Window -> ComplWindowDim -> XP ()
setComplWin w wi =
  modify (\s -> s { complWin = Just w, complWinDim = Just wi })

setComplList :: [String] -> XP ()
setComplList l =
  modify (\s -> s { compList = Just l })

destroyComplWin :: XP ()
destroyComplWin = do
  d <- gets dpy
  cw <- gets complWin
  case cw of
    Just w -> do io $ destroyWindow d w
                 modify (\s -> s { complWin = Nothing, complWinDim = Nothing, compList = Nothing })
    Nothing -> return ()

completionLoop :: XP ()
completionLoop = do
  cl <- gets compList
  let nc oc | oc == [] = []
            | otherwise = head $ fromMaybe [oc] cl 
  case cl of
    Just (l:_) -> do modify (\s -> s { command = l, offset = length l })
                     updateWin
    _ -> eventLoop

type ComplWindowDim = (Position,Position,Dimension,Dimension,Rows,Columns)
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
  let c = config st
      d = dpy st
      scr = defaultScreenOfDisplay d
      wh = widthOfScreen scr
      ht = height c
      fontst = fs st

  let compl_number = length compl
      max_compl_len =  (fi ht `div` 2) + (maximum . map (textWidth fontst) $ compl)
      columns = wh `div` (fi max_compl_len)
      rem_height =  heightOfScreen scr - ht
      needed_rows = max 1 (compl_number `div` fi columns)
      actual_max_number_of_rows = rem_height `div` ht
      actual_rows = min actual_max_number_of_rows (fi needed_rows)
      actual_height = actual_rows * ht
      (x,y) = case position c of
                Top -> (0,ht)
                Bottom -> (0, (0 + rem_height - actual_height))

  let (_,asc,desc,_) = textExtents fontst $ head compl
      yp = fi $ (ht + fi (asc + desc)) `div` 2
      xp = (asc + desc) `div` 2
      yy = map fi . take (fi actual_rows) $ [yp,(yp + ht)..]
      xx = take (fi columns) [xp,(xp + max_compl_len)..]

  return (x, fi y, wh, actual_height, xx, yy)

drawComplWin :: Window -> [String] -> XP ()
drawComplWin w compl = do
  st <- get
  let c = config st
      d = dpy st
      scr = defaultScreenOfDisplay d
      bw = borderWidth c
      gc = gcon st
  bgcolor <- io $ initColor d (bgColor c)
  fgcolor <- io $ initColor d (fgColor c)
  border <- io $ initColor d (borderColor c)

  (_,_,wh,ht,xx,yy) <- getComplWinDim compl

  p <- io $ createPixmap d w wh ht
                         (defaultDepthOfScreen scr)
  io $ fillDrawable d p gc border bgcolor (fi bw) wh ht
  let ac = splitInSubListsAt (length yy) (take ((length xx) * (length yy)) compl)
  printComplList d p gc fgcolor bgcolor xx yy ac
  io $ copyArea d p w gc 0 0 wh ht 0 0
  io $ freePixmap d p

printComplList :: Display -> Drawable -> GC -> Pixel -> Pixel
               -> [Position] -> [Position] -> [[String]] -> XP ()
printComplList _ _ _ _ _ _ _ [] = return ()
printComplList _ _ _ _ _ [] _ _ = return ()
printComplList d drw gc fc bc (x:xs) y (s:ss) = do
  printComplColumn d drw gc fc bc x y s
  printComplList d drw gc fc bc xs y ss

printComplColumn :: Display -> Drawable -> GC -> Pixel -> Pixel
                 -> Position -> [Position] -> [String] -> XP ()
printComplColumn _ _ _ _ _ _ _ [] = return ()
printComplColumn _ _ _ _ _ _ [] _ = return ()
printComplColumn d drw gc fc bc x (y:yy) (s:ss) = do
  printComplString d drw gc fc bc x y s
  printComplColumn d drw gc fc bc x yy ss

printComplString :: Display -> Drawable -> GC -> Pixel -> Pixel
                 -> Position -> Position -> String  -> XP ()
printComplString d drw gc fc bc x y s = do
  st <- get
  if s == command st
     then do c <- io $ initColor d (hLight $ config st)
             io $ printString d drw gc fc c x y s
     else io $ printString d drw gc fc bc x y s

-- More general X Stuff

printString :: Display -> Drawable -> GC -> Pixel -> Pixel
            -> Position -> Position -> String  -> IO ()
printString d drw gc fc bc x y s = do
  setForeground d gc fc
  setBackground d gc bc
  drawImageString d drw gc x y s

fillDrawable :: Display -> Drawable -> GC -> Pixel -> Pixel
             -> Dimension -> Dimension -> Dimension -> IO ()
fillDrawable d drw gc border bgcolor bw wh ht = do
  -- we strat with the border
  setForeground d gc border
  fillRectangle d drw gc 0 0 wh ht
  -- this foreground is the background of the text
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

-- Utilities

-- completions
mkComplFunFromList :: [String] -> String -> IO [String]
mkComplFunFromList _ [] = return []
mkComplFunFromList l s =
  return $ filter (\x -> take (length s) x == s) l


-- Lift an IO action into the XP
io :: IO a -> XP a
io = liftIO

-- shorthand
fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

splitInSubListsAt :: Int -> [a] -> [[a]]
splitInSubListsAt _ [] = []
splitInSubListsAt i x = f : splitInSubListsAt i rest
    where (f,rest) = splitAt i x

