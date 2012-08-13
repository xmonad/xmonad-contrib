-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.DebugWindow
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2012
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module to dump window information for diagnostic/debugging purposes.  See 
-- "XMonad.Hooks.DebugEvents" and "XMonad.Hooks.DebugStack" for practical uses.
--
-----------------------------------------------------------------------------

module XMonad.Util.DebugWindow (debugWindow) where

import           Prelude                  hiding (catch)

import           XMonad

import           Codec.Binary.UTF8.String        (decodeString)
import           Control.Exception
import           Control.Monad                   (when)
import           Data.List                       (unfoldr
                                                 ,intercalate
                                                 )
import           Foreign
import           Foreign.C.String
import           Numeric                         (showHex)
import           System.Exit

-- | Output a window by ID in hex, decimal, its ICCCM resource name and class,
--   and its title if available.  Also indicate override_redirect with an
--   exclamation mark, and wrap in brackets if it is unmapped or withdrawn.
debugWindow   :: Window -> X String
debugWindow 0 =  return "None"
debugWindow w =  do
  let wx = pad 8 '0' $ showHex w ""
  w' <- withDisplay $ \d -> io (safeGetWindowAttributes d w)
  case w' of
    Nothing                                   ->
      return $ "(deleted window " ++ wx ++ ")"
    Just (WindowAttributes x y wid ht bw m o) -> do
      c' <- withDisplay $ \d ->
            io (getWindowProperty8 d wM_CLASS w)
      let c = case c' of
                Nothing -> ""
                Just c''  -> intercalate "/" $
                             flip unfoldr (map (toEnum . fromEnum) c'') $
                             \s -> if null s
                                     then Nothing
                                     else let (w'',s'') = break (== '\NUL') s
                                              s'      = if null s''
                                                          then s''
                                                          else tail s''
                                          in Just (w'',s')
      t <- catchX' (wrap `fmap` getEWMHTitle  "VISIBLE" w) $
           catchX' (wrap `fmap` getEWMHTitle  ""        w) $
           catchX' (wrap `fmap` getICCCMTitle           w) $
           return ""
      let (lb,rb) = case () of
                      () | m == waIsViewable -> ("","")
                         | otherwise         -> ("[","]")
          o'      = if o then "!" else ""
      return $ concat [lb
                      ,o'
                      ,"window "
                      ,wx
                      ,t
                      ," ("
                      ,show wid
                      ,',':show ht
                      ,')':if bw == 0 then "" else '+':show bw
                      ,"@("
                      ,show x
                      ,',':show y
                      ,')':if null c then "" else ' ':c
                      ,rb
                      ]

getEWMHTitle       :: String -> Window -> X String
getEWMHTitle sub w =  do
  a <- getAtom $ "_NET_WM_" ++ (if null sub then "" else '_':sub) ++ "_NAME"
  (Just t) <- withDisplay $ \d -> io $ getWindowProperty32 d a w
  return $ map (toEnum . fromEnum) t

getICCCMTitle   :: Window -> X String
getICCCMTitle w =  do
  t@(TextProperty t' _ 8 _) <- withDisplay $ \d -> io $ getTextProperty d w wM_NAME
  [s] <- catchX' (tryUTF8     t) $
         catchX' (tryCompound t) $
         io ((:[]) `fmap` peekCString t')
  return s

tryUTF8                          :: TextProperty -> X [String]
tryUTF8 (TextProperty s enc _ _) =  do
  uTF8_STRING <- getAtom "UTF8_STRING"
  when (enc == uTF8_STRING) $ error "String is not UTF8_STRING"
  (map decodeString . splitNul) `fmap` io (peekCString s)

tryCompound                            :: TextProperty -> X [String]
tryCompound t@(TextProperty _ enc _ _) =  do
  cOMPOUND_TEXT <- getAtom "COMPOUND_TEXT"
  when (enc == cOMPOUND_TEXT) $ error "String is not COMPOUND_TEXT"
  withDisplay $ \d -> io $ wcTextPropertyToTextList d t

splitNul    :: String -> [String]
splitNul "" =  []
splitNul s  =  let (s',ss') = break (== '\NUL') s in s' : splitNul ss'

pad       :: Int -> Char -> String -> String
pad w c s =  replicate (w - length s) c ++ s

-- modified 'catchX' without the print to 'stderr'
catchX' :: X a -> X a -> X a
catchX' job errcase = do
  st <- get
  c <- ask
  (a, s') <- io $ runX c st job `catch` \e -> case fromException e of
    Just x -> throw e `const` (x `asTypeOf` ExitSuccess)
    _ -> runX c st errcase
  put s'
  return a

wrap   :: String -> String
wrap s =  ' ' : '"' : wrap' s ++ "\""
  where
    wrap' (s':ss) | s' == '"'  = '\\' : s' : wrap' ss
                  | s' == '\\' = '\\' : s' : wrap' ss
                  | otherwise  =        s' : wrap' ss
    wrap' ""                   =             ""

-- Graphics.X11.Extras.getWindowAttributes is bugggggggy
safeGetWindowAttributes     :: Display -> Window -> IO (Maybe WindowAttributes)
safeGetWindowAttributes d w =  alloca $ \p -> do
  s <- xGetWindowAttributes d w p
  case s of
    0 -> return Nothing
    _ -> Just `fmap` peek p
