-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.DebugWindow
-- Description :  Dump window information for diagnostic\/debugging purposes.
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2014
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module to dump window information for diagnostic/debugging purposes. See
-- "XMonad.Hooks.DebugEvents" and "XMonad.Hooks.DebugStack" for practical uses.
--
-----------------------------------------------------------------------------

module XMonad.Util.DebugWindow (debugWindow) where

import           Prelude

import           XMonad
import           XMonad.Prelude

import           Codec.Binary.UTF8.String        (decodeString)
import           Control.Exception                                     as E
import           Foreign.C.String
import           Numeric                         (showHex)
import           System.Exit

-- | Output a window by ID in hex, decimal, its ICCCM resource name and class,
--   its title if available, and EWMH type and state if available.  Also
--   indicate override_redirect with an exclamation mark, and wrap in brackets
--   if it is unmapped or withdrawn.
debugWindow   :: Window -> X String
debugWindow 0 =  return "-no window-"
debugWindow w =  do
  d <- asks display
  let wx = pad 8 '0' $ showHex w ""
  w' <- safeGetWindowAttributes w
  case w' of
    Nothing                                   ->
      return $ "(deleted window " ++ wx ++ ")"
    Just WindowAttributes
      { wa_x                 = x
      , wa_y                 = y
      , wa_width             = wid
      , wa_height            = ht
      , wa_border_width      = bw
      , wa_map_state         = m
      , wa_override_redirect = o
      } -> do
      c' <- io (getWindowProperty8 d wM_CLASS w)
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
      t <- catchX' (wrap <$> getEWMHTitle  "VISIBLE" w) $
           catchX' (wrap <$> getEWMHTitle  ""        w) $
           catchX' (wrap <$> getICCCMTitle           w) $
           return ""
      h' <- getMachine w
      let h = if null h' then "" else '@':h'
      -- if it has WM_COMMAND use it, else use the appName
      -- NB. modern stuff often does not set WM_COMMAND since it's only ICCCM required and not some
      -- horrible gnome/freedesktop session manager thing like Wayland intended. How helpful of them.
      p' <- safeGetCommand d w
      let p = if null p' then "" else wrap $ unwords p'
      nWP <- getAtom "_NET_WM_PID"
      pid' <- io $ getWindowProperty32 d nWP w
      let pid = case pid' of
                  Just [pid''] -> '(':show pid'' ++ ")"
                  _            -> ""
      let cmd = p ++ pid ++ h
      let (lb,rb) = case () of
                      () | m == waIsViewable -> ("","")
                         | otherwise         -> ("[","]")
          o'      = if o then "!" else ""
      wT <- getAtom "_NET_WM_WINDOW_TYPE"
      wt' <- io $ getWindowProperty32 d wT w
      ewmh <- case wt' of
                Just wt'' -> windowType d w (fmap fi wt'')
                _         -> return ""
      return $ concat [lb
                      ,o'
                      ,wx
                      ,t
                      ," "
                      ,show wid
                      ,'x':show ht
                      ,if bw == 0 then "" else '+':show bw
                      ,"@"
                      ,show x
                      ,',':show y
                      ,if null c then "" else ' ':c
                      ,if null cmd then "" else ' ':cmd
                      ,ewmh
                      ,rb
                      ]

getEWMHTitle       :: String -> Window -> X String
getEWMHTitle sub w =  do
  a <- getAtom $ "_NET_WM_" ++ (if null sub then "" else '_':sub) ++ "_NAME"
  getDecodedStringProp w a -- should always be UTF8_STRING but rules are made to be broken

getICCCMTitle   :: Window -> X String
getICCCMTitle w =  getDecodedStringProp w wM_NAME

getDecodedStringProp     :: Window -> Atom -> X String
getDecodedStringProp w a =  do
  t@(TextProperty t' _ 8 _) <- withDisplay $ \d -> io $ getTextProperty d w a
  [s] <- catchX' (tryUTF8     t) $ -- shouldn't happen but some apps do it
         catchX' (tryCompound t) $
         io ((:[]) <$> peekCString t')
  return s

tryUTF8                          :: TextProperty -> X [String]
tryUTF8 (TextProperty s enc _ _) =  do
  uTF8_STRING <- getAtom "UTF8_STRING"
  when (enc /= uTF8_STRING) $ error "String is not UTF8_STRING"
  map decodeString . splitNul <$> io (peekCString s)

tryCompound                            :: TextProperty -> X [String]
tryCompound t@(TextProperty _ enc _ _) =  do
  cOMPOUND_TEXT <- getAtom "COMPOUND_TEXT"
  when (enc /= cOMPOUND_TEXT) $ error "String is not COMPOUND_TEXT"
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
  (a, s') <- io $ runX c st job `E.catch` \e -> case fromException e of
    Just x -> throw e `const` (x `asTypeOf` ExitSuccess)
    _      -> runX c st errcase
  put s'
  return a

wrap   :: String -> String
wrap s =  ' ' : '"' : wrap' s ++ "\""
  where
    wrap' (s':ss) | s' == '"'  = '\\' : s' : wrap' ss
                  | s' == '\\' = '\\' : s' : wrap' ss
                  | otherwise  =        s' : wrap' ss
    wrap' ""                   =             ""

-- and so is getCommand
safeGetCommand     :: Display -> Window -> X [String]
safeGetCommand d w =  do
  wC <- getAtom "WM_COMMAND"
  p <- io $ getWindowProperty8 d wC w
  case p of
    Nothing  -> return []
    Just cs' -> do
      let cs                    = map (toEnum . fromEnum) cs'
          go  (a,(s,"\NUL"))    = (s:a,("",""))
          go  (a,(s,'\NUL':ss)) = go (s:a,go' ss)
          go  r                 = r -- ???
          go'                   = break (== '\NUL')
       in return $ reverse $ fst $ go ([],go' cs)

getMachine   :: Window -> X String
getMachine w =  catchX' (getAtom "WM_CLIENT_MACHINE" >>= getDecodedStringProp w) (return "")

-- if it's one EWMH atom then we strip prefix and lowercase, otherwise we
-- return the whole thing. we also get the state here, with similar rules
-- (all EWMH = all prefixes removed and lowercased)
windowType        :: Display -> Window -> [Atom] -> X String
windowType d w ts =  do
  tstr <- decodeType ts
  wS <- getAtom "_NET_WM_STATE"
  ss' <- io $ getWindowProperty32 d wS w
  sstr <- case ss' of
            Just ss -> windowState (fmap fi ss)
            _       -> return ""
  return $ " (" ++ tstr ++ sstr ++ ")"
  where
    decodeType     :: [Atom] -> X String
    decodeType []  =  return ""
    decodeType [t] =  simplify "_NET_WM_WINDOW_TYPE_" t
    decodeType tys =  unAtoms tys " (" False

    unAtoms             :: [Atom] -> String -> Bool -> X String
    unAtoms []     t i  =  return $ if i then t else t ++ ")"
    unAtoms (a:as) t i  =  do
                            s' <- io $ getAtomName d a
                            let s = case s' of
                                      Just s'' -> s''
                                      _        -> '<':show a ++ ">"
                            unAtoms as (t ++ (if i then ' ':s else s)) True
    
    simplify       :: String -> Atom -> X String
    simplify pfx a =  do
                        s' <- io $ getAtomName d a
                        case s' of
                          Nothing -> return $ '<':show a ++ ">"
                          Just s  -> if pfx `isPrefixOf` s then
                                       return $ map toLower (drop (length pfx) s)
                                     else
                                       return s

    -- note that above it says this checks all of them before simplifying.
    -- I'll do that after I'm confident this works as intended.    
    windowState     :: [Atom] -> X String
    windowState []  =  return ""
    windowState as' =  go as' ";"
      where
        go []     t = return t
        go (a:as) t = simplify "_NET_WM_STATE_" a >>= \t' -> go as (t ++ ' ':t') 
