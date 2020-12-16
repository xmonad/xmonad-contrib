{-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Hooks.DebugEvents
-- Copyright   :  (c) Brandon S Allbery KF8NH, 2012
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  allbery.b@gmail.com
-- Stability   :  unstable
-- Portability :  not portable
--
-- Module to dump diagnostic information about X11 events received by
-- @xmonad@.  This is incomplete due to 'Event' being incomplete and not
-- providing information about a number of events, and enforcing artificial
-- constraints on others (for example 'ClientMessage'); the @X11@ package
-- will require a number of changes to fix these problems.
--
-----------------------------------------------------------------------------

module XMonad.Hooks.DebugEvents (debugEventsHook) where

import           Prelude

import           XMonad                               hiding (windowEvent
                                                             ,(-->)
                                                             )

import           XMonad.Hooks.DebugKeyEvents                 (debugKeyEvents)
import           XMonad.Util.DebugWindow                     (debugWindow)

-- import           Graphics.X11.Xlib.Extras.GetAtomName        (getAtomName)

import           Control.Exception.Extensible         as E
import           Control.Monad.State
import           Control.Monad.Reader
import           Data.Char                                   (isDigit)
import           Data.Maybe                                  (fromJust)
import           Data.List                                   (genericIndex
                                                             ,genericLength
                                                             ,unfoldr
                                                             )
import           Codec.Binary.UTF8.String
import           Data.Maybe                                  (fromMaybe)
import           Data.Monoid
import           Foreign
import           Foreign.C.Types
import           Numeric                                     (showHex)
import           System.Exit
import           System.IO
import           System.Process

-- | Event hook to dump all received events.  You should probably not use this
--   unconditionally; it will produce massive amounts of output.
debugEventsHook   :: Event -> X All
debugEventsHook e =  debugEventsHook' e >> return (All True)

-- | Dump an X11 event.  Can't be used directly as a 'handleEventHook'.
debugEventsHook' :: Event -> X ()

debugEventsHook' (ConfigureRequestEvent {ev_window       = w
                                        ,ev_parent       = p
                                        ,ev_x            = x
                                        ,ev_y            = y
                                        ,ev_width        = wid
                                        ,ev_height       = ht
                                        ,ev_border_width = bw
                                        ,ev_above        = above
                                        ,ev_detail       = place
                                        ,ev_value_mask   = msk
                                        }) = do
  windowEvent "ConfigureRequest" w
  windowEvent "  parent"         p
--  mask <- quickFormat msk $ dumpBits wmCRMask
--  say "  requested parameters" $ concat ['(':show wid
--                                        ,'x':show ht
--                                        ,')':if bw == 0 then "" else '+':show bw
--                                        ,'@':'(':show x
--                                        ,',':show y
--                                        ,") mask "
--                                        ,mask
--                                        ]
  s <- quickFormat [x,y,wid,ht,bw,fromIntegral above,place] $
       dumpListByMask' msk [("x"           ,dump32              ,cARDINAL)
                           ,("y"           ,dump32              ,cARDINAL)
                           ,("width"       ,dump32              ,cARDINAL)
                           ,("height"      ,dump32              ,cARDINAL)
                           ,("border_width",dump32              ,cARDINAL)
                           ,("sibling"     ,dumpWindow          ,wINDOW  )
                           ,("detail"      ,dumpEnum wmPlacement,cARDINAL)
                           ]
  say "  requested" s

debugEventsHook' (ConfigureEvent        {ev_window = w
                                        ,ev_above  = above
                                        }) = do
  windowEvent "Configure" w
  -- most of the content is covered by debugWindow
  when (above /= none) $ debugWindow above >>= say "  above"

debugEventsHook' (MapRequestEvent       {ev_window     = w
                                        ,ev_parent     = p
                                        }) =
  windowEvent "MapRequest" w >>
  windowEvent "  parent"   p

debugEventsHook' e@(KeyEvent {ev_event_type = t})
    | t == keyPress =
  io (hPutStr stderr "KeyPress ") >>
  debugKeyEvents e >>
  return ()

debugEventsHook' (ButtonEvent           {ev_window = w
                                        ,ev_state  = s
                                        ,ev_button = b
                                        }) = do
  windowEvent "Button" w
  nl <- gets numberlockMask
  let msk | s == 0    = ""
          | otherwise = "modifiers " ++ vmask nl s
  say "  button" $ show b ++ msk

debugEventsHook' (DestroyWindowEvent    {ev_window = w
                                        }) =
  windowEvent "DestroyWindow" w

debugEventsHook' (UnmapEvent            {ev_window = w
                                        }) =
  windowEvent "Unmap" w

debugEventsHook' (MapNotifyEvent        {ev_window = w
                                        }) =
  windowEvent "MapNotify" w

{- way too much output; suppressed.

debugEventsHook' (CrossingEvent         {ev_window    = w
                                        ,ev_subwindow = s
                                        }) =
  windowEvent "Crossing"    w >>
  windowEvent "  subwindow" s
-}
debugEventsHook' (CrossingEvent         {}) =
  return ()

debugEventsHook' (SelectionRequest      {ev_requestor = rw
                                        ,ev_owner     = ow
                                        ,ev_selection = a
                                        }) =
  windowEvent "SelectionRequest" rw >>
  windowEvent "  owner"          ow >>
  atomEvent   "  atom"           a

debugEventsHook' (PropertyEvent         {ev_window    = w
                                        ,ev_atom      = a
                                        ,ev_propstate = s
                                        }) = do
  a' <- atomName a
  -- too many of these, and they're not real useful
  if a' `elem` ["_NET_WM_USER_TIME"
--               ,"_NET_WM_WINDOW_OPACITY"
               ] then return () else do
  windowEvent "Property on" w
  s' <- case s of
          1 -> return "deleted"
          0 -> dumpProperty a a' w (7 + length a')
          _ -> error "Illegal propState; Xlib corrupted?"
  say "  atom" $ a' ++ s'

debugEventsHook' (ExposeEvent           {ev_window = w
                                        }) =
  windowEvent "Expose" w

debugEventsHook' (ClientMessageEvent    {ev_window       = w
                                        ,ev_message_type = a
                                        -- @@@ they did it again!  no ev_format,
                                        --     and ev_data is [CInt]
                                        -- @@@ and get a load of the trainwreck
                                        --     that is setClientMessageEvent!
--                                        ,ev_format       = b
                                        ,ev_data         = vs'
                                        }) = do
  windowEvent "ClientMessage on" w
  n <- atomName a
  -- this is a sort of custom property
  -- @@@ this likely won't work as is; type information varies, I think
  (ta,b,l) <- case lookup n clientMessages of
                Nothing        -> return (a,32,length vs')
                Just (ta',b,l) -> do
                  ta <- getAtom ta'
                  return (ta,b,l)
  let wl = bytes b
  vs <- io $ take (l * wl) <$> splitCInt vs'
  s <- dumpProperty' w a n ta b vs 0 (10 + length n)
  say "  message" $ n ++ s

debugEventsHook' _                      = return ()

-- | Emit information about an atom.
atomName   :: Atom -> X String
atomName a =  withDisplay $ \d ->
  io $ fromMaybe ("(unknown atom " ++ show a ++ ")") <$> getAtomName d a

-- | Emit an atom with respect to the current event.
atomEvent     :: String -> Atom -> X ()
atomEvent l a =  atomName a >>= say l

-- | Emit a window with respect to the current event.
windowEvent     :: String -> Window -> X ()
windowEvent l w =  debugWindow w >>= say l

-- | Helper to emit tagged event information.
say     :: String -> String -> X ()
say l s =  trace $ l ++ ' ':s

-- | Deconstuct a list of 'CInt's into raw bytes
splitCInt    :: [CInt] -> IO Raw
splitCInt vs =  io $ withArray vs $ \p ->
                peekArray (4 * length vs) (castPtr p :: Ptr CUChar)

-- | Specify how to decode some common client messages.
clientMessages :: [(String,(String,Int,Int))]
clientMessages =  [("_NET_ACTIVE_WINDOW",("_NET_ACTIVE_WINDOW",32,1))
                  ,("WM_CHANGE_STATE"   ,("WM_STATE"          ,32,2))
                  ,("WM_COMMAND"        ,("STRING"            , 8,0))
                  ,("WM_SAVE_YOURSELF"  ,("STRING"            , 8,0))
                  ]

#if __GLASGOW_HASKELL__ < 707
finiteBitSize :: Bits a => a -> Int
finiteBitSize x = bitSize x
#endif


-- | Convert a modifier mask into a useful string
vmask                 :: KeyMask -> KeyMask -> String
vmask numLockMask msk =  unwords $
                         reverse $
                         fst     $
                         foldr vmask' ([],msk) masks
    where
      masks = map (\m -> (m,show m)) [0..toEnum (finiteBitSize msk - 1)] ++
              [(numLockMask,"num"  )
              ,(   lockMask,"lock" )
              ,(controlMask,"ctrl" )
              ,(  shiftMask,"shift")
              ,(   mod5Mask,"mod5" )
              ,(   mod4Mask,"mod4" )
              ,(   mod3Mask,"mod3" )
              ,(   mod2Mask,"mod2" )
              ,(   mod1Mask,"mod1" )
              ]
      vmask'   _   a@( _,0)                = a
      vmask' (m,s)   (ss,v) | v .&. m == m = (s : ss,v .&. complement m)
      vmask'   _        r                  = r

-- formatting properties.  ick. --

-- @@@ Document the parser.  Someday.

type Raw     = [CUChar]

data Decode = Decode {property :: Atom          -- original property atom
                     ,pName    :: String        -- its name
                     ,pType    :: Atom          -- base property type atom
                     ,width    :: Int           -- declared data width
                     ,window   :: Window        -- source window
                     ,indent   :: Int           -- current indent (via local)
                     ,limit    :: Int           -- line length
                     }

-- the result accumulates here mainly for the benefit of the indenter
data DecodeState = DecS {value :: Raw           -- unconsumed raw property value
                        ,accum :: String        -- output accumulator
                        ,joint :: String        -- separator when adding to accumulator
                        }

newtype Decoder a = Decoder (ReaderT Decode (StateT DecodeState X) a)
#ifndef __HADDOCK__
    deriving (Functor
             ,Applicative
             ,Monad
             ,MonadIO
             ,MonadState  DecodeState
             ,MonadReader Decode
             )
#endif

-- | Retrive, parse, and dump a window property.  As all the high-level property
--   interfaces lose information necessary to decode properties correctly, we 
--   work at the lowest level available.
dumpProperty          :: Atom -> String -> Window -> Int -> X String
dumpProperty a n w i  =  do
  prop <- withDisplay $ \d ->
    io     $
    alloca $ \fmtp ->
    alloca $ \szp  ->
    alloca $ \lenp ->
    alloca $ \ackp ->
    alloca $ \vsp  -> do
    rc   <- xGetWindowProperty
              d
              w
              a
              0
              maxBound
              False
              anyPropertyType
              fmtp
              szp
              lenp
              ackp
              vsp
    case rc of
      0 -> do
        fmt <- fromIntegral <$> peek fmtp
        vs' <-                     peek vsp
        sz  <- fromIntegral <$> peek szp
        case () of
          () | fmt == none     -> xFree vs' >> return (Left   "(property deleted)"   )
             | sz < 0          -> xFree vs' >> return (Left $ "(illegal bit size " ++
                                                              show sz              ++
                                                              ")"                    )
             | sz `mod` 8 /= 0 -> xFree vs' >> return (Left $ "(illegal bit size " ++
                                                              show sz              ++
                                                              ")"                    )
             | otherwise       -> do
                 len <- fromIntegral <$> peek lenp
                 -- that's as in "ack! it's fugged!"
                 ack <- fromIntegral <$> peek ackp
                 vs <- peekArray (len * bytes sz) vs'
                 _ <- xFree vs'
                 return $ Right (fmt,sz,ack,vs)
      e -> return $ Left $ "getWindowProperty failed: " ++ show e
  case prop of
    Left  _               -> return ""
    Right (fmt,sz,ack,vs) -> dumpProperty' w a n fmt sz vs ack i

-- @@@ am I better off passing in the Decode and DecodeState?
-- | Parse and dump a property (or a 'ClientMessage').
dumpProperty'                             :: Window -- source window
                                          -> Atom   -- property id
                                          -> String -- property name
                                          -> Atom   -- property type
                                          -> Int    -- bit width
                                          -> Raw    -- raw value
                                          -> CULong -- size of un-dumped content
                                          -> Int    -- indent for output formatting
                                          -> X String
dumpProperty' w a n fmt sz vs ack i =  do
  ptn <- atomName fmt
  let dec  = Decode {property = a
                    ,pName    = n
                    ,pType    = fmt
                    ,width    = sz
                    ,indent   = i + length ptn + 6
                    ,window   = w
                    ,limit    = 96
                    }
      dec' = dec    {pType    = cARDINAL
                    ,width    = 8
                    }
      ds   = DecS   {value    = vs
                    -- @@@ probably should push this outside, since it doesn't
                    --     make sense for ClientMessage
                    ,accum    = " (" ++ ptn ++ ") "
                    ,joint    = "= "
                    }
  (_,ds') <- runDecode dec ds $ dumpProp a n
  let fin = length (value ds')
      len = length vs
      lost = if ack == 0 then "" else "and " ++ show ack ++ " lost bytes"
      unk = case () of
              () | fin == len -> "undecodeable "
                 | fin == 0   -> "."
                 | otherwise  -> "and remainder (" ++ show (len - fin) ++ '/':show len ++ ")"
  (_,ds'') <- if fin == 0
              then return (True,ds')
              else runDecode dec' (withJoint' unk ds' ) $ dumpArray dump8
  (_,ds''') <- if ack == 0
               then return (True,ds'')
               else runDecode dec' (withJoint' " " ds'') $ propSimple lost -- @@@
  return $ accum ds'''

-- | A simplified version of 'dumpProperty\'', to format random values from
--   events.
quickFormat     :: (Storable i, Integral i) => [i] -> Decoder Bool -> X String
quickFormat v f =  do
  let vl = length v
  vs <- io $
        allocaArray vl $
        \p -> pokeArray p (map fromIntegral v :: [CULong]) >>
              peekArray (4 * vl) (castPtr p :: Ptr CUChar)
  let dec = Decode {property = none
                   ,pName    = ""
                   ,pType    = cARDINAL
                   ,width    = 32
                   ,indent   = 0
                   ,window   = none
                   ,limit    = maxBound
                   }
      ds  = DecS   {value    = vs
                   ,accum    = ""
                   ,joint    = ""
                   }
  (r,ds') <- runDecode dec ds f
  return $ accum ds' ++ if r then "" else "?"

-- | Launch a decoding parser, returning success and final state.
runDecode                 :: Decode -> DecodeState -> Decoder Bool -> X (Bool,DecodeState)
runDecode c s (Decoder p) =  runStateT (runReaderT p c) s

-- Coerce bit size to bytes.
bytes   :: Int -> Int
bytes w =  w `div` 8

-- | The top level property decoder, for a wide variety of standard ICCCM and 
--   EWMH window properties.  We pass part of the 'ReaderT' as arguments for 
--   pattern matching.
dumpProp                                              :: Atom -> String -> Decoder Bool

dumpProp _ "CLIPBOARD"                                =  dumpSelection
dumpProp _ "_NET_SUPPORTED"                           =  dumpArray dumpAtom
dumpProp _ "_NET_CLIENT_LIST"                         =  dumpArray dumpWindow
dumpProp _ "_NET_CLIENT_LIST_STACKING"                =  dumpArray dumpWindow
dumpProp _ "_NET_NUMBER_OF_DESKTOPS"                  =  dump32
dumpProp _ "_NET_VIRTUAL_ROOTS"                       =  dumpArray dumpWindow
dumpProp _ "_NET_DESKTOP_GEOMETRY"                    =  dumpArray dump32
dumpProp _ "_NET_DESKTOP_VIEWPORT"                    =  dumpList [("w",dump32)
                                                                  ,("h",dump32)
                                                                  ]
dumpProp _ "_NET_CURRENT_DESKTOP"                     =  dump32
dumpProp _ "_NET_DESKTOP_NAMES"                       =  dumpArray dumpUTF
dumpProp _ "_NET_ACTIVE_WINDOW"                       =  dumpActiveWindow
dumpProp _ "_NET_WORKAREA"                            =  dumpList [("start"
                                                                   ,dumpList [("x",dump32)
                                                                             ,("y",dump32)
                                                                             ]
                                                                   )
                                                                  ,("size"
                                                                   ,dumpList [("w",dump32)
                                                                             ,("h",dump32)
                                                                             ]
                                                                   )
                                                                  ]
dumpProp _ "_NET_SUPPORTING_WM_CHECK"                 =  dumpWindow
dumpProp _ "_NET_DESKTOP_LAYOUT"                      =  dumpList [("orientation"
                                                                   ,dumpEnum nwmOrientation
                                                                   )
                                                                  ,("size"
                                                                   ,dumpList [("cols",dump32)
                                                                             ,("rows",dump32)
                                                                             ]
                                                                   )
                                                                  ,("origin"
                                                                   ,dumpEnum nwmOrigin
                                                                   )
                                                                  ]
dumpProp _ "_NET_SHOWING_DESKTOP"                     =  dump32
dumpProp _ "_NET_WM_NAME"                             =  dumpUTF
dumpProp _ "_NET_WM_VISIBLE_NAME"                     =  dumpUTF
dumpProp _ "_NET_WM_ICON_NAME"                        =  dumpUTF
dumpProp _ "_NET_WM_VISIBLE_ICON_NAME"                =  dumpUTF
dumpProp _ "_NET_WM_DESKTOP"                          =  dumpExcept [(0xFFFFFFFF,"all")]
                                                                    dump32
dumpProp _ "_NET_WM_WINDOW_TYPE"                      =  dumpArray dumpAtom
dumpProp _ "_NET_WM_STATE"                            =  dumpArray dumpAtom
dumpProp _ "_NET_WM_ALLOWED_ACTIONS"                  =  dumpArray dumpAtom
dumpProp _ "_NET_WM_STRUT"                            =  dumpList [("left gap"  ,dump32)
                                                                  ,("right gap" ,dump32)
                                                                  ,("top gap"   ,dump32)
                                                                  ,("bottom gap",dump32)
                                                                  ]
dumpProp _ "_NET_WM_STRUT_PARTIAL"                    =  dumpList [("left gap"    ,dump32)
                                                                  ,("right gap"   ,dump32)
                                                                  ,("top gap"     ,dump32)
                                                                  ,("bottom gap"  ,dump32)
                                                                  ,("left start"  ,dump32)
                                                                  ,("left end"    ,dump32)
                                                                  ,("right start" ,dump32)
                                                                  ,("right end"   ,dump32)
                                                                  ,("top start"   ,dump32)
                                                                  ,("top end"     ,dump32)
                                                                  ,("bottom start",dump32)
                                                                  ,("bottom end"  ,dump32)
                                                                  ]
dumpProp _ "_NET_WM_ICON_GEOMETRY"                    =  dumpList [("x",dump32)
                                                                  ,("y",dump32)
                                                                  ,("w",dump32)
                                                                  ,("h",dump32)
                                                                  ]
-- no, I'm not going to duplicate xprop *completely*!
dumpProp _ "_NET_WM_ICON"                             =  propSimple "(icon)"
dumpProp _ "_NET_WM_PID"                              =  dumpPid
dumpProp _ "_NET_WM_HANDLED_ICONS"                    =  propSimple "(defined)"
dumpProp _ "_NET_WM_USER_TIME"                        =  dumpExcept [(0,"do not map initially")]
                                                                    dumpTime
dumpProp _ "_NET_FRAME_EXTENTS"                       =  dumpList [("left"  ,dump32)
                                                                  ,("right" ,dump32)
                                                                  ,("top"   ,dump32)
                                                                  ,("bottom",dump32)
                                                                  ]
dumpProp _ "_NET_WM_SYNC_REQUEST_COUNTER"             =  dumpExcept [(0,"illegal value 0")]
                                                                    dump64
dumpProp _ "_NET_STARTUP_ID"                          =  dumpUTF
dumpProp _ "WM_PROTOCOLS"                             =  dumpArray dumpAtom
dumpProp _ "WM_COLORMAP_WINDOWS"                      =  dumpArray dumpWindow
dumpProp _ "WM_STATE"                                 =  dumpState
dumpProp _ "WM_LOCALE_NAME"                           =  dumpString
dumpProp _ "WM_CLIENT_LEADER"                         =  dumpWindow
dumpProp _ "_NET_WM_WINDOW_OPACITY"                   =  dumpPercent
dumpProp _ "XdndAware"                                =  dumpArray dumpAtom
dumpProp _ "_XKLAVIER_TRANSPARENT"                    =  dumpInteger 32
dumpProp _ "_XKLAVIER_STATE"                          =  dumpList [("state"     ,dumpInteger 32)
                                                                  ,("indicators",dumpXKlInds)
                                                                  ]
dumpProp _ "_MOTIF_DRAG_RECEIVER_INFO"                =  dumpMotifDragReceiver
dumpProp _ "_OL_WIN_ATTR"                             =  dumpOLAttrs
dumpProp _ "_OL_DECOR_ADD"                            =  dumpArray dumpAtom
dumpProp _ "_OL_DECOR_DEL"                            =  dumpArray dumpAtom
dumpProp _ "_MOTIF_WM_HINTS"                          =  dumpMwmHints
dumpProp _ "_MOTIF_WM_INFO"                           =  dumpMwmInfo
dumpProp _ "_XMONAD_DECORATED_BY"                     =  dumpWindow
dumpProp _ "_XMONAD_DECORATION_FOR"                   =  dumpWindow
dumpProp a _ | a == wM_NAME                           =  dumpString
             | a == pRIMARY                           =  dumpSelection
             | a == sECONDARY                         =  dumpSelection
               -- this is gross
             | a == wM_TRANSIENT_FOR                  =  do
                 root <- fromIntegral <$> inX (asks theRoot)
                 w <- asks window
                 WMHints {wmh_window_group = group} <-
                   inX $ asks display >>= io . flip getWMHints w
                 dumpExcept [(0   ,"window group " ++ show group)
                            ,(root,"window group " ++ show group)
                            ]
                            dumpWindow
             | a == rESOURCE_MANAGER                  =  dumpString
             | a == wM_COMMAND                        =  dumpString
             | a == wM_HINTS                          =  dumpWmHints
             | a == wM_CLIENT_MACHINE                 =  dumpString
             | a == wM_ICON_NAME                      =  dumpString
             | a == wM_ICON_SIZE                      =  dumpList [("min size"
                                                                   ,dumpList [("w",dump32)
                                                                             ,("h",dump32)
                                                                             ]
                                                                   )
                                                                  ,("max size"
                                                                   ,dumpList [("w",dump32)
                                                                             ,("h",dump32)
                                                                             ]
                                                                   )
                                                                  ,("increment"
                                                                   ,dumpList [("w",dump32)
                                                                             ,("h",dump32)
                                                                             ]
                                                                   )
                                                                  ]
             | a == wM_NORMAL_HINTS                   =  (...)
             | a == wM_ZOOM_HINTS                     =  (...) -- same as previous
             | a == rGB_DEFAULT_MAP                   =  (...) -- XStandardColormap
             | a == rGB_BEST_MAP                      =  (...) -- "
             | a == rGB_RED_MAP                       =  (...) -- "
             | a == rGB_GREEN_MAP                     =  (...) -- "
             | a == rGB_BLUE_MAP                      =  (...) -- "
             | a == rGB_GRAY_MAP                      =  (...) -- "
             | a == wM_CLASS                          =  dumpList [("name" ,dumpString)
                                                                  ,("class",dumpString)
                                                                  ]
dumpProp _ s | s `isCountOf` "WM_S"                   =  dumpSelection
             | s `isCountOf` "_NET_WM_CM_S"           =  dumpSelection
             | s `isCountOf` "_NET_DESKTOP_LAYOUT_S"  =  dumpSelection
             | s `isCountOf` "CUT_BUFFER"             =  dumpString
             -- and dumpProperties does the rest
             | otherwise                              =  return False

-- lower level decoders --

-- alter the current joint
withJoint   :: String -> Decoder a -> Decoder a
withJoint j =  ((modify $ withJoint' j) >>)

withJoint'     :: String -> DecodeState -> DecodeState
withJoint' j s =  s {joint = j}

-- lift an X into a Decoder
inX :: X a -> Decoder a
inX =  Decoder . lift . lift

-- flip isPrefixOf, but the remainder must be all digits
isCountOf         :: String -> String -> Bool
-- note that \NUL is safe because atom names have to be C strings
s `isCountOf` pfx =  null                     $
                     dropWhile isDigit        $
                     map fst                  $
                     dropWhile (uncurry (==)) $
                     zip s                    $
                     pfx ++ repeat '\NUL'

-- localize an increased indent
withIndent   :: Int -> Decoder a -> Decoder a
withIndent w =  local (\r -> r {indent = indent r + w})

-- dump an array of items.  this dumps the entire property
dumpArray      :: Decoder Bool -> Decoder Bool
dumpArray item =  do
  withIndent 1 $ append "[" >> withJoint "" (dumpArray' item "")

-- step through values as an array, ending on parse error or end of list
dumpArray'          :: Decoder Bool -> String -> Decoder Bool
dumpArray' item pfx =  do
  vs <- gets value
  if vs == []
    then append "]"
    else append pfx >> whenD item (dumpArray' item ",")

-- keep parsing until a parse step fails
-- @@@ which points out that all my uses of @whenX (return ...)@ are actually 'when',
--     which suggests that 'whenX' is *also* the same function... yep.  ISAGN
whenD     :: Monad m => m Bool -> m Bool -> m Bool
whenD p f =  p >>= \b -> if b then f else return False

-- verify a decoder parameter, else call error reporter
-- once again, it's more general than I originally wrote
guardR                  :: (MonadReader r m, Eq v)
                        => (r -> v)                -- value selector
                        -> v                       -- expected value
                        -> (v -> v -> m a)         -- error reporter
                        -> m a                     -- continuation (hush)
                        -> m a
guardR sel val err good =  do
  v <- asks sel
  if v == val then good else err v val

-- this is kinda dumb
fi       :: Bool -> a -> a -> a
fi p n y =  if p then y else n -- flip (if' p), if that existed

-- verify we have the expected word size
guardSize      :: Int -> Decoder Bool -> Decoder Bool
-- see XSync documentation for this insanity
guardSize 64 =  guardR width 32 propSizeErr . guardSize' 8         propShortErr
guardSize  w =  guardR width  w propSizeErr . guardSize' (bytes w) propShortErr

guardSize'       :: Int -> Decoder a -> Decoder a -> Decoder a
guardSize' l n y =  gets value >>= \vs -> fi (length vs >= l) n y

-- verify we have the expected property type
guardType    :: Atom -> Decoder Bool -> Decoder Bool
guardType  t =  guardR pType t propTypeErr

-- dump a structure as a named tuple
dumpList       :: [(String,Decoder Bool)] -> Decoder Bool
dumpList proto =  do
  a <- asks pType
  dumpList'' (maxBound :: CULong) (map (\(s,d) -> (s,d,a)) proto) "("

-- same but elements have their own distinct types
dumpList'       :: [(String,Decoder Bool,Atom)] -> Decoder Bool
dumpList' proto =  dumpList'' (maxBound :: CULong) proto "("

-- same but only dump elements identified by provided mask
dumpListByMask     :: CULong -> [(String,Decoder Bool)] -> Decoder Bool
dumpListByMask m p =  do
  a <- asks pType
  dumpList'' m (map (\(s,d) -> (s,d,a)) p) "("

-- and the previous two combined
dumpListByMask'     :: CULong -> [(String,Decoder Bool,Atom)] -> Decoder Bool
dumpListByMask' m p =  dumpList'' m p "("

dumpList''                    :: CULong -> [(String,Decoder Bool,Atom)] -> String -> Decoder Bool
dumpList'' _ []           _   =  append ")" >> return True
dumpList'' 0 _            _   =  append ")" >> return True
dumpList'' m ((l,p,t):ps) sep = do
  (e,sep') <- if m .&. 1 == 0
              then do
                -- @@@ ew
                st <- get
                e <- local (\r -> r {pType = t}) p
                v' <- gets value
                put $ st {value = v'}
                return (e,sep)
              else do
                let label = sep ++ l ++ " = "
                append label
                e <- withJoint "" $ do
                       local (\r -> r {pType  = t
                                      ,indent = indent r + length label
                                      })
                             p
                return (e,",")
  if e then dumpList'' (m `shiftR` 1) ps sep' else return e

-- do the getTextProperty dance, the hard way.
-- @@@ @COMPOUND_TEXT@ not supported yet.
dumpString :: Decoder Bool
dumpString =  do
  fmt <- asks pType
  x <- inX $ mapM getAtom ["COMPOUND_TEXT","UTF8_STRING"]
  case x of
    [cOMPOUND_TEXT,uTF8_STRING] -> case () of
      () | fmt == cOMPOUND_TEXT -> guardSize 16 (...)
         | fmt == sTRING        -> guardSize  8 $ do
                                     vs <- gets value
                                     modify (\r -> r {value = []})
                                     let ss = flip unfoldr (map twiddle vs) $
                                              \s -> if null s
                                                    then Nothing
                                                    else let (w,s'') = break (== '\NUL') s
                                                             s'      = if null s''
                                                                       then s''
                                                                       else tail s''
                                                          in Just (w,s')
                                     case ss of
                                       [s] -> append $ show s
                                       ss' -> let go (s:ss'') c = append c        >>
                                                                  append (show s) >>
                                                                  go ss'' ","
                                                  go []       _ = append "]"
                                               in append "[" >> go ss' ""
         | fmt == uTF8_STRING   -> dumpUTF -- duplicate type test instead of code :)
         | otherwise            -> (inX $ atomName fmt) >>=
                                   failure . ("unrecognized string type " ++)

-- show who owns a selection
dumpSelection :: Decoder Bool
dumpSelection =  do
  -- system selections contain a window ID; others are random
  -- note that the window ID will be the same as the owner, so
  -- we don't really care anyway.  we *do* want the selection owner
  a <- asks property
  owner <- inX $ withDisplay $ \d -> io $ xGetSelectionOwner d a
  if owner == none
    then append "unowned"
    else do
      w <- inX $ debugWindow owner
      append $ "owned by " ++ w

-- for now, not querying Xkb
dumpXKlInds :: Decoder Bool
dumpXKlInds =  guardType iNTEGER $ do
                 n <- fmap fromIntegral <$> getInt' 32
                 case n of
                   Nothing -> propShortErr
                   Just is -> append $ "indicators " ++ unwords (dumpInds is 1 1 [])
  where
    dumpInds                               :: Word32 -> Word32 -> Int -> [String] -> [String]
    dumpInds n bt c bs | n == 0 && c == 1 =  ["none"]
                       | n == 0           =  bs
                       | n .&. bt /= 0    =  dumpInds (n .&. complement bt)
                                                      (bt `shiftL` 1)
                                                      (c + 1)
                                                      ((show c):bs)
                       | otherwise        =  dumpInds n
                                                      (bt `shiftL` 1)
                                                      (c + 1)
                                                      bs

-- decode an Atom
dumpAtom :: Decoder Bool
dumpAtom =
  guardType aTOM $ do
  a <- getInt' 32
  case a of
    Nothing -> return False
    Just a' -> do
           an <- inX $ atomName $ fromIntegral a'
           append an

dumpWindow :: Decoder Bool
dumpWindow =  guardSize 32 $ guardType wINDOW $ do
                w <- getInt' 32
                case w of
                  Nothing -> return False
                  Just w' -> inX (debugWindow (fromIntegral w')) >>= append

-- a bit of a hack; as a Property it's a wINDOW, as a ClientMessage it's a list
dumpActiveWindow :: Decoder Bool
dumpActiveWindow =  guardSize 32 $ do
                      t <- asks pType
                      nAW <- inX $ getAtom "_NET_ACTIVE_WINDOW"
                      case () of
                        () | t == wINDOW -> dumpWindow
                           | t == nAW    -> dumpList' [("source"       ,dumpEnum awSource,cARDINAL)
                                                      ,("timestamp"    ,dumpTime         ,cARDINAL)
                                                      ,("active window",dumpWindow       ,wINDOW  )
                                                      ]
                        _                -> do
                                     t' <- inX $ atomName t
                                     failure $ concat ["(bad type "
                                                      ,t'
                                                      ,"; expected WINDOW or _NET_ACTIVE_WINDOW"
                                                      ]
-- dump a generic CARDINAL value
dumpInt   :: Int -> Decoder Bool
dumpInt w =  guardSize w $ guardType cARDINAL $ getInt w show

-- INTEGER is the signed version of CARDINAL
dumpInteger   :: Int -> Decoder Bool
dumpInteger w =  guardSize w $ guardType iNTEGER $ getInt w (show . signed w)

-- reinterpret an unsigned as a signed
signed     :: Int -> Integer -> Integer
signed w i =  bit (w + 1) - i

-- and wrappers to keep the parse list in bounds
dump64 :: Decoder Bool
dump64 =  dumpInt 64

dump32 :: Decoder Bool
dump32 =  dumpInt 32

{- not used in standard properties
dump16 :: Decoder Bool
dump16 =  dumpInt 16
-}

dump8 :: Decoder Bool
dump8 =  dumpInt 8

-- I am assuming for the moment that this is a single string.
-- This might be false; consider the way the STRING properties
-- handle lists.
dumpUTF :: Decoder Bool
dumpUTF =  do
  uTF8_STRING <- inX $ getAtom "UTF8_STRING"
  guardType uTF8_STRING $ guardSize 8 $ do
  s <- gets value
  modify (\r -> r {value = []})
  append . show . decode . map fromIntegral $ s
  return True

-- dump an enumerated value using a translation table
dumpEnum'        :: [String] -> Atom -> Decoder Bool
dumpEnum' ss fmt =  guardType fmt $
                    getInt 32     $
                    \r -> case () of
                            () | r < 0                 -> "undefined value " ++ show r
                               | r >= genericLength ss -> "undefined value " ++ show r
                               | otherwise             -> genericIndex ss r

-- we do not, unlike @xev@, try to ascii-art pixmaps.
dumpPixmap :: Decoder Bool
dumpPixmap =  guardType pIXMAP $ do
                p' <- getInt' 32
                case p' of
                  Nothing -> return False
                  Just p  -> do
                    append $ "pixmap " ++ showHex p ""
                    g' <- inX $ withDisplay $ \d -> io $
                            (Just <$> getGeometry d (fromIntegral p))
                            `E.catch`
                            \e -> case fromException e of
                                    Just x -> throw e `const` (x `asTypeOf` ExitSuccess)
                                    _      -> return Nothing
                    case g' of
                      Nothing                   -> append " (deleted)"
                      Just (_,x,y,wid,ht,bw,dp) ->
                          append $ concat
                                     [" ("
                                     ,show wid
                                     ,'x':show ht
                                     ,'x':show dp
                                     ,')':if bw == 0 then "" else '+':show bw
                                     ,"@("
                                     ,show x
                                     ,',':show y
                                     ,")"
                                     ]

dumpOLAttrs :: Decoder Bool
dumpOLAttrs = do
  pt <- inX $ getAtom "_OL_WIN_ATTR"
  guardType pt $ do
    msk <- getInt' 32
    case msk of
      Nothing   -> propShortErr
      Just msk' -> dumpListByMask (fromIntegral msk') [("window type" ,dumpAtom     )
                                                      ,("menu"        ,dump32       ) -- @@@ unk
                                                      ,("pushpin"     ,dumpEnum bool)
                                                      ,("limited menu",dump32       ) -- @@@ unk
                                                      ]

dumpMwmHints :: Decoder Bool
dumpMwmHints =  do
  ta <- asks property
  guardType ta $ do
    msk <- getInt' 32
    case msk of
      Nothing   -> propShortErr
      Just msk' -> dumpListByMask (fromIntegral msk') [("functions"  ,dumpBits mwmFuncs    )
                                                      ,("decorations",dumpBits mwmDecos    )
                                                      ,("input mode" ,dumpEnum mwmInputMode)
                                                      ,("status"     ,dumpBits mwmState    )
                                                      ]

dumpMwmInfo :: Decoder Bool
dumpMwmInfo =  do
  ta <- asks property
  guardType ta $ dumpList' [("flags" ,dumpBits mwmHints,cARDINAL)
                           ,("window",dumpWindow       ,wINDOW  )
                           ]
             
-- the most common case
dumpEnum    :: [String] -> Decoder Bool
dumpEnum ss =  dumpEnum' ss cARDINAL

-- implement exceptional cases atop a normal dumper
-- @@@ there's gotta be a better way
dumpExcept           :: [(Integer,String)] -> Decoder Bool -> Decoder Bool
dumpExcept xs item = do
  -- this horror brought to you by reparsing to get the right value for our use
  sp <- get
  rc <- item
  if not rc then return False else do
    that <- get -- if none match then we just restore the value parse
    vs <- gets value
    let w = (length (value sp) - length vs) * 8
    -- now we get to reparse again so we get our copy of it
    put sp
    v <- fmap fromJust (getInt' w)
    -- and after all that, we can process the exception list
    dumpExcept' xs that v

dumpExcept'                                      :: [(Integer,String)]
                                                 -> DecodeState
                                                 -> Integer
                                                 -> Decoder Bool
dumpExcept' []             that _                =  put that >> return True
dumpExcept' ((exc,str):xs) that val | exc == val =  append str
                                    | otherwise  =  dumpExcept' xs that val

-- use @ps@ to get process information.
-- @@@@ assumes a POSIX @ps@, not a BSDish one.
dumpPid :: Decoder Bool
dumpPid =  guardType cARDINAL $ do
             n <- getInt' 32
             case n of
               Nothing   -> return False
               Just pid' -> do
                      let pid = show pid'
                          ps  = (proc "/bin/ps" ["-fp" ++ pid]) {std_out = CreatePipe}
                      (_,o,_,_) <- io $ createProcess ps
                      case o of
                        Nothing -> append $ "pid " ++ pid
                        Just p' -> do
                                  prc <- io $ lines <$> hGetContents p'
                                  -- deliberately forcing it
                                  append $ if length prc < 2
                                           then "pid " ++ pid
                                           else prc !! 1

dumpTime :: Decoder Bool
dumpTime =  append "server event # " >> dump32

dumpState :: Decoder Bool
dumpState =  do
  wM_STATE <- inX $ getAtom "WM_STATE"
  guardType wM_STATE $ dumpList' [("state"      ,dumpEnum wmState,cARDINAL)
                                 ,("icon window",dumpWindow      ,wINDOW  )
                                 ]

dumpMotifDragReceiver :: Decoder Bool
dumpMotifDragReceiver =  do
  ta <- inX $ getAtom "_MOTIF_DRAG_RECEIVER_INFO"
  guardType ta $ dumpList' [("endian"    ,dumpMotifEndian,cARDINAL)
                           ,("version"   ,dump8          ,cARDINAL)
                           ,("style"     ,dumpMDropStyle ,cARDINAL) -- @@@ dummy
                           ]

dumpMDropStyle :: Decoder Bool
dumpMDropStyle =  do
  d <- getInt' 8
  pad 1 $ case d of
            Nothing             -> propShortErr
            Just ps | ps == 0   -> pad 12 $ append "none"
                    | ps == 1   -> pad 12 $ append "drop only"
                    | ps == 2   ->          append "prefer preregister " >> dumpMDPrereg
                    | ps == 3   ->          append "preregister "        >> dumpMDPrereg
                    | ps == 4   -> pad 12 $ append "prefer dynamic"
                    | ps == 5   -> pad 12 $ append "dynamic"
                    | ps == 6   -> pad 12 $ append "prefer receiver"
                    | otherwise -> failure $ "unknown drop style " ++ show ps

dumpMDPrereg :: Decoder Bool
dumpMDPrereg =  do
  -- this is a bit ugly; we pretend to be extending the above dumpList'
  append ","
  append "proxy window = "
  withIndent 15 dumpWindow
  append ","
  append "drop sites = "
  dsc' <- getInt' 16
  case dsc' of
    Nothing  -> propShortErr
    Just dsc -> do
      withIndent 13 $ append (show dsc)
      pad 2 $ do
        append ","
        append "total size = "
        withIndent 13 dump32
        dumpMDBlocks $ fromIntegral dsc
    
dumpMDBlocks   :: Int -> Decoder Bool
dumpMDBlocks _ =  propSimple "(drop site info)" -- @@@ maybe later if needed

dumpMotifEndian :: Decoder Bool
dumpMotifEndian =  guardType cARDINAL $ guardSize 8 $ do
  c <- map twiddle <$> eat 1
  case c of
    ['l'] -> append "little"
    ['B'] -> append "big"
    _     -> failure "bad endian flag"

pad     :: Int -> Decoder Bool -> Decoder Bool
pad n p =  do
  vs <- gets value
  if length vs < n
    then propShortErr
    else modify (\r -> r {value = drop n vs}) >> p

dumpPercent :: Decoder Bool
dumpPercent =  guardType cARDINAL $ do
                 n <- getInt' 32
                 case n of
                   Nothing -> return False
                   Just n' -> 
                       let pct = 100 * fromIntegral n' / fromIntegral (maxBound :: Word32)
                           pct :: Double
                        in append $ show (round pct :: Integer) ++ "%"

dumpWmHints :: Decoder Bool
dumpWmHints =
  guardType wM_HINTS $ do
  msk <- getInt' 32
  case msk of
    Nothing   -> return False
    Just msk' -> dumpListByMask' (fromIntegral msk')
                                 [("input"        ,dumpEnum bool   ,cARDINAL)
                                 ,("initial_state",dumpEnum wmState,cARDINAL)
                                 ,("icon_pixmap"  ,dumpPixmap      ,pIXMAP  )
                                 ,("icon_window"  ,dumpWindow      ,wINDOW  )
                                 ,("icon_x"       ,dump32          ,cARDINAL)
                                 ,("icon_y"       ,dump32          ,cARDINAL)
                                 ,("icon_mask"    ,dumpPixmap      ,pIXMAP  )
                                 ,("window_group" ,dumpWindow      ,wINDOW  )
                                 ]

dumpBits    :: [String] -> Decoder Bool
dumpBits bs =  guardType cARDINAL $ do
                 n <- getInt' 32
                 case n of
                   Nothing -> return False
                   Just n' -> dumpBits' bs 1 (fromIntegral n') ""

dumpBits' :: [String] -> Int -> Int -> String -> Decoder Bool
dumpBits' []     _ n p = if n == 0 then return True else append (p ++ show n)
dumpBits' (s:ss) b n p = do
  p' <- if n .&. b /= 0
        then append (p ++ s) >> return "|"
        else return p
  dumpBits' ss (b `shiftL` 1) (n .&. complement b) p'

-- enum definitions --

mwmFuncs :: [String]
mwmFuncs =  ["all except"
            ,"resize"
            ,"move"
            ,"minimize"
            ,"maximize"
            ,"close"
            ]

mwmDecos :: [String]
mwmDecos =  ["all except"
            ,"border"
            ,"resize handle"
            ,"title"
            ,"menu button"
            ,"maximize button"
            ,"minimize button"
            ]

mwmInputMode :: [String]
mwmInputMode =  ["modeless"
                ,"application modal"
                ,"system model"
                ,"full application modal"
                ]

mwmState :: [String]
mwmState =  ["tearoff window"
            ]

mwmHints :: [String]
mwmHints =  ["standard startup"
            ,"custom startup"
            ]

awSource :: [String]
awSource =  ["unspecified"
            ,"application"
            ,"pager/task list"
            ]

{- eventually...
wmHintsFlags :: [String]
wmHintsFlags =  ["Input"
                ,"State"
                ,"IconPixmap"
                ,"IconWindow"
                ,"IconX"
                ,"IconY"
                ,"IconMask"
                ,"WindowGroup"
                ]

wmCRMask :: [String]
wmCRMask =  ["X"
            ,"Y"
            ,"Width"
            ,"Height"
            ,"BorderWidth"
            ,"Sibling"
            ,"StackMode"
            ]
-}

wmPlacement :: [String]
wmPlacement =  ["Above"
               ,"Below"
               ,"TopIf"
               ,"BottomIf"
               ,"Opposite"
               ]

bool :: [String]
bool =  ["False","True"]

nwmOrientation :: [String]
nwmOrientation =  nwmEnum (Just "ORIENTATION") ["HORZ","VERT"]

nwmOrigin :: [String]
nwmOrigin =  nwmEnum Nothing ["TOPLEFT","TOPRIGHT","BOTTOMRIGHT","BOTTOMLEFT"]

wmState :: [String]
wmState =  ["Withdrawn","Normal","Zoomed (obsolete)","Iconified","Inactive"]

nwmEnum                  :: Maybe String
                         -> [String]
                         -> [String]
nwmEnum Nothing       vs =  map ( "_NET_WM_"                   ++) vs
nwmEnum (Just prefix) vs =  map (("_NET_WM_" ++ prefix ++ "_") ++) vs

-- and the lowest level coercions --

-- parse and return an integral value
getInt'    :: Int -> Decoder (Maybe Integer)
-- see XSync documentation for this insanity
getInt' 64 =  guardR width 32 (\a e -> propSizeErr a e >> return Nothing) $
              guardSize' 8 (propShortErr >> return Nothing) $ do
                lo <- inhale 32
                hi <- inhale 32
                return $ Just $ lo + hi * (fromIntegral (maxBound :: Word32) + 1)
getInt' w  =  guardR width w  (\a e -> propSizeErr a e >> return Nothing) $
              guardSize' (bytes w) (propShortErr >> return Nothing)       $
              Just <$> inhale w

-- parse an integral value and feed it to a show-er of some kind
getInt     :: Int -> (Integer -> String) -> Decoder Bool
getInt w f =  getInt' w >>= maybe (return False) (append . f)

-- bottommost level:  parse an integral value out of the stream.
-- Not much in the way of error checking; it is assumed you used
-- the appropriate guards.
-- @@@@@@@@@ evil beyond evil.  there *has* to be a better way
inhale    :: Int -> Decoder Integer
inhale  8 =  do
               x <- eat 1
               case x of
                 [b] -> return $ fromIntegral b
inhale 16 =  do
               x <- eat 2
               case x of
                 [b0,b1] -> io $ allocaArray 2 $ \p -> do
                              pokeArray p [b0,b1]
                              [v] <- peekArray 1 (castPtr p :: Ptr Word16)
                              return $ fromIntegral v
inhale 32 =  do
               x <- eat 4
               case x of
                 [b0,b1,b2,b3] -> io $ allocaArray 4 $ \p -> do
                                    pokeArray p [b0,b1,b2,b3]
                                    [v] <- peekArray 1 (castPtr p :: Ptr Word32)
                                    return $ fromIntegral v
inhale  b =  error $ "inhale " ++ show b

eat   :: Int -> Decoder Raw
eat n =  do
  (bs,rest) <- splitAt n <$> gets value
  modify (\r -> r {value = rest})
  return bs

-- actually do formatting type stuffs
-- sorta stubbed for the moment
-- eventually we should do indentation foo here
append :: String -> Decoder Bool
append =  append' True

-- and the same but for errors
failure :: String -> Decoder Bool
failure =  append' False

-- common appender
append'     :: Bool -> String -> Decoder Bool
append' b s =  do
  j <- gets joint
  modify (\r -> r {accum = accum r ++ j ++ s})
  return b

-- consume all and output a constant string
propSimple   :: String -> Decoder Bool
propSimple s =  modify (\r -> r {value = []}) >> append s

-- report various errors
propShortErr :: Decoder Bool
propShortErr =  failure "(property ended prematurely)"

propSizeErr     :: Int -> Int -> Decoder Bool
propSizeErr e a =  failure $ "(bad bit width " ++
                             show a            ++
                             "; expected "     ++
                             show e            ++
                             ")"

propTypeErr     :: Atom -> Atom -> Decoder Bool
propTypeErr a e =  do
  e' <- inX $ atomName e
  a' <- inX $ atomName a
  failure $ "(bad type " ++ a' ++"; expected " ++ e' ++ ")"

-- for stubs
(...) :: Decoder Bool
(...) =  do
  fmt <- asks pType >>= inX . atomName
  propSimple $ "(unimplemented type " ++ fmt ++ ")"

-- you like fi, I like this
twiddle :: (Enum a, Enum b) => a -> b
twiddle =  toEnum . fromEnum
