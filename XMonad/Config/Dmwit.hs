-- boilerplate {{{
{-# LANGUAGE ExistentialQuantification, NoMonomorphismRestriction, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults #-}
module XMonad.Config.Dmwit where

-- system imports
import Control.Monad
import Control.Monad.Trans
import Data.Char
import Data.List
import Data.Map (Map, fromList)
import Data.Ratio
import Data.Word
import GHC.Real
import System.Environment
import System.Exit
import System.IO
import System.Process

-- xmonad core
import XMonad
import XMonad.StackSet hiding (workspaces)

-- xmonad contrib
import XMonad.Actions.SpawnOn
import XMonad.Actions.Warp
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.Magnifier
import XMonad.Layout.NoBorders
import XMonad.Util.Dzen hiding (x, y)
import XMonad.Util.SpawnOnce
-- }}}
-- volume {{{
outputOf :: String -> IO String
outputOf s = do
    uninstallSignalHandlers
    (hIn, hOut, hErr, p) <- runInteractiveCommand s
    mapM_ hClose [hIn, hErr]
    hGetContents hOut <* waitForProcess p <* installSignalHandlers

geomMean :: Floating a => [a] -> a
geomMean xs = product xs ** (recip . fromIntegral . length $ xs)

arithMean :: Floating a => [a] -> a
arithMean xs = sum xs / fromIntegral (length xs)

namedNumbers n s = do
    l <- lines s
    guard (sentinel `isPrefixOf` l)
    return (drop (length sentinel) l)
    where sentinel = n ++ " #"

-- Data.List.Split.splitOn ":", but without involving an extra dependency
splitColon xs = case break (==':') xs of
    (a, ':':b) -> a : splitColon b
    (a, _)     -> [a]

parse s = arithMean $ do
    l <- lines s
    guard ("\tVolume: " `isPrefixOf` l)
    part <- splitColon l
    (n,'%':_) <- reads part
    return n

modVolume :: String -> Integer -> IO Double
modVolume kind n = do
    is <- namedNumbers parseKind <$> outputOf listCommand
    forM_ is (outputOf . setCommand)
    parse <$> outputOf listCommand
    where
    sign | n > 0 = "+" | otherwise = "-"
    ctlKind      = map (\c -> if c == ' ' then '-' else c) kind
    parseKind    = unwords . map (\(c:cs) -> toUpper c : cs) . words $ kind
    setCommand i = "pactl set-" ++ ctlKind ++ "-volume " ++ i ++ " -- " ++ sign ++ show (abs n) ++ "%"
    listCommand  = "pactl list " ++ ctlKind ++ "s"
-- }}}
-- convenient actions {{{
centerMouse = warpToWindow (1/2) (1/2)
statusBarMouse = warpToScreen 0 (5/1600) (5/1200)
withScreen s f = screenWorkspace s >>= flip whenJust (windows . f)

makeLauncher yargs run exec close = concat
    ["exe=`yeganesh ", yargs, "` && ", run, " ", exec, "$exe", close]
launcher     = makeLauncher "" "eval" "\"exec " "\""
termLauncher = makeLauncher "-p withterm" "exec urxvt -e" "" ""
viewShift  i = view i . shift i
floatAll     = composeAll . map (\s -> className =? s --> doFloat)
sinkFocus    = peek >>= maybe id sink
showMod  k n = liftIO (modVolume k n) >>= volumeDzen . show . round
volumeDzen   = dzenConfig $ onCurr (center 170 66) >=> font "-*-helvetica-*-r-*-*-64-*-*-*-*-*-*-*,-*-terminus-*-*-*-*-64-*-*-*-*-*-*-*"
-- }}}
altMask = mod1Mask
bright  = "#80c0ff"
dark    = "#13294e"
-- manage hooks for mplayer {{{
fullscreen43on169 = expand $ RationalRect 0 (-1/6) 1 (4/3) where
    expand (RationalRect x y w h) = RationalRect (x - bwx) (y - bwy) (w + 2 * bwx) (h + 2 * bwy)
    bwx = 2 / 1920 -- borderwidth
    bwy = 2 / 1080

fullscreenMPlayer = className =? "MPlayer" --> do
    dpy   <- liftX $ asks display
    win   <- ask
    hints <- liftIO $ getWMNormalHints dpy win
    case fmap (approx . fst) (sh_aspect hints) of
        Just ( 4 :% 3)  -> viewFullOn 0 "5" win
        Just (16 :% 9)  -> viewFullOn 1 "5" win
        _               -> doFloat
    where
    fi               = fromIntegral :: Dimension -> Double
    approx (n, d)    = approxRational (fi n / fi d) (1/100)

operationOn f s n w = do
    let ws = marshall s n
    currws <- liftX $ screenWorkspace s
    doF $ view ws . maybe id view currws . shiftWin ws w . f w

viewFullOn = operationOn sink
centerWineOn = operationOn (`XMonad.StackSet.float` RationalRect (79/960) (-1/540) (401/480) (271/270))
-- }}}
-- debugging {{{
class Show a => PPrint a where
    pprint :: Int -> a -> String
    pprint _ = show

data PPrintable = forall a. PPrint a => P a
instance Show   PPrintable where show     (P x) = show x
instance PPrint PPrintable where pprint n (P x) = pprint n x

record :: String -> Int -> [(String, PPrintable)] -> String
record s n xs = preamble ++ intercalate newline fields ++ postlude where
    indentation = '\n' : replicate n '\t'
    preamble    = s ++ " {" ++ indentation
    postlude    = indentation ++ "}"
    newline     = ',' : indentation
    fields      = map (\(name, value) -> name ++ " = " ++ pprint (n+1) value) xs

instance PPrint a => PPrint (Maybe a) where
    pprint n (Just x) = "Just (" ++ pprint n x ++ ")"
    pprint _ x        = show x

instance PPrint a => PPrint [a] where
    pprint _ [] = "[]"
    pprint n xs = preamble ++ intercalate newline allLines ++ postlude where
        indentation = '\n' : replicate n '\t'
        preamble    = "[" ++ indentation
        allLines    = map (pprint (n+1)) xs
        newline     = ',' : indentation
        postlude    = indentation ++ "]"

instance PPrint Rectangle where
    pprint n x = record "Rectangle" n [
        ("rect_x", P (rect_x x)),
        ("rect_y", P (rect_y x)),
        ("rect_width", P (rect_width x)),
        ("rect_height", P (rect_height x))
        ]

instance PPrint a => PPrint (Stack a) where
    pprint n x = record "Stack" n [
        ("focus", P (XMonad.StackSet.focus x)),
        ("up", P (up x)),
        ("down", P (down x))
        ]

instance (PPrint i, PPrint l, PPrint a) => PPrint (Workspace i l a) where
    pprint n x = record "Workspace" n [
        ("tag", P (tag x)),
        ("layout", P (layout x)),
        ("stack", P (stack x))
        ]

instance PPrint ScreenDetail where
    pprint n x = record "SD" n [("screenRect", P (screenRect x))]

instance (PPrint i, PPrint l, PPrint a, PPrint sid, PPrint sd) => PPrint (XMonad.StackSet.Screen i l a sid sd) where
    pprint n x = record "Screen" n [
        ("workspace", P (workspace x)),
        ("screen", P (screen x)),
        ("screenDetail", P (screenDetail x))
        ]

instance (PPrint i, PPrint l, PPrint a, PPrint sid, PPrint sd) => PPrint (StackSet i l a sid sd) where
    pprint n x = record "StackSet" n [
        ("current", P (current x)),
        ("visible", P (visible x)),
        ("hidden", P (hidden x)),
        ("floating", P (floating x))
        ]

instance PPrint (Layout a)
instance PPrint Int
instance PPrint XMonad.Screen
instance PPrint Integer
instance PPrint Position
instance PPrint Dimension
instance PPrint Char
instance PPrint Word64
instance PPrint ScreenId
instance (Show a, Show b) => PPrint (Map a b)
-- }}}
-- main {{{
dmwitConfig nScreens = docks $ def {
    borderWidth             = 2,
    workspaces              = withScreens nScreens (map show [1..5]),
    terminal                = "urxvt",
    normalBorderColor       = dark,
    focusedBorderColor      = bright,
    modMask                 = mod4Mask,
    keys                    = keyBindings,
    layoutHook              = magnifierOff $ avoidStruts (GridRatio 0.9) ||| noBorders Full,
    manageHook              =     (title =? "CGoban: Main Window" --> doF sinkFocus)
                              <+> (className =? "Wine" <&&> (appName =? "hl2.exe" <||> appName =? "portal2.exe") --> ask >>= viewFullOn {-centerWineOn-} 1 "5")
                              <+> (className =? "VirtualBox" --> ask >>= viewFullOn 1 "5")
                              <+> (isFullscreen --> doFullFloat) -- TF2 matches the "isFullscreen" criteria, so its manage hook should appear after (e.g., to the left of a <+> compared to) this one
                              <+> (appName =? "huludesktop" --> doRectFloat fullscreen43on169)
                              <+> fullscreenMPlayer
                              <+> floatAll ["Gimp", "Wine"]
                              <+> manageSpawn,
    logHook                 = allPPs nScreens,
    startupHook             = refresh
                           >> mapM_ (spawnOnce . xmobarCommand) [0 .. nScreens-1]
    }

main = countScreens >>= xmonad . dmwitConfig
-- }}}
-- keybindings {{{
keyBindings conf = let m = modMask conf in fromList . anyMask $ [
    ((m                , xK_BackSpace  ), spawnHere "urxvt"),
    ((m                , xK_p          ), spawnHere launcher),
    ((m .|. shiftMask  , xK_p          ), spawnHere termLauncher),
    ((m .|. shiftMask  , xK_c          ), kill),
    ((m                , xK_q          ), restart "xmonad" True),
    ((m .|. shiftMask  , xK_q          ), io (exitWith ExitSuccess)),
    ((m                , xK_grave      ), sendMessage NextLayout),
    ((m .|. shiftMask  , xK_grave      ), setLayout $ layoutHook conf),
    ((m                , xK_o          ), sendMessage Toggle),
    ((m                , xK_x          ), withFocused (windows . sink)),
    ((m                , xK_Home       ), windows focusUp),
    ((m .|. shiftMask  , xK_Home       ), windows swapUp),
    ((m                , xK_End        ), windows focusDown),
    ((m .|. shiftMask  , xK_End        ), windows swapDown),
    ((m                , xK_a          ), windows focusMaster),
    ((m .|. shiftMask  , xK_a          ), windows swapMaster),
    ((m                , xK_Control_L  ), withScreen 0 view),
    ((m .|. shiftMask  , xK_Control_L  ), withScreen 0 viewShift),
    ((m                , xK_Alt_L      ), withScreen 1 view),
    ((m .|. shiftMask  , xK_Alt_L      ), withScreen 1 viewShift),
    ((m                , xK_u          ), centerMouse),
    ((m .|. shiftMask  , xK_u          ), statusBarMouse),
    ((m                , xK_s          ), spawnHere "chromium --password-store=gnome"),
    ((m                , xK_n          ), spawnHere "gvim todo"),
    ((m                , xK_t          ), spawnHere "mpc toggle"),
    ((m                , xK_h          ), spawnHere "urxvt -e alsamixer"),
    ((m                , xK_d          ), spawnHere "wyvern"),
    ((m                , xK_l          ), spawnHere "urxvt -e sup"),
    ((m                , xK_r          ), spawnHere "urxvt -e ncmpcpp"),
    ((m                , xK_c          ), spawnHere "urxvt -e ghci"),
    ((m                , xK_g          ), spawnHere "slock" >> spawnHere "xscreensaver-command -lock"),
    ((m                , xK_f          ), spawnHere "gvim ~/.xmonad/xmonad.hs"),
    ((      noModMask  , xK_F8         ), showMod "sink input" (-4)),
    ((      noModMask  , xK_F9         ), showMod "sink input"   4 ),
    ((      shiftMask  , xK_F8         ), showMod "sink"       (-4)),
    ((      shiftMask  , xK_F9         ), showMod "sink"         4 ),
    ((      noModMask  , xK_Super_L    ), return ()) -- make VirtualBox ignore stray hits of the Windows key
    ] ++ [
    ((m .|. e          , key           ), windows (onCurrentScreen f ws))
    | (key, ws) <- zip [xK_1..xK_9] (workspaces' conf)
    , (e, f)    <- [(0, view), (shiftMask, viewShift)]
    ]

atSchool school home = do
    host <- liftIO (getEnv "HOST")
    return $ case host of
        "sorghum"   -> home
        "buckwheat" -> home
        _           -> school

anyMask xs = do
    ((mask, key), action) <- xs
    extraMask             <- [0, controlMask, altMask, controlMask .|. altMask]
    return ((mask .|. extraMask, key), action)
-- }}}
-- logHook {{{
pipeName n s = "/home/dmwit/.xmonad/pipe-" ++ n ++ "-" ++ show s

xmobarCommand (S s) = unwords ["xmobar",
    "-x", show s,
    "-t", template s,
    "-C", pipeReader
    ]
    where
    template 0 = "}%focus%{%workspaces%"
    template _ = "%date%}%focus%{%workspaces%"
    pipeReader = "'[\
        \Run PipeReader \"" ++ pipeName "focus"      s ++ "\" \"focus\",\
        \Run PipeReader \"" ++ pipeName "workspaces" s ++ "\" \"workspaces\"\
        \]'"

allPPs nScreens = sequence_ [dynamicLogWithPP (pp s) | s <- [0..nScreens-1], pp <- [ppFocus, ppWorkspaces]]
color c = xmobarColor c ""

ppFocus s@(S s_) = whenCurrentOn s def {
    ppOrder  = \(_:_:windowTitle:_) -> [windowTitle],
    ppOutput = appendFile (pipeName "focus" s_) . (++ "\n")
    }

ppWorkspaces s@(S s_) = marshallPP s def {
    ppCurrent           = color "white",
    ppVisible           = color "white",
    ppHiddenNoWindows   = color dark,
    ppUrgent            = color "red",
    ppSep               = "",
    ppOrder             = \(wss:_layout:_title:_) -> [wss],
    ppOutput            = appendFile (pipeName "workspaces" s_) . (++"\n")
    }
-- }}}
