{-# LANGUAGE 
  FlexibleInstances, 
  FlexibleContexts, 
  MultiParamTypeClasses, 
  ExistentialQuantification 
  #-}

-------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config.PlainConfig
-- Copyright   :  Braden Shepherdson <Braden.Shepherdson@gmail.com>
-- License     :  BSD3
-- 
-- Maintainer  :  Braden Shepherdson <Braden.Shepherdson@gmail.com>
--
-- Proof-of-concept (but usable) plain-text configuration file
-- parser, for use instead of xmonad.hs. Does not require recompilation,
-- allowing xmonad to be free of the GHC dependency.
--
-------------------------------------------------------------------------


module XMonad.Config.PlainConfig
    (
     -- * Introduction
     -- $usage

     -- * Supported Layouts
     -- $layouts

     -- * Support Key Bindings
     -- $keys

     -- * Other Notes
     -- $notes

     -- * Example Config File
     -- $example

     plainConfig ,readConfig, checkConfig
    )
where


import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List
import Data.Maybe (isJust,fromJust)
import Data.Char (isSpace)


--import Control.Monad
import Control.Monad.Error
import Control.Monad.Identity

import Control.Arrow ((&&&))

import Text.ParserCombinators.ReadP

import System.IO
import Control.Exception (bracket)

import XMonad.Util.EZConfig (mkKeymap)



-- $usage
-- The @xmonad.hs@ file is very minimal when used with PlainConfig.
-- It typically contains only the following:
--
-- > module Main where
-- > import XMonad
-- > import XMonad.Config.PlainConfig (plainConfig)
-- > main = plainConfig
--
-- The 'plainConfig' function parses @~\/.xmonad\/xmonad.conf@, 
-- the format of which is described below.


-- $layouts
-- Only 'Tall', 'Wide' and 'Full' are supported at present.



-- $keys
-- 
-- Key bindings are specified as a pair of an arbitrary EZConfig and 
-- one of the following:
--
-- @   Name                     Haskell equivalent                                          Default binding(s)@
-- 
-- * @spawn \<cmd\>           spawn \"\<cmd\>\"                                               none@
-- 
-- * @kill                  kill                                                        M-S-c@
-- 
-- * @nextLayout            sendMessage NextLayout                                      M-\<Space\>@
-- 
-- * @refresh               refresh                                                     M-S-\<Space\>@
-- 
-- * @focusDown             windows W.focusDown                                         M-\<Tab\>, M-j@
-- 
-- * @focusUp               windows W.focusUp                                           M-k@
-- 
-- * @focusMaster           windows W.focusMaster                                       M-m@
-- 
-- * @swapDown              windows W.swapDown                                          M-S-j@
-- 
-- * @swapUp                windows W.swapUp                                            M-S-k@
-- 
-- * @swapMaster            windows W.swapMaster                                        M-\<Return\>@
-- 
-- * @shrink                sendMessage Shrink                                          M-h@
-- 
-- * @expand                sendMessage Expand                                          M-l@
-- 
-- * @sink                  withFocused $ windows . W.sink                              M-t@
-- 
-- * @incMaster             sendMessage (IncMasterN 1)                                  M-,@
-- 
-- * @decMaster             sendMessage (IncMasterN (-1))                               M-.@
-- 
-- * @quit                  io $ exitWith ExitSuccess                                   M-S-q@
-- 
-- * @restart               broadcastMessageReleaseResources >> restart \"xmonad\" True   M-q@
-- 


-- $notes
-- Submaps are allowed.
-- These settings override the defaults. Changes made here will be used over
-- the default bindings for those keys.


-- $example
-- An example @~\/.xmonad\/xmonad.conf@ file follows:
--
-- @modMask       = 3@
--
-- @numlockMask   = 2@
--
-- @borderWidth   = 1@
--
-- @normalBorderColor    	=   #dddddd@
--
-- @focusedBorderColor      =   #00ff00@
--
-- @terminal=urxvt@
--
-- @workspaces=[\"1: IRC\",\"2: Web\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\"]@
--
-- @focusFollowsMouse=True@
--
-- @layouts=[\"Tall\",\"Full\",\"Wide\"]@
--
-- @key=(\"M-x t\", \"spawn xmessage Test\")@
--
-- @manageHook=(ClassName \"MPlayer\"       , \"float\"  )@
--
-- @manageHook=(ClassName \"Gimp\"          , \"float\"  )@
--
-- @manageHook=(Resource  \"desktop_window\", \"ignore\" )@
--
-- @manageHook=(Resource  \"kdesktop\"      , \"ignore\" )@
--
-- @manageHook=(Resource  \"gnome-panel\"   , \"ignore\" )@
--






----------------------------------------------------------------
------ Several functions for parsing the key-value file. -------
----------------------------------------------------------------

parseKVBy :: Char -> ReadP (String,String)
parseKVBy sep = do
  skipSpaces 
  k <- munch1 (\x -> x /= ' ' && x /= sep) 
  skipSpaces
  char kvSep
  skipSpaces
  v <- munch1 (\x -> x /= ' ') --or EOS 
  return (k,v)

parseKVVBy :: Char -> ReadP (String,String)
parseKVVBy sep = do
  skipSpaces 
  k <- munch1 (\x -> x /= ' ' && x /= sep) 
  skipSpaces
  char kvSep
  skipSpaces
  v <- munch1 (const True) -- until EOS
  return (k,v)


kvSep :: Char
kvSep = '='

parseKV, parseKVV :: ReadP (String,String)
parseKV  = parseKVBy  kvSep
parseKVV = parseKVVBy kvSep



readKV :: String -> Integer -> RC (String,String)
readKV s ln = case readP_to_S parseKV s of
                [((k,v),"")] -> return (k,v) --single, correct parse
                []           -> throwError [(ln,"No parse")]
                _           -> do
                  case readP_to_S parseKVV s of
                    [((k,v),"")] -> return (k,v) --single, correct parse
                    []           -> throwError [(ln,"No parse")]
                    xs           -> throwError [(ln,"Ambiguous parse: "
                                                 ++ show xs)]



isComment :: String -> Bool
isComment = not . null . readP_to_S parseComment 
  where parseComment = skipSpaces >> char '#' >> return ()
    -- null means failed parse, so _not_ a comment.


isBlank :: String -> Bool
isBlank = null . filter (not . isSpace)


type RC = ErrorT [(Integer,String)] Identity

instance Error [(Integer,String)] where
    noMsg  = [(-1, "Unknown error.")]
    strMsg s = [(-1, s)]


parseFile :: [String] -> RC (XConfig Layout)
parseFile ss = parseLines baseConfig theLines
  where theLines = filter (not . liftM2 (||) isComment isBlank . snd) 
                   $ zip [1..] ss
        


parseLines :: XConfig Layout -> [(Integer,String)] -> RC (XConfig Layout)
parseLines = foldM parse


parse :: XConfig Layout -> (Integer, String) -> RC (XConfig Layout)
parse xc (ln,s) = do
  (k,v) <- readKV s ln
  case M.lookup k commands of
    Nothing -> throwError [(ln,"Unknown command: "++k)]
    Just f  -> f v ln xc




----------------------------------------------------------------
-- Now the semantic parts, that convert from the relevant     --
-- key-value entries to values in an XConfig                  --
----------------------------------------------------------------



type Command = String -> Integer -> XConfig Layout -> RC (XConfig Layout)

commands :: M.Map String Command
commands = M.fromList $ 
           [("modMask"             , cmd_modMask           )
           ,("numlockMask"         , cmd_numlockMask       )
           ,("normalBorderColor"   , cmd_normalBorderColor )
           ,("focusedBorderColor"  , cmd_focusedBorderColor)
           ,("terminal"            , cmd_terminal          )
           ,("workspaces"          , cmd_workspaces        )
           ,("focusFollowsMouse"   , cmd_focusFollowsMouse )
           ,("layouts"             , cmd_layouts           )
           ,("key"                 , cmd_key               )
           ,("manageHook"          , cmd_manageHook        )
           ,("borderWidth"         , cmd_borderWidth       )
           ]


-- | Behind-the-scenes helper for both 'cmd_modMask' and 'cmd_numlockMask'.
genericModKey :: (KeyMask -> XConfig Layout) -> Command
genericModKey f s ln _ = do
  x <- rcRead s ln :: RC Integer
  case lookup x (zip [1..] [mod1Mask,mod2Mask,mod3Mask,mod4Mask,mod5Mask]) of
    Just y  -> return $ f y
    Nothing -> throwError [(ln,"Invalid mod key number: "++ show x)]
  

-- | Reads the mod key modifier number.
cmd_modMask :: Command
cmd_modMask s ln xc = genericModKey (\k -> xc{modMask = k}) s ln xc

-- | Reads the numlock key modifier number.
cmd_numlockMask :: Command
cmd_numlockMask s ln xc = genericModKey (\k -> xc{numlockMask = k}) s ln xc


-- | Reads the border width.
cmd_borderWidth :: Command
cmd_borderWidth s ln xc = do
  w <- rcRead s ln
  return $ xc { borderWidth = w }


-- | Reads the colors but just keeps them as RRGGBB Strings.
cmd_normalBorderColor, cmd_focusedBorderColor :: Command
cmd_normalBorderColor  s _ xc = return $ xc{ normalBorderColor  = s } 
cmd_focusedBorderColor s _ xc = return $ xc{ focusedBorderColor = s }


-- | Reads the terminal. It is just a String, no parsing.
cmd_terminal :: Command
cmd_terminal s _ xc = return $ xc{ terminal = s }


-- | Reads the workspace tag list. This is given as a Haskell [String].
cmd_workspaces :: Command
cmd_workspaces s ln xc = rcRead s ln >>= \x -> return xc{ workspaces = x }


-- | Reads the focusFollowsMouse, as a Haskell Bool.
cmd_focusFollowsMouse :: Command
cmd_focusFollowsMouse s ln xc = rcRead s ln >>= 
                                \x -> return xc{focusFollowsMouse = x}


-- | The list known layouts, mapped by name.
--   An easy location for improvement is to add more contrib layouts here.
layouts :: M.Map String (Layout Window)
layouts = M.fromList
          [("Tall", Layout (Tall 1 (3/100) (1/2)))
          ,("Wide", Layout (Mirror (Tall 1 (3/100) (1/2))))
          ,("Full", Layout Full)
          ]


-- | Expects a [String], the strings being layout names. Quotes required.
--   Draws from the `layouts' list above.
cmd_layouts :: Command
cmd_layouts s ln xc = do
  xs <- rcRead s ln -- read the list of strings
  let ls = map (id &&& (flip M.lookup) layouts) xs
  when (null ls) $ throwError [(ln,"Empty layout list")]
  case filter (not . isJust . snd) ls of
    [] -> return $ xc{ layoutHook = foldr1 
                       (\(Layout l) (Layout r) -> 
                            Layout (l ||| r)) (map (fromJust . snd) ls) 
                     }
    ys -> throwError $ map (\(x,_) -> (ln, "Unknown layout: "++ x)) ys



-- | A Map from names to key binding actions.
key_actions :: M.Map String (X ())
key_actions = M.fromList
              [("kill"            , kill                   )
              ,("nextLayout"      , sendMessage NextLayout )
              --,("prevLayout"      , sendMessage PrevLayout )
              --,("resetLayout"     , setLayout $ XMonad.layoutHook conf)
              ,("refresh"         , refresh                )
              ,("focusDown"       , windows W.focusDown    )
              ,("focusUp"         , windows W.focusUp      )
              ,("focusMaster"     , windows W.focusMaster  )
              ,("swapMaster"      , windows W.swapMaster   )
              ,("swapDown"        , windows W.swapDown     )
              ,("swapUp"          , windows W.swapUp       )
              ,("shrink"          , sendMessage Shrink     )
              ,("expand"          , sendMessage Expand     )
              ,("sink"            , withFocused $ windows . W.sink)
              ,("incMaster"       , sendMessage (IncMasterN   1))
              ,("decMaster"       , sendMessage (IncMasterN (-1)))
              ,("quit"            , io $ exitWith ExitSuccess)
              ,("restart"         , broadcastMessage ReleaseResources 
                                      >> restart "xmonad" True)
              ]


-- | Expects keys as described in the preamble, as 
--   (\"EZConfig key name\", \"action name\"), 
--   eg. (\"M-S-t\", \"spawn thunderbird\")
--   One key per "key=" line.
cmd_key :: Command
cmd_key s ln xc = do
  (k,v) <- rcRead s ln
  if "spawn " `isPrefixOf` v
    then return $ xc { 
                      keys = \c -> M.union (mkKeymap c 
                                            [(k, spawn (drop 6 v))]
                                           ) ((keys xc) c) 
                     }
    else do
          case M.lookup v key_actions of
            Nothing -> throwError [(ln, "Unknown key action \"" ++ v ++ "\"")]
            Just ac -> return $ 
                       xc { keys = \c -> M.union (mkKeymap c [(k, ac)])
                                   ((keys xc) c) 
                          }



-- | Map of names to actions for 'ManageHook's.
manageHook_actions :: M.Map String ManageHook
manageHook_actions = M.fromList 
                     [("float"  , doFloat  )
                     ,("ignore" , doIgnore )
                     ]


-- | Parses 'ManageHook's in the form given in the preamble.
--   eg. (ClassName \"MPlayer\", \"float\")
cmd_manageHook :: Command
cmd_manageHook s ln xc = do
  (k,v) <- rcRead s ln
  let q = parseQuery k
  if "toWorkspace " `isPrefixOf` v
    then return $ xc { manageHook = manageHook xc <+> 
                       (q --> doShift (drop 12 v))
                     }
    else case M.lookup v manageHook_actions of
           Nothing -> throwError [(ln, "Unknown ManageHook action \"" 
                                   ++ v ++ "\"")]
           Just ac -> return $ xc { manageHook = manageHook xc <+> (q --> ac) }



-- | Core of the ManageHook expression parser.
--   Taken from Roman Cheplyaka's WindowProperties
parseQuery :: Property -> Query Bool
parseQuery (Title s)       = title =? s
parseQuery (ClassName s)   = className =? s
parseQuery (Resource s)    = resource =? s
parseQuery (And p q)       = parseQuery p <&&> parseQuery q
parseQuery (Or  p q)       = parseQuery p <&&> parseQuery q
parseQuery (Not p)         = not `fmap` parseQuery p
parseQuery (Const b)       = return b


-- | Property constructors are quite self-explaining.
--   Taken from Roman Cheplyaka's WindowProperties
data Property = Title String
              | ClassName String
              | Resource String
              | And Property Property  
              | Or  Property Property
              | Not Property
              | Const Bool
              deriving (Read, Show)



-- | A wrapping of the read function into the RC monad.
rcRead :: (Read a) => String -> Integer -> RC a
rcRead s ln = case reads s of 
                [(x,"")] -> return x
                _        -> throwError [(ln, "Failed to parse value")]



-- | The standard Config.hs 'defaultConfig', with the layout wrapped.
baseConfig :: XConfig Layout
baseConfig = defaultConfig{ layoutHook = Layout (layoutHook defaultConfig) }



-- | Core function that attempts to parse @~\/.xmonad\/xmonad.conf@
readConfig :: IO (Maybe (XConfig Layout))
readConfig = do
  cs <- bracket (openFile "/home/braden/.xmonad/xmonad.conf" ReadMode)
             (\h -> hClose h) -- vv force the lazy IO
             (\h -> (lines `fmap` hGetContents h) >>= \ss -> 
                    length ss `seq` return ss)
  let xce = runIdentity $ runErrorT $ parseFile cs
  case xce of
    Left es  -> mapM_ (\(ln,e) -> 
                           putStrLn $ "readConfig error: line "++show ln++
                                        ": "++ e) es 
                >> return Nothing
    Right xc -> return $ Just xc


-- | Attempts to run readConfig, and checks if it failed.
checkConfig :: IO Bool
checkConfig = isJust `fmap` readConfig



{-  REMOVED: It was for debugging, and causes an 'orphaned instances'
             warning to boot.



-- | Reads in the config, and then prints the resulting XConfig
dumpConfig :: IO ()
dumpConfig = readConfig >>= print


instance Show (XConfig Layout) where
    show x = "XConfig { "
             ++ "normalBorderColor = "++ normalBorderColor x ++", "
             ++ "focusedBorderColor = "++ focusedBorderColor x++", "
             ++ "terminal = "++ terminal x ++", "
             ++ "workspaces = "++ show (workspaces x) ++", "
             ++ "numlockMask = "++ show (numlockMask x) ++", "
             ++ "modMask = "++ show (modMask x) ++", "
             ++ "borderWidth = "++ show (borderWidth x) ++", "
             ++ "focusFollowsMouse = "++ show (focusFollowsMouse x) ++", "
             ++ "layouts = "++ show (layoutHook x) ++" }"

-}

-- | Handles the unwrapping of the Layout. Intended for use as
--   @main = plainConfig@
plainConfig :: IO ()
plainConfig = do
  conf <- readConfig
  case conf of
    (Just xc@XConfig{layoutHook= (Layout l)}) -> 
        xmonad (xc{ layoutHook = l })
    Nothing                                   -> 
        spawn $ "xmessage Failed to read xmonad.conf. See xmonad.errors."

