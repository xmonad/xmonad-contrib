{-|

Module      : XMonad.Actions.Menu.Dzen
Description : A module that displays Menus using Dzen
Copyright   : (C) David Janssen, 2016
License     : BSD3
Maintainer  : David Janssen <janssen.dhj@gmail.com>
Stability   : unstable
Portability : unportable

This module translates Menus into Dzen strings, providing lots of options for
customizing the appearance of said menus.

-}

module XMonad.Actions.Menu.Dzen
  (
    DzenCfg (..)
  , DzenItemCfg (..)
  , DzenLayout (..)
  , DzenItem
  , DzenMenu
  , DzenRenderer
  , DzenTuple
  , dzenMenu
  )
where

import           Data.Maybe           (fromJust)
import           Data.List            (transpose, intercalate)
import           System.IO
import           XMonad               hiding (Color)
import           XMonad.Actions.Menu.KeyParse (readStroke, showStroke)
import           XMonad.Actions.Menu.Core
import           XMonad.Util.Run      (spawnPipe)

-- | Colors in Dzen are just hex-strings
type Color        = String

-- | Specify the Menu-parts into Dzen-configured items
type DzenItem     = Item     DzenItemCfg
type DzenMenu     = Menu     DzenItemCfg
type DzenRenderer = Renderer DzenItemCfg
type DzenTuple    = (String, String, X(), DzenItemCfg)

-- | DzenCfg describes the behavior of the menu
data DzenCfg = DzenCfg {
    name     :: String
  , nameCol  :: Color
  , fgCol    :: Color
  , bgCol    :: Color
  , font     :: String
  , tAlign   :: String
  , layout   :: DzenLayout
  , top      :: Int
  , left     :: Int
  , width    :: Int
  }

instance Default DzenCfg where
  def = DzenCfg {
    name     = "Menu:"
  , nameCol  = "#EC5800"
  , fgCol    = "#839496"
  , bgCol    = "#002b36"
  , font     = "Source Code Pro-12"
  , tAlign   = "c"
  , layout   = FixedCol 5 20 2 1
  , top      = 0
  , left     = 0
  , width    = 1600
  }

data DzenItemCfg = DzenItemCfg {
    keyCol   :: Color
  , sepStr   :: String
  , sepCol   :: Color
  , descrCol :: Color
  , hidden   :: Bool
    }

-- | DzenItemCfg describes the behavior of an Item in a DzenMenu
instance Default DzenItemCfg where
  def = DzenItemCfg {
    keyCol   = "#FDF6E3"
  , sepStr   = " -> "
  , sepCol   = "#eee8d5"
  , descrCol = "#839496"
  , hidden   = False }

-- | Various layouts that we're prepared to handle
data DzenLayout = Naive
                | FixedCol { nRow :: Int , wMax :: Int , vMargin :: Int, bMargin :: Int }
                deriving (Eq, Show)

-- | mkItem creates 1 DzenItem
mkItem :: DzenTuple -> DzenItem
mkItem (ks, d, x, cfg) = Item (fromJust . readStroke $ ks) d x cfg

-- | mkMenu creates a DzenMenu
dzenMenu :: DzenCfg -> [DzenTuple] -> DzenMenu
dzenMenu cfg es = Menu (dzenRender cfg) (map mkItem es)

-- | dzenRender displays the menu in Dzen and returns an action that closes the
-- handle to Dzen's stdin (which in turn closes dzen)
dzenRender :: DzenCfg -> DzenMenu -> X (X ())
dzenRender cfg m = do
  str  <- dzenStr cfg m
  cmd  <- dzenCmd cfg str
  h    <- spawnPipe cmd
  io . hPutStr h $ str

  return (io $ hClose h)

-- | Generate the command to run dzen
dzenCmd :: DzenCfg -> String -> X String
dzenCmd c s = return $ unwords
  [ "dzen2"
  , "-e",  "onstart=uncollapse"
  , "-fg", "\"" ++ fgCol c ++ "\""
  , "-bg", "\"" ++ bgCol c ++ "\""
  , "-fn", "\"" ++ font c ++ "\""
  , "-x" , show . left $ c
  , "-y" , show . top $ c
  , "-w" , show . width $ c
  , "-ta", tAlign c
  , "-l",  show (n - 1)]
  where n = length . filter ('\n'==) $ s

-- | Generate the string that dzen will display
dzenStr :: DzenCfg -> DzenMenu -> X String
dzenStr cfg m = do
  let mTitle = rdrWord (DD (nameCol cfg) (name cfg))
  mLines <- genLines cfg (items m)
  return $ mTitle ++ "\n" ++ mLines

-- | Turn the DzenItem's stored in the menu into the dzen string
genLines :: DzenCfg -> [DzenItem] -> X String
genLines cfg xs = do
  let raws   = map readItem . filter (not . hidden . tags) $ xs
  return $ rdrLines (layout cfg) raws

-- | Join the separate menu-items into 1 string
rdrLines :: DzenLayout -> [CString] -> String
rdrLines Naive = unlines . map rdrString
rdrLines ly@FixedCol{ nRow = nR, wMax = wM } =
  joinCol ly . map (renderCol wM) . groupN nR

-- | Align all the menu-items and render the CStrings into Dzen-capable strings
renderCol :: Int -> [CString] -> [String]
renderCol n xs =
  let bits   = transpose xs
      maxW   = map (foldr (max . length . thing) 0) bits
      padded = zipWith pad bits maxW in
    map rdrString . transpose $ padded
  where
    pad bs w = map (pad' w) bs
    pad' w b@DD{thing = t} = b { thing = t ++ replicate (w - length t)' ' }

-- | Join a bunch of columns into 1 Dzen string
joinCol :: DzenLayout -> [[String]] -> String
joinCol FixedCol {vMargin = vm, bMargin = bm} =
  (++ replicate bm '\n') . unlines . map (intercalate vSep . ("":)) . transpose
  where
    vSep = replicate vm ' '

-- | Group a list of items into n-sized groups
groupN :: Int -> [a] -> [[a]]
groupN _ [] = []
groupN n xs = let (h, t) = splitAt n xs in h : groupN n t

-- | Render 1 item into a coloured string
readItem :: DzenItem -> CString
readItem i@Item{tags = d} = [
    DD {deco = keyCol d, thing = showStroke . keyStroke $ i}
  , DD {deco = sepCol d, thing = sepStr d}
  , DD {deco = descrCol d, thing = descr i} ]




-- A bit of code to make manipulating colored strings more easy

-- | Basic "Annotated data" functor.

data DecoData a b = DD { deco :: a, thing :: b }

instance (Show b) => Show (DecoData a b) where
  show DD{ thing = t } = "Deco: " ++ show t

instance Functor (DecoData a) where
  fmap f d = d { thing = f . thing $ d }

-- | CWords are bits of text of the same color and CStrings are sequences of
-- CWords. It should be straightforward to change "Color" to some more complex
-- markup (like Bold and Italics etc.) to create richer formats for other
-- rendering engines, but Dzen only supports color.
type CWord   = DecoData Color String
type CString = [CWord]

-- | Render 1 colored word to a Dzen-string
rdrWord :: CWord -> String
rdrWord DD {deco = col, thing = s} = "^fg(" ++ col ++ ")" ++ s ++ "^fg()"

-- | Render a list of colored words to a Dzen-string
rdrString :: CString -> String
rdrString = concatMap rdrWord
