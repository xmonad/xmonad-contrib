{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedLabels          #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{- | Two-dimensional workspaces for XMonad -}

module XMonad.WorkspaceLayout.Grid
  ( Formatted (..)
  , IsFormatted (..)
  , unsafeChangeFormatting
  , Mapping (..)
  , SomeMapping (..)
  , Dims(..)
  , fromMap
  , fromFunction
  , grid
  , grid'
  , group
  , column
  , Coord (..)
  , Wrapping (..)
  , State (..)
  , move
  , swap
  , update
  , Init (..)
  , hook
  , getView
  ) where

import           Prelude                     hiding (span)

import           Control.Category            ((>>>))
import           Data.Foldable               (fold, toList)
import           Data.Function               ((&))
import           Data.List                   (intercalate, nub)
import           Data.List.Split             (splitOn)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (catMaybes, fromMaybe)
import           Data.Monoid                 (Endo (..), appEndo)
import           GHC.Generics                (Generic)
import           XMonad                      hiding (config, state, trace)
import           XMonad.StackSet             (greedyView, shift)

import qualified XMonad.Util.OneState        as St
import           XMonad.WorkspaceLayout.Core (WorkspaceLayoutView (..))
import           XMonad.WorkspaceLayout.Util (affineMod)



data Formatted

  = Unformatted
      -- ^
      --
      -- Workspace IDs are left untouched

  | Formatted
      -- ^
      -- Workspace IDs are formatted
      --
      -- This means *specifically* that workspace IDs are of the format
      --   <uniq>:<name>
      -- Where
      -- * <uniq> contains a uniquely-identifying string
      -- * <name> contains the workspace name
      -- * <uniq> may not contain colons
      --
      -- (Editor's note: ideally, I'd like to allow custom formats beyond
      -- just this specific one. Such a desire is, however, in tension with
      -- retaining the static guarantee that formats are not mixed. Achieving
      -- this might be possible, but I'm judging it not worth the effort.)


doFormat :: forall ftd. IsFormatted ftd => Coord -> String -> WorkspaceId
doFormat (XY x y) name =
  case demoteFormatted @ftd of
    Formatted   -> show y <> "/" <> show x <> ":" <> name
    Unformatted -> name


doToName :: forall ftd. IsFormatted ftd => WorkspaceId -> String
doToName =
  case demoteFormatted @ftd of
    Unformatted -> id
    Formatted   -> splitOn ":" >>> drop 1 >>> intercalate ":"


class IsFormatted (ftd :: Formatted) where
  demoteFormatted :: Formatted

instance IsFormatted 'Unformatted where
  demoteFormatted = Unformatted

instance IsFormatted 'Formatted where
  demoteFormatted = Formatted


newtype Mapping (ftd :: Formatted) = Mapping { unMapping :: Map Coord WorkspaceId }
  deriving (Show, Generic)

unsafeChangeFormatting :: forall old (ftd :: Formatted). Mapping old -> Mapping ftd
unsafeChangeFormatting (Mapping mp) = Mapping mp

instance Semigroup (Mapping ftd) where
  Mapping ma <> Mapping mb = Mapping (mb <> ma)  -- right-biased

instance Monoid (Mapping ftd) where
  mempty = Mapping mempty


data SomeMapping = forall ftd. IsFormatted ftd => SomeMapping (Mapping ftd)

-- onTheMap :: (Map Coord WorkspaceId -> Map Coord WorkspaceId) -> (SomeMapping -> SomeMapping)
-- onTheMap f (SomeMapping (Mapping mp :: Mapping ftd)) = (SomeMapping (Mapping @ftd (f mp)))

getTheMap :: SomeMapping -> Map Coord WorkspaceId
getTheMap (SomeMapping (Mapping mp)) = mp

range :: Ord x => (Coord -> x) -> SomeMapping -> Maybe (x, x)
range proj (getTheMap -> mapping) =
  let xs = proj <$> Map.keys mapping
  in case xs of
      [] -> Nothing
      _  -> Just (minimum xs, maximum xs)

span :: (Ord x, Enum x) => (Coord -> x) -> SomeMapping -> [x]
span proj (getTheMap -> mapping) =
  let xs = proj <$> Map.keys mapping
  in case xs of
      [] -> []
      _  -> [minimum xs .. maximum xs]



data Dims = Dims { width :: Int, height :: Int }


-- | Construct a Mapping from a Map
fromMap :: Map Coord WorkspaceId -> Mapping 'Unformatted
fromMap = Mapping


-- | Construct a Mapping from a function
fromFunction :: Dims -> (Coord -> WorkspaceId) -> Mapping 'Unformatted
fromFunction (Dims { width, height }) =
  let domain = XY <$> [0 .. width - 1] <*> [0 .. height - 1]
  in fromMap . funToMap domain

  where
  funToMap :: Ord k => [k] -> (k -> v) -> Map k v
  funToMap xs f = Map.fromList $ (\k -> (k, f k)) <$> xs


-- |
--
-- Construct a 2d grid
--
-- Each coordinate (x, y) will be given the name @show (x + 1)@
grid :: Dims -> Mapping 'Formatted
grid = grid' (\(XY x _) -> show $ x + 1)


-- |
--
-- Construct a 2d grid
--
-- Each coordinate will be assigned a name according to the supplied function
grid' :: (Coord -> String) -> Dims -> Mapping 'Formatted
grid' toName (Dims { width, height }) =
  Mapping . fold $ do
    y <- [0 .. height - 1]
    x <- [0 .. width - 1]
    let coord = XY x y
    pure $ Map.singleton coord (doFormat @'Formatted coord $ toName coord)


-- | Glue together a group of workspaces
group :: forall f ftd. (IsFormatted ftd, Foldable f) => f Coord -> String -> Mapping ftd
group (toList -> xs) name = case xs of
  []     -> Mapping mempty
  (x0:_) -> Mapping $ xs & foldMap (\x -> Map.singleton x (doFormat @ftd x0 name))


-- | Glue together a column of workspaces
column :: forall ftd. IsFormatted ftd => Dims -> Int -> String -> Mapping ftd
column (Dims { height }) x name =
  Mapping . fold $ do
    y <- [0 .. height - 1]
    let topLeft = XY x 0
    pure $ Map.singleton (XY x y) (doFormat @ftd topLeft name)




data Coord = XY { x :: Int, y :: Int }
  deriving (Show, Ord, Eq, Generic)

data Wrapping = Wrapping
  { wrapX :: Bool
  , wrapY :: Bool
  }
  deriving (Show, Generic)

data State = State
  { mapping  :: SomeMapping
      -- ^ Coordinate -> WorkspaceId mapping
  , labelf   :: Coord -> Maybe String
      -- ^ Labels
  , wrapping :: Wrapping
      -- ^ Wrapping mode
  , coord    :: Coord
      -- ^ Current coordinate
  } deriving (Generic)

instance St.OneState State where
  type Mod State = State -> State
  merge ma s = pure (ma s)
  defaultState = State
    { mapping = SomeMapping (grid $ Dims 5 5)
    , wrapping = Wrapping False False
    , coord = XY 0 0
    , labelf = const Nothing
    }

-- Wrap a coordinate around the x/y axes according to the configured wrapping mode
wrap :: State -> (Coord -> Coord)
wrap state =

  appEndo . fromMaybe (Endo id) $ do
    xRange <- range x (mapping state)
    yRange <- range y (mapping state)

    pure $ guard (state & wrapping & wrapX) (Endo $ \c -> c { x = affineMod xRange (x c) })
        <> guard (state & wrapping & wrapY) (Endo $ \c -> c { y = affineMod yRange (y c) })

  where

  guard :: Monoid m => Bool -> m -> m
  guard = \case { True -> id; False -> const mempty }



-- |
--
-- The call @move f@ replaces the current coordinate to @f currentCoord@
--
-- If @f currentCoord@ is out-of-bounds, do nothing
--
-- Use this to move around workspaces
move :: (Coord -> Coord) -> X ()
move = update $ \coord wid -> do
  St.modify $ \st -> st { coord = coord }
  windows (greedyView wid)


-- |
--
-- The call @swap f@ moves the selected window to @f currentCoord@
--
-- If @f currentCoord@ is out-of-bounds, do nothing
swap :: (Coord -> Coord) -> X ()
swap = update $ \_ wid -> windows (shift wid)

update :: (Coord -> WorkspaceId -> X ()) -> (Coord -> Coord) -> X ()
update act f = do
  state@State { coord, mapping } <- St.get
  let coord' = coord & f & wrap state
  case Map.lookup coord' (getTheMap mapping) of
    Nothing -> pure ()
    Just wid -> do
     act coord' wid


data Init = Init
  { initMapping  :: SomeMapping
      -- ^ Physical layout
  , initWrapping :: Wrapping
      -- ^ Wrapping mode
  , initLabelf   :: Coord -> Maybe String
      -- ^
      -- Displayed to the left of the axis workspaces
      -- When Nothing is returned, uses a fallback
  }

-- | Hook the grid layout into XMonad
hook :: Init -> XConfig l -> XConfig l
hook Init { initMapping, initWrapping, initLabelf } =
  St.once @State
    (\xc -> xc { XMonad.workspaces = workspaces })
    (\state -> state
        { mapping = initMapping
        , wrapping = initWrapping
        , labelf = initLabelf
        })

  where
  workspaces = toList (getTheMap initMapping)
             & nub  -- account for two coordinates pointing to the same workspace

getView :: X WorkspaceLayoutView
getView = do
  State { coord, mapping, labelf } <- St.get
  let XY _ y = coord
  pure $ WSLView
    { neighborhood =
           let coords = (flip XY y) <$> span x mapping
           in coords & fmap (flip Map.lookup (getTheMap mapping)) & catMaybes & nub
    , toName = case mapping of SomeMapping (_ :: Mapping ftd) -> doToName @ftd
    , label = labelf coord & fromMaybe (show (y + 1) <> " / ")
    }

