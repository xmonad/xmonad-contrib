{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances, PatternGuards, DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.NoBorders
-- Copyright   :  (c) --    David Roundy <droundy@darcs.net>
--                    2018  Yclept Nemo
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Spencer Janssen <spencerjanssen@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Make a given layout display without borders.  This is useful for
-- full-screen or tabbed layouts, where you don't really want to waste a
-- couple of pixels of real estate just to inform yourself that the visible
-- window has focus.
--
-----------------------------------------------------------------------------

module XMonad.Layout.NoBorders ( -- * Usage
                                 -- $usage
                                 noBorders
                               , smartBorders
                               , withBorder
                               , lessBorders
                               , hasBorder
                               , SetsAmbiguous(..)
                               , Ambiguity(..)
                               , With(..)
                               , BorderMessage (..), borderEventHook
                               , SmartBorder, WithBorder, ConfigurableBorder
                               ) where

import           XMonad
import           XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet                as W
import qualified XMonad.Util.Rectangle          as R

import           Data.List
import           Data.Monoid
import qualified Data.Map                       as M
import           Data.Function                  (on)
import           Control.Monad                  (guard)


-- $usage
-- You can use this module with the following in your ~\/.xmonad\/xmonad.hs file:
--
-- > import XMonad.Layout.NoBorders
--
-- and modify the layouts to call noBorders on the layouts you want to lack
-- borders:
--
-- > layoutHook = ... ||| noBorders Full ||| ...
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"

-- todo, use an InvisibleList.
data WithBorder a = WithBorder Dimension [a] deriving ( Read, Show )

instance LayoutModifier WithBorder Window where
    unhook (WithBorder _ s) = asks (borderWidth . config) >>= setBorders s

    redoLayout (WithBorder n s) _ _ wrs = do
        asks (borderWidth . config) >>= setBorders (s \\ ws)
        setBorders ws n
        return (wrs, Just $ WithBorder n ws)
     where
        ws = map fst wrs

-- | Removes all window borders from the specified layout.
noBorders :: LayoutClass l Window => l Window -> ModifiedLayout WithBorder l Window
noBorders = withBorder 0

-- | Forces a layout to use the specified border width. 'noBorders' is
-- equivalent to @'withBorder' 0@.
withBorder :: LayoutClass l a => Dimension -> l a -> ModifiedLayout WithBorder l a
withBorder b = ModifiedLayout $ WithBorder b []

setBorders :: [Window] -> Dimension -> X ()
setBorders ws bw = withDisplay $ \d -> mapM_ (\w -> io $ setWindowBorderWidth d w bw) ws

singleton :: [a] -> Bool
singleton = null . drop 1

type SmartBorder = ConfigurableBorder Ambiguity

-- | Removes the borders from a window under one of the following conditions:
--
--  * There is only one screen and only one window. In this case it's obvious
--  that it has the focus, so no border is needed.
--
--  * A floating window covers the entire screen (e.g. mplayer).
--
smartBorders :: LayoutClass l a => l a -> ModifiedLayout SmartBorder l a
smartBorders = lessBorders Never

-- | Apply a datatype that has a SetsAmbiguous instance to provide a list of
-- windows that should not have borders.
--
-- This gives flexibility over when borders should be drawn, in particular with
-- xinerama setups: 'Ambiguity' has a number of useful 'SetsAmbiguous'
-- instances
lessBorders :: (SetsAmbiguous p, Read p, Show p, LayoutClass l a) =>
        p -> l a -> ModifiedLayout (ConfigurableBorder p) l a
lessBorders amb = ModifiedLayout (ConfigurableBorder amb [] [] [])

-- | 'ManageHook' for sending 'HasBorder' messages:
--
-- >    title =? "foo" --> hasBorder True
--
-- There is no equivalent for 'ResetBorder'.
hasBorder :: Bool -> ManageHook
hasBorder b = ask >>= \w -> liftX (broadcastMessage $ HasBorder b w) >> idHook

data BorderMessage
    = HasBorder Bool Window
        -- ^ If @True@, never remove the border from the specified window. If
        -- @False@, always remove the border from the specified window.
    | ResetBorder Window
        -- ^ Reset the effects of any 'HasBorder' messages on the specified
        -- window.
    deriving (Typeable)

instance Message BorderMessage

data ConfigurableBorder p w = ConfigurableBorder
    { _generateHidden :: p
        -- ^ Generates a list of windows without borders. Uses 'SetsAmbiguous'
        -- to filter the current layout.
    , alwaysHidden   :: [w]
        -- ^ Windows that never have borders. This list is added to the result
        -- of 'generateHidden'.
    , neverHidden    :: [w]
        -- ^ Windows that always have borders - i.e. ignored by this module.
        -- This list is subtraced from 'alwaysHidden' and so has higher
        -- precendence.
    , currentHidden  :: [w]
        -- ^ The current set of windows without borders, i.e. the state.
    } deriving (Read, Show)

-- | Only necessary with 'BorderMessage' - remove non-existent windows from the
-- 'alwaysHidden' or 'neverHidden' lists.
borderEventHook :: Event -> X All
borderEventHook (DestroyWindowEvent { ev_window = w }) = do
    broadcastMessage $ ResetBorder w
    return $ All True
borderEventHook _ = return $ All True

instance (Read p, Show p, SetsAmbiguous p) => LayoutModifier (ConfigurableBorder p) Window where
    unhook (ConfigurableBorder _ _ _ ch) = asks (borderWidth . config) >>= setBorders ch

    redoLayout cb@(ConfigurableBorder gh ah nh ch) lr mst wrs = do
        let gh' wset = let lh = (hiddens gh wset lr mst wrs)
                       in  return $ (ah `union` lh) \\ nh
        ch' <- withWindowSet gh'
        asks (borderWidth . config) >>= setBorders (ch \\ ch')
        setBorders ch' 0
        return (wrs, Just $ cb { currentHidden = ch' })

    pureMess cb@(ConfigurableBorder gh ah nh ch) m
        | Just (HasBorder b w) <- fromMessage m =
            let consNewIf l True  = if w `elem` l then Nothing else Just (w:l)
                consNewIf l False = Just l
            in  (ConfigurableBorder gh) <$> consNewIf ah (not b)
                                        <*> consNewIf nh b
                                        <*> pure ch
        | Just (ResetBorder w) <- fromMessage m =
            let delete' e l = if e `elem` l then (True,delete e l) else (False,l)
                (da,ah') = delete' w ah
                (dn,nh') = delete' w nh
            in  if da || dn
                then Just cb { alwaysHidden = ah', neverHidden = nh' }
                else Nothing
        | otherwise = Nothing

-- | SetsAmbiguous allows custom actions to generate lists of windows that
-- should not have borders drawn through 'ConfigurableBorder'
--
-- To add your own (though perhaps those options would better belong as an
-- additional constructor to 'Ambiguity'), you can add the following function.
-- Note that @lr@, the parameter representing the 'Rectangle' of the parent
-- layout, was added to 'hiddens' in 0.14. Update your instance accordingly.
--
-- > data MyAmbiguity = MyAmbiguity deriving (Read, Show)
--
-- > instance SetsAmbiguous MyAmbiguity where
-- >  hiddens _ wset lr mst wrs = otherHiddens Screen \\ otherHiddens OnlyScreenFloat
-- >     where otherHiddens p = hiddens p wset lr mst wrs
--
-- The above example is redundant, because you can have the same result with:
--
-- > layoutHook = lessBorders (Combine Difference Screen OnlyScreenFloat) (Tall 1 0.5 0.03 ||| ... )
--
-- To get the same result as 'smartBorders':
--
-- > layoutHook = lessBorders Never (Tall 1 0.5 0.03 ||| ...)
--
-- This indirect method is required to keep the 'Read' and 'Show' for
-- ConfigurableBorder so that xmonad can serialize state.
class SetsAmbiguous p where
    hiddens :: p -> WindowSet -> Rectangle -> Maybe (W.Stack Window) -> [(Window, Rectangle)] -> [Window]

-- Quick overview since the documentation lacks clarity:
-- * Overall stacking order =
--      tiled stacking order ++ floating stacking order
--   Where tiled windows are (obviously) stacked below floating windows.
-- * Tiled stacking order =
--      [(window, Rectangle] order
--   Given by 'XMonad.Core.LayoutClass' where earlier entries are stacked
--   higher.
-- * Floating stacking order =
--      focus order
--   Given by the workspace stack where a higher focus corresponds to a higher
--   stacking position.
--
-- Integrating a stack returns a list in order of [highest...lowest].
--
-- 'XMonad.Core.LayoutClass' is given a stack with all floating windows removed
-- and returns a list (in stack order) of only the visible tiled windows, while
-- the workspace stack contains all windows (visible/hidden, floating/tiled) in
-- focus order. The StackSet 'floating' field maps all floating windows across
-- all workspaces to relative rectangles - without the associated screen.
--
-- 'XMonad.Operations.windows' gets the windowset from the state, mutates it,
-- then updates the state before calling 'runLayout' with the new windowset -
-- excluding any floating windows. Aside from the filtering, the stack received
-- by the layout should be identical to the one received from 'withWindowSet'.
instance SetsAmbiguous Ambiguity where
    hiddens amb wset lr mst wrs
      | Combine Union a b <- amb = on union next a b
      | Combine Difference a b <- amb = on (\\) next a b
      | Combine Intersection a b <- amb = on intersect next a b
      | otherwise = tiled ms ++ floating
      where next p = hiddens p wset lr mst wrs

            screens = [ scr | scr <- W.screens wset
                            , case amb of
                                    Never -> True
                                    _     -> not $ null $ integrate scr
                            , not . R.empty . screenRect
                                $ W.screenDetail scr
                            ]

            -- Find the screen containing the workspace being layouted.
            -- (This is a list only to avoid the need to specialcase when it
            -- can't be found or when several contain @lr@. When that happens,
            -- the result will probably be incorrect.)
            thisScreen = [ scr | scr <- W.screens wset
                               , screenRect (W.screenDetail scr) `R.supersetOf` lr ]

            -- This originally considered all floating windows across all
            -- workspaces. It seems more efficient to have each screen manage
            -- its own floating windows - and necessary to support the
            -- additional OnlyLayoutFloat* variants correctly in multihead
            -- setups. In some cases the previous code would redundantly add
            -- then remove borders from already-borderless windows.
            floating = do
                scr <- thisScreen
                let wz :: Integer -> (Window,Rectangle)
                       -> (Integer,Window,Rectangle)
                    wz i (w,wr) = (i,w,wr)
                    -- For the following: in stacking order lowest -> highest.
                    ts = reverse . zipWith wz [-1,-2..] $ wrs
                    fs = zipWith wz [0..] $ do
                        w       <- reverse . W.integrate' . W.stack . W.workspace $ scr
                        Just wr <- [M.lookup w (W.floating wset)]
                        return (w,scaleRationalRect sr wr)
                    sr = screenRect . W.screenDetail $ scr
                (i1,w1,wr1) <- fs
                guard $ case amb of
                    OnlyLayoutFloatBelow ->
                        let vu = do
                            gr           <- sr `R.difference` lr
                            (i2,_w2,wr2) <- ts ++ fs
                            guard $ i2 < i1
                            [wr2 `R.intersects` gr]
                        in lr == wr1 && (not . or) vu
                    OnlyLayoutFloat ->
                        lr == wr1
                    _ ->
                        wr1 `R.supersetOf` sr
                return w1

            ms = filter (`elem` W.integrate' mst) $ map fst wrs
            tiled [w]
              | Screen <- amb = [w]
              | OnlyScreenFloat <- amb = []
              | OnlyLayoutFloat <- amb = []
              | OnlyLayoutFloatBelow <- amb = []
              | OtherIndicated <- amb
              , let nonF = map integrate $ W.current wset : W.visible wset
              , length (concat nonF) > length wrs
              , singleton $ filter (1==) $ map length nonF = [w]
              | singleton screens = [w]
            tiled _ = []
            integrate y = W.integrate' . W.stack $ W.workspace y

-- | In order of increasing ambiguity (less borders more frequently), where
-- subsequent constructors add additional cases where borders are not drawn
-- than their predecessors. These behaviors make most sense with with multiple
-- screens: for single screens, 'Never' or 'smartBorders' makes more sense.
data Ambiguity
    = Combine With Ambiguity Ambiguity
        -- ^ This constructor is used to combine the borderless windows
        -- provided by the SetsAmbiguous instances from two other 'Ambiguity'
        -- data types.
    | OnlyLayoutFloatBelow
        -- ^ Like 'OnlyLayoutFloat', but only removes borders if no window
        -- stacked below remains visible. Considers all floating windows on the
        -- current screen and all visible tiled windows of the child layout. If
        -- any such window (that is stacked below) shows in any gap between the
        -- parent layout rectangle and the physical screen, the border will
        -- remain drawn.
    | OnlyLayoutFloat
        -- ^ Only remove borders on floating windows that exactly cover the
        -- parent layout rectangle.
    | OnlyScreenFloat
        -- ^ Only remove borders on floating windows that cover the whole
        -- screen.
    | Never
        -- ^ Like 'OnlyScreenFloat', and also remove borders of tiled windows
        -- when not ambiguous: this is the same as 'smartBorders'.
    | EmptyScreen
        -- ^ Focus in an empty screen does not count as ambiguous.
    | OtherIndicated
        -- ^ No borders on full when all other screens have borders.
    | Screen
        -- ^ Borders are never drawn on singleton screens.  With this one you
        -- really need another way such as a statusbar to detect focus.
    deriving (Read, Show)

-- | Used to indicate to the 'SetsAmbiguous' instance for 'Ambiguity' how two
-- lists should be combined.
data With = Union        -- ^ uses 'Data.List.union'
          | Difference   -- ^ uses 'Data.List.\\'
          | Intersection -- ^ uses 'Data.List.intersect'
        deriving (Read, Show)
