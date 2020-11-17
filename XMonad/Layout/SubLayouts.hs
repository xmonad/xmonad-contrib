{-# LANGUAGE PatternGuards, ParallelListComp, DeriveDataTypeable, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.SubLayouts
-- Copyright   :  (c) 2009 Adam Vogt
-- License     :  BSD-style (see xmonad/LICENSE)
--
-- Maintainer  :  vogt.adam@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- A layout combinator that allows layouts to be nested.
--
-----------------------------------------------------------------------------

module XMonad.Layout.SubLayouts (
    -- * Usage
    -- $usage
    subLayout,
    subTabbed,

    pushGroup, pullGroup,
    pushWindow, pullWindow,
    onGroup, toSubl, mergeDir,

    GroupMsg(..),
    Broadcast(..),

    defaultSublMap,

    Sublayout,

    -- * Screenshots
    -- $screenshots

    -- * Todo
    -- $todo
    )
    where

import XMonad.Layout.Circle () -- so haddock can find the link

import XMonad.Layout.Decoration(Decoration, DefaultShrinker)
import XMonad.Layout.LayoutModifier(LayoutModifier(handleMess, modifyLayout,
                                    redoLayout),
                                    ModifiedLayout(..))
import XMonad.Layout.Simplest(Simplest(..))
import XMonad.Layout.Tabbed(shrinkText,
                            TabbedDecoration, addTabs)
import XMonad.Layout.WindowNavigation(Navigate(Apply))
import XMonad.Util.Invisible(Invisible(..))
import XMonad.Util.Types(Direction2D(..))
import XMonad hiding (def)
import Control.Arrow(Arrow(second, (&&&)))
import Control.Monad(MonadPlus(mplus), foldM, guard, when, join)
import Data.Function(on)
import Data.List(nubBy, (\\), find)
import Data.Maybe(isNothing, fromMaybe, listToMaybe, mapMaybe)

import qualified XMonad as X
import qualified XMonad.Layout.BoringWindows as B
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Map(Map)

-- $screenshots
--
-- <<http://haskell.org/sitewiki/images/thumb/8/8b/Xmonad-SubLayouts-xinerama.png/480px-Xmonad-SubLayouts-xinerama.png>>
--
-- Larger version: <http://haskell.org/sitewiki/images/8/8b/Xmonad-SubLayouts-xinerama.png>

-- $todo
--  /Issue 288/
--
--  "XMonad.Layout.ResizableTile" assumes that its environment
--  contains only the windows it is running: sublayouts are currently run with
--  the stack containing only the windows passed to it in its environment, but
--  any changes that the layout makes are not merged back.
--
--  Should the behavior be made optional?
--
--  /Features/
--
--   * suggested managehooks for merging specific windows, or the apropriate
--     layout based hack to find out the number of groups currently showed, but
--     the size of current window groups is not available (outside of this
--     growing module)
--
--  /SimpleTabbed as a SubLayout/
--
--  'subTabbed' works well, but it would be more uniform to avoid the use of
--  addTabs, with the sublayout being Simplest (but
--  'XMonad.Layout.Tabbed.simpleTabbed' is this...).  The only thing to be
--  gained by fixing this issue is the ability to mix and match decoration
--  styles. Better compatibility with some other layouts of which I am not
--  aware could be another benefit.
--
--  'simpleTabbed' (and other decorated layouts) fail horribly when used as
--  subLayouts:
--
--    * decorations stick around: layout is run after being told to Hide
--
--    * mouse events do not change focus: the group-ungroup does not respect
--      the focus changes it wants?
--
--    * sending ReleaseResources before running it makes xmonad very slow, and
--      still leaves borders sticking around
--

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.SubLayouts
-- > import XMonad.Layout.WindowNavigation
--
-- Using "XMonad.Layout.BoringWindows" is optional and it allows you to add a
-- keybinding to skip over the non-visible windows.
--
-- > import XMonad.Layout.BoringWindows
--
-- Then edit your @layoutHook@ by adding the 'subTabbed' layout modifier:
--
-- > myLayout = windowNavigation $ subTabbed $ boringWindows $
-- >                        Tall 1 (3/100) (1/2) ||| etc..
-- > main = xmonad def { layoutHook = myLayout }
--
-- "XMonad.Layout.WindowNavigation" is used to specify which windows to merge,
-- and it is not integrated into the modifier because it can be configured, and
-- works best as the outer modifier.
--
-- Then to your keybindings add:
--
--  > , ((modm .|. controlMask, xK_h), sendMessage $ pullGroup L)
--  > , ((modm .|. controlMask, xK_l), sendMessage $ pullGroup R)
--  > , ((modm .|. controlMask, xK_k), sendMessage $ pullGroup U)
--  > , ((modm .|. controlMask, xK_j), sendMessage $ pullGroup D)
--  >
--  > , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
--  > , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
--  >
--  > , ((modm .|. controlMask, xK_period), onGroup W.focusUp')
--  > , ((modm .|. controlMask, xK_comma), onGroup W.focusDown')
--
--  These additional keybindings require the optional
--  "XMonad.Layout.BoringWindows" layoutModifier. The focus will skip over the
--  windows that are not focused in each sublayout.
--
--  > , ((modm, xK_j), focusDown)
--  > , ((modm, xK_k), focusUp)
--
--  A 'submap' can be used to make modifying the sublayouts using 'onGroup' and
--  'toSubl' simpler:
--
--  > ,((modm, xK_s), submap $ defaultSublMap conf)
--
--  /NOTE:/ is there some reason that @asks config >>= submap . defaultSublMap@
--  could not be used in the keybinding instead? It avoids having to explicitly
--  pass the conf.
--
-- For more detailed instructions, see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
-- "XMonad.Doc.Extending#Adding_key_bindings"

-- | The main layout modifier arguments:
--
-- @subLayout advanceInnerLayouts innerLayout outerLayout@
--
--  [@advanceInnerLayouts@] When a new group at index @n@ in the outer layout
--  is created (even with one element), the @innerLayout@ is used as the
--  layout within that group after being advanced with @advanceInnerLayouts !!
--  n@ 'NextLayout' messages. If there is no corresponding element in the
--  @advanceInnerLayouts@ list, then @innerLayout@ is not given any 'NextLayout'
--  messages.
--
--  [@innerLayout@] The single layout given to be run as a sublayout.
--
--  [@outerLayout@] The layout that determines the rectangles given to each
--  group.
--
--  Ex. The second group is 'Tall', the third is 'Circle', all others are tabbed
--  with:
--
--  > myLayout = addTabs shrinkText def
--  >          $ subLayout [0,1,2] (Simplest ||| Tall 1 0.2 0.5 ||| Circle)
--  >          $ Tall 1 0.2 0.5 ||| Full
subLayout :: [Int] -> subl a -> l a -> ModifiedLayout (Sublayout subl) l a
subLayout nextLayout sl x = ModifiedLayout (Sublayout (I []) (nextLayout,sl) []) x

-- | @subTabbed@ is a use of 'subLayout' with 'addTabs' to show decorations.
subTabbed :: (Eq a, LayoutModifier (Sublayout Simplest) a, LayoutClass l a) =>
    l a -> ModifiedLayout (Decoration TabbedDecoration DefaultShrinker)
                          (ModifiedLayout (Sublayout Simplest) l) a
subTabbed  x = addTabs shrinkText X.def $ subLayout [] Simplest x

-- | @defaultSublMap@ is an attempt to create a set of keybindings like the
-- defaults ones but to be used as a 'submap' for sending messages to the
-- sublayout.
defaultSublMap :: XConfig l -> Map (KeyMask, KeySym) (X ())
defaultSublMap (XConfig { modMask = modm }) = M.fromList
         [((modm, xK_space), toSubl NextLayout),
          ((modm, xK_j), onGroup W.focusDown'),
          ((modm, xK_k), onGroup W.focusUp'),
          ((modm, xK_h), toSubl Shrink),
          ((modm, xK_l), toSubl Expand),
          ((modm, xK_Tab), onGroup W.focusDown'),
          ((modm .|. shiftMask, xK_Tab), onGroup W.focusUp'),
          ((modm, xK_m), onGroup focusMaster'),
          ((modm, xK_comma), toSubl $ IncMasterN 1),
          ((modm, xK_period), toSubl $ IncMasterN (-1)),
          ((modm, xK_Return), onGroup swapMaster')
         ]
        where
         -- should these go into XMonad.StackSet?
         focusMaster' st = let (f:fs) = W.integrate st
            in W.Stack f [] fs
         swapMaster' (W.Stack f u d) = W.Stack f [] $ reverse u ++ d

data Sublayout l a = Sublayout
    { delayMess :: Invisible [] (SomeMessage,a)
                          -- ^ messages are handled when running the layout,
                          -- not in the handleMessage, I'm not sure that this
                          -- is necessary
    , def :: ([Int], l a) -- ^ how many NextLayout messages to send to newly
                          -- populated layouts. If there is no corresponding
                          -- index, then don't send any.
    , subls :: [(l a,W.Stack a)]
                          -- ^ The sublayouts and the stacks they manage
    }
    deriving (Read,Show)

-- | Groups assumes this invariant:
--     M.keys gs == map W.focus (M.elems gs)  (ignoring order)
--     All windows in the workspace are in the Map
--
-- The keys are visible windows, the rest are hidden.
--
-- This representation probably simplifies the internals of the modifier.
type Groups a = Map a (W.Stack a)

-- | GroupMsg take window parameters to determine which group the action should
-- be applied to
data GroupMsg a
    = UnMerge a -- ^ free the focused window from its tab stack
    | UnMergeAll a
                -- ^ separate the focused group into singleton groups
    | Merge a a -- ^ merge the first group into the second group
    | MergeAll a
                -- ^ make one large group, keeping the parameter focused
    | Migrate a a
                -- ^ used to the window named in the first argument to the
                -- second argument's group, this may be replaced by a
                -- combination of 'UnMerge' and 'Merge'
    | WithGroup (W.Stack a -> X (W.Stack a)) a
    | SubMessage SomeMessage  a
                -- ^ the sublayout with the given window will get the message
    deriving (Typeable)

-- | merge the window that would be focused by the function when applied to the
-- W.Stack of all windows, with the current group removed. The given window
-- should be focused by a sublayout. Example usage: @withFocused (sendMessage .
-- mergeDir W.focusDown')@
mergeDir :: (W.Stack Window -> W.Stack Window) -> Window -> GroupMsg Window
mergeDir f w = WithGroup g w
 where g cs = do
        let onlyOthers = W.filter (`notElem` W.integrate cs)
        flip whenJust (sendMessage . Merge (W.focus cs) . W.focus . f)
            =<< fmap (onlyOthers =<<) currentStack
        return cs

data Broadcast = Broadcast SomeMessage -- ^ send a message to all sublayouts
    deriving (Typeable)

instance Message Broadcast
instance Typeable a => Message (GroupMsg a)

-- | @pullGroup@, @pushGroup@ allow you to merge windows or groups inheriting
-- the position of the current window (pull) or the other window (push).
--
-- @pushWindow@ and @pullWindow@ move individual windows between groups. They
-- are less effective at preserving window positions.
pullGroup,pushGroup,pullWindow,pushWindow :: Direction2D -> Navigate
pullGroup = mergeNav (\o c -> sendMessage $ Merge o c)
pushGroup = mergeNav (\o c -> sendMessage $ Merge c o)
pullWindow = mergeNav (\o c -> sendMessage $ Migrate o c)
pushWindow = mergeNav (\o c -> sendMessage $ Migrate c o)

mergeNav :: (Window -> Window -> X ()) -> Direction2D -> Navigate
mergeNav f = Apply (\o -> withFocused (f o))

-- | Apply a function on the stack belonging to the currently focused group. It
-- works for rearranging windows and for changing focus.
onGroup :: (W.Stack Window -> W.Stack Window) -> X ()
onGroup f = withFocused (sendMessage . WithGroup (return . f))

-- | Send a message to the currently focused sublayout.
toSubl :: (Message a) => a -> X ()
toSubl m = withFocused (sendMessage . SubMessage (SomeMessage m))

instance (Read (l Window), Show (l Window), LayoutClass l Window) => LayoutModifier (Sublayout l) Window where
    modifyLayout (Sublayout { subls = osls }) (W.Workspace i la st) r = do
            let gs' = updateGroup st $ toGroups osls
                st' = W.filter (`elem` M.keys gs') =<< st
            updateWs gs'
            oldStack <- gets $ W.stack . W.workspace . W.current . windowset
            setStack st'
            runLayout (W.Workspace i la st') r <* setStack oldStack
            -- FIXME: merge back reordering, deletions?

    redoLayout (Sublayout { delayMess = I ms, def = defl, subls = osls }) _r st arrs = do
        let gs' = updateGroup st $ toGroups osls
        sls <- fromGroups defl st gs' osls

        let newL :: LayoutClass l Window => Rectangle -> WorkspaceId -> (l Window) -> Bool
                    -> (Maybe (W.Stack Window)) -> X ([(Window, Rectangle)], l Window)
            newL rect n ol isNew sst = do
                orgStack <- currentStack
                let handle l (y,_)
                        | not isNew = fromMaybe l <$> handleMessage l y
                        | otherwise = return l
                    kms = filter ((`elem` M.keys gs') . snd) ms
                setStack sst
                nl <- foldM handle ol $ filter ((`elem` W.integrate' sst) . snd) kms
                result <- runLayout (W.Workspace n nl sst) rect
                setStack orgStack -- FIXME: merge back reordering, deletions?
                return $ fromMaybe nl `second` result

            (urls,ssts) = unzip [ (newL gr i l isNew sst, sst)
                    | (isNew,(l,_st)) <- sls
                    | i <- map show [ 0 :: Int .. ]
                    | (k,gr) <- arrs, let sst = M.lookup k gs' ]

        arrs' <- sequence urls
        sls' <- return . Sublayout (I []) defl . map snd <$> fromGroups defl st gs'
                        [ (l,s) | (_,l) <- arrs' | (Just s) <- ssts ]
        return (concatMap fst arrs', sls')

    handleMess (Sublayout (I ms) defl sls) m
        | Just (SubMessage sm w) <- fromMessage m =
            return $ Just $ Sublayout (I ((sm,w):ms)) defl sls

        | Just (Broadcast sm) <- fromMessage m = do
            ms' <- fmap (zip (repeat sm) . W.integrate') currentStack
            return $ if null ms' then Nothing
                else Just $ Sublayout (I $ ms' ++ ms) defl sls

        | Just B.UpdateBoring <- fromMessage m = do
            let bs = concatMap unfocused $ M.elems gs
            ws <- gets (W.workspace . W.current . windowset)
            flip sendMessageWithNoRefresh ws $ B.Replace "Sublayouts" bs
            return Nothing

        | Just (WithGroup f w) <- fromMessage m
        , Just g <- M.lookup w gs = do
            g' <- f g
            let gs' = M.insert (W.focus g') g' $ M.delete (W.focus g) gs
            when (gs' /= gs) $ updateWs gs'
            when (w /= W.focus g') $ windows (W.focusWindow $ W.focus g')
            return Nothing

        | Just (MergeAll w) <- fromMessage m =
            let gs' = fmap (M.singleton w)
                    $ (focusWindow' w =<<) $ W.differentiate
                    $ concatMap W.integrate $ M.elems gs
            in maybe (return Nothing) fgs gs'

        | Just (UnMergeAll w) <- fromMessage m =
            let ws = concatMap W.integrate $ M.elems gs
                _ = w :: Window
                mkSingleton f = M.singleton f (W.Stack f [] [])
            in fgs $ M.unions $ map mkSingleton ws

        | Just (Merge x y) <- fromMessage m
        , Just (W.Stack _ xb xn) <- findGroup x
        , Just yst <- findGroup y =
            let zs = W.Stack x xb (xn ++ W.integrate yst)
            in fgs $ M.insert x zs $ M.delete (W.focus yst) gs

        | Just (UnMerge x) <- fromMessage m =
            fgs . M.fromList . map (W.focus &&& id) . M.elems
                    $ M.mapMaybe (W.filter (x/=)) gs

        -- XXX sometimes this migrates an incorrect window, why?
        | Just (Migrate x y) <- fromMessage m
        , Just xst <- findGroup x
        , Just (W.Stack yf yu yd) <- findGroup y =
            let zs = W.Stack x (yf:yu) yd
                nxsAdd = maybe id (\e -> M.insert (W.focus e) e) $ W.filter (x/=) xst
            in fgs $ nxsAdd $ M.insert x zs $ M.delete yf gs


        | otherwise = join <$> sequenceA (catchLayoutMess <$> fromMessage m)
     where gs = toGroups sls
           fgs gs' = do
                st <- currentStack
                Just . Sublayout (I ms) defl . map snd <$> fromGroups defl st gs' sls

           findGroup z = mplus (M.lookup z gs) $ listToMaybe
                    $ M.elems $ M.filter ((z `elem`) . W.integrate) gs
           -- catchLayoutMess :: LayoutMessages -> X (Maybe (Sublayout l Window))
           --  This l must be the same as from the instance head,
           --  -XScopedTypeVariables should bring it into scope, but we are
           --  trying to avoid warnings with ghc-6.8.2 and avoid CPP
           catchLayoutMess x = do
            let m' = x `asTypeOf` (undefined :: LayoutMessages)
            ms' <- zip (repeat $ SomeMessage m') . W.integrate'
                    <$> currentStack
            return $ do guard $ not $ null ms'
                        Just $ Sublayout (I $ ms' ++ ms) defl sls

currentStack :: X (Maybe (W.Stack Window))
currentStack = gets (W.stack . W.workspace . W.current . windowset)

-- | update Group to follow changes in the workspace
updateGroup :: Ord a => Maybe (W.Stack a) -> Groups a -> Groups a
updateGroup mst gs =
        let flatten = concatMap W.integrate . M.elems
            news = W.integrate' mst \\ flatten gs
            deads = flatten gs \\ W.integrate' mst

            uniNew = M.union (M.fromList $ map (\n -> (n,single n)) news)
            single x = W.Stack x [] []

            -- pass through a list to update/remove keys
            remDead = M.fromList . map (\w -> (W.focus w,w))
                        . mapMaybe (W.filter (`notElem` deads)) . M.elems

            -- update the current tab group's order and focus
            followFocus hs = fromMaybe hs $ do
                f' <- W.focus <$> mst
                xs <- find (elem f' . W.integrate) $ M.elems hs
                xs' <- W.filter (`elem` W.integrate xs) =<< mst
                return $ M.insert f' xs' $ M.delete (W.focus xs) hs

        in remDead $ uniNew $ followFocus gs

-- | rearrange the windowset to put the groups of tabs next to eachother, so
-- that the stack of tabs stays put.
updateWs :: Groups Window -> X ()
updateWs = windowsMaybe . updateWs'

updateWs' :: Groups Window -> WindowSet -> Maybe WindowSet
updateWs' gs ws = do
    w <- W.stack . W.workspace . W.current $ ws
    let gs' = updateGroup (Just w) gs
        nes = concatMap W.integrate $ mapMaybe (flip M.lookup gs') $ W.integrate w
    w' <- focusWindow' (W.focus w) =<< W.differentiate nes
    guard $ w' /= w
    return $ W.modify' (const w') ws

-- | focusWindow'. focus an element of a stack, is Nothing if that element is
-- absent. See also 'W.focusWindow'
focusWindow' :: (Eq a) => a -> W.Stack a -> Maybe (W.Stack a)
focusWindow' w st = do
    guard $ not $ null $ filter (w==) $ W.integrate st
    if W.focus st == w then Just st
        else focusWindow' w $ W.focusDown' st

-- update only when Just
windowsMaybe :: (WindowSet -> Maybe WindowSet) -> X ()
windowsMaybe f = do
    xst <- get
    ws <- gets windowset
    let up fws = put xst { windowset = fws }
    maybe (return ()) up $ f ws

unfocused :: W.Stack a -> [a]
unfocused x = W.up x ++ W.down x

toGroups :: (Ord a) => [(a1, W.Stack a)] -> Map a (W.Stack a)
toGroups ws = M.fromList . map (W.focus &&& id) . nubBy (on (==) W.focus)
                    $ map snd ws

-- | restore the default layout for each group. It needs the X monad to switch
-- the default layout to a specific one (handleMessage NextLayout)
fromGroups :: (LayoutClass layout a, Ord k) =>
              ([Int], layout a)
              -> Maybe (W.Stack k)
              -> Groups k
              -> [(layout a, b)]
              -> X [(Bool,(layout a, W.Stack k))]
fromGroups (skips,defl) st gs sls = do
    defls <- mapM (iterateM nextL defl !!) skips
    return $ fromGroups' defl defls st gs (map fst sls)
        where nextL l = fromMaybe l <$> handleMessage l (SomeMessage NextLayout)
              iterateM f = iterate (>>= f) . return

fromGroups' :: (Ord k) => a -> [a] -> Maybe (W.Stack k) -> Groups k -> [a]
                    -> [(Bool,(a, W.Stack k))]
fromGroups' defl defls st gs sls =
    [ (isNew,fromMaybe2 (dl, single w) (l, M.lookup w gs))
        | l <- map Just sls ++ repeat Nothing, let isNew = isNothing l
        | dl <- defls ++ repeat defl
        | w <- W.integrate' $ W.filter (`notElem` unfocs) =<< st ]
    where unfocs = unfocused =<< M.elems gs
          single w = W.Stack w [] []
          fromMaybe2 (a,b) (x,y) = (fromMaybe a x, fromMaybe b y)


-- this would be much cleaner with some kind of data-accessor
setStack :: Maybe (W.Stack Window) -> X ()
setStack x = modify (\s -> s { windowset = (windowset s)
                { W.current = (W.current $ windowset s)
                { W.workspace = (W.workspace $ W.current $ windowset s) { W.stack = x }}}})
