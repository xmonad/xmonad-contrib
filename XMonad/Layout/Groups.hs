{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds #-}
{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, PatternGuards, Rank2Types #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.Groups
-- Description :  Split windows in layout groups that are managed by another layout.
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Two-level layout with windows split in individual layout groups,
-- themselves managed by a user-provided layout.
--
-----------------------------------------------------------------------------

module XMonad.Layout.Groups ( -- * Usage
                              -- $usage
                              -- * Creation
                              group
                              -- * Messages
                            , GroupsMessage(..)
                            , ModifySpec
                            , ModifySpecX
                              -- ** Useful 'ModifySpec's
                            , swapUp
                            , swapDown
                            , swapMaster
                            , focusUp
                            , focusDown
                            , focusMaster
                            , swapGroupUp
                            , swapGroupDown
                            , swapGroupMaster
                            , focusGroupUp
                            , focusGroupDown
                            , focusGroupMaster
                            , moveToGroupUp
                            , moveToGroupDown
                            , moveToNewGroupUp
                            , moveToNewGroupDown
                            , splitGroup
                              -- * Types
                            , Groups
                            , Group(..)
                            , onZipper
                            , onLayout
                            , WithID
                            , sameID
                            ) where

import XMonad
import XMonad.Prelude hiding (group)
import qualified XMonad.StackSet as W

import XMonad.Util.Stack

import Control.Arrow ((>>>))

-- $usage
-- This module provides a layout combinator that allows you
-- to manage your windows in independent groups. You can provide
-- both the layout with which to arrange the windows inside each
-- group, and the layout with which the groups themselves will
-- be arranged on the screen.
--
-- The "XMonad.Layout.Groups.Examples" and "XMonad.Layout.Groups.Wmii"
-- modules contain examples of layouts that can be defined with this
-- combinator. They're also the recommended starting point
-- if you are a beginner and looking for something you can use easily.
--
-- One thing to note is that 'Groups'-based layout have their own
-- notion of the order of windows, which is completely separate
-- from XMonad's. For this reason, operations like 'XMonad.StackSet.SwapUp'
-- will have no visible effect, and those like 'XMonad.StackSet.focusUp'
-- will focus the windows in an unpredictable order. For a better way of
-- rearranging windows and moving focus in such a layout, see the
-- example 'ModifySpec's (to be passed to the 'Modify' message) provided
-- by this module.
--
-- If you use both 'Groups'-based and other layouts, The "XMonad.Layout.Groups.Helpers"
-- module provides actions that can work correctly with both, defined using
-- functions from "XMonad.Actions.MessageFeedback".

-- | Create a 'Groups' layout.
--
-- Note that the second parameter (the layout for arranging the
-- groups) is not used on 'Windows', but on 'Group's. For this
-- reason, you can only use layouts that don't specifically
-- need to manage 'Window's. This is obvious, when you think
-- about it.
group :: l Window -> l2 (Group l Window) -> Groups l l2 Window
group l l2 = Groups l l2 startingGroups (U 1 0)
    where startingGroups = fromJust $ singletonZ $ G (ID (U 0 0) l) emptyZ

-- * Stuff with unique keys

data Uniq = U Integer Integer
  deriving (Eq, Show, Read)

-- | From a seed, generate an infinite list of keys and a new
-- seed. All keys generated with this method will be different
-- provided you don't use 'gen' again with a key from the list.
-- (if you need to do that, see 'split' instead)
gen :: Uniq -> (Uniq, [Uniq])
gen (U i1 i2) = (U (i1+1) i2, map (U i1) [i2..])

-- | Split an infinite list into two. I ended up not
-- needing this, but let's keep it just in case.
-- split :: [a] -> ([a], [a])
-- split as = snd $ foldr step (True, ([], [])) as
--     where step a (True, (as1, as2)) = (False, (a:as1, as2))
--           step a (False, (as1, as2)) = (True, (as1, a:as2))

-- | Add a unique identity to a layout so we can
-- follow it around.
data WithID l a = ID { getID :: Uniq
                     , unID :: l a}
  deriving (Show, Read)

-- | Compare the ids of two 'WithID' values
sameID :: WithID l a -> WithID l a -> Bool
sameID (ID id1 _) (ID id2 _) = id1 == id2

instance Eq (WithID l a) where
    ID id1 _ == ID id2 _ = id1 == id2

instance LayoutClass l a => LayoutClass (WithID l) a where
    runLayout ws@W.Workspace { W.layout = ID id l } r
        = do (placements, ml') <- runLayout ws{ W.layout = l} r
             return (placements, ID id <$> ml')
    handleMessage (ID id l) sm = do ml' <- handleMessage l sm
                                    return $ ID id <$> ml'
    description (ID _ l) = description l



-- * The 'Groups' layout


-- ** Datatypes

-- | A group of windows and its layout algorithm.
data Group l a = G { gLayout :: WithID l a
                   , gZipper :: Zipper a }
  deriving (Show, Read, Eq)

onLayout :: (WithID l a -> WithID l a) -> Group l a -> Group l a
onLayout f g = g { gLayout = f $ gLayout g }

onZipper :: (Zipper a -> Zipper a) -> Group l a -> Group l a
onZipper f g = g { gZipper = f $ gZipper g }

-- | The type of our layouts.
data Groups l l2 a = Groups { -- | The starting layout for new groups
                              baseLayout :: l a
                              -- | The layout for placing each group on the screen
                            , partitioner :: l2 (Group l a)
                              -- | The window groups
                            , groups :: W.Stack (Group l a)
                              -- | A seed for generating unique ids
                            , seed :: Uniq
                            }

deriving instance (Show a, Show (l a), Show (l2 (Group l a))) => Show (Groups l l2 a)
deriving instance (Read a, Read (l a), Read (l2 (Group l a))) => Read (Groups l l2 a)

-- | Messages accepted by 'Groups'-based layouts.
-- All other messages are forwarded to the layout of the currently
-- focused subgroup (as if they had been wrapped in 'ToFocused').
data GroupsMessage = ToEnclosing SomeMessage -- ^ Send a message to the enclosing layout
                                             -- (the one that places the groups themselves)
                   | ToGroup Int SomeMessage -- ^ Send a message to the layout for nth group
                                             -- (starting at 0)
                   | ToFocused SomeMessage -- ^ Send a message to the layout for the focused
                                           -- group
                   | ToAll SomeMessage -- ^ Send a message to all the sub-layouts
                   | Refocus -- ^ Refocus the window which should be focused according
                             -- to the layout.
                   | Modify ModifySpec -- ^ Modify the ordering\/grouping\/focusing
                                       -- of windows according to a 'ModifySpec'
                   | ModifyX ModifySpecX -- ^ Same as 'Modify', but within the 'X' monad

instance Show GroupsMessage where
    show (ToEnclosing _) = "ToEnclosing {...}"
    show (ToGroup i _) = "ToGroup "++show i++" {...}"
    show (ToFocused _) = "ToFocused {...}"
    show (ToAll _) = "ToAll {...}"
    show Refocus = "Refocus"
    show (Modify _) = "Modify {...}"
    show (ModifyX _) = "ModifyX {...}"

instance Message GroupsMessage

modifyGroups :: (Zipper (Group l a) -> Zipper (Group l a))
             -> Groups l l2 a -> Groups l l2 a
modifyGroups f g = let (seed', ids) = gen (seed g)
                       defaultGroups = fromJust $ singletonZ $ G (ID (head ids) $ baseLayout g) emptyZ
                   in g { groups = fromMaybe defaultGroups . f . Just $ groups g
                        , seed = seed' }

modifyGroupsX :: (Zipper (Group l a) -> X (Zipper (Group l a)))
              -> Groups l l2 a -> X (Groups l l2 a)
modifyGroupsX f g = do
  let (seed', ids) = gen (seed g)
      defaultGroups = fromJust $ singletonZ $ G (ID (head ids) $ baseLayout g) emptyZ
  g' <- f . Just $ groups g
  return g { groups = fromMaybe defaultGroups g', seed = seed' }

-- ** Readaptation

-- | Adapt our groups to a new stack.
-- This algorithm handles window additions and deletions correctly,
-- ignores changes in window ordering, and tries to react to any
-- other stack changes as gracefully as possible.
readapt :: Eq a => Zipper a -> Groups l l2 a -> Groups l l2 a
readapt z g = let mf = getFocusZ z
                  (seed', ids) = gen $ seed g
                  g' = g { seed = seed' }
              in flip modifyGroups g' $ mapZ_ (onZipper $ removeDeleted z)
                                        >>> filterKeepLast (isJust . gZipper)
                                        >>> findNewWindows (W.integrate' z)
                                        >>> addWindows (ID (head ids) $ baseLayout g)
                                        >>> focusGroup mf
                                        >>> onFocusedZ (onZipper $ focusWindow mf)
    where filterKeepLast _ Nothing = Nothing
          filterKeepLast f z@(Just s) =  filterZ_ f z
                                     <|> singletonZ (W.focus s)

-- | Remove the windows from a group which are no longer present in
-- the stack.
removeDeleted :: Eq a => Zipper a -> Zipper a -> Zipper a
removeDeleted z = filterZ_ (`elemZ` z)

-- | Identify the windows not already in a group.
findNewWindows :: Eq a => [a] -> Zipper (Group l a)
               -> (Zipper (Group l a), [a])
findNewWindows as gs = (gs, foldrZ_ removePresent as gs)
    where removePresent g = filter (not . flip elemZ (gZipper g))

-- | Add windows to the focused group. If you need to create one,
-- use the given layout and an id from the given list.
addWindows :: WithID l a -> (Zipper (Group l a), [a]) -> Zipper (Group l a)
addWindows l (Nothing, as) = singletonZ $ G l (W.differentiate as)
addWindows _ (z, as) = onFocusedZ (onZipper add) z
    where add z = foldl (flip insertUpZ) z as

-- | Focus the group containing the given window
focusGroup :: Eq a => Maybe a -> Zipper (Group l a) -> Zipper (Group l a)
focusGroup Nothing = id
focusGroup (Just a) = fromTags . map (tagBy $ elemZ a . gZipper) . W.integrate'

-- | Focus the given window
focusWindow :: Eq a => Maybe a -> Zipper a -> Zipper a
focusWindow Nothing = id
focusWindow (Just a) = fromTags . map (tagBy (==a)) . W.integrate'


-- * Interface

-- ** Layout instance

instance (LayoutClass l Window, LayoutClass l2 (Group l Window))
    => LayoutClass (Groups l l2) Window where

        description (Groups _ p gs _) = s1++" by "++s2
            where s1 = description $ gLayout $ W.focus gs
                  s2 = description p

        runLayout ws@(W.Workspace _ _l z) r = let l = readapt z _l in
            do (areas, mpart') <- runLayout ws { W.layout = partitioner l
                                               , W.stack = Just $ groups l } r

               results <- forM areas $ \(g, r') -> runLayout ws { W.layout = gLayout g
                                                                , W.stack = gZipper g } r'

               let hidden = map gLayout (W.integrate $ groups _l) \\ map (gLayout . fst) areas
               hidden' <- mapM (flip handleMessage $ SomeMessage Hide) hidden

               let placements = concatMap fst results
                   newL = justMakeNew l mpart' (map snd results ++ hidden')

               return (placements, newL)

        handleMessage l@(Groups _ p _ _) sm | Just (ToEnclosing sm') <- fromMessage sm
            = do mp' <- handleMessage p sm'
                 return $ maybeMakeNew l mp' []

        handleMessage l@(Groups _ p gs _) sm | Just (ToAll sm') <- fromMessage sm
            = do mp' <- handleMessage p sm'
                 mg's <- mapZM_ (handle sm') $ Just gs
                 return $ maybeMakeNew l mp' $ W.integrate' mg's
            where handle sm (G l _) = handleMessage l sm

        handleMessage l sm | Just a <- fromMessage sm
            = let _rightType = a == Hide -- Is there a better-looking way
                                         -- of doing this?
              in handleMessage l $ SomeMessage $ ToAll sm

        handleMessage l@(Groups _ _ z _) sm = case fromMessage sm of
              Just (ToFocused sm') -> do mg's <- W.integrate' <$> handleOnFocused sm' z
                                         return $ maybeMakeNew l Nothing mg's
              Just (ToGroup i sm') -> do mg's <- handleOnIndex i sm' z
                                         return $ maybeMakeNew l Nothing mg's
              Just (Modify spec) -> case applySpec spec l of
                                      Just l' -> refocus l'
                                      Nothing -> return Nothing
              Just (ModifyX spec) -> do ml' <- applySpecX spec l
                                        whenJust ml' (void . refocus)
                                        return (ml' <|> Just l)
              Just Refocus -> refocus l
              Just _ -> return Nothing
              Nothing -> handleMessage l $ SomeMessage (ToFocused sm)
            where handleOnFocused sm z = mapZM step $ Just z
                      where step True (G l _) = handleMessage l sm
                            step False _ = return Nothing
                  handleOnIndex i sm z = mapM step $ zip [0..] $ W.integrate z
                      where step (j, G l _) | i == j = handleMessage l sm
                            step _ = return Nothing


justMakeNew :: Groups l l2 a -> Maybe (l2 (Group l a)) -> [Maybe (WithID l a)]
            -> Maybe (Groups l l2 a)
justMakeNew g mpart' ml's = Just g { partitioner = fromMaybe (partitioner g) mpart'
                                   , groups = combine (groups g) ml's }
    where combine z ml's = let table = map (\(ID id a) -> (id, a)) $ catMaybes ml's
                           in flip mapS_ z $ \(G (ID id l) ws) -> case lookup id table of
                                        Nothing -> G (ID id l) ws
                                        Just l' -> G (ID id l') ws
          mapS_ f = fromJust . mapZ_ f . Just


maybeMakeNew :: Groups l l2 a -> Maybe (l2 (Group l a)) -> [Maybe (WithID l a)]
             -> Maybe (Groups l l2 a)
maybeMakeNew _ Nothing ml's | all isNothing ml's = Nothing
maybeMakeNew g mpart' ml's = justMakeNew g mpart' ml's

refocus :: Groups l l2 Window -> X (Maybe (Groups l l2 Window))
refocus g =
  let mw = (getFocusZ . gZipper . W.focus . groups) g
  in  g <$ mw <$ whenJust mw (modifyWindowSet . W.focusWindow)

-- ** ModifySpec type

-- | Type of functions describing modifications to a 'Groups' layout. They
-- are transformations on 'Zipper's of groups.
--
-- Things you shouldn't do:
--
-- * Forge new windows (they will be ignored)
--
-- * Duplicate windows (whatever happens is your problem)
--
-- * Remove windows (they will be added again)
--
-- * Duplicate layouts (only one will be kept, the rest will
--   get the base layout)
--
-- Note that 'ModifySpec' is a rank-2 type (indicating that 'ModifySpec's must
-- be polymorphic in the layout type), so if you define functions taking
-- 'ModifySpec's as arguments, or returning them,  you'll need to write a type
-- signature and add @{-# LANGUAGE Rank2Types #-}@ at the beginning
type ModifySpec = forall l. WithID l Window
                -> Zipper (Group l Window)
                -> Zipper (Group l Window)

-- ** ModifierSpecX type

-- | This is the same as 'ModifySpec', but it allows the function to use
-- actions inside the 'X' monad. This is useful, for example, if the function
-- has to make decisions based on the results of a 'runQuery'.
type ModifySpecX = forall l. WithID l Window
                 -> Zipper (Group l Window)
                 -> X (Zipper (Group l Window))

-- | Apply a ModifySpec.
applySpec :: ModifySpec -> Groups l l2 Window -> Maybe (Groups l l2 Window)
applySpec f g =
    let (seed', ids) =  gen $ seed g
        g' = flip modifyGroups g $ f (ID (head ids) $ baseLayout g)
                                   >>> toTags
                                   >>> foldr (reID g) ((tail ids, []), [])
                                   >>> snd
                                   >>> fromTags
    in if groups g == groups g'
       then Nothing
       else Just g' { seed = seed' }

applySpecX :: ModifySpecX -> Groups l l2 Window -> X (Maybe (Groups l l2 Window))
applySpecX f g = do
    let (seed', ids) = gen $ seed g
    g' <- flip modifyGroupsX g $ f (ID (head ids) $ baseLayout g)
                                >>> fmap toTags
                                >>> fmap (foldr (reID g) ((tail ids, []), []))
                                >>> fmap snd
                                >>> fmap fromTags
    return $ if groups g == groups g'
             then Nothing
             else Just g' { seed = seed' }

reID :: Groups l l2 Window
     -> Either (Group l Window) (Group l Window)
     -> (([Uniq], [Uniq]), [Either (Group l Window) (Group l Window)])
     -> (([Uniq], [Uniq]), [Either (Group l Window) (Group l Window)])
reID _ _ (([], _), _) = undefined -- The list of ids is infinite
reID g eg ((id:ids, seen), egs) = if myID `elem` seen
                                  then ((ids, seen), mapE_ (setID id) eg:egs)
                                  else ((id:ids, myID:seen), eg:egs)
    where myID = getID $ gLayout $ fromE eg
          setID id (G (ID _ _) z) = G (ID id $ baseLayout g) z

-- ** Misc. ModifySpecs

-- | helper
onFocused :: (Zipper Window -> Zipper Window) -> ModifySpec
onFocused f _ = onFocusedZ (onZipper f)

-- | Swap the focused window with the previous one.
swapUp :: ModifySpec
swapUp = onFocused swapUpZ

-- | Swap the focused window with the next one.
swapDown :: ModifySpec
swapDown = onFocused swapDownZ

-- | Swap the focused window with the (group's) master
-- window.
swapMaster :: ModifySpec
swapMaster = onFocused swapMasterZ

-- | Swap the focused group with the previous one.
swapGroupUp :: ModifySpec
swapGroupUp _ = swapUpZ

-- | Swap the focused group with the next one.
swapGroupDown :: ModifySpec
swapGroupDown _ = swapDownZ

-- | Swap the focused group with the master group.
swapGroupMaster :: ModifySpec
swapGroupMaster _ = swapMasterZ

-- | Move focus to the previous window in the group.
focusUp :: ModifySpec
focusUp = onFocused focusUpZ

-- | Move focus to the next window in the group.
focusDown :: ModifySpec
focusDown = onFocused focusDownZ

-- | Move focus to the group's master window.
focusMaster :: ModifySpec
focusMaster = onFocused focusMasterZ

-- | Move focus to the previous group.
focusGroupUp :: ModifySpec
focusGroupUp _ = focusUpZ

-- | Move focus to the next group.
focusGroupDown :: ModifySpec
focusGroupDown _ = focusDownZ

-- | Move focus to the master group.
focusGroupMaster :: ModifySpec
focusGroupMaster _ = focusMasterZ

-- | helper
_removeFocused :: W.Stack a -> (a, Zipper a)
_removeFocused (W.Stack f (u:up) down) = (f, Just $ W.Stack u up down)
_removeFocused (W.Stack f [] (d:down)) = (f, Just $ W.Stack d [] down)
_removeFocused (W.Stack f [] []) = (f, Nothing)

-- helper
_moveToNewGroup :: WithID l Window -> W.Stack (Group l Window)
                -> (Group l Window -> Zipper (Group l Window)
                                   -> Zipper (Group l Window))
                -> Zipper (Group l Window)
_moveToNewGroup l0 s insertX | G l (Just f) <- W.focus s
    = let (w, f') = _removeFocused f
          s' = s { W.focus = G l f' }
      in insertX (G l0 $ singletonZ w) $ Just s'
_moveToNewGroup _ s _ = Just s

-- | Move the focused window to a new group before the current one.
moveToNewGroupUp :: ModifySpec
moveToNewGroupUp _ Nothing = Nothing
moveToNewGroupUp l0 (Just s) = _moveToNewGroup l0 s insertUpZ

-- | Move the focused window to a new group after the current one.
moveToNewGroupDown :: ModifySpec
moveToNewGroupDown _ Nothing = Nothing
moveToNewGroupDown l0 (Just s) = _moveToNewGroup l0 s insertDownZ


-- | Move the focused window to the previous group.
-- If 'True', when in the first group, wrap around to the last one.
-- If 'False', create a new group before it.
moveToGroupUp :: Bool -> ModifySpec
moveToGroupUp _ _ Nothing = Nothing
moveToGroupUp False l0 (Just s) = if null (W.up s) then moveToNewGroupUp l0 (Just s)
                                                   else moveToGroupUp True l0 (Just s)
moveToGroupUp True _ (Just s@(W.Stack _ [] [])) = Just s
moveToGroupUp True _ (Just s@(W.Stack (G l (Just f)) _ _))
    = let (w, f') = _removeFocused f
      in onFocusedZ (onZipper $ insertUpZ w) $ focusUpZ $ Just s { W.focus = G l f' }
moveToGroupUp True _ gs = gs

-- | Move the focused window to the next group.
-- If 'True', when in the last group, wrap around to the first one.
-- If 'False', create a new group after it.
moveToGroupDown :: Bool -> ModifySpec
moveToGroupDown _ _ Nothing = Nothing
moveToGroupDown False l0 (Just s) = if null (W.down s) then moveToNewGroupDown l0 (Just s)
                                                       else moveToGroupDown True l0 (Just s)
moveToGroupDown True _ (Just s@(W.Stack _ [] [])) = Just s
moveToGroupDown True _ (Just s@(W.Stack (G l (Just f)) _ _))
    = let (w, f') = _removeFocused f
      in onFocusedZ (onZipper $ insertUpZ w) $ focusDownZ $ Just s { W.focus = G l f' }
moveToGroupDown True _ gs = gs

-- | Split the focused group into two at the position of the focused window (below it,
-- unless it's the last window - in that case, above it).
splitGroup :: ModifySpec
splitGroup _ Nothing = Nothing
splitGroup l0 z@(Just s) | G l (Just ws) <- W.focus s
    = case ws of
        W.Stack _ [] [] -> z
        W.Stack f (u:up) [] -> let g1 = G l  $ Just $ W.Stack f [] []
                                   g2 = G l0 $ Just $ W.Stack u up []
                               in insertDownZ g1 $ onFocusedZ (const g2) z
        W.Stack f up (d:down) -> let g1 = G l  $ Just $ W.Stack f up []
                                     g2 = G l0 $ Just $ W.Stack d [] down
                                 in insertUpZ g1 $ onFocusedZ (const g2) z
splitGroup _ _ = Nothing
