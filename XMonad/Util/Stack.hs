{-# LANGUAGE PatternGuards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.Stack
-- Copyright   :  Quentin Moser <moserq@gmail.com>
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  orphaned
-- Stability   :  unstable
-- Portability :  unportable
--
-- Utility functions for manipulating @Maybe Stack@s.
--
-----------------------------------------------------------------------------

module XMonad.Util.Stack ( -- * Usage
                           -- | This is a developer-oriented module, intended to be used
                           -- for writing new extentions.
                           Zipper
                         , emptyZ
                         , singletonZ

                           -- * Conversions
                         , fromIndex
                         , toIndex
                         , fromTags
                         , toTags

                           -- * 'Zipper' manipulation functions
                           -- ** Insertion, movement
                         , insertUpZ
                         , insertDownZ
                         , swapUpZ
                         , swapDownZ
                         , swapMasterZ
                           -- ** Focus movement
                         , focusUpZ
                         , focusDownZ
                         , focusMasterZ
                         , findS
                         , findZ
                           -- ** Extraction
                         , getFocusZ
                         , getIZ
                           -- ** Sorting
                         , sortZ
                         , sortByZ
                           -- ** Maps
                         , mapZ
                         , mapZ_
                         , mapZM
                         , mapZM_
                         , onFocusedZ
                         , onFocusedZM
                         , onIndexZ
                         , onIndexZM
                           -- ** Filters
                         , filterZ
                         , filterZ_
                         , deleteFocusedZ
                         , deleteIndexZ
                           -- ** Folds
                         , foldrZ
                         , foldlZ
                         , foldrZ_
                         , foldlZ_
                         , elemZ

                           -- * Other utility functions
                         , getI
                         , tagBy
                         , fromE
                         , mapE
                         , mapE_
                         , mapEM
                         , mapEM_
                         , reverseS
                         , reverseZ
                         ) where

import qualified XMonad.StackSet as W
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (sortBy)



type Zipper a = Maybe (W.Stack a)

emptyZ :: Zipper a
emptyZ = Nothing

singletonZ :: a -> Zipper a
singletonZ a = Just $ W.Stack a [] []

-- * Conversions

-- | Create a stack from a list, and the 0-based index of the focused element.
-- If the index is out of bounds, focus will go to the first element.
fromIndex :: [a] -> Int -> Zipper a
fromIndex as i = fromTags $ zipWith ($) (replicate i Left ++ [Right] ++ repeat Left) as

-- | Turn a stack into a list and the index of its focused element.
toIndex :: Zipper a -> ([a], Maybe Int)
toIndex Nothing = ([], Nothing)
toIndex (Just s) = (W.integrate s, Just $ length $ W.up s)

-- | Create a stack from a list of 'Either'-tagged values. Focus will go to
-- the first 'Right' value, or if there is none, to the first 'Left' one.
fromTags :: [Either a a] -> Zipper a
fromTags = finalize . foldr step ([], Nothing, [])
    where step (Right a) (u, Just f, d) = ([], Just a, u++f:d)
          step (Right a) (u, Nothing, d) = (u, Just a, d)
          step (Left a) (u, Just f, d) = (a:u, Just f, d)
          step (Left a) (u, Nothing, d) = (u, Nothing, a:d)
          finalize (u, Just f, d) = Just $ W.Stack f (reverse u) d
          finalize (u, Nothing, a:d) = Just $ W.Stack a (reverse u) d
          finalize (_, Nothing, []) = Nothing

-- | Turn a stack into an 'Either'-tagged list. The focused element
-- will be tagged with 'Right', the others with 'Left'.
toTags :: Zipper a -> [Either a a]
toTags Nothing = []
toTags (Just s) = map Left (reverse . W.up $ s) ++ [Right . W.focus $ s]
                  ++ map Left (W.down s)


-- * Zipper functions

-- ** Insertion, movement

-- | Insert an element before the focused one, and focus it
insertUpZ :: a -> Zipper a -> Zipper a
insertUpZ a Nothing = W.differentiate [a]
insertUpZ a (Just s) = Just s { W.focus = a , W.down = W.focus s : W.down s }

-- | Insert an element after the focused one, and focus it
insertDownZ :: a -> Zipper a -> Zipper a
insertDownZ a Nothing = W.differentiate [a]
insertDownZ a (Just s) = Just s { W.focus = a, W.up = W.focus s : W.up s }

-- | Swap the focused element with the previous one
swapUpZ :: Zipper a -> Zipper a
swapUpZ Nothing = Nothing
swapUpZ (Just s) | u:up <- W.up s = Just s { W.up = up, W.down = u:W.down s}
swapUpZ (Just s) = Just s { W.up = reverse (W.down s), W.down = [] }

-- | Swap the focused element with the next one
swapDownZ :: Zipper a -> Zipper a
swapDownZ Nothing = Nothing
swapDownZ (Just s) | d:down <- W.down s = Just s { W.down = down, W.up = d:W.up s }
swapDownZ (Just s) = Just s { W.up = [], W.down = reverse (W.up s) }

-- | Swap the focused element with the first one
swapMasterZ :: Zipper a -> Zipper a
swapMasterZ Nothing = Nothing
swapMasterZ (Just (W.Stack f up down)) = Just $ W.Stack f [] (reverse up ++ down)

-- ** Focus movement

-- | Move the focus to the previous element
focusUpZ :: Zipper a -> Zipper a
focusUpZ Nothing = Nothing
focusUpZ (Just s) | u:up <- W.up s = Just $ W.Stack u up (W.focus s:W.down s)
focusUpZ (Just s) | null $ W.down s = Just s
focusUpZ (Just (W.Stack f _ down)) = Just $ W.Stack (last down) (reverse (init down) ++ [f]) []

-- | Move the focus to the next element
focusDownZ :: Zipper a -> Zipper a
focusDownZ Nothing = Nothing
focusDownZ (Just s) | d:down <- W.down s = Just $ W.Stack d (W.focus s:W.up s) down
focusDownZ (Just s) | null $ W.up s = Just s
focusDownZ (Just (W.Stack f up _)) = Just $ W.Stack (last up) [] (reverse (init up) ++ [f])

-- | Move the focus to the first element
focusMasterZ :: Zipper a -> Zipper a
focusMasterZ Nothing = Nothing
focusMasterZ (Just (W.Stack f up down)) | not $ null up
    = Just $ W.Stack (last up) [] (reverse (init up) ++ [f] ++ down)
focusMasterZ (Just s) = Just s

-- | Refocus a @Stack a@ on an element satisfying the predicate, or fail to
--   @Nothing@.
findS :: (a -> Bool) -> W.Stack a -> Maybe (W.Stack a)
findS p st = st <$ (guard . p . W.focus) st <|> findUp st <|> findDown st
  where findDown = reverseZ . findUp . reverseS
        findUp s | u:ups <- W.up s = (if p u then Just else findUp)
                                   $ W.Stack u ups (W.focus s : W.down s)
                 | otherwise       = Nothing

-- | Refocus a @Zipper a@ on an element satisfying the predicate, or fail to
--   @Nothing@.
findZ :: (a -> Bool) -> Zipper a -> Zipper a
findZ _ Nothing   = Nothing
findZ p (Just st) = findS p st

-- ** Extraction

-- | Get the focused element
getFocusZ :: Zipper a -> Maybe a
getFocusZ = fmap W.focus

-- | Get the element at a given index
getIZ :: Int -> Zipper a -> Maybe a
getIZ i = getI i . W.integrate'

-- ** Sorting

-- | Sort a stack of elements supporting 'Ord'
sortZ :: Ord a => Zipper a -> Zipper a
sortZ = sortByZ compare

-- | Sort a stack with an arbitrary sorting function
sortByZ :: (a -> a -> Ordering) -> Zipper a -> Zipper a
sortByZ f = fromTags . sortBy (adapt f) . toTags
    where adapt g e1 e2 = g (fromE e1) (fromE e2)

-- ** Maps

-- | Map a function over a stack. The boolean argument indcates whether
-- the current element is the focused one
mapZ :: (Bool -> a -> b) -> Zipper a -> Zipper b
mapZ f as = fromTags . map (mapE f) . toTags $ as

-- | 'mapZ' without the 'Bool' argument
mapZ_ :: (a -> b) -> Zipper a -> Zipper b
mapZ_ = mapZ . const

-- | Monadic version of 'mapZ'
mapZM :: Monad m => (Bool -> a -> m b) -> Zipper a -> m (Zipper b)
mapZM f as = fromTags <$> (mapM (mapEM f) . toTags) as


-- | Monadic version of 'mapZ_'
mapZM_ :: Monad m => (a -> m b) -> Zipper a -> m (Zipper b)
mapZM_ = mapZM . const

-- | Apply a function to the focused element
onFocusedZ :: (a -> a) -> Zipper a -> Zipper a
onFocusedZ f = mapZ $ \b a -> if b then f a else a

-- | Monadic version of 'onFocusedZ'
onFocusedZM :: Monad m => (a -> m a) -> Zipper a -> m (Zipper a)
onFocusedZM f = mapZM $ \b a -> if b then f a else return a

-- | Apply a function to the element at the given index
onIndexZ :: Int -> (a -> a) -> Zipper a -> Zipper a
onIndexZ i _ as | i < 0 = as
onIndexZ i f as = case splitAt i $ toTags as of
                    (before, []) -> fromTags before
                    (before, a:after) -> fromTags $ before ++ mapE (const f) a : after

-- | Monadic version of 'onIndexZ'
onIndexZM :: Monad m => Int -> (a -> m a) -> Zipper a -> m (Zipper a)
onIndexZM i f as = case splitAt i $ toTags as of
                     (before, []) -> return $ fromTags before
                     (before, a:after) -> do a' <- mapEM (const f) a
                                             return $ fromTags $ before ++ a' : after

-- ** Filters

-- | Fiter a stack according to a predicate. The refocusing behavior
-- mimics XMonad's usual one. The boolean argument indicates whether the current
-- element is the focused one.
filterZ :: (Bool -> a -> Bool) -> Zipper a -> Zipper a
filterZ _ Nothing = Nothing
filterZ p (Just s) = case ( p True (W.focus s)
                          , filter (p False) (W.up s)
                          , filter (p False) (W.down s) ) of
                       (True, up', down') -> Just s { W.up = up', W.down = down' }
                       (False, [], []) -> Nothing
                       (False, f:up', []) -> Just s { W.focus = f, W.up = up', W.down = [] }
                       (False, up', f:down') ->  Just s { W.focus = f
                                                        , W.up = up'
                                                        , W.down = down' }

-- | 'filterZ' without the 'Bool' argument
filterZ_ :: (a -> Bool) -> Zipper a -> Zipper a
filterZ_ = filterZ . const

-- | Delete the focused element
deleteFocusedZ :: Zipper a -> Zipper a
deleteFocusedZ = filterZ (\b _ -> not b)

-- | Delete the ith element
deleteIndexZ :: Int -> Zipper a -> Zipper a
deleteIndexZ i z = let numbered = (fromTags . zipWith number [0..] . toTags) z
                       number j ea = mapE (\_ a -> (j,a)) ea
                   in mapZ_ snd $ filterZ_ ((/=i) . fst) numbered

-- ** Folds

-- | Analogous to 'foldr'. The 'Bool' argument to the step functions indicates
-- whether the current element is the focused one
foldrZ :: (Bool -> a -> b -> b) -> b -> Zipper a -> b
foldrZ _ b Nothing = b
foldrZ f b (Just s) = let b1 = foldr (f False) b (W.down s)
                          b2 = f True (W.focus s) b1
                          b3 = foldl (flip $ f False) b2 (W.up s)
                      in b3

-- | Analogous to 'foldl'. The 'Bool' argument to the step functions indicates
-- whether the current element is the focused one
foldlZ :: (Bool -> b -> a -> b) -> b -> Zipper a -> b
foldlZ _ b Nothing = b
foldlZ f b (Just s) = let b1 = foldr (flip $ f False) b (W.up s)
                          b2 = f True b1 (W.focus s)
                          b3 = foldl (f False) b2 (W.down s)
                      in b3

-- | 'foldrZ' without the 'Bool' argument.
foldrZ_ :: (a -> b -> b) -> b -> Zipper a -> b
foldrZ_ = foldrZ . const

-- | 'foldlZ' without the 'Bool' argument.
foldlZ_ :: (b -> a -> b) -> b -> Zipper a -> b
foldlZ_ = foldlZ . const

-- | Find whether an element is present in a stack.
elemZ :: Eq a => a -> Zipper a -> Bool
elemZ a as = foldlZ_ step False as
    where step True _ = True
          step False a' = a' == a


-- * Other utility functions

-- | Safe version of '!!'
getI :: Int -> [a] -> Maybe a
getI _ [] = Nothing
getI 0 (a:_) = Just a
getI i (_:as) = getI (i-1) as

-- | Map a function across both 'Left's and 'Right's.
-- The 'Bool' argument is 'True' in a 'Right', 'False'
-- in a 'Left'.
mapE :: (Bool -> a -> b) -> Either a a -> Either b b
mapE f (Left a) = Left $ f False a
mapE f (Right a) = Right $ f True a

mapE_ :: (a -> b) -> Either a a -> Either b b
mapE_ = mapE . const

-- | Monadic version of 'mapE'
mapEM :: Monad m => (Bool -> a -> m b) -> Either a a -> m (Either b b)
mapEM f (Left a) = Left <$> f False a
mapEM f (Right a) = Right <$> f True a

mapEM_ :: Monad m => (a -> m b) -> Either a a -> m (Either b b)
mapEM_ = mapEM . const

-- | Get the @a@ from an @Either a a@
fromE :: Either a a -> a
fromE (Right a) = a
fromE (Left a) = a

-- | Tag the element with 'Right' if the property is true, 'Left' otherwise
tagBy :: (a -> Bool) -> a -> Either a a
tagBy p a = if p a then Right a else Left a

-- | Reverse a @Stack a@; O(1).
reverseS :: W.Stack a -> W.Stack a
reverseS (W.Stack foc ups downs) = W.Stack foc downs ups

-- | Reverse a @Zipper a@; O(1).
reverseZ :: Zipper a -> Zipper a
reverseZ = (reverseS <$>)
