{-# LANGUAGE NamedFieldPuns, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Util.History
-- Description :  Track history in /O(log n)/ time.
-- Copyright   :  (c) 2022 L. S. Leary
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  @LSLeary (on github)
-- Stability   :  unstable
-- Portability :  unportable
--
-- Provides 'History', a variation on a LIFO stack with a uniqueness property.
-- In order to achieve the desired asymptotics, the data type is implemented as
-- an ordered Map.
--
-----------------------------------------------------------------------------

module XMonad.Util.History (
  History,
  origin,
  event,
  erase,
  recall,
  ledger,
  transcribe,
  ) where

-- base
import Data.Function (on)
import Text.Read
  ( Read(readPrec, readListPrec), Lexeme(Ident)
  , parens, prec, lexP, step, readListPrecDefault
  )

-- containers
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Map (Map)
import qualified Data.Map.Strict as M


-- | A history of unique @k@-events with @a@-annotations.
--
--   @History k a@ can be considered a (LIFO) stack of @(k, a)@ values with the
--   property that each @k@ is unique. From this point of view, 'event' pushes
--   and 'ledger' pops/peeks all.
--
--   The naive implementation has /O(n)/ 'event' and 'erase' due to the
--   uniqueness condition, but we can still use it as a denotation:
--
-- > mu :: History k a -> [(k, a)]
--
--   As an opaque data type with strict operations, @History k a@ values are all
--   finite expressions in the core interface: 'origin', 'erase' and 'event'.
--   Hence we define @mu@ by structural induction on these three cases.
--
data History k a = History
  { annals   :: !(IntMap (k, a))
  , recorded :: !(Map k Int)
  } deriving (Functor, Foldable, Traversable)

instance (Eq  k, Eq  a) => Eq  (History k a) where (==)    = (==)    `on` ledger
instance (Ord k, Ord a) => Ord (History k a) where compare = compare `on` ledger

instance (Show k, Show a) => Show (History k a) where
  showsPrec d h
    = showParen (d > app_prec)
    $ showString "transcribe "
    . showsPrec (app_prec+1) (ledger h)
    where app_prec = 10

instance (Read k, Read a, Ord k) => Read (History k a) where
  readPrec = parens . prec app_prec $ do
    Ident "transcribe" <- lexP
    l <- step readPrec
    pure (transcribe l)
    where app_prec = 10
  readListPrec = readListPrecDefault


-- | /O(1)/. A history of nothing.
--
-- > mu origin := []
--
origin :: History k a
origin = History I.empty M.empty

-- | /O(log n)/. A new event makes history; its predecessor forgotten.
--
-- > mu (event k a h) := (k, a) : mu (erase k h)
--
event :: Ord k => k -> a -> History k a -> History k a
event k a History{annals,recorded} = History
  { annals   = I.insert ik (k, a) . maybe id I.delete mseen $ annals
  , recorded = recorded'
  }
  where
    ik = maybe 0 (\((i, _), _) -> pred i) (I.minViewWithKey annals)
    (mseen, recorded') = M.insertLookupWithKey (\_ x _ -> x) k ik recorded

-- | /O(log n)/. Erase an event from history.
--
-- > mu (erase k h) := filter ((k /=) . fst) (mu h)
--
erase :: Ord k => k -> History k a -> History k a
erase k History{annals,recorded} = History
  { annals   = maybe id I.delete mseen annals
  , recorded = recorded'
  }
  where (mseen, recorded') = M.updateLookupWithKey (\_ _ -> Nothing) k recorded


-- | /O(log n)/. Recall an event.
recall :: Ord k => k -> History k a -> Maybe a
recall k History{annals,recorded} = do
  ik     <- M.lookup k recorded
  (_, a) <- I.lookup ik annals
  pure a

-- | /O(n)/. Read history, starting with the modern day. @ledger@ is @mu@.
ledger :: History k a -> [(k, a)]
ledger = I.elems . annals

-- | /O(n * log n)/. Transcribe a ledger.
transcribe :: Ord k => [(k, a)] -> History k a
transcribe = foldr (uncurry event) origin
