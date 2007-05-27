module XMonadContrib.Anneal ( Rated(Rated), the_value, the_rating, anneal ) where

data Rated a b = Rated !a !b
                 deriving ( Show )
instance Functor (Rated a) where
    f `fmap` (Rated v a) = Rated v (f a)

the_value :: Rated a b -> b
the_value (Rated _ b) = b
the_rating :: Rated a b -> a
the_rating (Rated a _) = a

instance Eq a => Eq (Rated a b) where
    (Rated a _) == (Rated a' _) = a == a'
instance Ord a => Ord (Rated a b) where
    compare (Rated a _) (Rated a' _) = compare a a'

anneal :: a -> (a -> Double) -> (a -> [a]) -> Rated Double a
anneal = undefined
