module Data.InstanceDependencies where

import Prelude
import Data.Array ((:))
import Data.Foldable (class Foldable, foldMap, foldl, foldr)

data NonEmpty a = NonEmpty a (Array a)

ne1 :: NonEmpty Int
ne1 = NonEmpty 1 []
ne2 :: NonEmpty Int
ne2 = NonEmpty 2 []

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = (x : xs) == (y : ys)

instance semigroupNonEmpty :: (Semigroup a) => Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> y : ys)

instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

instance foldableNonEmpty :: Foldable NonEmpty where
  foldr f m (NonEmpty x xs) = foldr f m (x : xs)
  foldl f m (NonEmpty x xs) = foldl f m (x : xs)
  foldMap f (NonEmpty x xs) = foldMap f (x : xs)

data Extended a = Finite a | Infinite

instance eqExtended :: Eq a => Eq (Extended a) where
  eq (Finite x) (Finite y) = x == y
  eq (Finite _) Infinite = false
  eq Infinite (Finite _) = false
  eq Infinite Infinite = true

instance ordExtended :: Ord a => Ord (Extended a) where
  compare (Finite x) (Finite y) = compare x y
  compare (Finite _) Infinite = LT
  compare Infinite (Finite _) = GT
  compare Infinite Infinite = EQ

data OneMore f a = OneMore a (f a)

o1 :: OneMore Array Int
o1 = OneMore 1 (pure 1)

instance showOneMore :: (Show a, Show (f a)) => Show (OneMore f a) where
  show (OneMore x xs) = show x <> ", " <> show xs

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f z (OneMore x xs) = f x (foldr f z xs)
  foldl f z (OneMore x xs) = f (foldl f z xs) x
  foldMap f (OneMore x xs) = (f x) <> (foldMap f xs)

instance functorOneMore :: Functor f => Functor (OneMore f) where
  map f (OneMore x xs) = OneMore (f x) (map f xs)
