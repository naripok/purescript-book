module Data.Superclass where

import Prelude
import Data.Foldable (maximum)
import Data.Maybe (fromJust)
import Data.Monoid (power)

findMaxInt :: Partial => Array Int -> Int
findMaxInt [] = 0
findMaxInt arr = fromJust $ maximum arr


class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance monoidMultiply :: Monoid Multiply where
  mempty = Multiply 1

instance repeatAction :: Action Multiply String where
  act (Multiply n) s = power s n

instance mapActions :: Action m a => Action m (Array a) where
  act m a = (act m) <$> a


newtype Self m = Self m

instance semigroupSelf :: Semigroup m => Semigroup (Self m) where
  append (Self n) (Self m) = Self (n <> m)

{-- instance monoidSelf :: Monoid m => Monoid (Self m) where --}
{--   mempty = mempty Self --}

instance selfAction :: Monoid m => Action m (Self m) where
  act a m = m <> m

instance showSelf :: Show m => Show (Self m) where
  show (Self m) = show m
