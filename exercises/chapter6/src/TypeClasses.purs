module Data.TypeClasses where

import Prelude

newtype Point = Point {x :: Int, y :: Int}
data Shape = Circle Point Int | Line Point Point

showPoint :: Point -> String
showPoint (Point {x, y}) = "x: " <> show x <> ", y:" <> show y

{-- showShape :: Shape -> String --}
{-- showShape (Circle o r) = "o: " <> showPoint o <> ", r: " <> show r --}
{-- showShape (Line p1 p2) = "p1: " <> showPoint p1 <> ", p2: " <> showPoint p2 --}

circle :: Shape
circle = Circle (Point {x: 1, y: 2}) 10

instance showShape :: Show Shape where
  show (Circle o r) = "o: " <> showPoint o <> ", r: " <> show r
  show (Line p1 p2) = "p1: " <> showPoint p1 <> ", p2: " <> showPoint p2


-- SemiGroup
-- defines:
  -- append (<>)
-- SemiGroup laws
-- Associativity a (b c) == (a b) c

-- Monoid
-- defines:
  -- append (<>)
  -- mempty (0, "", [], etc)
-- Monoid laws
  -- Totalidade
  -- Associativity
  -- Identity

-- Functor
-- defines:
  -- fmap :: (a -> b) -> f a -> f b
  -- (<$) :: a -> f b -> f a      (replaces all the values in f b by a, returning f a)
-- Laws
  -- Must preserve identity morphism (id)
  -- Must preserve composition of morphisms fmap (f . g) == fmap f . fmap g

newtype Complex = Complex {real :: Number, imaginary :: Number}

instance showComplex :: Show Complex where
  show (Complex x) = "real: " <> show x.real <> ", imaginary: " <> show x.imaginary

instance eqComplex :: Eq Complex where
  eq (Complex x) (Complex y) = (x.real == y.real) && (x.imaginary == y.imaginary)

c1 :: Complex
c1 = Complex {real: 1.0, imaginary: 2.0}
c2 :: Complex
c2 = Complex {real: 1.0, imaginary: 2.0}
c3 :: Complex
c3 = Complex {real: 2.0, imaginary: 2.0}
