module Data.Cap4 where

import Prelude
import Data.Array
import Data.Maybe
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Math (pow)
import Control.MonadZero (guard)
import Data.Foldable (product)
import Data.Path

{-- Recursion on arrays --}

isEven :: Int -> Int
isEven n = if n `mod` 2 == 0
  then 1
  else 0

evenNumber :: Array Int -> Int
evenNumber arr =
  if null arr
    then 0
    else isEven (unsafePartial head arr) + evenNumber (unsafePartial tail arr)

arr = [1, 2, 3, 4, 5, 6]

{-- Map --}

square :: Array Number -> Array Number
square arr = pow2 <$> arr
  where pow2 n = pow n 2.0

{-- rectify :: Array Number -> Array Number --}
{-- rectify = filter (\i -> i >= 0.0) --}

infix 1 filter as <$?>

rectifyFirst :: Array Number -> Number
rectifyFirst arr = (unsafePartial head) $ (\i -> i >= 0.0) <$?> arr

{-- Monad compreension --}

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]


isPrime :: Int -> Boolean
isPrime n = (length $ factors n) <= 1

cartesian :: Array Int -> Array Int -> Array (Array Int)
cartesian arr1 arr2 = do
  i <- arr1
  j <- arr2
  pure [i, j]

{-- pairs n = concatMap (\i -> map (\j -> [i, j]) (i .. n)) (1 .. n) --}

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i * i + j * j == k * k
  pure [i, j, k]

{-- You got to be kidding me... That is really hard! --}
{-- fact :: Int -> Array (Array Int) --}
{-- fact n = do --}
{--   i <- 1 .. n --}
{--   guard $ n `mod` i == 0 && n / i > i --}
{--   pure [i, n / i] --}


{-- reverse from foldl --}
reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [x] <> xs) []

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true


onlyFiles :: Path -> Array Path
onlyFiles file = filter (not isDirectory) $ file : concatMap onlyFiles (ls file)


largest :: Path
largest = foldl (\acc el -> if size(acc) < size(el) then el else acc) root $ onlyFiles root

smallest :: Path
smallest = foldl (\acc el -> if size(acc) > size(el) then el else acc) largest $ onlyFiles root

{-- this is not optimal... --}
{-- whereIs :: Path -> Maybe Path --}
{-- whereIs path = do --}
{--   p <- ls path --}
{--   guard $ foldl (\acc x -> path == x && acc) false $ onlyFiles path --}
{--   path --}


