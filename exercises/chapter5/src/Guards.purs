module Data.Guards where

import Prelude
import Data.Foldable (foldl)
import Data.Array ((..))
import Effect.Class.Console (log)

{-- factorial :: Int -> Int --}
{-- factorial 0 = 1 --}
{-- factorial 1 = 1 --}
{-- factorial n = foldl (*) 1 (1 .. n) --}

{-- max representable int32 = 2147483647 --}
{--   overflow otherwise --}
factorial :: Int -> Int
factorial 0 = 1
factorial n | n > 1 = n * factorial (n - 1)
            | otherwise = 1

binomial :: Int -> Int -> Int
binomial n k = (factorial n) `div` ((factorial k) * factorial (n - k))

{-- pascal :: Int -> Int -> Int --}
{-- pascal n k =  --}
{-- what??? maybe I'm too dumb for this.... --}
