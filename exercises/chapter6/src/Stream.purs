module Data.Stream where

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(Nothing, Just))
import Data.String.CodeUnits as String
{-- import Effect.Class.Console (log) --}

class Stream stream element where
  uncons :: stream -> Maybe {head :: element, tail :: stream}

instance streamArray :: Stream (Array a) a where
  uncons = Array.uncons

instance streamString :: Stream String Char where
  uncons = String.uncons

instance streamMaybe :: Stream (Maybe a) a where
  uncons Nothing = Nothing
  uncons (Just a) = Just { head: a, tail: Nothing }

foldStream :: forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
foldStream f list =
  case uncons list of
       Nothing -> mempty
       Just cons -> f cons.head <> foldStream f cons.tail

s1 :: String
s1 = "some"

intToString :: Int -> String
intToString n = show n

maybeIntToString :: Maybe Int -> String
maybeIntToString stream = foldStream intToString stream

{-- main = do --}
  -- To help you figure out:
  -- forall l e m. Stream l e => Monoid m => (e -> m) -> l -> m
  -- Replace l with Maybe Int, e with Int, m with String
  -- Stream (Maybe Int) Int => Monoid String => (Int -> String) -> Maybe Int -> String
  -- Conditions:
  -- - Stream (Maybe Int) Int, there's a class instance for Stream (Maybe Int) Int, hurray
  -- - Monoid String, there's a class instance for Monoid String, hurray
  -- Function signature: (Int -> String) -> Maybe Int -> String
  -- - intToStream is Int -> String, hurray
  -- - Just 5 is Maybe Int, hurray
  -- - so result will be String
  {-- let result = foldStream intToString (Just 5) --}
  {-- log result --}
