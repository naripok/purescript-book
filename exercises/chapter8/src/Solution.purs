module Solution where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Array (head, tail, sort, nub)
import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (foldM)
import Data.List (List(..), filterM, (:))
import Effect.Exception (throwException, error)

--

third :: forall a. Array a -> Maybe a
third arr = do
  f <- tail arr
  s <- tail f
  t <- head s
  pure t


sums :: Array Int -> Array Int
sums = sort <<< nub <<< foldM (\acc x -> [acc, acc + x]) 0


maybeAp :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
maybeAp mf ma = do
  f <- mf
  a <- ma
  pure (f a)


maybeApply :: forall a b. Maybe (a -> b) -> Maybe a -> Maybe b
maybeApply (Just f) (Just x) = Just (f x)
maybeApply _        _        = Nothing


{-- filter :: forall a. (a -> Boolean) -> Array a -> Array a --}
filterM' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM' f (x : xs) = f x >>= \z -> filterM f xs >>= \xs' -> pure (if z then x : xs' else xs')
filterM' _ Nil = pure Nil

filterMDo' :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterMDo' _ Nil = pure Nil
filterMDo' f (x : xs) = do
  z <- f x
  xs' <- filterM f xs
  pure (if z then x : xs' else xs')


{-- Monad laws --}
{-- right-identity --}
{-- expr = do --}
{--   x <- expr --}
{--   pure x --}
{--  --}
{-- left-identity --}
{-- next y = do --}
{--   x <- pure y --}
{--   next --}
{--  --}
{-- associativity law --}
{-- c1 = do --}
{--   y <- do --}
{--      x <- m1 --}
{--      m2 --}
{--   m3 --}
{--  --}
{-- c2 = do --}
{--   x <- m1 --}
{--   y <- m2 --}
{--   m3 --}
{--  --}
{-- c1 == c2 --}

{-- given --}
{-- functor monad --}
{-- map f a = do --}
{--   x <- a --}
{--   pure (f x) --}

{-- and --}
{-- lift2 :: forall f a b c. Applicative f => (a -> b -> c) -> f a -> f b -> f c --}
{-- lift2 f a b = f <$> a <*> b --}

{-- prove that the following holds: --}
{-- lift2 f (pure a) (pure b) = pure (f a b) --}

{-- given that --}

{-- map :: forall a b f. Functor f => (a -> b) -> f a -> f b --}
{-- map f m = m >>= \x -> pure (f x) --}

{-- apply :: forall a b. f (a -> b) -> f a -> f b --}

{-- lift 2 f a b = f <$> a <*> b --}

{-- lift 2 f (pure a) (pure b) = f <$> (pure a) <*> (pure b) --}
{--                            = pure (f a) <*> (pure b) --}
{--                            = pure (f a b) --}


--

{-- safeDiv :: Int -> Int -> Maybe Int --}
{-- safeDiv _ 0 = Nothing --}
{-- safeDiv a b = Just (a / b) --}

safeDiv :: Int -> Int -> Effect Int
safeDiv _ 0 = throwException $ error "Division By Zero"
safeDiv a b = pure (a / b)

--



--

main :: Effect Unit
main = do
  log $ show $ third [1,2,3,4,5]
  log $ show $ third [1,2]
  log $ show $ (maybeAp (Just ((+) 1)) (Just 2)) == (maybeApply (Just ((+) 1)) (Just 2))
  log $ show $ (maybeAp (Just ((+) 1)) (Just 2)) == (maybeApply (Just ((+) 1)) (Just 1))
  log $ show $ (maybeAp (Just ((+) 1)) Nothing)
  log $ show $ (maybeApply (Just ((+) 1)) Nothing)
