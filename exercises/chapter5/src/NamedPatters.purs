module Data.NamedPatterns where

import Prelude

type Person = {city :: String}

rec1 :: Person
rec1 = {city: "New York"}
rec2 :: Person
rec2 = {city: "New York"}
rec3 :: Person
rec3 = {city: "São Paulo"}

sameCity :: Person -> Person -> Boolean
sameCity {city: x} {city: y} = x == y

{-- sameCity row polimorphic type --}
{-- forall t10 t5 t8. --}
{--   Eq t10 => { city :: t10 --}
{--             | t5 --}
{--             } --}
{--             -> { city :: t10 --}
{--                | t8 --}
{--                } --}
{--                -> Boolean --}

{-- livesInLA row polimorphic type --}
{-- forall t11 t14. --}
{--   { address :: { city :: String --}
{--                | t14 --}
{--                } --}
{--   | t11 --}
{--   } --}
{--   -> Boolean --}

fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [a] = a
fromSingleton a _ = a
