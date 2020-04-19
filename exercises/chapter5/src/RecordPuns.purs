module Data.RecordPuns where

import Prelude
import Data.Maybe (Maybe(..))
import Math (pi, pow)

-- type: just an alias
type Radius = Number
-- newtype, create exactly one new type constructor, gets optimized at compile time
newtype Point = Point {x :: Number, y :: Number}
-- data create one or more type constructors that shares the same type
data Shape = Circle Point Radius
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  {-- | Clipped Picture --}

origin :: Point
origin = Point {x, y}
  where
    x = 1.0
    y = 0.0

circle :: Shape
circle = Circle o r
  where
    o = origin
    r = 10.0

text :: Shape
text = Text origin "WOW!"

showPoint :: Point -> String
showPoint (Point {x, y})= show x <> ", " <> show y

showShape :: Shape -> String
showShape (Circle o r) = "o: " <> showPoint o <> ", r: " <> show r
showShape (Rectangle _ _ _) = ""
showShape (Line p1 p2) = "p1: " <> showPoint p1 <> ", p2: " <> showPoint p2
showShape (Text o t) = "o: " <> showPoint o <> ", r: " <> t

scaleCenterShape :: Shape -> Number -> Shape
scaleCenterShape (Circle _ r) n = Circle o (n * r)
  where
    o = Point {x: 0.0, y: 0.0}
scaleCenterShape (Rectangle _ l1 l2) n = Rectangle o (n * l1) (n * l2)
  where
    o = Point {x: 0.0, y: 0.0}
scaleCenterShape (Text _ t) _ = Text o t
  where
    o = Point {x: 0.0, y: 0.0}
scaleCenterShape (Line p1 p2) _ = Line p1 p2


extractText :: Shape -> Maybe String
extractText (Text _ t) = Just t
extractText _ = Nothing


type Picture = Array Shape

showPicture :: Picture -> Array String
showPicture = map showShape

pic :: Picture
pic = [
  Line
    (Point {x: 1.0, y: 0.0})
    (Point {x: 2.0, y: 0.0}),
  Line
    (Point {x: 3.0, y: 0.0})
    (Point {x: 2.0, y: 0.0})
  ]

area :: Shape -> Number
area (Circle _ r) = pi * (pow r 2.0)
area (Rectangle _ l1 l2) = l1 * l2
area _ = 0.0

