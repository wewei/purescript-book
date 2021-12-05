module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Person, Volt(..))
import Data.Maybe (Maybe(..))
import Data.Picture (Bounds, Picture, Point, Shape(..), intersect, bounds)
import Data.Picture as P
import Math (pi)

factorial :: Int -> Int
factorial = fac 1
  where
    fac :: Int -> Int -> Int
    fac k 0 = k
    fac k n = fac (k * n) (n - 1)

binomial :: Int -> Int -> Int
binomial n k
  | k < 0 || n < k = 0
  | 2 * k > n = binomial n (n - k)
  | otherwise = (fac 1 n (n - k)) `div` (fac 1 k 0)
    where
      fac r p q
        | p == q = r
        | otherwise = fac (r * p) (p - 1) q

pascal :: Int -> Int -> Int
pascal n k
  | k < 0 || n < k = 0
  | 2 * k > n = pascal n (n - k)
  | k == 0 || k == n = 1
  | otherwise = pascal (n - 1) k + pascal (n - 1) (k - 1)

sameCity :: Person -> Person -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } } = c1 == c2

fromSingleton :: forall a . a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _   = x

circleAtOrigin :: Shape
circleAtOrigin = Circle ({ x: 0.0, y: 0.0 }) 0.0

scaleShape :: Number -> Shape -> Shape
scaleShape s (Circle c r) = Circle c (r * s)
scaleShape s (Rectangle o w h) = Rectangle o (w * s) (h * s)
scaleShape s (Line f t) = Line f (f + (t - f) * { x: s, y: s})
scaleShape _ t = t

centerShape :: Shape -> Shape
centerShape (Circle _ r) = Circle { x: 0.0, y: 0.0 } r
centerShape (Rectangle _ w h) = Rectangle { x: 0.0, y: 0.0 } w h
centerShape (Line { x: x1, y: y1 } { x: x2, y: y2 }) =
  Line { x: (x1 - x2) / 2.0, y: (y1 - y2) / 2.0 } { x: (x2 - x1) / 2.0, y: (y2 - y1) / 2.0 }
centerShape (Text _ s) = Text { x: 0.0, y: 0.0 } s

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = centerShape <<< scaleShape 2.0

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

newtype Watt = Watt Number
calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt (a * v)

area :: Shape -> Number
area (Circle _ r) = pi * r * r 
area (Rectangle _ w h) = w * h
area _ = 0.0

data ShapeExt
  = Clipped Picture Point Number Number
  | Shape Shape

shapeBounds :: ShapeExt -> Bounds
shapeBounds (Clipped p { x, y } w h) = intersect (bounds p)
  { left: x - w / 2.0, top: y - h / 2.0, right: x + w / 2.0, bottom: y + h / 2.0 }
shapeBounds (Shape s) = P.shapeBounds s