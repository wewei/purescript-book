module Test.MySolutions where

import Data.Path
import Prelude

import Control.Alt ((<|>))
import Control.Alternative (guard)
import Data.Array (any, concatMap, filter, foldl, head, length, tail, (..), (:))
import Data.Foldable (maximumBy, minimumBy)
import Data.Maybe (Maybe(..), fromMaybe)
import Test.Examples (factors)

-- Note to reader: Add your solutions to this file
isEven :: Int -> Boolean
isEven n = n `mod` 2 == 0

countEven :: Array Int -> Int
countEven = cnt 0 where
  cnt :: Int -> Array Int -> Int
  cnt c [] = c
  cnt c arr
    = let x = fromMaybe 1 <<< head $ arr
          y = if isEven x then 1 else 0
      in cnt (c + y) <<< fromMaybe [] <<< tail $ arr

squared :: Array Number -> Array Number
squared = map \x -> x * x

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 5 filter as <$?>
keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = ((_ >= 0.0) <$?> _)

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard (a * a + b * b == c * c)
  pure [a, b, c]

primeFactors :: Int -> Array Int
primeFactors n
  | n <= 1 = []
  | otherwise = factorial 2
    where
      factorial p =
        if (n `mod` p == 0)
        then [p] <> primeFactors (n `div` p)
        else factorial (p + 1)

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec = fibTR 0 1
  where
    fibTR a _ 0 = a
    fibTR a b n = fibTR b (a + b) (n - 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> x : xs) []

onlyFiles :: Path -> Array Path
onlyFiles p
  | isDirectory p = concatMap onlyFiles (ls p)
  | otherwise = pure p

whereIs :: Path -> String -> Maybe Path
-- whereIs p n = findIn p
--   where
--     findIn :: Path -> Maybe Path
--     findIn q =
--       let
--         matched  = (==) fullname <<< filename
--         fullname = filename q <> n
--       in case q of
--         _ | any matched (ls q) -> Just q
--           | otherwise -> foldl (<|>) Nothing (map findIn <<< ls $ q)
whereIs p n = head $ findIn p
  where
    findIn q = do
      ch <- ls q
      if (filename ch == filename q <> n)
      then pure q
      else findIn ch

largestSmallest :: Path -> Array Path
largestSmallest = foldl step [] <<< onlyFiles
  where
    step [largest, smallest] current
      | size current > size largest = [current, smallest]
      | size current < size smallest = [largest, current]
      | otherwise = [largest, smallest]
    step [last] current
      | size current > size last = [current, last]
      | otherwise = [last, current]
    step _ current = [current]