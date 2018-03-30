module Exercises where

import Prelude

import Control.MonadZero (guard)
import Data.Array (filter, null, (..), concat, (:))
import Data.Array.Partial (head, tail)
import Data.FoldableWithIndex (allWithIndex)
import Data.Functor (voidRight)
import Partial.Unsafe (unsafePartial)
import Control.MonadPlus

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n = isEven (mod n 2)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else (1 + (length (unsafePartial tail arr)))

countEven :: Array Int -> Int
countEven arr = 
    if null arr
        then 0
        else if isEven(unsafePartial head arr)
            then (1 + countEven(unsafePartial tail arr))
            else (0 + countEven(unsafePartial tail arr))

-- part b
squares :: Array Int -> Array Int
squares numbers = map (\n -> n * n) numbers

noNegative :: Array Int -> Array Int
noNegative numbers = filter (\n -> n >= 0) numbers

-- (Medium) Define an infix synonym <$?> for filter. Rewrite your answer to the previous question to use your new operator. Experiment with the precedence level and associativity of your operator in PSCi.
infix 8 filter as <$?>

noNegativeInf :: Array Int -> Array Int
noNegativeInf ns = (\n -> true) <$?> ns

factors :: Int -> Array (Array Int)
factors n = do
  i <- (1 .. n)
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime n = length (factors n) == 1

catesian :: Array Int -> Array Int -> Array (Array Int)
catesian a b = do
    x <- a
    y <- b
    pure [x, y]

triples :: Int -> Array (Array Int)
triples n = do
  a <- (1..n)
  b <- (1..n)
  c <- (1..n)
  guard $ (a * a) + (b * b) == c * c 
  pure [a, b, c]
