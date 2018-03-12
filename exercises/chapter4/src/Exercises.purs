module Exercises where

import Data.Array (null)
import Data.Array.Partial (head, tail)
import Partial.Unsafe (unsafePartial)
import Prelude

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

