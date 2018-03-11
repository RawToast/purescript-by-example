module Main where

import Control.Monad.Eff.Console (logShow)
import Math (sqrt, pi)
import Prelude ((+), (*))

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = (r * r) * pi

main = logShow (circleArea 4.2)
