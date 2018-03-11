module Chapter2 where

import Prelude

import Math (pi, sqrt)

-- main = log "Hello, World!"

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea x = (x * x) * pi

-- Ex 2
-- bower install purescript-globals --save
-- pulp repl
-- :browse
-- :browse Global   or :browse Global.Unsafe
-- import Global.Unsafe
-- unsafeStringify 5