{-# LANGUAGE Rank2Types, UnicodeSyntax #-}
-- A minimal example of using the autodiff library `ad`.

-- Mistakes I made in using ad:
-- -1. don't need to use the internal Reverse representation; typeclass will suffice
-- 0. we're passing in a polymorphic function
-- 1. therefore, we need rank 2 types
-- 2. the quantified a in a forall is separate, so it needs its own Floating typeclass constraint
-- 3. `grad` returns a list
-- https://stackoverflow.com/questions/41330431/minimal-numeric-ad-example-wont-compile/41335652#41335652

import Numeric.AD

dist :: Floating a => [a] -> a
dist [x, y] = sqrt $ x^2 + y^2

l :: [Double]
l = [1,2]

app :: Floating a => (forall a . Floating a => [a] -> a) -> [a] -> [a]
app f l = grad f l

main = print $ app dist l
