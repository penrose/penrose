{-# LANGUAGE RankNTypes #-}

import Numeric.AD 
import Numeric.AD.Internal.Reverse
-- import Numeric.AD.Rank1.Forward

-- compiles but I can't figure out how to use it in code
grad2 :: (Show a, Num a, Floating a) => (forall s.[Reverse s a] -> Reverse s a) -> [a] -> [a]
grad2 f l = grad f l

-- compiles with the right type, but the resulting gradient is all 0s...
grad2' :: (Show a, Num a, Floating a) => (forall a.[a] -> a) -> [a] -> [a]
grad2' f l = grad f' l
       where f' = Lift . f . extractAll
       -- i've tried using the Reverse constructor with Reverse 0 _, Reverse 1 _, and Reverse 2 _, but those don't yield the correct gradient. Not sure how the modes work

extractAll :: [Reverse t a] -> [a]
extractAll xs = map extract xs
           where extract (Lift x) = x -- non-exhaustive pattern match

dist :: (Show a, Num a, Floating a) => [a] -> a
dist [x, y] = sqrt(x^2 + y^2)

l :: (Show a, Num a, Floating a) => [a]
l = [1, 2]

-- incorrect output: [0.0, 0.0]
main = print $ grad2' dist l
