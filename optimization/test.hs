-- TODO ask about Haskell's function behavior on typeclasses
-- e.g. adding a Floating with a Float produces a Float (or whatever more specific type)
-- not the more general type
-- assuming + is supported by Floating, why shouldn't it return the more general type?

type Time a = a

-- f :: (Floating a, Eq a, Ord a, Show a) => Time a -> Float -> Float
f x y = sqrt $ x^2 + y^2

a :: (Floating a, Eq a, Ord a, Show a) => Time a
a = 2

b :: Float
b = 5

main = print $ f a b
