data FnType a = Type1 (a -> String) | Type2 (Int -> Int)

f :: Show a => a -> String
f x = show x

g :: Int -> Int
g x = x + 3

-- Do I have more information about whether there is eventually a uniform return type???
-- Or can I define a limited type for computations?
lookup :: Show a => String -> FnType a
lookup "g" = Type2 g
lookup "f" = Type1 f
