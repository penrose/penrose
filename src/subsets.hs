import Data.List

-- ordering? here order does NOT matter
-- abc
-- a, b, c
-- ab, bc, ac
-- abc only

-- abcd
-- a, b, c, d
-- ab, ac, ad, bc, bd, ad
-- abc, abd, acb, a
subsetsOfSize :: Int -> [a] -> [[a]]
subsetsOfSize 0 _ = []
subsetsOfSize 1 l = map (\x -> [x]) l
subsetsOfSize n l = 
              where prev = subsetsOfSize (n-1) l



main = putStrLn "hello"
