import Debug.Trace

weirdFn :: Floating a => a -> a
weirdFn z = trace "eval weirdFn" $ z - 3.5

f :: Floating a => a -> a -> a
f x = \y -> x + y

outer :: Floating a => (a, a)
outer =
    let -- partialRes = weirdFn 0.0
        g = f (weirdFn 0.0)
        res1 = g 1.0
        res2 = g 2.0 in
    (res1, res2)

main :: IO ()
main = do putStrLn "hello"
          putStrLn $ show outer
