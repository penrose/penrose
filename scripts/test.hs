{-# LANGUAGE RankNTypes #-}
import Debug.Trace

-- Prints out two "hello"s
data Translation = Trans { m :: forall a . Floating a => a }

g :: Floating a => a -> a
g x = x + 1

f :: Floating a => a -> a
f x = trace "hello" $ x - 2.0

poly :: Float -> (forall a . Floating a => a)
poly x = realToFrac x

-- Only one "hello"
-- data Translation = Trans { m :: Float }
--
-- f :: Float -> Float
-- f x = trace "hello" $ x - 2.0

main :: IO ()
main = do
    let y = f 1.5 :: Float
    let trans = Trans { m = poly y }
    -- let trans = Trans { m = f 1.5 }
    putStrLn $ show $ m trans
    putStrLn $ show $ m trans
