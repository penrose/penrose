{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
import Debug.Trace

data Dict c where Dict :: c => Dict c

-- An isomorphism between explicit dictionary-passing style (Dict c -> a)
-- and typeclass constraints (c => a) exists:
from :: (c => a) -> (Dict c -> a)
from v Dict = v

to :: (Dict c -> a) -> (c => a)
to f = f Dict

data Translation = Trans { m :: forall a . Floating a => a }

f1, f2 :: Dict (Floating a) -> a -> a
f1 = trace "hello" $ \Dict x -> x - 2.0
f2 = \Dict -> trace "hello" $ \x -> x - 2.0

main = do
    let trans1 = Trans { m = to (flip f1 1.5) }
        trans2 = Trans { m = to (flip f2 1.5) }
    putStrLn "trans1"
    print (m trans1)
    print (m trans1)
    putStrLn "trans2"
    print (m trans2)
    print (m trans2)
