{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}

import Debug.Trace

type Autofloat a = (RealFloat a, Floating a, Real a, Show a, Ord a)
type Objfn a = forall a . (Autofloat a) => a -> a

data State = State {
    objFn :: forall a . (Autofloat a) => a -> a,
    vstate :: forall a . (Autofloat a) => [a]
}

instance Show State where
    show p = "this is a state\n" ++ show (vstate p)

genObjFn :: (Autofloat a) => Int -> a -> a
genObjFn a =
    \x ->
       (\y ->  trace "hello" 0.0) 0.0

stepCount :: (State, Int) -> (State, Int)
stepCount (s, n) = traceShowId (step s, n + 1)

step :: State -> State
step s = ((objFn s) $ head (vstate s)) `seq` s

--s { vstate = fst $ step' (objfn s) (vstate s) }

-- step' :: (Autofloat a) => State -> [a] -> ([a], State)
-- step' state vstate = ((objFn state) $ head vstate) `seq` (vstate, state)

main :: IO ()
main = do
    let fn = genObjFn 1000
    let vs = [100.0, 100.0, 100.0]
    let initState = State { objFn = fn, vstate = vs }
    let res = fst $ stepCount $ stepCount $ stepCount $ stepCount (initState, 0)

    print res
    print "done!"
