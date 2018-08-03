{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}

import Debug.Trace

type Objfn = forall a . (Floating a, Show a) => a -> a
type Vstate = forall a . (Floating a, Show a) => [a]

-- instance Show State where
--     show p = "this is a state\n" ++ show (vstate p)

genObjFn :: (Floating a, Show a) => a -> a -> a
genObjFn a b = trace "hello" 0.0

f = genObjFn 1000

step :: Vstate -> Vstate
step v = traceShowId ((f 100.0) `seq` v)
     -- traceShowId (s { vstate = ((objFn s) $ 100.0) `seq` vstate s })

main :: IO ()
main = do
    let vs = [100.0]
    let initState = vs --State { objFn = fn, vstate = vs }
    let res = step $ step $ step $ step initState

    -- print $ vstate res
    print res
    print "done!"
