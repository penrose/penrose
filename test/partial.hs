{-# LANGUAGE RankNTypes #-}

-- | Possibilities:
-- vstate poly?
-- vstate update record
-- vstate show
-- fn app with vstate
-- now we know that the performance *is* tied to the hellos
-- if vstate is [Float], the extra hellos go away

import Debug.Trace

-- Not sure how to get rid of the record
data State = State {
    vstate :: forall a . (Floating a) => [a]
}

instance Show State where
    show p = "this is a state\n" ++ show (vstate p)

fn :: Float -> Float
fn a = trace "hello" 0.0

step :: State -> State
step s = traceShowId (s { vstate = fn 100.0 `seq` vstate s })

main :: IO ()
main = do
    let initState = State { vstate = [100.0] }
    let res = step $ step $ step $ step initState
    print res
    print "done!"
