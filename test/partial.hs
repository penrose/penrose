{-# LANGUAGE RankNTypes #-}

-- | Possibilities:
-- vstate poly?
-- vstate update record

-- now we know that the performance *is* tied to the hellos
-- if vstate is [Float], the extra hellos go away

import Debug.Trace

-- Not sure how to get rid of the record
data State = State {
    -- vstate :: Float
    vstate :: forall a . (Fractional a) => a
}

step :: State -> State
step s =
    -- s { vstate = trace "hello" (vstate s) `seq` vstate s } -- many hellos

    -- let vs = trace "hello" (vstate s) in
    -- s { vstate = vs `seq` vstate s } -- one hello
    --
    s { vstate = vstate s `seq` vstate s } -- many hellos

    -- let vs = vstate s in
    -- s { vstate = vs `seq` vstate s } -- one hello

main :: IO ()
main = do
    let initState = State { vstate = 0 }
    let res = iterate step initState
        -- step $ step $ step initState
    print $ vstate $ last $ take 10000 res
    print "done"
