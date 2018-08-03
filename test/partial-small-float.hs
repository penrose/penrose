import Debug.Trace

data State = State {
    objFn :: Float -> Float,
    vstate :: [Float]
}

instance Show State where
    show p = "this is a state\n" ++ show (vstate p)

genObjFn :: Float -> Float -> Float
genObjFn a b = trace "hello" 0.0

step :: State -> State
step s = traceShowId (s { vstate = ((objFn s) $ 100.0) `seq` vstate s })

main :: IO ()
main = do
    let fn = genObjFn 1000
    let vs = [100.0]
    let initState = State { objFn = fn, vstate = vs }
    let res = step $ step $ step $ step initState

    -- print $ vstate res
    print res
    print "done!"
