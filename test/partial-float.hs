import Debug.Trace

data Param = Param {
    objFn :: Float -> Float
}
instance Show Param where
    show p = "this is an obj fn"

data State = State {
    param  :: Param,
    vstate :: [Float]
} deriving Show

genObjFn :: Float -> (Float -> Float)
genObjFn a =
    \x ->
       (\y ->  trace "hello" 0.0) 0.0

stepCount :: (State, Int) -> (State, Int)
stepCount (s, n) = traceShowId (step s, n + 1)

step :: State -> State
step s = s { vstate = fst $ step' (param s) (vstate s) }

step' :: Param -> [Float] -> ([Float], Param)
step' param vstate = ((objFn param) $ head vstate) `seq` (vstate, param)

main :: IO ()
main = do
    let fn = genObjFn 1000
    let vs = [100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0,100.0]
    let initState = State { param = Param { objFn = fn }, vstate = vs }
    let res = fst $ stepCount $ stepCount $ stepCount $ stepCount $ stepCount $ stepCount $ stepCount $ stepCount $ (initState, 0)

    print res
    print "done!"
