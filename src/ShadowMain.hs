-- | Main module of the Penrose system (split out for testing; Main is the real main)

{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, DeriveDataTypeable #-}

module ShadowMain where
import Utils
import qualified Server
import qualified Runtime as R
import qualified Substance as C
import qualified NewStyle as NS -- COMBAK: remove
import qualified Optimizer as O -- COMBAK: remove
import qualified Style as S
import qualified Dsll as D
import qualified Text.Megaparsec as MP (runParser, parseErrorPretty)
import System.Environment
import System.IO
import System.Exit
import Debug.Trace
import Text.Show.Pretty
import Control.Monad (when, forM)
import qualified Env as E -- DEBUG: remove
import qualified Data.Map.Strict as M -- DEBUG: remove

fromRight a (Left x) = a
fromRight _ (Right a) = a

-- | `main` runs the Penrose system
shadowMain :: IO ()
shadowMain = do
    -- Reading in from file
    -- Objective function is currently hard-coded
    -- Comment in (or out) this block of code to read from a file (need to fix parameter tuning!)
    args <- getArgs
    when (length args /= 3) $ die "Usage: ./Main prog1.sub prog2.sty prog3.dsl"
    let (subFile, styFile, dsllFile) = (head args, args !! 1, args !! 2)
    subIn  <- readFile subFile
    styIn  <- readFile styFile
    dsllIn <- readFile dsllFile
    putStrLn "\nSubstance program:\n"
    putStrLn subIn
    divLine
    putStrLn "Style program:\n"
    putStrLn styIn
    divLine
    putStrLn "DSLL program:\n"
    putStrLn dsllIn
    divLine

    dsllEnv <- D.parseDsll dsllFile dsllIn
    divLine
    -- putStrLn "Dsll Env program:\n"
    -- print dsllEnv

    (subProg, (subEnv, eqEnv), labelMap) <- C.parseSubstance subFile subIn dsllEnv
    divLine

    putStrLn "Parsed Substance program:\n"
    pPrint subProg
    divLine

    putStrLn "Substance type env:\n"
    pPrint subEnv
    divLine

    putStrLn "Substance dyn env:\n"
    pPrint eqEnv
    divLine

    putStrLn "Label mappings:\n"
    pPrint labelMap
    divLine

    styProg <- NS.parseStyle styFile styIn
    putStrLn "Style AST:\n"
    pPrint styProg
    divLine

    putStrLn "Running Style semantics\n"
    let selEnvs = NS.checkSels subEnv styProg
    putStrLn "Selector static semantics and local envs:\n"
    forM selEnvs pPrint
    divLine

    let subss = NS.find_substs_prog subEnv eqEnv subProg styProg
    putStrLn "Selector matches:\n"
    forM subss pPrint
    divLine

    let trans = NS.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [NS.Error] (NS.Translation a)
    putStrLn "Translated Style program:\n"
    pPrint trans
    divLine

    let initState = NS.genOptProblemAndState (fromRight NS.initTrans trans)
    putStrLn "Generated initial state:\n"

    -- TODO improve printing code
    -- putStrLn "Shapes:"
    -- pPrint $ NS.shapesr initState
    -- putStrLn "\nShape names:"
    -- pPrint $ NS.shapeNames initState
    -- putStrLn "\nShape properties:"
    -- pPrint $ NS.shapeProperties initState
    -- putStrLn "\nTranslation:"
    -- pPrint $ NS.transr initState
    -- putStrLn "\nVarying paths:"
    -- pPrint $ NS.varyingPaths initState
    -- putStrLn "\nUninitialized paths:"
    -- pPrint $ NS.uninitializedPaths initState
    -- putStrLn "\nVarying state:"
    -- pPrint $ NS.varyingState initState
    -- putStrLn "\nParams:"
    -- pPrint $ NS.paramsr initState
    -- putStrLn "\nAutostep:"
    -- pPrint $ NS.autostep initState
    -- print initState
    -- divLine

    putStrLn "Visualizing Substance program:\n"

    -- Step without server: test time taken to converge
    -- let finalState = stepsWithoutServer initState
    -- pPrint finalState

    -- Starting serving penrose on the web
    let (domain, port) = ("127.0.0.1", 9160)
    Server.servePenrose domain port initState

-- Versions of main for the tests to use that takes arguments internally, and returns initial and final state
-- (extracted via unsafePerformIO)
-- Very similar to shadowMain but does not depend on rendering  so it does not return SVG
-- TODO take initRng seed as argument
mainRetInit :: String -> String -> String -> IO (Maybe NS.RState)
mainRetInit subFile styFile dsllFile = do
    subIn <- readFile subFile
    styIn <- readFile styFile
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn
    (subProg, (subEnv, eqEnv), labelMap) <- C.parseSubstance subFile subIn dsllEnv
    styProg <- NS.parseStyle styFile styIn
    -- let initState = R.genInitState styProg
    let selEnvs = NS.checkSels subEnv styProg
    let subss = NS.find_substs_prog subEnv eqEnv subProg styProg
    let trans = NS.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [NS.Error] (NS.Translation a)
    let initState = NS.genOptProblemAndState (fromRight NS.initTrans trans)
    return $ Just initState

-- stepsWithoutServer :: NS.RState -> NS.RState
-- stepsWithoutServer initState =
--          let (finalState, numSteps) = head $ dropWhile notConverged $ iterate stepAndCount (initState, 0) in
--          trace ("\nnumber of outer steps: " ++ show numSteps) $ finalState
--          where stepAndCount (s, n) = traceShowId (O.step s, n + 1)
--                notConverged (s, n) = NS.optStatus (NS.paramsr s) /= NS.EPConverged
--                                      || n < maxSteps
--                maxSteps = 10 ** 3 -- Not sure how many steps it usually takes to converge
