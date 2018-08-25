-- | Main module of the Penrose system (split out for testing; Main is the real main)

{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, DeriveDataTypeable #-}

module ShadowMain where
import Utils
import qualified Server
import qualified Substance as C
import qualified Style as S
import qualified Optimizer as O
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
import qualified Data.List as L (intercalate)
import           System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)

fromRight :: (Show a, Show b) => Either a b -> b
fromRight (Left x) = error ("Failed with error: " ++ show x)
fromRight (Right y) = y

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

    styProg <- S.parseStyle styFile styIn
    putStrLn "Style AST:\n"
    pPrint styProg
    divLine

    putStrLn "Running Style semantics\n"
    let selEnvs = S.checkSels subEnv styProg
    putStrLn "Selector static semantics and local envs:\n"
    forM selEnvs pPrint
    divLine

    let subss = S.find_substs_prog subEnv eqEnv subProg styProg selEnvs
    putStrLn "Selector matches:\n"
    forM subss pPrint
    divLine

    let trans = S.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [S.Error] (S.Translation a)
    putStrLn "Translated Style program:\n"
    pPrint trans
    divLine

    let initState = S.genOptProblemAndState (fromRight trans)
    putStrLn "Generated initial state:\n"

    -- TODO improve printing code
    putStrLn "Shapes:"
    pPrint $ S.shapesr initState
    putStrLn "\nShape names:"
    pPrint $ S.shapeNames initState
    putStrLn "\nShape properties:"
    pPrint $ S.shapeProperties initState
    putStrLn "\nTranslation:"
    pPrint $ S.transr initState
    putStrLn "\nVarying paths:"
    pPrint $ S.varyingPaths initState
    putStrLn "\nUninitialized paths:"
    pPrint $ S.uninitializedPaths initState
    putStrLn "\nVarying state:"
    pPrint $ S.varyingState initState
    putStrLn "\nParams:"
    pPrint $ S.paramsr initState
    putStrLn "\nAutostep:"
    pPrint $ S.autostep initState
    print initState
    divLine

    putStrLn (bgColor Cyan $ style Italic "   Style program warnings   ")
    let warns = S.warnings $ fromRight trans
    putStrLn (color Red $ L.intercalate "\n" warns ++ "\n")

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
mainRetInit :: String -> String -> String -> IO (Maybe S.RState)
mainRetInit subFile styFile dsllFile = do
    subIn <- readFile subFile
    styIn <- readFile styFile
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn
    (subProg, (subEnv, eqEnv), labelMap) <- C.parseSubstance subFile subIn dsllEnv
    styProg <- S.parseStyle styFile styIn
    -- let initState = R.genInitState styProg
    let selEnvs = S.checkSels subEnv styProg
    let subss = S.find_substs_prog subEnv eqEnv subProg styProg selEnvs
    let trans = S.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [S.Error] (S.Translation a)
    let initState = S.genOptProblemAndState (fromRight trans)
    return $ Just initState

stepsWithoutServer :: S.RState -> S.RState
stepsWithoutServer initState =
         let (finalState, numSteps) = head $ dropWhile notConverged $ iterate stepAndCount (initState, 0) in
         trace ("\nnumber of outer steps: " ++ show numSteps) $ finalState
         where stepAndCount (s, n) = traceShowId (O.step s, n + 1)
               notConverged (s, n) = S.optStatus (S.paramsr s) /= S.EPConverged
                                     || n < maxSteps
               maxSteps = 10 ** 3 -- Not sure how many steps it usually takes to converge
