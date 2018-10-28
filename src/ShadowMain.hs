-- | Main module of the Penrose system (split out for testing; Main is the real main)

{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, DeriveDataTypeable, OverloadedStrings #-}

module ShadowMain where
import Utils
import qualified Server
import qualified Substance as C
import qualified Control.Concurrent as CC
import qualified Style as S
import qualified GenOptProblem as G
import qualified Optimizer as O
import qualified Dsll as D
import qualified Text.Megaparsec as MP (runParser, parseErrorPretty)
import System.Environment
import System.IO
import System.Exit
import Debug.Trace
import Text.Show.Pretty
import Web.Scotty
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B 
import Control.Monad (when, forM)
import Control.Monad.Trans
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
    when ((length args /= 3) && (length args /= 2)) $ die "Usage: ./Main prog1.sub prog2.sty prog3.dsl"
    case (length args) of
        3 -> do
            let (subFile, styFile, dsllFile) = (head args, args !! 1, args !! 2)
            subIn <- readFile subFile
            styIn  <- readFile styFile
            dsllIn <- readFile dsllFile
            dsllEnv <- D.parseDsll dsllFile dsllIn
            (subProg, (subEnv, eqEnv), labelMap) <- C.parseSubstance subFile subIn dsllEnv
            styProg <- S.parseStyle styFile styIn
            let selEnvs = S.checkSels subEnv styProg
            let subss = S.find_substs_prog subEnv eqEnv subProg styProg selEnvs
            let trans = S.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [S.Error] (S.Translation a)
            let initState = G.genOptProblemAndState (fromRight trans)
            putStrLn (bgColor Cyan $ style Italic "   Style program warnings   ")
            let warns = S.warnings $ fromRight trans
            putStrLn (color Red $ L.intercalate "\n" warns ++ "\n")
            let (domain, port) = ("127.0.0.1", 9160)
            Server.servePenrose domain port initState
        2 -> do
            let (styFile, dsllFile) = (head args, args !! 1)
            styIn  <- readFile styFile
            dsllIn <- readFile dsllFile
            dsllEnv <- D.parseDsll dsllFile dsllIn
            styProg <- S.parseStyle styFile styIn
            scotty 3939 $
                post "/" $ do
                sub <- body
                let subIn = B.unpack sub
                _ <- liftIO (putStrLn subIn)
                (subProg, (subEnv, eqEnv), labelMap) <- liftIO (C.parseSubstance "" subIn dsllEnv)
                let selEnvs = S.checkSels subEnv styProg
                let subss = S.find_substs_prog subEnv eqEnv subProg styProg selEnvs
                let trans = S.translateStyProg subEnv eqEnv subProg styProg labelMap
                                    :: forall a . (Autofloat a) => Either [S.Error] (S.Translation a)

                let initState = G.genOptProblemAndState (fromRight trans)
                let warns = S.warnings $ fromRight trans
                -- Starting serving penrose on the web
                let (domain, port) = ("127.0.0.1", 9160)
                _ <- liftIO $ CC.forkIO $ Server.servePenrose domain port initState
                text "127.0.0.1:9160"
                -- status status200


-- Versions of main for the tests to use that takes arguments internally, and returns initial and final state
-- (extracted via unsafePerformIO)
-- Very similar to shadowMain but does not depend on rendering  so it does not return SVG
-- TODO take initRng seed as argument
mainRetInit :: String -> String -> String -> IO (Maybe G.State)
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
    let initState = G.genOptProblemAndState (fromRight trans)
    return $ Just initState

stepsWithoutServer :: G.State -> G.State
stepsWithoutServer initState =
         let (finalState, numSteps) = head $ dropWhile notConverged $ iterate stepAndCount (initState, 0) in
         trace ("\nnumber of outer steps: " ++ show numSteps) $ finalState
         where stepAndCount (s, n) = traceShowId (O.step s, n + 1)
               notConverged (s, n) = G.optStatus (G.paramsr s) /= G.EPConverged
                                     || n < maxSteps
               maxSteps = 10 ** 3 -- Not sure how many steps it usually takes to converge
