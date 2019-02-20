-- | Main module of the Penrose system (split out for testing; Main is the real main)
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, OverloadedStrings #-}

module ShadowMain where
import Utils
import Plugins
import qualified Server
import qualified Substance as C
import qualified Control.Concurrent as CC
import qualified Style as S
import qualified GenOptProblem as G
import qualified Optimizer as O
import qualified Dsll as D
import qualified Sugarer
import qualified Text.Megaparsec as MP (runParser, parseErrorPretty)
import System.Environment
import System.Process
import System.Directory
import System.IO
import Prelude hiding (catch)
import System.Directory
import Control.Exception
import System.IO.Error hiding (catch)
import System.Exit
import Debug.Trace
import Text.Show.Pretty
import Web.Scotty
import Network.HTTP.Types.Status
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad (when, forM_)
import Control.Monad.Trans
import qualified Env as E -- DEBUG: remove
import qualified Data.Map.Strict as M -- DEBUG: remove
import qualified Data.List.Split as LS (splitOn)
import qualified Data.List as L (intercalate)
import           System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
import System.Console.Docopt

argPatterns :: Docopt
argPatterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith argPatterns

-- | `shadowMain` runs the Penrose system
shadowMain :: IO ()
shadowMain = do
    args <- parseArgsOrExit argPatterns =<< getArgs
    styFile  <- args `getArgOrExit` argument "style"
    dsllFile <- args `getArgOrExit` argument "element"
    domain   <- args `getArgOrExit` longOption "domain"
    port     <- args `getArgOrExit` longOption "port"

    if args `isPresent` argument "substance" then do
        subFile <- args `getArgOrExit` argument "substance"
        penroseRenderer subFile styFile dsllFile domain $ read port
    else
        penroseEditor styFile dsllFile domain $ read port

penroseEditor :: String -> String -> String -> Int -> IO ()
penroseEditor styFile dsllFile domain port = do
    styIn  <- readFile styFile
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn

    styProg <- S.parseStyle styFile styIn dsllEnv
    putStrLn "Style AST:\n"
    pPrint styProg
    divLine

    Server.servePenrose dsllEnv styProg domain port

penroseRenderer :: String -> String -> String -> String -> Int -> IO ()
penroseRenderer subFile styFile dsllFile domain port = do
    subIn <- readFile subFile
    styIn  <- readFile styFile
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn

    -- Desugar Substance source file
    let subFileSugared = subFile ++ "desugared"
    writeFile subFileSugared (Sugarer.sugarStmts subIn dsllEnv)
    desugaredSub <- readFile subFileSugared
    subOut <- C.parseSubstance subFileSugared desugaredSub dsllEnv
    removeIfExists subFileSugared

    print subOut

    -- Find Substance instantiator plugin (if it exists in Style file + directory) and run it
    -- Don't forget to recompile the plugin!
    -- NOTE: we are not expecting multiple processes to use these tempfiles
    -- TODO: add more error checking to deal with paths or files that don't exist
    instantiations <- S.parsePlugins styFile styIn dsllEnv
    putStrLn $ "instantiation: " ++ (show instantiations)

    originalDir <- getCurrentDirectory
    let pluginName = "haskell-test" -- TODO: use user string from Style
    binPath <- makeAbsolute $ catchPathError pluginName (M.lookup pluginName plugins)
    let dirPath = cdUp binPath
    putStrLn $ "plugin bin path: " ++ binPath
    putStrLn $ "plugin dir path: " ++ dirPath
    let outFile = dirPath ++ "/Sub_enduser.sub"
    let inFile = dirPath ++ "/Sub_instantiated.sub"
    let subProgOut = subIn -- TODO: use a nicer format like JSON
    writeFile outFile subProgOut
    setCurrentDirectory dirPath -- Change to plugin dir so the plugin gets the right path. Otherwise pwd sees "penrose/src"
    callCommand binPath
    setCurrentDirectory originalDir -- Return to original directory
    newSubProg <- readFile inFile
    putStrLn "Penrose received file: "
    putStrLn newSubProg

    styProg <- S.parseStyle styFile styIn dsllEnv
    putStrLn "Style AST:\n"
    pPrint styProg
    divLine

    initState <- G.compileStyle styProg subOut

    Server.serveRenderer domain port initState


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
    subOut@(C.SubOut subProg (subEnv, eqEnv) labelMap) <- C.parseSubstance subFile subIn dsllEnv
    styProg <- S.parseStyle styFile styIn dsllEnv
    let selEnvs = S.checkSels subEnv styProg
    let subss = S.find_substs_prog subEnv eqEnv subProg styProg selEnvs
    let !trans = S.translateStyProg subEnv eqEnv subProg styProg labelMap
                        :: forall a . (Autofloat a) => Either [S.Error] (S.Translation a)
    let initState = G.genOptProblemAndState (fromRight trans)
    return $ Just initState

stepsWithoutServer :: G.State -> G.State
stepsWithoutServer initState =
         let (finalState, numSteps) = head $ dropWhile notConverged $ iterate stepAndCount (initState, 0) in
         trace ("\nnumber of outer steps: " ++ show numSteps) $ finalState
         where stepAndCount (s, n) = traceShowId (O.step s, n + 1)
               notConverged (s, n) = G.optStatus (G.paramsr s) /= G.EPConverged
                                     && n < maxSteps
               maxSteps = 10 ** 3 -- Not sure how many steps it usually takes to converge

-- | Remove file if exist, used for removing the desugared file after we are done
removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
 where handleExists e
         | isDoesNotExistError e = return ()
         | otherwise = throwIO e

-- Go up a directory. "/dir1/dir2/end" -> "/dir1/dir2"
cdUp :: FilePath -> FilePath
cdUp path =
     let dirs = LS.splitOn "/" path in
     L.intercalate "/" $ take (length dirs - 1) dirs

catchPathError :: String -> Maybe FilePath -> FilePath
catchPathError name Nothing = error "path to plugin '" ++ name ++ "' doesn't exist!"
catchPathError _ (Just x) = x
