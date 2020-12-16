-- | Main module of the Penrose system (split out for testing; Main is the real main)
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnicodeSyntax             #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.ShadowMain
  ( shadowMain
  ) where

import qualified Control.Concurrent         as CC
import           Control.Exception
import           Control.Monad              (forM_, when)
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as L (intercalate, isSuffixOf)
import qualified Data.List.Split            as LS (splitOn)
import qualified Data.Map.Strict            as M
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Debug.Trace
import           Network.HTTP.Types.Status
import           Penrose.API
import qualified Penrose.Element            as D
import qualified Penrose.Env                as E
import qualified Penrose.GenOptProblem      as G
import qualified Penrose.Optimizer          as O
import           Penrose.Plugins
import           Penrose.Serializer         (Packet (..))
import qualified Penrose.Server             as Server
import qualified Penrose.Style              as S
import qualified Penrose.Substance          as C
import qualified Penrose.Sugarer
import           Penrose.Util
import           Prelude                    hiding (catch)
import           System.Console.Docopt
import           System.Console.Pretty      (Color (..), Style (..), bgColor,
                                             color, style, supportsPretty)
import           System.Environment         hiding (getEnv)
import           System.Exit
import           System.IO
import           System.IO.Error            hiding (catch)
import qualified Text.Megaparsec            as MP (parseErrorPretty, runParser)
import           Text.Show.Pretty
import           Web.Scotty

default (Int, Float)

-- when false, executes headless for profiling
useFrontend :: Bool
useFrontend = True

argPatterns :: Docopt
argPatterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith argPatterns

-- | 'shadowMain' runs the Penrose system
shadowMain :: IO ()
shadowMain = processArgs =<< parseArgsOrExit argPatterns =<< getArgs

-- | 'processArgs` takes in a list of arguments passed to the `penrose` binary and determine what's next.
processArgs :: Arguments -> IO ()
processArgs args
  | args `isPresent` longOption "version" = putStrLn getVersion
  | args `isPresent` command "runAPI" = do
    x <- B.getContents
    let y = runAPI x
    B.putStr y
  | otherwise = do
    domain <- args `getArgOrExit` longOption "domain"
    port <- args `getArgOrExit` longOption "port"
    config <- args `getArgOrExit` longOption "config"
    if args `isPresent` command "editor"
      then let isVerbose = args `isPresent` longOption "verbose"
           in Server.serveEditor domain (read port) isVerbose
      else do
        let inputs = args `getAllArgs` argument "input"
        let files = (\x -> filter (x `L.isSuffixOf`)) <$> [".sub", ".sty", ".dsl"] <*> pure inputs
        if any ((/= 1) . length) files
          then exitWithUsage argPatterns
          else let [[subFile], [styFile], [elementFile]] = files
               in penroseRenderer subFile styFile elementFile domain config $ read port

--------------------------------------------------------------------------------
-- CLI wrapper
-- 'runAPI' takes a string of a JSON-encoded APICall read from stdin as an input and return another JSON-encoded Packet as the output.
runAPI :: B.ByteString -> B.ByteString
runAPI strIn =
  case decode strIn of
    Just call ->
      case call of
        Step steps s -> toStdout "state" $ step s steps
        Resample samples s -> toStdout "state" $ resample s samples
        StepUntilConvergence s -> toStdout "state" $ stepUntilConvergence s
        CompileTrio sub sty elm ->
          toStdout "compilerOutput" $ compileTrio sub sty elm
        ReconcileNext s sub sty elm ->
          toStdout "compilerOutput" $ reconcileNext s sub sty elm
        GetEnv sub elm -> toStdout "varEnv" $ getEnv sub elm
        EnergyValues s ->
          encode $
          Packet
          { packetType = "energies"
          , packetContents = energyValues s
          , packetSession = Nothing
          }
    Nothing -> error "Error reading JSON string."
  where
    toStdout :: (ToJSON a, ToJSON b) => String -> Either a b -> B.ByteString
    toStdout flag output =
      case output of
        Right res ->
          encode $
          Packet
          {packetType = flag, packetContents = res, packetSession = Nothing}
        Left err ->
          encode $
          Packet
          {packetType = "error", packetContents = err, packetSession = Nothing}

--------------------------------------------------------------------------------
penroseRenderer ::
     String -> String -> String -> String -> String -> Int -> IO ()
penroseRenderer subFile styFile elementFile domain configPath port = do
  subIn <- readFile subFile
  styIn <- readFile styFile
  elementIn <- readFile elementFile
  initState <-
    case compileTrio subIn styIn elementIn of
      Left err     -> error $ show err
      Right (s, _) -> return s
  -- Read optimization config and so it can be included in the initial state
  -- configStr <- B.readFile configPath
  -- let configBstr = decode configStr :: Maybe G.OptConfig
  -- let optConfig =
  --       case configBstr of
  --         Nothing -> error "couldn't read opt config JSON"
  --         Just x  -> x
  -- putStrLn "Opt config:\n"
  -- print optConfig
  let state = initState {G.oConfig = G.defaultOptConfig}
  if useFrontend
    then Server.serveRenderer domain port state
    else let numTrials = 1000
         in let res = map (\x -> stepsWithoutServer state) [1 .. numTrials]
            in print $ map G.varyingState res -- Needed so all of res is evaluated, but don't spend so much time prettyprinting

stepsWithoutServer :: G.State -> G.State
stepsWithoutServer initState =
  let (finalState, numSteps) =
        head $ dropWhile notConverged $ iterate stepAndCount (initState, 0)
  in trace ("\nnumber of outer steps: " ++ show numSteps) $ finalState
  where
    stepAndCount (s, n) = (O.step s, n + 1)
    notConverged (s, n) =
      G.optStatus (G.paramsr s) /= G.EPConverged && n < maxSteps
    maxSteps = 10 ** 3 -- Not sure how many steps it usually takes to converge
