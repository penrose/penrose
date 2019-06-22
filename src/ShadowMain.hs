-- | Main module of the Penrose system (split out for testing; Main is the real main)
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnicodeSyntax             #-}

module ShadowMain where

import qualified Control.Concurrent         as CC
import           Control.Exception
import           Control.Monad              (forM_, when)
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.List                  as L (intercalate)
import qualified Data.List.Split            as LS (splitOn)
import qualified Data.Map.Strict            as M
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Debug.Trace
import qualified Element                    as D
import qualified Env                        as E
import qualified GenOptProblem              as G
import           Interface
import           Network.HTTP.Types.Status
import qualified Optimizer                  as O
import           Plugins
import           Prelude                    hiding (catch)
import qualified Server
import qualified Style                      as S
import qualified Substance                  as C
import qualified SubstanceJSON              as J
import qualified Sugarer
import           System.Console.Docopt
import           System.Console.Pretty      (Color (..), Style (..), bgColor,
                                             color, style, supportsPretty)
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Error            hiding (catch)
import qualified Text.Megaparsec            as MP (parseErrorPretty, runParser)
import           Text.Show.Pretty
import           Utils
import           Web.Scotty

default (Int, Float)

-- when false, executes headless for profiling
useFrontend :: Bool
useFrontend = True

argPatterns :: Docopt
argPatterns = [docoptFile|USAGE.txt|]

getArgOrExit = getArgOrExitWith argPatterns

-- | `shadowMain` runs the Penrose system
shadowMain :: IO ()
shadowMain = do
  args <- parseArgsOrExit argPatterns =<< getArgs
  styFile <- args `getArgOrExit` argument "style"
  elementFile <- args `getArgOrExit` argument "element"
  domain <- args `getArgOrExit` longOption "domain"
  port <- args `getArgOrExit` longOption "port"
  config <- args `getArgOrExit` longOption "config"
  if args `isPresent` argument "substance"
    then do
      subFile <- args `getArgOrExit` argument "substance"
      penroseRenderer subFile styFile elementFile domain config $ read port
    else penroseEditor styFile elementFile domain $ read port

penroseEditor :: String -> String -> String -> Int -> IO ()
penroseEditor styFile elementFile domain port = do
  styIn <- readFile styFile
  elementIn <- readFile elementFile
  let elementEnv =
        either (\e -> error $ "Element parser errror" ++ show e) id $
        D.parseElement elementFile elementIn
  let styProg =
        either (\e -> error $ "Style parser errror" ++ show e) id $
        S.parseStyle styFile styIn elementEnv
  putStrLn "Style AST:\n"
  pPrint styProg
  divLine
  Server.serveEditor domain port elementEnv styProg

penroseRenderer ::
     String -> String -> String -> String -> String -> Int -> IO ()
penroseRenderer subFile styFile elementFile domain configPath port = do
  subIn <- readFile subFile
  styIn <- readFile styFile
  elementIn <- readFile elementFile
  initState <-
    case Interface.compileTrio subIn styIn elementIn of
      Left err     -> error $ show err
      Right (s, _) -> return s
  -- Read optimization config and so it can be included in the initial state
  configStr <- B.readFile configPath
  let configBstr = (decode configStr) :: Maybe G.OptConfig
  let optConfig =
        case configBstr of
          Nothing -> error "couldn't read opt config JSON"
          Just x  -> x
  putStrLn "Opt config:\n"
  putStrLn $ show optConfig
  -- TODO: use the loaded optconfig
  if useFrontend
    then Server.serveRenderer domain port initState
    else let numTrials = 1000
         in let res = map (\x -> stepsWithoutServer initState) [1 .. numTrials]
            in putStrLn $ show $ map G.varyingState res -- Needed so all of res is evaluated, but don't spend so much time prettyprinting

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
