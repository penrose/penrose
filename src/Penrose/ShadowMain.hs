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
import qualified Data.List                  as L (intercalate)
import qualified Data.List.Split            as LS (splitOn)
import qualified Data.Map.Strict            as M
import qualified Data.Text.Lazy             as T
import           Data.Text.Lazy.Encoding    (decodeUtf8)
import           Data.Version               (showVersion)
import           Debug.Trace
import           Network.HTTP.Types.Status
import           Paths_penrose              (version)
import qualified Penrose.Element            as D
import qualified Penrose.Env                as E
import qualified Penrose.GenOptProblem      as G
import           Penrose.Interface
import qualified Penrose.Optimizer          as O
import           Penrose.Plugins
import qualified Penrose.Server             as Server
import qualified Penrose.Style              as S
import qualified Penrose.Substance          as C
import qualified Penrose.SubstanceJSON      as J
import qualified Penrose.Sugarer
import           Penrose.Util
import           Prelude                    hiding (catch)
import           System.Console.Docopt
import           System.Console.Pretty      (Color (..), Style (..), bgColor,
                                             color, style, supportsPretty)
import           System.Environment
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

-- | `shadowMain` runs the Penrose system
shadowMain :: IO ()
shadowMain = do
  args <- parseArgsOrExit argPatterns =<< getArgs
  if args `isPresent` longOption "version"
    then do
      putStrLn $ showVersion version
      return ()
    else do
      domain <- args `getArgOrExit` longOption "domain"
      port <- args `getArgOrExit` longOption "port"
      config <- args `getArgOrExit` longOption "config"
      if args `isPresent` command "editor"
        then let isVerbose = args `isPresent` longOption "verbose"
             in Server.serveEditor domain (read port) isVerbose
        else do
          subFile <- args `getArgOrExit` argument "substance"
          styFile <- args `getArgOrExit` argument "style"
          elementFile <- args `getArgOrExit` argument "element"
          penroseRenderer subFile styFile elementFile domain config $ read port

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
  configStr <- B.readFile configPath
  let configBstr = (decode configStr) :: Maybe G.OptConfig
  let optConfig =
        case configBstr of
          Nothing -> error "couldn't read opt config JSON"
          Just x  -> x
  putStrLn "Opt config:\n"
  putStrLn $ show optConfig
  let state = initState {G.oConfig = optConfig}
  if useFrontend
    then Server.serveRenderer domain port state
    else let numTrials = 1000
         in let res = map (\x -> stepsWithoutServer state) [1 .. numTrials]
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
