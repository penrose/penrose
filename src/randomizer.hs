-- Get as an input the a DSLL, and the number of programs it should produce
-- Author: Dor Ma'ayan
-- Version: 07/03/2018

import System.Random
import System.IO
import System.IO.Unsafe
import System.Exit
import System.Environment
import Data.List
import Control.Monad (when)
import Env

import qualified Dsll as D
import qualified Namegen as NG

main = do
    args <- getArgs
    when (length args /= 1) $ die "Usage: ./Main prog.dsl"
    let dsllFile = head args
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn
    generateProgram dsllEnv
    return ()

generateProgram dsllEnv = do
    putStrLn "\n hi we here let's generate: \n"
    generateTypes dsllEnv
    generateOperations dsllEnv
    generatePredicates dsllEnv

    return ()

generateTypes dsllEnv = do
     samples <- NG.loadSamples "names"
     let language = NG.fromSamples samples
         myGeneratedName = NG.generateName language (unsafePerformIO $ getStdRandom (randomR (1,1000))) -- seed could be ontained from a Random Generator
     putStrLn myGeneratedName
     return ()

--putStrLn "\n ...generateTypes... \n"

generateOperations dsllEnv = do
     samples <- NG.loadSamples "names"
     let language = NG.fromSamples samples
         myGeneratedName = NG.generateName language (unsafePerformIO $ getStdRandom (randomR (1,1000))) -- seed could be ontained from a Random Generator
     putStrLn myGeneratedName
     return ()

generatePredicates dsllEnv = putStrLn "\n ...generatePredicates... \n"



--seed <- randomIO :: IO Int -- Currently randomizes the seed to...
