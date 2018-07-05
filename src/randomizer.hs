-- Automatic random generator for Substance programs
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
import qualified Data.Map.Strict as M


-----------------------------  Generator Env -----------------------------------
-- The goal of the local env is to track the
newtype LocalEnv = LocalEnv {
                       declaredTypes    :: M.Map String [String] -- Map from type to list of object from that type
                     }
              deriving (Show, Eq)

-- | Add declared type statement to the local env of the generator
addDeclaredType :: LocalEnv -> String -> String -> LocalEnv
addDeclaredType localEnv t typeName = case M.lookup t (declaredTypes localEnv) of
  Nothing -> localEnv { declaredTypes =  M.insert t [typeName] (declaredTypes localEnv) }
  Just v -> localEnv { declaredTypes = M.insert t (typeName : v) (declaredTypes localEnv) }



-- | The main function of the genrator.
--   arguments: Dsll file, name file
--   parse the input Dsll file, preform type checking and get the environment
--   load the names from the names file

------------------------------ Substance Generator -----------------------------

main = do
    args <- getArgs
    when (length args /= 2) $ die "Usage: ./Main prog.dsl namesFile"
    let (dsllFile, namesFile) = (head args,args !! 1)
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn
    samples <- NG.loadSamples namesFile
    let language = NG.fromSamples samples
    generateProgram dsllEnv language
    return ()


generateProgram dsllEnv language = do
    let prog = ""
        (prog1, localEnv1) = generateTypes dsllEnv initLocalEnv language prog 10 --(unsafePerformIO $ getStdRandom (randomR (1,6)))
        (prog2, localEnv2) = generateOperations dsllEnv localEnv1 language prog1 3 --(unsafePerformIO $ getStdRandom (randomR (1,6)))
        (prog3, localEnv3) = generatePredicates dsllEnv localEnv2 language prog2
    putStrLn prog3
    putStrLn "\n And the local env is: \n"
    putStrLn (show localEnv3)
    return ()
    where initLocalEnv = LocalEnv {declaredTypes = M.empty}



-- | Generate random type statements
generateTypes dsllEnv localEnv language prog n =
    let (nProg , nLocalEnv) = generateType dsllEnv localEnv language
        prog1 = prog ++ nProg
    in if n /= 1 then generateTypes dsllEnv nLocalEnv language prog1 (n - 1)
                   else (prog1, nLocalEnv)

generateType dsllEnv localEnv language =
    let types = M.toAscList (typeConstructors dsllEnv)
        t = (unsafePerformIO $ getStdRandom (randomR (0,length types-1)))
        name = NG.generateName language (unsafePerformIO $ getStdRandom (randomR (1,1000)))
        tName = fst (types !! t)
        nLocalEnv = addDeclaredType localEnv tName name
    in  (tName ++ " " ++ name ++ " \n" , nLocalEnv)


-- | Generate random operation statements
generateOperations dsllEnv localEnv language prog n =
    let prog1 = prog ++ generateOperation dsllEnv language
    in if n /= 1 then generateOperations dsllEnv localEnv language prog1 (n - 1)
                   else (prog1, localEnv)

generateOperation dsllEnv language =
    let operations = M.toAscList (operators dsllEnv)
        o = (unsafePerformIO $ getStdRandom (randomR (0,length operations -1)))
        operation = snd (operations !! o)
        name = NG.generateName language (unsafePerformIO $ getStdRandom (randomR (1,1000)))
    in nameop operation ++ "(" ++ show (tlsop operation) ++ ")\n"


-- | Generate random predicate statements
generatePredicates dsllEnv localEnv language prog = (prog,localEnv)
