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
import qualified Data.Map.Strict as M


-----------------------------  Generator Env -----------------------------------
-- The goal of the local env is to track the
data LocalEnv = LocalEnv {
                       availableNames :: Language,
                       declaredTypes :: M.Map String [String] -- Map from type to list of object from that type
                     }
              deriving (Show, Eq)

-- | Add declared type statement to the local env of the generator
addDeclaredType :: LocalEnv -> String -> String -> LocalEnv
addDeclaredType localEnv t typeName = case M.lookup t (declaredTypes localEnv) of
  Nothing -> localEnv { declaredTypes =  M.insert t [typeName] (declaredTypes localEnv) }
  Just v -> localEnv { declaredTypes = M.insert t (typeName : v) (declaredTypes localEnv) }

------------------------------ Substance Generator -----------------------------

-- | The main function of the genrator.
--   arguments: Dsll file, name file
--   parse the input Dsll file, preform type checking and get the environment
--   load the names from the names file
main = do
    args <- getArgs
    when (length args /= 2) $ die "Usage: ./Main prog.dsl namesFile"
    let (dsllFile, namesFile) = (head args,args !! 1)
    dsllIn <- readFile dsllFile
    dsllEnv <- D.parseDsll dsllFile dsllIn
    generateProgram dsllEnv namesFile
    return ()

-- | The top level function for automatic generation of substance programs,
--   calls other functions to generate specific statements
generateProgram dsllEnv namesFile = do
    let prog = ""
        (prog1, localEnv1) = generateTypes dsllEnv initLocalEnv prog 10 --(unsafePerformIO $ getStdRandom (randomR (1,6)))
        (prog2, localEnv2) = generateOperations dsllEnv localEnv1 prog1 3 --(unsafePerformIO $ getStdRandom (randomR (1,6)))
        (prog3, localEnv3) = generatePredicates dsllEnv localEnv2 prog2
    putStrLn prog3
    putStrLn "\n And the local env is: \n"
    print localEnv3
    return ()
    where initLocalEnv = LocalEnv {declaredTypes = M.empty, availableNames = loadLanguage namesFile }

-- | Generate random type statements
generateTypes :: VarEnv -> LocalEnv -> String -> Int -> (String, LocalEnv)
generateTypes dsllEnv localEnv prog n =
    let (nProg , localEnv1) = generateType dsllEnv localEnv
        prog1 = prog ++ nProg
    in if n /= 1 then generateTypes dsllEnv localEnv1 prog1 (n - 1)
                   else (prog1, localEnv1)

-- | Generate single random type statement
generateType :: VarEnv -> LocalEnv -> (String, LocalEnv)
generateType dsllEnv localEnv =
    let types = M.toAscList (typeConstructors dsllEnv)
        t = (unsafePerformIO $ getStdRandom (randomR (0,length types-1)))
        (name, localEnv1) = getName localEnv
        tName = fst (types !! t)
        localEnv2 = addDeclaredType localEnv1 tName name
    in  (tName ++ " " ++ name ++ " \n" , localEnv2)

-- | Generate random operation statements
generateOperations :: VarEnv -> LocalEnv -> String -> Int -> (String, LocalEnv)
generateOperations dsllEnv localEnv prog n =
    let prog1 = prog ++ generateOperation dsllEnv localEnv
    in if n /= 1 then generateOperations dsllEnv localEnv prog1 (n - 1)
                   else (prog1, localEnv)

-- | Generate single random operation statement
generateOperation dsllEnv localEnv =
    let operations = M.toAscList (operators dsllEnv)
        o = (unsafePerformIO $ getStdRandom (randomR (0,length operations -1)))
        operation = snd (operations !! o)
        (name, localEnv1) = getName localEnv
    in nameop operation ++ generateArguments (getTypeNames (tlsop operation)) localEnv

getTypeNames = map convert

convert :: T ->  String
convert (TTypeVar t) = typeVarName t
convert (TConstr t) = nameCons t

generateArguments :: [String] -> LocalEnv -> String
generateArguments types localEnv = let args1 = map (generateArgument localEnv) types
                                   in "(" ++ intercalate "," args1 ++ ")\n"

-- | Get an argument for operations / predicates / val constructors
generateArgument :: LocalEnv -> String -> String
generateArgument localEnv t = case M.lookup t (declaredTypes localEnv) of
  Nothing -> error "No declaration of type " ++ t
  Just lst -> let a = (unsafePerformIO $ getStdRandom (randomR (0,length lst -1)))
              in lst !! a


-- | Generate random predicate statements
generatePredicates dsllEnv localEnv prog = (prog,localEnv)

-----------------------------  Randomize Names ---------------------------------
-- The mechanism for randomization of names for entities in
-- the random Substance programs

-- | Language is defined as a list of strings
type Language = [String]

-- | Load the language from a file in FilePath
loadLanguage :: FilePath -> [String]
loadLanguage path = let content = readFile path
                        ls = lines (unsafePerformIO content)
                    in  ls

-- | Given localEnv, get a random name from all the available names and return
--   it as well as the localEnv excluding that name
getName :: LocalEnv -> (String, LocalEnv)
getName localEnv = let names = availableNames localEnv
                       i =(unsafePerformIO $ getStdRandom (randomR (0,length names -1)))
                       name = names !! i
                   in (name, localEnv {availableNames = delete name (availableNames localEnv)})
