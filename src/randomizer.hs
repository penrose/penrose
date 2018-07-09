-- Automatic random generator for Substance programs
-- Author: Dor Ma'ayan, July 2018

{-# LANGUAGE ScopedTypeVariables #-}

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
                       declaredTypes :: M.Map String [String], -- Map from type to list of object from that type
                       pred1Lst :: [Predicate1],
                       pred2Lst :: [Predicate2],
                       prog :: String --String representing the generated program
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
    let localEnv1 = generateTypes dsllEnv initLocalEnv 15 --(unsafePerformIO $ getStdRandom (randomR (1,6)))
        localEnv2 = generateStatements dsllEnv localEnv1 10
    putStrLn (prog localEnv2)
    putStrLn "\n And the local env is: \n"
    print localEnv2
    return ()
    where initLocalEnv = LocalEnv {declaredTypes = M.empty, availableNames = loadLanguage namesFile,
       pred1Lst = getPred1Lst dsllEnv, pred2Lst = getPred2Lst dsllEnv, prog = "" }

-- | Generate random Substance statements
generateStatements :: VarEnv -> LocalEnv -> Int -> LocalEnv
generateStatements dsllEnv localEnv n =
    let localEnv1 = generateStatement dsllEnv localEnv
    in if n /= 1 then generateStatements dsllEnv localEnv1 (n - 1)
                 else localEnv1

-- | Generate single random Substance statement
generateStatement :: VarEnv -> LocalEnv -> LocalEnv
generateStatement dsllEnv localEnv =
    let statementId = (unsafePerformIO $ getStdRandom (randomR (0,2))) :: Integer
        l = case statementId of
                 0 -> generateBinding dsllEnv localEnv
                 1 -> generatePrediacte dsllEnv localEnv
                 2 -> generatePrediacteEquality dsllEnv localEnv
    in l {prog = prog l ++ "\n"}

-- | Generate random type statements
generateTypes :: VarEnv -> LocalEnv -> Int -> LocalEnv
generateTypes dsllEnv localEnv n =
    let localEnv1 = generateType dsllEnv localEnv
    in if n /= 1 then generateTypes dsllEnv localEnv1 (n - 1)
                   else localEnv1

-- | Generate single random type statement
generateType :: VarEnv -> LocalEnv -> LocalEnv
generateType dsllEnv localEnv =
    let types = M.toAscList (typeConstructors dsllEnv)
        t = (unsafePerformIO $ getStdRandom (randomR (0,length types-1)))
        (name, localEnv1) = getName localEnv
        tName = fst (types !! t)
        localEnv2 = addDeclaredType localEnv1 tName name
        localEnv3 = localEnv2 {prog = prog localEnv2 ++ tName ++ " " ++ name ++ " \n" }
    in  localEnv3

-- | Generate single random binding statement
generateBinding :: VarEnv -> LocalEnv -> LocalEnv
generateBinding dsllEnv localEnv =
    let operations = M.toAscList (operators dsllEnv)
        o = (unsafePerformIO $ getStdRandom (randomR (0,length operations -1)))
        operation = snd (operations !! o)
        (name, localEnv1) = getName localEnv
    in localEnv { prog = prog localEnv ++ generateArgument localEnv (convert (top operation))
      ++ " := " ++ nameop operation ++ generateArguments (getTypeNames (tlsop operation)) localEnv}

-- | Generate single random predicate equality
generatePrediacteEquality :: VarEnv -> LocalEnv -> LocalEnv
generatePrediacteEquality dsllEnv localEnv =
    let localEnv1 = generatePrediacte dsllEnv localEnv
        localEnv2 = localEnv1 {prog = prog localEnv1 ++ "<->"}
        in generatePrediacte dsllEnv localEnv2

-- | Generate single random operation statement
generatePrediacte :: VarEnv -> LocalEnv -> LocalEnv
generatePrediacte dsllEnv localEnv =
    let preds = M.toAscList (predicates dsllEnv)
        p = (unsafePerformIO $ getStdRandom (randomR (0,length preds -1)))
        predicate = snd (preds !! p)
    in case predicate of
            Pred1 p1 -> generatePrediacte1 dsllEnv localEnv
            Pred2 p2 -> generatePrediacte2 dsllEnv localEnv

generatePrediacte1 :: VarEnv -> LocalEnv -> LocalEnv
generatePrediacte1 dsllEnv localEnv =
    let p = (unsafePerformIO $ getStdRandom (randomR (0,length (pred1Lst localEnv) -1)))
        predicate = (pred1Lst localEnv) !! p
    in localEnv { prog = prog localEnv ++ (namepred1 predicate ++ generateArguments (getTypeNames (tlspred1 predicate)) localEnv)}

generatePrediacte2 :: VarEnv -> LocalEnv -> LocalEnv
generatePrediacte2 dsllEnv localEnv =
        let --allPreds = M.toAscList (predicates dsllEnv)
            p = (unsafePerformIO $ getStdRandom (randomR (0,length (pred2Lst localEnv) -1)))
            predicate = (pred2Lst localEnv) !! p
            localEnv1 = localEnv { prog = prog localEnv ++ namepred2 predicate ++ "("}
            localEnv2 = generatePred2Args dsllEnv localEnv1 2--(plspred2Length predicate)--(plspred2 predicate))
        in  localEnv2 {prog = prog localEnv2 ++ ")"}

generatePred2Args :: VarEnv -> LocalEnv -> Int -> LocalEnv
generatePred2Args dsllEnv localEnv n =
     let localEnv1 = generatePrediacte1 dsllEnv localEnv
     in if n /= 1 then let localEnv2 = localEnv1 {prog = prog localEnv1 ++ ","}
                       in generatePred2Args dsllEnv localEnv2 (n - 1)
        else localEnv1

generatePred2Arg :: VarEnv -> LocalEnv -> LocalEnv
generatePred2Arg dsllEnv localEnv = generatePrediacte1 dsllEnv localEnv
------------------------------- Helper Functions -------------------------------
getTypeNames = map convert

convert :: T ->  String
convert (TTypeVar t) = typeVarName t
convert (TConstr t) = nameCons t

generateArguments :: [String] -> LocalEnv -> String
generateArguments types localEnv = let args1 = map (generateArgument localEnv) types
                                   in "(" ++ intercalate "," args1 ++ ")"

-- | Get an argument for operations / predicates / val constructors
generateArgument :: LocalEnv -> String -> String
generateArgument localEnv t = case M.lookup t (declaredTypes localEnv) of
  Nothing -> ""
  Just lst -> let a = (unsafePerformIO $ getStdRandom (randomR (0,length lst -1)))
              in lst !! a

getPred1Lst :: VarEnv -> [Predicate1]
getPred1Lst dsllEnv = let preds = M.toAscList (predicates dsllEnv)
                      in concat (map extractPred1 preds)

getPred2Lst :: VarEnv -> [Predicate2]
getPred2Lst dsllEnv = let preds = M.toAscList (predicates dsllEnv)
                      in concat (map extractPred2 preds)

extractPred1:: (String,PredicateEnv) -> [Predicate1]
extractPred1 (_ , Pred1 p) = [p]
extractPred1 _ = []

extractPred2:: (String,PredicateEnv) -> [Predicate2]
extractPred2 (_ , Pred2 p) = [p]
extractPred2 _ = []


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
