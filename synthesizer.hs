{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnicodeSyntax              #-}

import           Control.Applicative
import           Control.Monad       (when)
import           Control.Monad.State
import           Data.Char           (toLower)
import           Data.List
import           Data.Maybe
import           Data.String
import           Data.Typeable
import           Debug.Trace
import           Penrose.Env
import           Penrose.Substance
import           Penrose.Util        (pickOne)
import           System.Environment
import           System.Exit
import           System.IO
import           System.IO.Unsafe
import           System.Random

-- import           Text.Show.Pretty
import qualified Data.Map.Strict     as M
import qualified Penrose.Element     as D

-----------------------------  Randomness Definitions --------------------------
type Interval = (Int, Int)

{-# NOINLINE seedRnd #-}
seedRnd :: Int
seedRnd = unsafePerformIO $ getStdRandom (randomR (9, 99))

-- | Refresh the StdGen in a given context
updateGen :: Context -> Context
updateGen env = env {gen = mkStdGen (seed env * 7), seed = seed env * 7}

rndNum :: Context -> Interval -> (Int, Context)
rndNum env rng =
  let n = fst (randomR rng (gen env)) :: Int
      env1 = updateGen env
  in (n, env1)

--------------------------------------------------------------------------------
-- Synthesizer context
type Name = String

type Names = M.Map String Int

newtype Synthesize a = Synthesize
  { runSynthesizer :: State Context a
  } deriving (Functor, Applicative, Monad)

data Context = Context
  { names           :: Names
  , declaredTypes   :: M.Map String [String] -- Map from type to list of object from that type
  , pred1Lst        :: [Predicate1]
  , pred2Lst        :: [Predicate2]
  , preDeclarations :: String -- Store all the pre declarations in a current situation
  , prog            :: SubProg -- AST of the generated program
  , gen             :: StdGen -- A random generator which will be updated regulary
  , seed            :: Int -- A random seed which is updated regulary
  , nestingLevel    :: Int -- the nesting level of the system in a current position
  } deriving (Show)

-- | Add declared type statement to the local env of the generator
addDeclaredType :: Context -> String -> String -> Context
addDeclaredType cxt t typeName =
  cxt {declaredTypes = M.insertWith (++) t [typeName] (declaredTypes cxt)}

initContext :: VarEnv -> Context
initContext domainEnv =
  Context
  { declaredTypes = M.empty
  , names = M.empty
  , pred1Lst = getPred1Lst domainEnv
  , pred2Lst = getPred2Lst domainEnv
  , gen = mkStdGen seedRnd
  , seed = seedRnd
  , prog = ""
  , preDeclarations = ""
  , nestingLevel = 0
  }

-- | Zero the nesting level of the current environemt
zeroNestingLevel :: Context -> Context
zeroNestingLevel context = context {nestingLevel = 0}

------------------------------ Substance Generator -----------------------------
-- | The main function of the genrator.
--   arguments: Domain file, name file
--   parse the input Domain file, preform type checking and get the environment
--   load the names from the names file
main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ die "Usage: ./Main prog.dsl outputPath"
  let [domainFile, outputPath] = args
  domainIn <- readFile domainFile
  let domainEnv = D.parseElement domainFile domainIn
  case domainEnv of
    Left err -> error $ show err
    Right domainEnv ->
      generatePrograms domainEnv (initContext domainEnv) outputPath 1

-- | Generate n random programs
generatePrograms :: VarEnv -> Context -> String -> Int -> IO ()
generatePrograms domainEnv context path n a =
  foldM
    (generateProgram domainEnv)
    (initContext domainEnv)
    [path ++ "/prog-" ++ show n ++ ".sub" | [1 .. n]]

-- | The top level function for automatic generation of substance programs,
--   calls other functions to generate specific statements
generateProgram :: VarEnv -> Context -> String -> IO Context
generateProgram domainEnv context outputPath = do
  let (i1, context1) = rndNum (initContext domainEnv) (1, 2)
      (i2, context2) = rndNum context1 (1, 4)
      context3 = generateTypes domainEnv context2 i1
      context4 = generateStatements domainEnv context3 i2
  putStrLn "Program: \n"
  putStrLn (prog context4)
  writeFile outputPath (prog context4)
  return context4

-- | Generate random Substance statements
generateStatements :: VarEnv -> Context -> Int -> Context
generateStatements domainEnv context n =
  let context1 = generateStatement domainEnv context
  in if n /= 1
       then generateStatements domainEnv context1 (n - 1)
       else context1

-- | Generate single random Substance statement
generateStatement :: VarEnv -> Context -> Context
generateStatement domainEnv context =
  let (statementId, context1) = rndNum context (0, 3)
      context2 =
        case statementId of
          0 -> generateBinding domainEnv context1
          1 -> generateValueBinding domainEnv context1
          2 -> zeroNestingLevel (generatePrediacte domainEnv context1 False)
          3 -> zeroNestingLevel (generatePrediacte domainEnv context1 False) -- zeroNestingLevel( generatePrediacteEquality domainEnv context1)
  in context2 {prog = prog context2 ++ "\n"}

-- | Generate random type statements
generateTypes :: VarEnv -> Context -> Int -> Context
generateTypes domainEnv context n =
  let context1 = generateType domainEnv context
  in if n /= 1
       then generateTypes domainEnv context1 (n - 1)
       else context1

-- | Generate single random type statement
generateType :: VarEnv -> Context -> Context
generateType domainEnv cxt =
  let types = M.toAscList (typeConstructors domainEnv)
      ((tName, _), g') = pickOne types $ gen cxt
      (name, cxt') = freshName tName cxt
      cxt'' = addDeclaredType cxt' tName name
  in cxt'' {prog = prog cxt ++ tName ++ " " ++ name ++ " \n"}

-- | Generate single random specific type statement
generateSpecificGeneralType :: VarEnv -> Context -> String -> Context
generateSpecificGeneralType domainEnv context tName =
  let (name, context1) = freshName tName context
      allTypes = getAllPossibleTypes domainEnv context tName
      (n, context2) = rndNum context (0, length allTypes - 1)
      context3 = addDeclaredType context2 (allTypes !! n) name
  in context3 {prog = prog context3 ++ allTypes !! n ++ " " ++ name ++ " \n"}

-- | Generate single random specific type statement
generateSpecificType :: VarEnv -> Context -> String -> Context
generateSpecificType domainEnv context tName =
  let (name, context1) = freshName tName context
      context2 = addDeclaredType context1 tName name
  in context2 {prog = prog context2 ++ tName ++ " " ++ name ++ " \n"}

-- | Generate single random binding statement
generateBinding :: VarEnv -> Context -> Context
generateBinding domainEnv context =
  if not (null (operators domainEnv))
    then let operations = M.toAscList (operators domainEnv)
             (o, context1) = rndNum context (0, length operations - 1)
             operation = snd (operations !! o)
             allTypes = top operation : tlsop operation
             context2 =
               generatePreDeclarations
                 domainEnv
                 context1
                 (getTypeNames allTypes)
             context3 = generateArgument context2 (convert (top operation))
             context4 =
               context3 {prog = prog context3 ++ " := " ++ nameop operation}
         in generateArguments
              domainEnv
              (getTypeNames (tlsop operation))
              context4
    else context

-- | Generate single random binding statement
generateValueBinding :: VarEnv -> Context -> Context
generateValueBinding domainEnv context =
  if not (null (valConstructors domainEnv))
    then let valConsts = M.toAscList (valConstructors domainEnv)
             (v, context1) = rndNum context (0, length valConsts - 1)
             valConstructor = snd (valConsts !! v)
             allTypes = tvc valConstructor : tlsvc valConstructor
             context2 =
               generatePreDeclarations
                 domainEnv
                 context1
                 (getTypeNames allTypes)
             context3 = generateArgument context2 (convert (tvc valConstructor))
             context4 =
               context3
               {prog = prog context3 ++ " := " ++ namevc valConstructor}
         in generateArguments
              domainEnv
              (getTypeNames (tlsvc valConstructor))
              context4
    else context

-- | Get a list of all the types needed for a specific statement and
--   add type declrations for them in case needed
generatePreDeclarations :: VarEnv -> Context -> [String] -> Context
generatePreDeclarations varEnv = foldl (generatePreDeclaration varEnv)

-- | Add a specific declaration in case needed
generatePreDeclaration :: VarEnv -> Context -> String -> Context
generatePreDeclaration domainEnv context t =
  case M.lookup t (declaredTypes context) of
    Nothing -> generateSpecificType domainEnv context t
    Just lst ->
      let (i, context1) = rndNum context (0, length lst)
      in if i == 0 || i == 1
           then generateSpecificGeneralType domainEnv context1 t
           else context1

-- | Generate single random predicate equality
-- generatePrediacteEquality :: VarEnv -> Context -> Context
-- generatePrediacteEquality domainEnv context =
--   let context1 = generatePrediacte domainEnv context False
--       context2 = context1 {prog = prog context1 ++ " <-> "}
--   in generatePrediacte domainEnv context2 True
-- | Generate single random operation statement
generatePrediacte :: VarEnv -> Context -> Bool -> Context
generatePrediacte domainEnv context isNested =
  let preds = M.toAscList (predicates domainEnv)
      (p, context1) = rndNum context (0, length preds - 1)
      predicate = snd (preds !! p)
  in if isNested && nestingLevel context1 > 2
       then generatePrediacte1Nested domainEnv context1
       else case predicate of
              Pred1 p1 ->
                if isNested
                  then generatePrediacte1Nested domainEnv context1
                  else generatePrediacte1 domainEnv context1
              Pred2 p2 ->
                let context2 =
                      context1 {nestingLevel = nestingLevel context1 + 1}
                in generatePrediacte2 domainEnv context2

generatePrediacte1 :: VarEnv -> Context -> Context
generatePrediacte1 domainEnv context =
  let (p, context1) = rndNum context (0, length (pred1Lst context) - 1)
      predicate = pred1Lst context1 !! p
      context2 =
        generatePreDeclarations
          domainEnv
          context1
          (getTypeNames (tlspred1 predicate))
      context3 = context2 {prog = prog context2 ++ namepred1 predicate}
  in generateArguments domainEnv (getTypeNames (tlspred1 predicate)) context3

generatePrediacte1Nested :: VarEnv -> Context -> Context
generatePrediacte1Nested domainEnv context =
  if length (pred1Lst context) > 2
    then let (p, context1) = rndNum context (0, length (pred1Lst context) - 1)
             predicate = pred1Lst context1 !! p
             context2 = context1 {prog = prog context1 ++ namepred1 predicate}
         in generateArguments
              domainEnv
              (getTypeNames (tlspred1 predicate))
              context2
    else context

generatePrediacte2 :: VarEnv -> Context -> Context
generatePrediacte2 domainEnv context
            --allPreds = M.toAscList (predicates domainEnv)
 =
  let g = mkStdGen 9
      l = length (pred2Lst context) - 1
      p = fst (randomR (0, l) g)
      predicate = pred2Lst context !! p
      context1 = context {prog = prog context ++ namepred2 predicate ++ "("}
      n = length (plspred2 predicate)
      context2 = generatePred2Args domainEnv context1 n
  in context2

generatePred2Args :: VarEnv -> Context -> Int -> Context
generatePred2Args domainEnv context n =
  let context1 = generatePred2Arg domainEnv context
  in if n /= 1
       then let context2 = context1 {prog = prog context1 ++ ","}
            in generatePred2Args domainEnv context2 (n - 1)
       else context1 {prog = prog context1 ++ ")"}

generatePred2Arg :: VarEnv -> Context -> Context
generatePred2Arg domainEnv context = generatePrediacte domainEnv context True

------------------------------- Helper Functions -------------------------------
getTypeNames = map convert

convert :: T -> String
convert (TTypeVar t) = typeVarName t
convert (TConstr t)  = nameCons t

generateArguments :: VarEnv -> [String] -> Context -> Context
generateArguments domainEnv types context =
  let context1 = context {prog = prog context ++ "("}
      context2 = foldl (generateFullArgument domainEnv) context1 types
  in context2 {prog = init (fromString (prog context2)) ++ ")"}

-- | Get an argument for operations / predicates / val constructors
generateArgument :: Context -> String -> Context
generateArgument context t =
  case M.lookup t (declaredTypes context) of
    Nothing -> error "Generation Error!"
    Just lst ->
      let (a, context1) = rndNum context (0, length lst - 1)
      in context1 {prog = prog context1 ++ lst !! a}

-- | Get an argument for operations / predicates / val constructors
generateFullArgument :: VarEnv -> Context -> String -> Context
generateFullArgument domainEnv context t =
  let allPossibleArguments = getAllPossibleArguments domainEnv context t
      (a, context1) = rndNum context (0, length allPossibleArguments - 1)
  in context1 {prog = prog context1 ++ allPossibleArguments !! a ++ ","}

getAllPossibleArguments :: VarEnv -> Context -> String -> [String]
getAllPossibleArguments domainEnv context t =
  concatMap (getIdentifiers context) (getAllPossibleTypes domainEnv context t)

getAllPossibleTypes :: VarEnv -> Context -> String -> [String]
getAllPossibleTypes domainEnv context t =
  let subt = subTypes domainEnv
      allTypes = [t1 | (t1, t2) <- subt, getTypeNames [t2] == [t]]
  in (t : getTypeNames allTypes)

getIdentifiers :: Context -> String -> [String]
getIdentifiers context t = fromMaybe [] (M.lookup t (declaredTypes context))

getPred1Lst :: VarEnv -> [Predicate1]
getPred1Lst domainEnv =
  let preds = M.toAscList (predicates domainEnv)
  in concatMap extractPred1 preds

getPred2Lst :: VarEnv -> [Predicate2]
getPred2Lst domainEnv =
  let preds = M.toAscList (predicates domainEnv)
  in concatMap extractPred2 preds

extractPred1 :: (String, PredicateEnv) -> [Predicate1]
extractPred1 (_, Pred1 p) = [p]
extractPred1 _            = []

extractPred2 :: (String, PredicateEnv) -> [Predicate2]
extractPred2 (_, Pred2 p) = [p]
extractPred2 _            = []

--------------------------------------------------------------------------------
-- Names generation
-- | Generate a new name given a type
freshName :: String -> Context -> (Name, Context)
freshName typ cxt =
  let (n, names') = uniqueName (prefixOf typ) $ names cxt
  in (n, cxt {names = names'})

prefixOf :: String -> String
prefixOf = map toLower . take 1

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case M.lookup nm ns of
    Nothing -> (nm, M.insert nm 1 ns)
    Just ix -> (nm ++ show ix, M.insert nm (ix + 1) ns)
