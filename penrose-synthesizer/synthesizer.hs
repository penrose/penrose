{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnicodeSyntax             #-}

import           Control.Applicative
import           Control.Monad                  (when)
import           Control.Monad.State
import           Data.Aeson                     (encode)
import qualified Data.ByteString.Lazy.Char8     as B
import           Data.Char                      (toLower)
import           Data.Either                    (fromRight)

import           Data.List                      (elem)
import qualified Data.Map.Strict                as M
import           Data.Maybe
import           Data.String
import           Data.Typeable
import           Debug.Trace
import           Penrose.API
import qualified Penrose.Element                as D
import           Penrose.Env                    hiding (typeName)
import           Penrose.Pretty
import           Penrose.Substance
import           Penrose.Util                   (pickOne)
import           System.Console.Docopt
import           System.Console.Pretty          as CP
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment
import           System.Random
import           Text.PrettyPrint.HughesPJClass

--------------------------------------------------------------------------------
-- Synthesizer context
--------------------------------------------------------------------------------
type Name = String

type Names = M.Map String Int

type Synthesize a = State Context a

data GenericOption
  = Concrete
  | General VarEnv
  deriving (Show)

data ArgOption
  = Generated
  | Existing
  | Mixed
  deriving (Show)

data SubStmtT
  = DeclT
  | BindT
  | PredT
  deriving (Show, Eq)

data AllowDuplicates
  = Distinct
  | Repeated
  deriving (Show)

data Context = Context
  { names         :: Names
  , declaredTypes :: M.Map String [Name] -- | Map from type name to a list of names with the type
  , prog          :: SubProg -- | AST of the generated program
  , gen           :: StdGen -- | A random generator
  , setting       :: Setting -- | Synthesizer settings
  -- , nestingLevel    :: Int -- | the nesting level of the system in a current position
  } deriving (Show)

data Setting = Setting
  { lengthRange :: (Int, Int)
  , argOption   :: ArgOption
  , stmtTypes   :: [SubStmtT]
  } deriving (Show)

initContext :: Maybe String -> VarEnv -> Setting -> Context
initContext (Just subIn) env setting =
  let (SubOut subProg _ _) =
        fromRight (error "Failed to parse the input Substance program") $
        parseSubstance "" subIn env
      cxt = initContext Nothing env setting
  in cxt {prog = subProg}
initContext Nothing _ setting =
  Context
  { declaredTypes = M.empty
  , names = M.empty
  , gen = mkStdGen seedRnd
  , prog = []
  , setting = setting
  }

-- TODO: add binding
stmtTypeArgs :: [(String, SubStmtT)]
stmtTypeArgs = [("predicate", PredT), ("declaration", DeclT)]

reset :: Synthesize ()
reset =
  modify $ \cxt -> cxt {declaredTypes = M.empty, names = M.empty, prog = []}

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------
argPatterns :: Docopt
argPatterns = [docoptFile|penrose-synthesizer/USAGE.txt|]

getArgOrExit = getArgOrExitWith argPatterns

-- | The main function of the genrator.
main :: IO ()
main = do
  args <- parseArgsOrExit argPatterns =<< getArgs
  domainFile <- args `getArgOrExit` argument "domain"
  path <- args `getArgOrExit` longOption "path"
  numProgs <- args `getArgOrExit` longOption "num-programs"
  maxLength <- args `getArgOrExit` longOption "max-length"
  minLength <- args `getArgOrExit` longOption "min-length"
  let stmtTs =
        getStmtTypes $
        map snd $ filter (isPresent args . longOption . fst) stmtTypeArgs
  let [n, lmin, lmax] = map read [numProgs, minLength, maxLength] :: [Int]
  createDirectoryIfMissing True path -- create output dir if missing
  domainIn <- readFile domainFile
  let env = D.parseElement domainFile domainIn
  -- TODO: take in argoption as param
  let settings =
        Setting
        {lengthRange = (lmin, lmax), argOption = Mixed, stmtTypes = stmtTs}
  case env of
    Left err -> error $ show err
    Right env -> do
      let subFile = args `getArg` longOption "substance"
      initSub <- safeReadFile subFile
      let (progs, _) =
            runState (generatePrograms env n) (initContext initSub env settings)
      -- let (prog, cxt) = runState (generateProgram env) (initContext initSub settings)
      -- let progs = [prog]
      -- print cxt
      if args `isPresent` longOption "style"
        then do
          let files = map (\i -> path ++ "/prog-" ++ show i) [1 .. n]
          styleFile <- args `getArgOrExit` longOption "style"
          style <- readFile styleFile
          mapM_ (compileProg style domainIn) $ zip progs files
        else do
          let files = map (\i -> path ++ "/prog-" ++ show i ++ ".sub") [1 .. n]
          mapM_ writeSubstance $ zip progs files
  where
    getStmtTypes [] = map snd stmtTypeArgs
    getStmtTypes ss = ss
    safeReadFile Nothing  = return Nothing
    safeReadFile (Just s) = Just <$> readFile s

compileProg :: String -> String -> (SubProg, String) -> IO ()
compileProg style domain (subAST, prefix) = do
  let sub = show $ prettySubstance subAST
  case compileTrio sub style domain of
    Right (state, _) -> do
      putStrLn
        (CP.bgColor CP.Green $ "Generated new program (" ++ prefix ++ "): ")
      putStrLn sub
      putStrLn (CP.bgColor CP.Red $ "Compiled new program (" ++ prefix ++ "): ")
      B.writeFile (prefix ++ ".json") (encode state)
    Left err -> error $ show err

writeSubstance :: (SubProg, String) -> IO ()
writeSubstance (prog, file) = do
  putStrLn (CP.bgColor CP.Red $ "Generated new program (" ++ file ++ "): ")
  let progStr = show $ prettySubstance prog
  putStrLn progStr
  writeFile file progStr

--------------------------------------------------------------------------------
-- The Substance synthesizer
--------------------------------------------------------------------------------
generatePrograms :: VarEnv -> Int -> Synthesize [SubProg]
generatePrograms env n = replicateM n (generateProgram env <* reset)

-- | The top level function for automatic generation of substance programs,
--   calls other functions to generate specific statements
generateProgram :: VarEnv -> Synthesize SubProg
generateProgram env = do
  i <- rndNum (1, 2) -- FIXME: get rid of the hard-coded numbers
  (lmin, lmax) <- gets (lengthRange . setting)
  j <- rndNum (lmin, lmax)
  generateTypes env i
  generateStatements env j
  -- return $ ts ++ stmts
  p <- gets prog
  let labelOption = AutoLabel Penrose.Substance.Default -- TODO: enfore export list in Substance module
  return $ labelOption : p

-- | Generate random Substance statements
generateStatements :: VarEnv -> Int -> Synthesize [SubStmt]
generateStatements env n = replicateM n (generateStatement env)

-- | Generate single random Substance statement
-- NOTE: every synthesizer that 'generateStatement' calls is expected to append its result to the AST, instead of just returning it. This is because certain lower-level functions are allowed to append new statements (e.g. 'generateArg'). Otherwise, we could write this module as a combinator.
generateStatement :: VarEnv -> Synthesize SubStmt
generateStatement env = do
  allowedStmts <- gets (stmtTypes . setting)
  let stmtFs = map snd $ filter (\(s, _) -> s `elem` allowedStmts) stmts
  stmtF <- choice stmtFs
  stmtF env
  where
    stmts :: [(SubStmtT, VarEnv -> Synthesize SubStmt)]
    stmts =
      [ (PredT, generatePredicate)
      , (DeclT, generateType)
      -- , (BindT, generateValueBinding)
      ]

-- | Generate object declarations
generateTypes :: VarEnv -> Int -> Synthesize [SubStmt]
generateTypes env n = replicateM n (generateType env)

-- | Generate a single object declaration randomly
generateType :: VarEnv -> Synthesize SubStmt
generateType env = do
  let types = M.toList (typeConstructors env)
  (typ, _) <- choice types -- COMBAK: check for empty list
  generateType' typ Concrete

-- | Generate a single object declaration given the type name. 'Concrete' generic option will only genrate an object of the designated type, whereas 'General' option allows parent types and (?) child types.
-- TODO: make sure which types are supported
-- NOTE: general option currently not used
generateType' :: String -> GenericOption -> Synthesize SubStmt
generateType' typ Concrete = do
  name <- freshName typ
  let stmt =
        Decl
          (TConstr $ TypeCtorApp {nameCons = typ, argCons = []})
          (VarConst name)
  appendStmt stmt
  return stmt
generateType' typ (General env) = do
  name <- freshName typ
  let types = possibleTypes env typ
  typ' <- choice types
  generateType' typ' Concrete

-- | Generate a single predicate
-- FIXME: currently not handling nesting
generatePredicate :: VarEnv -> Synthesize SubStmt
generatePredicate env = do
  let preds = M.toList (predicates env)
  (_, p) <- choice preds
  gen p
  where
    gen (Pred1 p1) = generatePredicate1 env
    gen (Pred2 p2) = generatePredicate2 env

generatePredicate1, generatePredicate2 :: VarEnv -> Synthesize SubStmt
generatePredicate1 env = do
  pred <- choice (pred1s env)
  opt <- gets (argOption . setting)
  args <- map PE <$> generateArgs env opt (map typeName $ tlspred1 pred)
  let stmt =
        ApplyP $
        Predicate
        {predicateName = PredicateConst $ namepred1 pred, predicateArgs = args}
  appendStmt stmt
  return stmt

-- TODO: make sure pred2 is higher-level predicates?
generatePredicate2 env = do
  pred <- choice (pred2s env)
  let args = []
  let stmt =
        ApplyP $
        Predicate
        {predicateName = PredicateConst $ namepred2 pred, predicateArgs = args}
  appendStmt stmt
  return stmt

-- | Generate a list of arguments for predicates or functions
generateArgs :: VarEnv -> ArgOption -> [String] -> Synthesize [Expr]
generateArgs env opt = mapM (generateArg env opt)

-- | Generate a list of arguments for predicates or functions
generateArg :: VarEnv -> ArgOption -> String -> Synthesize Expr
generateArg env Existing typ = do
  existingTypes <- gets declaredTypes
  case M.lookup typ existingTypes of
    Nothing -> generateArg env Generated typ
    Just lst -> do
      n <- choice lst -- pick one existing id
      return $ VarE $ VarConst n
generateArg env Generated typ = do
  generateType' typ Concrete -- TODO: concrete types for now
  generateArg env Existing typ
generateArg e Mixed typ
  -- TODO: check lazy eval and see if both branches actually get executed
 = do
  f <- choice [generateArg e Existing, generateArg e Generated]
  f typ

-- FIXME: finish the implementation
-- generateBinding :: VarEnv -> Synthesize SubStmt
-- generateBinding env =
--   op <- choice $ operators env
--   f  <- generate
--------------------------------------------------------------------------------
-- Substance Helpers
--------------------------------------------------------------------------------
pred1s :: VarEnv -> [Predicate1]
pred1s env = map (\(Pred1 p) -> p) $ M.elems $ predicates env

pred2s :: VarEnv -> [Predicate2]
pred2s env = map (\(Pred2 p) -> p) $ M.elems $ predicates env

possibleTypes :: VarEnv -> String -> [String]
possibleTypes env t =
  let subt = subTypes env
      allTypes = [typeName t1 | (t1, t2) <- subt, typeName t2 == t]
  in (t : allTypes)

typeName :: T -> String
typeName (TTypeVar t) = typeVarName t
typeName (TConstr t)  = nameCons t

--------------------------------------------------------------------------------
-- Randomness Helpers
--------------------------------------------------------------------------------
seedRnd :: Int
seedRnd = 7

rndNum :: (Int, Int) -> Synthesize Int
rndNum interval = do
  (n, g') <- gets (randomR interval . gen)
  modify $ \c -> c {gen = g'}
  return n

choice :: [a] -> Synthesize a
choice lst = do
  (i, g') <- gets (randomR (0, length lst - 1) . gen)
  modify $ \c -> c {gen = g'}
  return $ lst !! i

choiceSafe :: [a] -> Synthesize (Maybe a)
choiceSafe [] = return Nothing
choiceSafe lst = do
  (i, g') <- gets (randomR (0, length lst - 1) . gen)
  modify $ \c -> c {gen = g'}
  return $ Just (lst !! i)

--------------------------------------------------------------------------------
-- Name generation
--------------------------------------------------------------------------------
-- | Generate a new name given a type
freshName :: String -> Synthesize String
freshName typ = do
  cxt <- get
  let (n, names') = uniqueName (prefixOf typ) $ names cxt
  modify $ \cxt ->
    cxt
    { declaredTypes = M.insertWith (++) typ [n] (declaredTypes cxt)
    , names = names'
    }
  return n

prefixOf :: String -> String
prefixOf = map toLower . take 1

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case M.lookup nm ns of
    Nothing -> (nm, M.insert nm 1 ns)
    Just ix -> (nm ++ show ix, M.insert nm (ix + 1) ns)

--------------------------------------------------------------------------------
-- Synthesis helpers
--------------------------------------------------------------------------------
-- | Add statement to the AST
appendStmt :: SubStmt -> Synthesize ()
appendStmt stmt = modify $ \cxt -> cxt {prog = prog cxt ++ [stmt]}
