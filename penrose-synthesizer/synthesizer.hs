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
import           Data.List                      (elem, group, lookup, nub, sort)
import qualified Data.Map.Strict                as M
import           Data.Maybe
import           Data.String
import           Data.Typeable
import           Penrose.API
import qualified Penrose.Element                as D
import           Penrose.Env                    hiding (typeName)
import           Penrose.Pretty
import           Penrose.Substance
import           Penrose.Util                   (pickOne, trRaw)
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

data AllowDuplicates
  = Distinct
  | Repeated
  deriving (Show)

data Context = Context
  { names         :: Names
  , declaredTypes :: M.Map String [Name] -- | Map from type name to a list of names with the type
  , prog          :: SubProg -- | AST of the generated program
  , initProg      :: SubProg -- | AST of an input Substance program
  , gen           :: StdGen -- | A random generator
  , setting       :: Setting -- | Synthesizer settings
  } deriving (Show)

data Setting = Setting
  { lengthRange :: (Int, Int)
  , argOption   :: ArgOption
  } deriving (Show)

reset :: Synthesize ()
reset =
  modify $ \cxt -> cxt {declaredTypes = M.empty, names = M.empty, prog = []}

--------------------------------------------------------------------------------
-- CLI
--------------------------------------------------------------------------------
initContext :: Maybe String -> VarEnv -> Setting -> Context
initContext (Just subIn) env setting =
  let (SubOut subProg _ _) =
        fromRight (error "Failed to parse the input Substance program") $
        parseSubstance "" subIn env
      cxt = initContext Nothing env setting
  in cxt {initProg = subProg}
initContext Nothing _ setting =
  Context
  { declaredTypes = M.empty
  , names = M.empty
  , gen = mkStdGen seedRnd
  , prog = []
  , initProg = []
  , setting = setting
  }

getArgMode :: String -> ArgOption
getArgMode "mixed" = Mixed
getArgMode "generated" = Generated
getArgMode "existing" = Existing
getArgMode _ =
  error
    "Invalid argument generation mode. Must be one of: mixed, generated, existing."

parseSpec :: Maybe String -> IO (Maybe VarEnv)
parseSpec Nothing = return Nothing
parseSpec (Just specFile) = do
  specStr <- readFile specFile
  case D.parseElement specFile specStr of
    Left err ->
      error $
      "Cannot parse the specification file at " ++ specFile ++ "\n" ++ show err
    Right env -> return $ Just env

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
  argModeStr <- args `getArgOrExit` longOption "arg-mode"
  let [n, lmin, lmax] = map read [numProgs, minLength, maxLength] :: [Int]
  createDirectoryIfMissing True path -- create output dir if missing
  domainIn <- readFile domainFile
  let env = D.parseElement domainFile domainIn
  -- TODO: take in argoption as param
  let settings =
        Setting {lengthRange = (lmin, lmax), argOption = getArgMode argModeStr}
  case env of
    Left err -> error $ show err
    Right env -> do
      let subFile = args `getArg` longOption "substance"
      let specFile = args `getArg` longOption "spec"
      initSub <- safeReadFile subFile
      spec <- fromMaybe env <$> parseSpec specFile
      let (progs, _) =
            runState
              (generatePrograms spec n)
              (initContext initSub env settings)
      -- let (prog, cxt) =
      --       runState (generateProgram env) (initContext initSub env settings)
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
  p0 <- gets initProg -- TODO: move the loading phase to another function
  mapM_ loadStmt p0
  (lmin, lmax) <- gets (lengthRange . setting)
  n <- rndNum (lmin, lmax)
  generateStatements env n
  -- return $ ts ++ stmts
  p <- nub <$> gets prog
  let labelOption = AutoLabel Penrose.Substance.Default -- TODO: enfore export list in Substance module
  return $ labelOption : p0 ++ p

-- | Generate random Substance statements
generateStatements :: VarEnv -> Int -> Synthesize [SubStmt]
generateStatements env n = replicateM n (generateStatement env)

-- | Generate single random Substance statement
-- NOTE: every synthesizer that 'generateStatement' calls is expected to append its result to the AST, instead of just returning it. This is because certain lower-level functions are allowed to append new statements (e.g. 'generateArg'). Otherwise, we could write this module as a combinator.
generateStatement :: VarEnv -> Synthesize SubStmt
generateStatement env = do
  stmtF <-
    fromMaybe (error "No valid statement types to be generated.") <$>
    choiceSafe (stmtTypes env)
  stmtF env

stmtTypes :: VarEnv -> [VarEnv -> Synthesize SubStmt]
stmtTypes env =
  let typesExist = [M.null $ typeConstructors env, M.null $ predicates env]
      validGens = map snd $ filter fst $ zip typesExist stmtGens
  in validGens
  where
    stmtGens =
      [ generatePredicate
      , generateType
      , generateValueBinding
      ]

-- | Generate object declarations
generateTypes :: VarEnv -> Int -> Synthesize [SubStmt]
generateTypes env n = replicateM n (generateType env)

-- | Generate a single object declaration randomly
generateType :: VarEnv -> Synthesize SubStmt
generateType env = do
  let types = M.toList (typeConstructors env)
  (typ, _) <-
    fromMaybe (error "No type constructor found in the Domain program.") <$>
    choiceSafe types
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
  cxt <- get
  let preds = filter (filterPred cxt) (M.elems $ predicates env)
  generatePredicateIn preds

-- | Generate a single predicate given a list of predicates of choice
-- FIXME: handle the case of empty list -> randomly pick another predicate in Generate mode?
generatePredicateIn :: [PredicateEnv] -> Synthesize SubStmt
generatePredicateIn [] = error "No predicates found for synthesis."
generatePredicateIn preds = do
  p <- choice preds
  gen p
  where
    gen (Pred1 p1) = generatePredicate1 p1
    gen (Pred2 p2) = generatePredicate2 p2

generatePredicate1 :: Predicate1 -> Synthesize SubStmt
generatePredicate1 pred = do
  opt <- gets (argOption . setting)
  args <- map PE <$> generateArgs opt (map typeName $ tlspred1 pred)
  let stmt =
        ApplyP $
        Predicate
        {predicateName = PredicateConst $ namepred1 pred, predicateArgs = args}
  appendStmt stmt
  return stmt

generatePredicate2 :: Predicate2 -> Synthesize SubStmt
-- TODO: make sure pred2 is higher-level predicates?
generatePredicate2 pred = do
  let args = []
  let stmt =
        ApplyP $
        Predicate
        {predicateName = PredicateConst $ namepred2 pred, predicateArgs = args}
  appendStmt stmt
  return stmt

-- | Generate a list of arguments for predicates or functions
generateArgs :: ArgOption -> [String] -> Synthesize [Expr]
generateArgs opt = mapM (generateArg opt)

-- | Generate a list of arguments for predicates or functions
generateArg :: ArgOption -> String -> Synthesize Expr
generateArg Existing typ = do
  existingTypes <- gets declaredTypes
  case M.lookup typ existingTypes of
    Nothing -> error $ "No existing types for: " ++ show typ
    Just lst -> do
      n <- choice lst -- pick one existing id
      return $ VarE $ VarConst n
generateArg Generated typ = do
  generateType' typ Concrete -- TODO: concrete types for now
  generateArg Existing typ
generateArg Mixed typ
  -- TODO: check lazy eval and see if both branches actually get executed
 = do
  f <- choice [generateArg Existing, generateArg Generated]
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

-- | Given a predicate and the current context, return if this predicate can be generated with _only the existing declarations_.
filterPred :: Context -> PredicateEnv -> Bool
filterPred cxt (Pred1 Prd1 {tlspred1 = types}) =
  let typeAndVars = M.toList $ declaredTypes cxt
      existingFreq = map (\(t, ns) -> (t, length ns)) typeAndVars
      predFreq = countTypes types
  in all
       (\(t, n) -> compareFreq n $ filter ((==) t . fst) existingFreq)
       predFreq
  where
    countTypes ts = freq $ map typeName ts
    freq = map (\x -> (head x, length x)) . group . sort
    compareFreq predCount [(typ, typCount)] = predCount <= typCount
    compareFreq predCount _                 = False
filterPred pred (Pred2 _) = True -- NOTE: higher-order predicates are always allowed

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
-- Substance loader
--------------------------------------------------------------------------------
-- | Load a statement from a supplied Substance program to be used as the template for synthesis
loadStmt :: SubStmt -> Synthesize ()
loadStmt (Decl (TConstr TypeCtorApp {nameCons = typ}) (VarConst v)) =
  insertName typ v
loadStmt _ = return ()

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

insertName :: String -> String -> Synthesize ()
insertName typ name = do
  cxt <- get
  let (n, names') = uniqueName name $ names cxt
  modify $ \cxt ->
    cxt
    { declaredTypes = M.insertWith (++) typ [n] (declaredTypes cxt)
    , names = names'
    }

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
