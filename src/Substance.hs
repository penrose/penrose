-- | "Substance" contains the grammar, parser, and semantic checker for
--   the Substance language. It also contains translators to Alloy and
--   the driver for it.

{-# OPTIONS_HADDOCK prune #-}
module Substance where
--module Main (main) where -- for debugging purposes
-- TODO split this up + do selective export

import Utils
import System.Process
import Data.Void
import Control.Monad (void)
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Env
-- import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Dsll as D
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------- Substance AST ---------------------------------------

data ValConstructorName = ValConst String             -- “Cons”, “Times”
    deriving (Show, Eq, Typeable)

data OperatorName = OperatorConst String             -- “Intersection” 
    deriving (Show, Eq, Typeable)

data PredicateName = PredicateConst String            -- “Intersect”
    deriving (Show, Eq, Typeable)


data Func = Func { nameFunc :: String, argFunc:: [Expr]}
    deriving (Eq, Typeable)
instance Show Func where
    show (Func nameFunc argFunc) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameFunc
              aString = show argFunc


data Expr = VarE Var | ApplyExpr Func
    deriving (Show, Eq, Typeable)


data PredArg = PE Expr | PP Predicate
    deriving (Show, Eq, Typeable)


data Predicate = Predicate { predicateName :: PredicateName, predicateArgs:: [PredArg]}
    deriving (Eq, Typeable)
instance Show Predicate where
    show (Predicate predicateName predicateArgs) = nString ++ "(" ++ aString ++ ")"
        where nString = show predicateName
              aString = show predicateArgs



data SubStmt = Decl T Var | Bind Var Expr | ApplyP Predicate
    deriving (Show, Eq, Typeable)


-- | Program is a sequence of statements
type SubProg = [SubStmt]
type SubObjDiv = ([SubDecl], [SubConstr])



-- | Special data types for passing on to the style parser ------------------------------------

-- | Declaration of Substance objects
data SubDecl
    = SubDeclConst T Var
    deriving (Show, Eq, Typeable)

-- | Declaration of Substance constaints
data SubConstr
    = SubConstrConst String [PredArg]
    deriving (Show, Eq, Typeable)

-- | Both declarations and constaints in Substance are regarded as objects,
--   which is possible for Style to select later.
data SubObj = LD SubDecl | LC SubConstr deriving (Show, Eq, Typeable)


--------------------------------------- Substance Parser --------------------------------------
-- | 'substanceParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
substanceParser :: Parser [SubStmt]
substanceParser = between scn eof subProg -- Parse all the statemnts between the spaces to the end of the input file

-- |'subProg' parses the entire actual Substance Core language program which is a collection of statements
subProg :: Parser [SubStmt]
subProg = do 
  stml <- subStmt `sepEndBy` newline'
  return stml


predicateNameParser ::Parser PredicateName
predicateNameParser = do
    i <- identifier
    return (PredicateConst i)

functionParser :: Parser Func
functionParser = do
  n <- identifier
  args <- parens (exprParser `sepBy1` comma) 
  return (Func {nameFunc = n, argFunc = args})


exprParser, varE, applyF :: Parser Expr
exprParser = try varE <|> try applyF
varE = do
  i <- varParser
  return (VarE i)
applyF = do
  f <- functionParser
  return (ApplyExpr f)

predicateArgParser, predicateArgParserE, predicateArgParserP  :: Parser PredArg
predicateArgParser = try predicateArgParserE <|> predicateArgParserP
predicateArgParserE = do
  e <- exprParser
  return (PE e)
predicateArgParserP = do
  p <- predicateParser
  return (PP p)

predicateParser :: Parser Predicate
predicateParser = do
  n <- predicateNameParser
  args <- parens (predicateArgParser `sepBy1` comma) 
  return (Predicate {predicateName = n, predicateArgs = args})

subStmt, decl, bind, applyP :: Parser SubStmt
subStmt = try decl <|> try applyP <|> bind
decl = do
  t' <- tParser
  v' <- varParser
  return (Decl t' v')
bind = do
  v' <- varParser
  eq
  e' <- exprParser
  return (Bind v' e')
applyP = do
  p <- predicateParser
  return (ApplyP p)

-- --------------------------------------- Substance TypeChecker ---------------------------

-- | 'check' is the top-level semantic checking function. It takes a Substance
-- AST as the input and the environment recived from the DSLL type checker, checks the validity of the program acoording to 
-- the typechecking rules, and outputs a collection of information.
check :: SubProg -> VarEnv -> VarEnv
check p varEnv = let env = foldl checkSubStmt varEnv p
                 in if (null (errors env)) then env else error("Substance type checking failed with the following problems: \n" ++ (errors env))


checkSubStmt :: VarEnv -> SubStmt -> VarEnv
checkSubStmt varEnv  (Decl t v) = let env = checkT varEnv t
                                  in env {varMap = M.insert v t $ varMap env}
checkSubStmt varEnv  (Bind v e) = let env1 = checkVar varEnv v
                                      env2 = checkExpression env1 e
                                  in env2
checkSubStmt varEnv  (ApplyP p) = checkPredicate varEnv p

checkPredicate :: VarEnv -> Predicate -> VarEnv 
checkPredicate varEnv (Predicate (PredicateConst p) args) = case checkAndGet p (predicates varEnv) of
                                              Right p  -> case p of 
                                                (Pred1 p1) -> checkVarPred varEnv args p1
                                                (Pred2 p2) -> checkRecursePred varEnv args
                                              Left err -> varEnv{errors = (errors varEnv) ++ err}

checkVarPred :: VarEnv -> [PredArg] -> Predicate1 -> VarEnv
checkVarPred varEnv args (Predicate1 name yls kls tls p) = let (argTypesLs, strls) = foldl argTypeLookup varEnv args
                                                           in if ((fold (++) strls) == "")
                                                              then let argTypes = fold (++) argTypesLs
                                                                       sigma = substitution M.empty argTypes tls
                                                                   in if (sigma == M.empty) then varEnv
                                                                      else varEnv
                                                              else varEnv{errors = (errors varEnv) ++ (fold (++) strls)}

isRecursedPredicate :: PredArg -> Predicate
isRecursedPredicate (PP p) = p
isRecursedPredicate (PE p) = error("Mixed predicate types!")

checkRecursePred :: VarEnv -> [PredArg] -> VarEnv
checkRecursePred varEnv args = let predArgs = map isRecursedPredicate args
                               in foldl checkPredicate varEnv predArgs

checkExpression :: VarEnv -> Expr -> VarEnv
checkExpression varEnv (VarE v) = varEnv
checkExpression varEnv (ApplyExpr f) = varEnv

checkFunc ::  VarEnv -> Func -> VarEnv
checkFunc varEnv f = varEnv

substitution :: M.Map Y Arg -> [K] -> [K] -> M.Map Y Arg
substitution sigma argTypes formalTypes = let types = zip argTypes formalTypes
                                              sigmas = foldl substitutionHelper sigma types
                                          in fold union sigmas

substitutionHelper :: M.Map Y Arg -> (K, K) -> M.Map Y Arg
substitutionHelper sigma ((Ktype aT), (Ktype fT)) = sigma
substitutionHelper sigma ((Ktype aT), (KT fT)) = error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
substitutionHelper sigma ((KT aT), (Ktype fT)) = error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
substitutionHelper sigma ((KT (TTypeVar atv)), (KT (TTypeVar ftv))) = substitutionInsert sigma (TypeVarY ftv) (AT (TTypeVar atv))
substitutionHelper sigma ((KT (TTypeVar atv)), (KT (TConstr ftc argsFT))) = error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
substitutionHelper sigma ((KT (TConstr atc argsAT)), (KT (TTypeVar ftv))) = substitutionInsert sigma (TypeVarY ftv) (AT (TConstr atc argsAT)) $ sigma
substitutionHelper sigma ((KT (TConstr atc argsAT)), (KT (TConstr ftc argsFT))) = if (atc /= ftc)
                                                                            then error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
                                                                            else let args = zip argsAT argsFT
                                                                                     sigmas = foldl substitutionHelper2 sigma args
                                                                                 in fold union sigmas

substitutionHelper2 :: M.Map Y Arg -> (Arg, Arg) -> M.Map Y Arg
substitutionHelper2 sigma ((AVar av), (AVar fv)) = substitutionInsert sigma (VarY fv) (AVar av) $ sigma
substitutionHelper2 sigma ((AVar av), (AT ft)) = error("Argument type's argument " ++ (show av) ++ " doesn't match expected type's argument " ++ (show ft))
substitutionHelper2 sigma ((AT at), (AVar fv)) = error("Argument type's argument " ++ (show at) ++ " doesn't match expected type's argument " ++ (show fv))
substitutionHelper2 sigma ((AT at), (AT ft)) = substitutionHelper sigma ((KT at), (KT ft))

substitutionInsert :: M.Map Y Arg -> Y -> Arg -> M.Map Y Arg
substitutionInsert sigma y arg = case M.lookup y sigma of
                                 Nothing -> M.insert y arg $ sigma
                                 arg' -> if (arg /= arg') then error("Substituions inconsistent - no substitution can exist")
                                         else sigma

argTypeLookup :: VarEnv -> Expr -> ([K], String)
argTypeLookup varEnv (VarE v) = let argTypes = []
                                in case argTypeLookup varEnv arg of
                                    Right err -> (argTypes, err)
                                    Left t -> (t: argTypes, "")
argTypeLookup varEnv (ApplyExpr f args) = 

argTypeLookupHelper :: VarEnv -> Y -> Either K String
argTypeLookupHelper varEnv (TypeVarY t) = case M.lookup t (typeVarMap varEnv) of
                                   Nothing -> Right "Argument " ++ (show t) ++ " not in environment\n"
                                   tType -> Left tType
argTypeLookupHelper varEnv (VarY v) = case M.lookup v (varMap varEnv) of
                               Nothing -> Right "Argument " ++ (show v) ++ " not in environment\n"
                               vType -> Left vType


-- --------------------------------------- Substance Loader --------------------------------
-- | Load all the substance objects and pack them for visualization at runtime.hs

data SubObjects = SubObjects {
    subObjs :: [SubObj] --,  -- declared Substance objects(including constraints, which is, again, viewed also as objects)
} deriving (Show, Eq, Typeable)


loadObjects :: SubProg -> SubObjects
loadObjects p = let env1 = foldl passDecls initE p
                    env2 = foldl passReferencess env1 p
                in
                    env2 { subObjs = reverse $ subObjs env2 }
                where initE = SubObjects { subObjs = []}

applyDef (n, m) d = case M.lookup n d of
    Nothing -> error "applyDef: definition not found!"
    Just (_, e) -> e

-- | 'checkDecls' checks the validity of declarations of objects.
passDecls :: SubObjects -> SubStmt -> SubObjects
passDecls e (Decl t s)  = e { subObjs = toObj t s : subObjs e}
passDecls e _ = e -- Ignore all other statements

-- 'toObj' translates [Type] + [Identiers] to concrete Substance objects, to be selected by the Style program
toObj :: T -> Var -> SubObj
toObj t v = LD $ (SubDeclConst t v)

-- 'checkReferencess' checks any statement that refers to other objects. For example,
-- > Subset A B
-- refers to identifiers @A@ and @B@. The function will perform a lookup in the symbol table, make sure the objects exist and are of desired types -- in this case, 'Set' -- and throws an error otherwise.
passReferencess :: SubObjects -> SubStmt -> SubObjects
passReferencess e (ApplyP (Predicate (PredicateConst t) ss)) = e { subObjs = newConstrs : subObjs e }
    where newConstrs = (toConstr t ss)
passReferencess e _ = e -- Ignore all other statements


-- | Similar to 'toObj'
toConstr :: String -> [PredArg] ->  SubObj
toConstr p vl = LC $  (SubConstrConst p vl)

-- helper function that checks the symbol table before inserting, making sure there is no duplicated declarations
checkAndInsert s t m = case M.lookup s m of
    Nothing -> M.insert s t m
    _ -> error ("Duplicated symbol: " ++ s)
-- helper function that checks the existence and type of an object
checkNameAndTyp m (s, t) = case M.lookup s m of
    Nothing -> error ("Undefined symbol: " ++ s)
    Just t' -> if t == t' then s
            else error ("Type of " ++ s ++ " is incorrect. Expecting " ++ show t ++ " , but have " ++ show t')

-- | `subSeparate` aplits a list of Substance objects into declared objects and constaints on these objects
subSeparate :: [SubObj] -> SubObjDiv
subSeparate = foldr separate ([], [])
            where separate line (decls, constrs) =
                           case line of
                           (LD x) -> (x : decls, constrs)
                           (LC x) -> (decls, x : constrs)


-- | 'parseSubstance' runs the actual parser function: 'substanceParser', taking in a program String, parses it, semantically checks it, and eventually invoke Alloy if needed. It outputs a collection of Substance objects at the end.
parseSubstance :: String -> String -> VarEnv -> IO ([SubObj],VarEnv)
parseSubstance subFile subIn varEnv = case runParser substanceParser subFile subIn of
     Left err -> error (parseErrorPretty err)
     Right xs -> do
         putStrLn ("Substance AST: \n")
         putStrLn (show xs)
         --mapM_ print xs
         divLine
         let subEnv = check xs varEnv
             c = loadObjects xs
         return ((subObjs c), subEnv)

-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc SubstanceCore.hs; ./SubstanceCore <substance core-file>

main :: IO ()
main = do
    args <- getArgs
    let subFile = head args
    subIn <- readFile subFile
    parseTest substanceParser subIn
    --parsed <- parseFromFile  
    --mapM_ print parsed
    return ()

