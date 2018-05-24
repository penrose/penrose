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
import Data.Maybe
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
checkSubStmt varEnv  (Bind v e) = let (vstr, vt) = checkVarE varEnv v
                                      (estr, et) = checkExpression varEnv e -- TODO: Check lazy evaluation on et
                                  in if (isJust vt && isJust et && vt /= et) then varEnv{errors = (errors varEnv) ++ vstr ++ estr ++ "Expression of type " ++ (show et) ++ " assigned to variable of type " ++ (show vt) ++ "\n"}
                                     else varEnv{errors = (errors varEnv) ++ vstr ++ estr}
checkSubStmt varEnv  (ApplyP p) = checkPredicate varEnv p

checkPredicate :: VarEnv -> Predicate -> VarEnv 
checkPredicate varEnv (Predicate (PredicateConst p) args) = case checkAndGet p (predicates varEnv) of
                                              Right p  -> case p of 
                                                (Pred1 p1) -> checkVarPred varEnv args p1
                                                (Pred2 p2) -> checkRecursePred varEnv args
                                              Left err -> varEnv{errors = (errors varEnv) ++ err}

checkVarPred :: VarEnv -> [PredArg] -> Predicate1 -> VarEnv
checkVarPred varEnv args (Predicate1 name yls kls tls p) = let exprArgs = map isVarPredicate args
                                                               errAndTypesLs = map (checkExpression varEnv) exprArgs
                                                               errls = map (\(err1,t1) -> err1) errAndTypesLs
                                                               err = foldl (\err1 err2 -> err1 ++ err2) "" errls
                                                               argTypes = map (\(err1,t1) -> t1) errAndTypesLs
                                                           in if (foldl (\b at1 -> b && isJust at1) True argTypes)
                                                              then let argTypes2 = map (\a -> KT (fromJust a)) argTypes
                                                                       tls2 = map (\a -> KT a) tls
                                                                       sigma = substitution M.empty argTypes2 tls2
                                                                   in if (sigma == M.empty) 
                                                                      then varEnv{errors = (errors varEnv) ++ err} -- err should be empty str
                                                                      else varEnv{errors = (errors varEnv) ++ err} -- err should be empty str
                                                              else 
                                                               varEnv{errors = (errors varEnv) ++ err}
                                                               
isVarPredicate :: PredArg -> Expr
isVarPredicate (PP p) = error("Mixed predicate types!")
isVarPredicate (PE p) = p

isRecursedPredicate :: PredArg -> Predicate
isRecursedPredicate (PP p) = p
isRecursedPredicate (PE p) = error("Mixed predicate types!")

checkRecursePred :: VarEnv -> [PredArg] -> VarEnv
checkRecursePred varEnv args = let predArgs = map isRecursedPredicate args
                               in foldl checkPredicate varEnv predArgs

checkExpression :: VarEnv -> Expr -> (String, Maybe T)
checkExpression varEnv (VarE v) = checkVarE varEnv v
checkExpression varEnv (ApplyExpr f) = checkFunc varEnv f

checkVarE :: VarEnv -> Var -> (String, Maybe T)
checkVarE varEnv v = case M.lookup v (varMap varEnv) of
                     Nothing -> ("Variable " ++ (show v) ++ " not in environment\n", Nothing)
                     vt -> ("", vt)

checkFunc :: VarEnv -> Func -> (String, Maybe T)
checkFunc varEnv (Func f args) = let vcEnv = M.lookup f (varConstructors varEnv)
                                     fEnv = M.lookup f (operations varEnv)
                                 in if (isNothing(vcEnv) && isNothing(fEnv))
                                    then ("Function or Val Constructor " ++ (show f) ++ " not in environment\n", Nothing)
                                    else if (isJust(vcEnv))
                                         then checkVarConsInEnv varEnv (Func f args) (fromJust vcEnv)
                                    else checkFuncInEnv varEnv (Func f args) (fromJust fEnv)
                                         

checkFuncInEnv :: VarEnv -> Func -> Operation -> (String, Maybe T)
checkFuncInEnv varEnv (Func f args) (Operation name yls kls tls t) = let errAndTypesLs = map (checkExpression varEnv) args
                                                                         errls = map (\(err1,t1) -> err1) errAndTypesLs
                                                                         err = foldl (\err1 err2 -> err1 ++ err2) "" errls
                                                                         argTypes = map (\(err1,t1) -> t1) errAndTypesLs
                                                                     in if (foldl (\b at1 -> b && isJust at1) True argTypes)
                                                                        then let argTypes2 = map (\a -> KT (fromJust a)) argTypes
                                                                                 tls2 = map (\a -> KT a) tls
                                                                                 sigma = substitution M.empty argTypes2 tls2
                                                                             in if (sigma == M.empty)
                                                                                then (err, Just t) -- err should be empty str
                                                                                else (err, Just (applySubstitution sigma t)) -- err should be empty str
                                                                        else  
                                                                         (err, Nothing)

checkVarConsInEnv  :: VarEnv -> Func -> VarConstructor -> (String, Maybe T)                                       
checkVarConsInEnv varEnv (Func f args) (VarConstructor name yls kls tls t) = let errAndTypesLs = map (checkExpression varEnv) args
                                                                                 errls = map (\(err1,t1) -> err1) errAndTypesLs
                                                                                 err = foldl (\err1 err2 -> err1 ++ err2) "" errls
                                                                                 argTypes = map (\(err1,t1) -> t1) errAndTypesLs
                                                                             in if (foldl (\b at1 -> b && isJust at1) True argTypes)
                                                                                then let argTypes2 = map (\a -> KT (fromJust a)) argTypes
                                                                                         tls2 = map (\a -> KT a) tls
                                                                                         sigma = substitution M.empty argTypes2 tls2
                                                                                      in if (sigma == M.empty) 
                                                                                         then (err, Just t) -- err should be empty str
                                                                                         else (err, Just (applySubstitution sigma t)) -- err should be empty str
                                                                                else 
                                                                                 (err, Nothing)

applySubstitution :: M.Map Y Arg -> T -> T
applySubstitution sigma (TTypeVar vt) = case sigma M.! (TypeVarY vt) of
                                        AVar v -> error("Type var being mapped to variable in substitution sigma, error in TypeChecker!")
                                        AT t -> t
applySubstitution sigma (TConstr (ConstructorInvoker t args)) = let argsSub = map (applySubstitutionHelper sigma) args
                                                                in (TConstr (ConstructorInvoker t argsSub))

applySubstitutionHelper :: M.Map Y Arg -> Arg -> Arg
applySubstitutionHelper sigma (AVar v) = case sigma M.! (VarY v) of
                                         AVar v2 -> (AVar v2)
                                         AT t -> error("Var being mapped to a type in substitution sigma, error in TypeChecker!")
applySubstitutionHelper sigma (AT t) = AT (applySubstitution sigma t)
                                                                                 
substitution :: M.Map Y Arg -> [K] -> [K] -> M.Map Y Arg
substitution sigma argTypes formalTypes = let types = zip argTypes formalTypes
                                              sigma2 = foldl substitutionHelper sigma types
                                          in sigma2

substitutionHelper :: M.Map Y Arg -> (K, K) -> M.Map Y Arg
substitutionHelper sigma ((Ktype aT), (Ktype fT)) = sigma
substitutionHelper sigma ((Ktype aT), (KT fT)) = error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
substitutionHelper sigma ((KT aT), (Ktype fT)) = error("Argument type " ++ (show aT) ++ " doesn't match expected type " ++ (show fT))
substitutionHelper sigma ((KT (TTypeVar atv)), (KT (TTypeVar ftv))) = substitutionInsert sigma (TypeVarY ftv) (AT (TTypeVar atv))
substitutionHelper sigma ((KT (TTypeVar atv)), (KT (TConstr (ConstructorInvoker ftc argsFT)))) = error("Argument type " ++ (show atv) ++ " doesn't match expected type " ++ (show ftc))
substitutionHelper sigma ((KT (TConstr (ConstructorInvoker atc argsAT))), (KT (TTypeVar ftv))) = substitutionInsert sigma (TypeVarY ftv) (AT (TConstr (ConstructorInvoker atc argsAT)))
substitutionHelper sigma ((KT (TConstr (ConstructorInvoker atc argsAT))), (KT (TConstr (ConstructorInvoker ftc argsFT)))) = if (atc /= ftc)
                                                                            then error("Argument type " ++ (show atc) ++ " doesn't match expected type " ++ (show ftc))
                                                                            else let args = zip argsAT argsFT
                                                                                     sigma2 = foldl substitutionHelper2 sigma args
                                                                                 in sigma2

substitutionHelper2 :: M.Map Y Arg -> (Arg, Arg) -> M.Map Y Arg
substitutionHelper2 sigma ((AVar av), (AVar fv)) = substitutionInsert sigma (VarY fv) (AVar av)
substitutionHelper2 sigma ((AVar av), (AT ft)) = error("Argument type's argument " ++ (show av) ++ " doesn't match expected type's argument " ++ (show ft))
substitutionHelper2 sigma ((AT at), (AVar fv)) = error("Argument type's argument " ++ (show at) ++ " doesn't match expected type's argument " ++ (show fv))
substitutionHelper2 sigma ((AT at), (AT ft)) = substitutionHelper sigma ((KT at), (KT ft))

substitutionInsert :: M.Map Y Arg -> Y -> Arg -> M.Map Y Arg
substitutionInsert sigma y arg = case M.lookup y sigma of
                                 Nothing -> M.insert y arg $ sigma
                                 arg' -> if (arg /= (fromJust arg')) then error("Substituions inconsistent - no substitution can exist")
                                         else sigma

--argTypeLookupHelper :: VarEnv -> Y -> Either K String
--argTypeLookupHelper varEnv (TypeVarY t) = case M.lookup t (typeVarMap varEnv) of
--                                   Nothing -> Right "Argument " ++ (show t) ++ " not in environment\n"
--                                   tType -> Left tType
--argTypeLookupHelper varEnv (VarY v) = case M.lookup v (varMap varEnv) of
--                               Nothing -> Right "Argument " ++ (show v) ++ " not in environment\n"
--                               vType -> Left vType


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

