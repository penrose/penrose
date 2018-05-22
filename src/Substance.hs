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
-- import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Dsll as D
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L




--------------------------------------- Substance AST ---------------------------------------


data SubTypeName = TypeConst String -- these are all names, e.g. “Set”
        | AllT -- specifically for global selection in Style
    deriving (Show, Eq, Typeable)

data ValConstructorName = ValConst String             -- “Cons”, “Times”
    deriving (Show, Eq, Typeable)

data OperatorName = OperatorConst String             -- “Intersection” 
    deriving (Show, Eq, Typeable)

data PredicateName = PredicateConst String            -- “Intersect”
    deriving (Show, Eq, Typeable)

data TypeVar = TypeVarConst String
    deriving (Show, Eq, Typeable)

data Var = VarConst String
    deriving (Show, Eq, Typeable)

data T = TConstr ConstructorInvoker | TypeVarT TypeVar
    deriving (Show, Eq, Typeable)


data ConstructorInvoker = ConstructorInvoker { nameCons :: SubTypeName, argCons:: [Arg]}
    deriving (Eq, Typeable)
instance Show ConstructorInvoker where
    show (ConstructorInvoker nameCons argCons) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameCons
              aString = show argCons

data Arg = AVar Var
    | AT T
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

-- | Both declarations and constaints in Substance are regarded as objects, which is possible for Style to select later.
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

typeVarParser :: Parser TypeVar
typeVarParser = do
    i <- identifier
    return (TypeVarConst i)

varParser :: Parser Var
varParser = do
    i <- identifier
    return (VarConst i)

subTypeNameParser :: Parser SubTypeName
subTypeNameParser = do
    i <- identifier
    return (TypeConst i)

predicateNameParser ::Parser PredicateName
predicateNameParser = do
    i <- identifier
    return (PredicateConst i)


argParser, varA , typeA :: Parser Arg
argParser = try varA <|> typeA
varA  = do
  i <- varParser
  return (AVar i)
typeA = do
  i <- tParser
  return (AT i)

constructorInvoker :: Parser ConstructorInvoker
constructorInvoker = do
  n <- subTypeNameParser
  args <- parens (argParser `sepBy1` comma)
  return (ConstructorInvoker{nameCons = n, argCons = args })


tParser, constructorInvokerT, tT :: Parser T
tParser = try constructorInvokerT <|> tT
constructorInvokerT = do
  ci <- constructorInvoker
  return (TConstr ci)
tT = do
  i <- typeVarParser
  return (TypeVarT i)


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
subStmt = try decl <|> try bind <|> applyP
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
data SubEnv = SubEnv {
   a :: String
} deriving (Show, Eq, Typeable)

check :: SubProg -> D.DsllEnv -> SubEnv
check p dsll = SubEnv{a = "hi"}


checkTypeVar :: SubEnv -> TypeVar -> SubEnv
checkTypeVar e v = e

checkVar :: SubEnv -> Var -> SubEnv
checkVar e v = e

checkType :: SubEnv -> T -> SubEnv
checkType e v = e

checkArg :: SubEnv -> Arg -> SubEnv
checkArg e v  = e

checkExpr :: SubEnv -> Expr -> SubEnv
checkExpr e v  = e

-- checkQ :: SubEnv ->  -> SubEnv
-- checkExpr e v  = e


-- --------------------------------------- Substance Loader --------------------------------

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
parseSubstance :: String -> String -> D.DsllEnv -> IO ([SubObj], SubEnv)
parseSubstance subFile subIn dsllEnv = case runParser substanceParser subFile subIn of
     Left err -> error (parseErrorPretty err)
     Right xs -> do
         mapM_ print xs
         --divLine
         let subEnv = check xs dsllEnv
         let c = loadObjects xs
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

