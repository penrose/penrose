-- | "Env" Contains all the shared code among substance and dsll:
--   AST, parser and typechecking functions
--   It also contains the environemt for the typechecker
--   Author: Dor Ma'ayan, May 2018

{-# OPTIONS_HADDOCK prune #-}
module Env where
--module Main (main) where -- for debugging purposes

import Utils
import System.Process
import Control.Monad (void)
import Data.Void
import Debug.Trace
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.Functor.Classes
import Data.List
import Data.Maybe (fromMaybe)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L

----------------------------------- AST -----------------------------------------------

data TypeName = TypeNameConst String -- these are all names, e.g. “Set”
        | AllT -- specifically for global selection in Style
    deriving (Show, Eq, Typeable)

data TypeVar = TypeVar { typeVarName :: String, typeVarPos :: SourcePos}
     deriving (Show, Typeable)
instance Eq TypeVar where
  (TypeVar n1 _) == (TypeVar n2 _) = n1 == n2
instance Ord TypeVar where
  (TypeVar s1 _) `compare` (TypeVar s2 _) = s1 `compare` s2

data Var = VarConst String
     deriving (Show, Eq, Typeable, Ord)

data Y = TypeVarY TypeVar | VarY Var
     deriving (Show, Eq, Typeable, Ord)


data T = TTypeVar TypeVar
    | TConstr ConstructorInvoker
    deriving (Show, Eq, Typeable)


data ConstructorInvoker = ConstructorInvoker { nameCons :: String, argCons:: [Arg], constructorInvokerPos :: SourcePos}
    deriving (Typeable)
instance Show ConstructorInvoker where
    show (ConstructorInvoker nameCons argCons posCons) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameCons
              aString = show argCons
instance Eq ConstructorInvoker where
  (ConstructorInvoker n1 a1 _) == (ConstructorInvoker n2 a2 _) = (n1 == n2 && a1 == a2)

data Arg = AVar Var
    | AT T
    deriving (Show, Eq, Typeable)

data K = Ktype Type
     | KT T
     deriving (Show, Eq, Typeable)


data Type = Type{ typeName :: String, typePos :: SourcePos}
     deriving (Show, Typeable)

instance Eq Type where
  (Type n1 _) == (Type n2 _) = n1 == n2


data Prop =  Prop {propName:: String, propPos :: SourcePos}
     deriving (Show, Typeable)
instance Eq Prop where
  (Prop n1 _) == (Prop n2 _) = n1 == n2

----------------------------------- Parser --------------------------------------------

typeNameParser ::Parser TypeName
typeNameParser = do
    i <- identifier
    return (TypeNameConst i)

typeParser :: Parser Type
typeParser = do
    rword "type"
    pos <- getPosition
    return Type{ typeName = "type", typePos = pos}

varParser :: Parser Var
varParser = do
    i <- identifier
    return (VarConst i)

typeVarParser :: Parser TypeVar
typeVarParser = do
    aps
    i <- identifier
    pos <- getPosition
    return TypeVar { typeVarName =  i, typeVarPos = pos}

yParser, y1, y2 :: Parser Y
yParser = try y1 <|> y2
y1 = do
  i <- varParser
  return (VarY i)
y2 = do
  i <- typeVarParser
  return (TypeVarY i)

propParser :: Parser Prop
propParser = do
    rword "Prop"
    pos <- getPosition
    return Prop { propName = "Prop", propPos = pos}


tParser, tConstructorInvokerParser, typeVarParser' :: Parser T
tParser = try tConstructorInvokerParser <|> typeVarParser'
tConstructorInvokerParser = do
    i         <- identifier
    arguments <- option [] $ parens (argParser `sepBy1` comma) --try (parens (argParser `sepBy1` comma)) <|> emptyArgList --option [] $ parens (argParser `sepBy1` comma) --try (parens (argParser `sepBy1` comma)) <|> emptyArgList-- -- --
    pos <- getPosition
    return (TConstr (ConstructorInvoker {nameCons = i, argCons = arguments, constructorInvokerPos = pos}))
typeVarParser' = do
    i <- typeVarParser
    return (TTypeVar i)

argParser, varParser', tParser'  :: Parser Arg
argParser = try tParser' <|> varParser'
varParser' = do
     i <- varParser
     return (AVar i)
tParser' = do
    t <- tParser
    return (AT t)

kParser, kTypeParser, tParser'' :: Parser K
kParser = try kTypeParser <|> try tParser''
kTypeParser = do
     rword "type"
     pos <- getPosition
     return (Ktype (Type { typeName = "type", typePos = pos}))
tParser'' = do
    t <- tParser
    return (KT t)


emptyArgList :: Parser [Arg]
emptyArgList = do
  lparen
  rparen
  return []
----------------------------------- Typechecker aux functions ------------------------------------------

firsts :: [(a,b)] -> [a]
firsts xs = [x | (x,_) <- xs]

seconds :: [(a,b)] -> [b]
seconds xs = [x | (_,x) <- xs]

second :: (a,b) -> b
second (a,b) = b


checkAndGet k m pos = case M.lookup k m of
  Nothing -> Left ("Error in " ++ (sourcePosPretty pos) ++ " : " ++ k ++ " Doesn't exist in the context \n")
  Just v ->  Right v


lookUpK :: VarEnv -> Arg -> K
lookUpK e (AT  (TTypeVar t))  = (Ktype ((typeVarMap e) M.! t)) --(Ktype (Type {typeName = "type", typePos = typeVarPos t }))
lookUpK e (AT  (TConstr t))  = if (nameCons t) `elem` (declaredNames e) then lookUpK e (AVar (VarConst (nameCons t))) else (Ktype (Type {typeName = "type", typePos = constructorInvokerPos t}))
lookUpK e (AVar v) = (KT ((varMap e) M.! v))

getTypesOfArgs :: VarEnv -> [Arg] -> [K]
getTypesOfArgs e args = map (lookUpK e) args

updateEnv :: VarEnv -> (Y,K) -> VarEnv
updateEnv e (TypeVarY y, Ktype t) = e {typeVarMap = M.insert y t $ typeVarMap e}
updateEnv e (VarY y, KT t) = e {varMap = M.insert y t $ varMap e}
updateEnv e err = e {errors = (errors e) ++ "Problem in update: " ++ (show err) ++ "\n"}

addName :: String -> VarEnv -> VarEnv
addName a e = if (a `elem` (names e)) then e {errors = (errors e) ++ ("Name already exsist in the context \n")} else e {names = a : (names e)}

addDeclaredName :: String -> VarEnv -> VarEnv
addDeclaredName a e = if (a `elem` (declaredNames e)) then e {errors = (errors e) ++ ("Name already exsist in the context \n")} else e {declaredNames = a : (declaredNames e)}

--------------------------------------- Env Data Types ---------------------------------------
-- | Environment for the dsll semantic checker. As the 'check' function executes, it
-- accumulate information such as symbol tables in the environment.

-- | list of elements that might appear in the global context

data Ttype = Ttype {yt :: Y, kt :: K}  deriving (Show, Eq, Typeable)
data TypeConstructor = TypeConstructor {nametc :: String, klstc :: [K], typtc :: Type} deriving (Show, Eq, Typeable)
data VarConstructor = VarConstructor {namevc :: String, ylsvc :: [Y], klsvc :: [K], tlsvc :: [T], tvc :: T} deriving (Show, Eq, Typeable)
data Operation = Operation {nameop :: String, ylsop :: [Y], klsop :: [K], tlsop :: [T], top :: T} deriving (Show, Eq, Typeable)
data PredicateEnv = Pred1 Predicate1 | Pred2 Predicate2 deriving (Show, Eq, Typeable)
data Predicate1 = Predicate1 {namepred1 :: String, ylspred1 :: [Y], klspred1 :: [K], tlspred1 :: [T], ppred1 :: Prop} deriving (Show, Eq, Typeable)
data Predicate2 = Predicate2 {namepred2 :: String, plspred2 :: [Prop], ppred2 :: Prop} deriving (Show, Eq, Typeable)

data VarEnv = VarEnv{ typeConstructors :: M.Map String TypeConstructor,
  varConstructors :: M.Map String VarConstructor,
  operations :: M.Map String Operation,
  predicates :: M.Map String PredicateEnv,
  typeVarMap :: M.Map TypeVar Type,
  varMap :: M.Map Var T,
  names :: [String], -- a global list which contains all the names of types in that env
  declaredNames :: [String],  -- a global list which contains all the names of elements declared in that env
  errors :: String -- a string which accumulates all the errors founded during the run of the typecheker
} deriving(Show, Eq, Typeable)



checkTypeVar :: VarEnv -> TypeVar -> VarEnv
checkTypeVar e v = if (M.member v (typeVarMap e)) then e
                else e {errors = (errors e) ++ ("TypeVar " ++ (show v) ++ "is not in scope \n")}

checkVar :: VarEnv -> Var -> VarEnv
checkVar e v = if (M.member v (varMap e)) then e
    else e {errors = (errors e) ++ ("Var " ++ (show v) ++ "is not in scope \n")}


checkY :: VarEnv -> Y -> VarEnv
checkY e (TypeVarY y) = checkTypeVar e y
checkY e (VarY y) = checkVar e y

checkArg :: VarEnv -> Arg -> VarEnv
checkArg e (AVar v) = checkVar e v
checkArg e (AT (TConstr i)) = if (nameCons i) `elem` (declaredNames e) then checkVar e (VarConst (nameCons i)) else checkT e (TConstr i)
checkArg e (AT t) = checkT e t

checkT :: VarEnv -> T -> VarEnv
checkT e (TTypeVar t) = checkTypeVar e t
checkT e (TConstr c) = checkConstructorInvoker e c

checkType :: VarEnv -> Type -> VarEnv
checkType e t = e


-- --TODO: fix back...
-- checkConstructorInvoker :: VarEnv -> ConstructorInvoker -> VarEnv
-- checkConstructorInvoker e const = e

checkConstructorInvoker :: VarEnv -> ConstructorInvoker -> VarEnv
checkConstructorInvoker e const = let name = (nameCons const)
                                      args = (argCons const)
                                      env1 = foldl checkArg e args
                                      kls1 = getTypesOfArgs e args
                                      in case (checkAndGet name (typeConstructors e) (constructorInvokerPos const)) of
                                      Right val -> let kls2 = klstc val
                                                     in if kls1 /= kls2 then env1{errors = (errors env1)
                                                         ++ ("Args do not match: " ++ (show kls1) ++
                                                         " != " ++ (show kls2) ++ "\n")}
                                                        else env1
                                      Left err -> env1 {errors = (errors env1) ++ err}

checkK :: VarEnv -> K -> VarEnv
checkK e (Ktype t) = (checkType e t)
checkK e (KT t) = (checkT e t)
