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
import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.Functor.Classes
import Data.List
import Data.Maybe (fromMaybe)
import Control.Monad (void)
import Data.Typeable
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import Data.Void
import Control.Monad.State.Lazy (StateT)
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Set                   as S
import qualified Data.Map.Strict            as M
import qualified Text.Megaparsec.Char.Lexer as L
import qualified SubstanceTokenizer         as T

--------------------------------------------------------------------------------
---- Lexer helper functions
-- TODO: separate reserved words and keywords for each of the DSLs

type BaseParser = Parsec ParserError String
type Parser = StateT (Maybe VarEnv) BaseParser
data ParserError
    = SubstanceError String
    | StyleError String
    deriving (Eq, Typeable, Ord, Read, Show)
instance ShowErrorComponent ParserError where
    showErrorComponent (SubstanceError msg) = "Substance Parser Error: " ++ msg

rws, attribs, attribVs, shapes :: [String] -- list of reserved words
rws =     ["avoid", "as"] ++ dsll
-- ++ types ++ attribs ++ shapes ++ colors
attribs = ["shape", "color", "label", "scale", "position"]
attribVs = shapes
shapes =  ["Auto", "None", "Circle", "Box", "SolidArrow", "SolidDot",
           "HollowDot", "Cross"]
labelrws = ["Label", "AutoLabel", "NoLabel"]
dsll = ["tconstructor","vconstructor","operator","ExprNotation","StmtNotation",
        "forvars","fortypes","predicate", "Prop", "type", "<:", "->", "<->",
        "at level", "associativity"]
-- colors =  ["Random", "Black", "Red", "Blue", "Yellow"]

upperId, lowerId, identifier :: Parser String
identifier = (lexeme . try) (p >>= checkId)
  where p = (:) <$> letterChar <*> many validChar
upperId = (lexeme . try) (p >>= checkId)
  where p = (:) <$> upperChar <*> many validChar
lowerId = (lexeme . try) (p >>= checkId)
  where p = (:) <$> lowerChar <*> many validChar
validChar = alphaNumChar <|> char '_' <|> char '-'



transPattern :: Parser String
transPattern = (lexeme . try) (p >>= checkPattern)
  where p = many anyChar --(:) <$> letterChar <*> many validChar

checkPattern :: String -> Parser String
checkPattern x = if x == "\"" || x == "->"
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

checkId :: String -> Parser String
checkId x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

texExpr :: Parser String
texExpr = dollar >> manyTill asciiChar dollar

-- | 'lineComment' and 'blockComment' are the two styles of commenting in Penrose. Line comments start with @--@. Block comments are wrapped by @/*@ and @*/@.
lineComment, blockComment :: Parser ()
lineComment  = L.skipLineComment "--"
blockComment = L.skipBlockComment "/*" "*/"

-- | A strict space consumer. 'sc' only eats space and tab characters. It does __not__ eat newlines.
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

-- | A normal space consumer. 'scn' consumes all whitespaces __including__ newlines.
scn :: Parser ()
scn = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

symboln :: String -> Parser String
symboln = L.symbol scn

newline' :: Parser ()
newline' = newline >> scn

semi' :: Parser ()
semi' = semi >> scn

backticks :: Parser a -> Parser a
backticks = between (symbol "`") (symbol "`")

semi, def, lparen, rparen, lbrac, rbrac, colon,
           arrow, comma, dollar, question, dot :: Parser ()
aps = void (symbol "'")
quote = void (symbol "\"")
lbrac = void (symbol "{")
rbrac = void (symbol "}")
lparen = void (symbol "(")
rparen = void (symbol ")")
slparen = void (symbol "[")
srparen = void (symbol "]")
colon = void (symbol ":")
semi = void (symbol ";")
arrow = void (symbol "->")
comma = void (symbol ",")
dot = void (symbol ".")
eq = void (symbol "=")
def = void (symbol ":=")
dollar = void (symbol "$")
question = void (symbol "?")


dollars :: Parser a -> Parser a
dollars = between (symbol "$") (symbol "$")

braces :: Parser a -> Parser a
-- NOTE: symboln is used here because all usages of braces in our system allow newlines in the middle of a stmt
-- May wanna change this later once we have stricter use case of it
braces = between (symboln "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | 'integer' parses an integer.
integer :: Parser Integer
unsignedInteger       = lexeme L.decimal
integer = L.signed sc unsignedInteger

-- | 'float' parses a floating point number.
float :: Parser Float
unsignedFloat = lexeme L.float
float = L.signed sc unsignedFloat

-- Reserved words
rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)

tryChoice :: [Parser a] -> Parser a
tryChoice list = choice $ map try list

----------------------------------- AST ----------------------------------------

data TypeName = TypeNameConst String  -- these are all names, e.g. “Set”
              | AllT              -- specifically for global selection in Style
                deriving (Show, Eq, Typeable)

data TypeVar = TypeVar { typeVarName :: String,
                         typeVarPos  :: SourcePos }
               deriving (Show, Typeable)

instance Eq TypeVar where
  (TypeVar n1 _) == (TypeVar n2 _) = n1 == n2

instance Ord TypeVar where
  (TypeVar s1 _) `compare` (TypeVar s2 _) = s1 `compare` s2

newtype Var = VarConst String
         deriving (Show, Eq, Typeable, Ord)

data Y = TypeVarY TypeVar
       | VarY Var
         deriving (Show, Eq, Typeable, Ord)

data T = TTypeVar TypeVar
       | TConstr TypeCtorApp
       -- TODO: rename to TCtor. Less confusing, more consistent w/ Sty
         deriving (Show, Eq, Typeable, Ord)

data TypeCtorApp = TypeCtorApp { nameCons :: String,
                                 argCons  :: [Arg],
                                 constructorInvokerPos :: SourcePos }
                          deriving (Typeable)

instance Show TypeCtorApp where
  show (TypeCtorApp nameCons argCons posCons) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameCons
              aString = show argCons

instance Eq TypeCtorApp where
  (TypeCtorApp n1 a1 _) == (TypeCtorApp n2 a2 _) = n1 == n2 && a1 == a2

instance Ord TypeCtorApp where
    (TypeCtorApp s1 _ _) `compare` (TypeCtorApp s2 _ _) = s1 `compare` s2

data Arg = AVar Var
         | AT T
           deriving (Show, Eq, Typeable,Ord)

data K = Ktype Type
       | KT T
       deriving (Show, Eq, Typeable)

data Type = Type { typeName :: String,
                   typePos  :: SourcePos }
            deriving (Show, Typeable)

instance Eq Type where
  (Type n1 _) == (Type n2 _) = n1 == n2

data Prop =  Prop { propName :: String,
                    propPos  :: SourcePos }
             deriving (Show, Typeable)

instance Eq Prop where
  (Prop n1 _) == (Prop n2 _) = n1 == n2

----------------------------------- Parser -------------------------------------

typeNameParser :: Parser TypeName
typeNameParser = TypeNameConst <$> identifier

typeParser :: Parser Type
typeParser = do
    rword "type"
    pos <- getPosition
    return Type{ typeName = "type", typePos = pos}

varParser :: Parser Var
varParser = VarConst <$> identifier

typeVarParser :: Parser TypeVar
typeVarParser = do
    aps
    i <- identifier
    pos <- getPosition
    return TypeVar { typeVarName =  i, typeVarPos = pos}

yParser, y1, y2 :: Parser Y
yParser = try y1 <|> y2
y1 = VarY <$> varParser
y2 = TypeVarY <$> typeVarParser

propParser :: Parser Prop
propParser = do
    rword "Prop"
    pos <- getPosition
    return Prop { propName = "Prop", propPos = pos}

tParser, tTypeCtorAppParser, typeVarParser' :: Parser T
tParser = try tTypeCtorAppParser <|> typeVarParser'
tTypeCtorAppParser = do
    i         <- identifier
    arguments <- option [] $ parens (argParser `sepBy1` comma)
    --try (parens (argParser `sepBy1` comma)) <|> emptyArgList --option [] $
    -- parens (argParser `sepBy1` comma)
    --try (parens (argParser `sepBy1` comma)) <|> emptyArgList
    pos <- getPosition
    return (TConstr (TypeCtorApp { nameCons = i, argCons = arguments,
                                   constructorInvokerPos = pos }))
typeVarParser' = TTypeVar <$> typeVarParser

argParser, varParser', tParser'  :: Parser Arg
argParser = try tParser' <|> varParser'
varParser' = AVar <$> varParser
tParser' = AT <$> tParser

kParser, kTypeParser, tParser'' :: Parser K
kParser = try kTypeParser <|> try tParser''
kTypeParser = do
     rword "type"
     pos <- getPosition
     return (Ktype (Type { typeName = "type", typePos = pos }))
tParser'' = KT <$> tParser

emptyArgList :: Parser [Arg]
emptyArgList = do
  lparen
  rparen
  return []

----------------------------------- Utility functions ------------------------------------------

-- Equality functions that don't compare SourcePos
-- TODO: use correct equality comparison in typechecker

argsEq :: Arg -> Arg -> Bool
argsEq (AVar v1) (AVar v2) = v1 == v2
argsEq (AT t1) (AT t2)     = typesEq t1 t2
argsEq _ _                 = False

typesEq :: T -> T -> Bool
typesEq (TTypeVar t1) (TTypeVar t2) = typeVarName t1 == typeVarName t2 -- TODO: better way to compare type vars
typesEq (TConstr t1) (TConstr t2) = nameCons t1 == nameCons t2 &&
                                    length (argCons t1) == length (argCons t2) &&
                                    (all (\(a1, a2) -> argsEq a1 a2) $ zip (argCons t1) (argCons t2))
typesEq _ _ = False

----------------------------------- Typechecker aux functions ------------------------------------------

-- | Compute the transitive closure of list of pairs
--   Useful for subtyping and equality subtyping checkings
transitiveClosure :: Eq a => [(a, a)] -> [(a, a)]
transitiveClosure closure
  | closure == closureAccum = closure
  | otherwise               = transitiveClosure closureAccum
  where closureAccum =
          nub $ closure ++ [(a, c) | (a, b) <- closure, (b', c) <- closure, b == b']

-- | Return whether a closure is cyclic (a, b) and (b, a) appears in the closure
isClosureNotCyclic :: Eq a => [(a,a)] -> Bool
isClosureNotCyclic lst = let c = [(a,a') | (a,a') <- lst, a == a']
                  in null c

firsts :: [(a, b)] -> [a]
firsts xs = [x | (x,_) <- xs]

seconds :: [(a, b)] -> [b]
seconds xs = [x | (_,x) <- xs]

second :: (a, b) -> b
second (a, b) = b

checkAndGet :: String -> M.Map String v -> SourcePos -> Either String v
checkAndGet k m pos = case M.lookup k m of
  Nothing -> Left ("Error in " ++ sourcePosPretty pos ++ " : " ++ k
                               ++ " Doesn't exist in the context \n")
  Just v ->  Right v

lookUpK :: VarEnv -> Arg -> K
lookUpK e (AT  (TTypeVar t))  = Ktype (typeVarMap e M.! t)
--(Ktype (Type {typeName = "type", typePos = typeVarPos t }))
lookUpK e (AT  (TConstr t)) =
        if nameCons t `elem` declaredNames e
        then lookUpK e (AVar (VarConst (nameCons t)))
        else Ktype (Type { typeName = "type", typePos = constructorInvokerPos t })
lookUpK e (AVar v) = KT (varMap e M.! v)

getTypesOfArgs :: VarEnv -> [Arg] -> [K]
getTypesOfArgs e = map (lookUpK e)

updateEnv :: VarEnv -> (Y, K) -> VarEnv
updateEnv e (TypeVarY y, Ktype t) = e { typeVarMap = M.insert y t $ typeVarMap e }
updateEnv e (VarY y, KT t)        = e { varMap = M.insert y t $ varMap e }
updateEnv e err                   = e { errors = errors e
                                  ++ "Problem in update: " ++ show err ++ "\n" }

addName :: String -> VarEnv -> VarEnv
addName a e = if a `elem` typeCtorNames e
              then e { errors = errors e ++ "Name " ++ a ++
               " already exists in the context \n" }
              else e { typeCtorNames = a : typeCtorNames e }

addValConstructor :: ValConstructor -> VarEnv -> VarEnv
addValConstructor v e = case M.lookup (tvc v) (typeValConstructor e) of
  Nothing -> e {typeValConstructor =  M.insert (tvc v) v $ typeValConstructor e}
  Just x -> e {errors = errors e ++
   "Multiple declarations of value constructors for type " ++ show (tvc v)}

addDeclaredName :: String -> VarEnv -> VarEnv
addDeclaredName a e = if a `elem` declaredNames e
                      then e { errors = errors e ++ "Name " ++ a
                                         ++ " already exsist in the context \n"}
                      else e { declaredNames = a : declaredNames e }

isSubtype :: T -> T -> VarEnv -> Bool
isSubtype t1 t2 e = typesEq t1 t2 -- A type is considered a subtype of itself
                    || (t1,t2) `elem` subTypes e

isSubtypeK :: K -> K -> VarEnv -> Bool
isSubtypeK (KT k1) (KT k2) e = isSubtype k1 k2 e

-- | For existing judgment G |- T1 <: T2,
-- | this rule (SUBTYPE-ARROW) checks if the first arrow type (i.e. function or value constructor type) is a subtype of the second
-- | The arrow types are contravariant in their arguments and covariant in their return type
-- | e.g. if Cat <: Animal, then Cat -> Cat <: Cat -> Animal, and Animal -> Cat <: Cat -> Cat
isSubtypeArrow :: [T] -> [T] -> VarEnv -> Bool
isSubtypeArrow [t] [s] e = isSubtype t s e
                       -- Covariant in return type (or simply the type for a nullary function)
isSubtypeArrow (t1:ts) (s1:ss) e = isSubtype s1 t1 e -- Contravariant in arguments
                                   && isSubtypeArrow ts ss e
isSubtypeArrow t s _ = False -- Functions have different numbers of arguments

--------------------------------------- Env Data Types ---------------------------------------

-- | Environment for the dsll semantic checker. As the 'check' function
-- executes, it accumulate information such as symbol tables in the environment.

-- | list of elements that might appear in the global context
data Ttype = Ttype { yt :: Y,
                     kt :: K }
             deriving (Show, Eq, Typeable)

data TypeConstructor = TypeConstructor { nametc :: String,
                                         kindstc  :: [K],
                                         typtc  :: Type }
                       deriving (Show, Eq, Typeable)

data ValConstructor = ValConstructor { namevc :: String,
                                       ylsvc  :: [Y],
                                       kindsvc  :: [K],
                                       nsvc   :: [Var],
                                       tlsvc  :: [T],
                                       tvc    :: T }
                      deriving (Show, Eq, Typeable)

data Operator = Operator { nameop :: String,
                             ylsop  :: [Y],
                             kindsop  :: [K],
                             tlsop  :: [T],
                             top  :: T}
                 deriving (Show, Eq, Typeable)

data PredicateEnv = Pred1 Predicate1
                  | Pred2 Predicate2
                  deriving (Show, Eq, Typeable)

data Predicate1 = Prd1 { namepred1 :: String,
                         ylspred1  :: [Y],
                         kindspred1  :: [K],
                         tlspred1  :: [T],
                         ppred1    :: Prop}
                  deriving (Show, Eq, Typeable)

data Predicate2 = Prd2 { namepred2 :: String,
                         plspred2  :: [Prop],
                         ppred2    :: Prop }
                  deriving (Show, Eq, Typeable)

data StmtNotationRule =
   StmtNotationRule { fromSnr :: [T.Token],
                      toSnr   :: [T.Token],
                      patternsSnr :: [T.Token],
                      entitiesSnr :: [T.Token] -- all the non pattern sugared entities
                    }
                    deriving (Show, Eq, Typeable)

data ExprNotationRule = ExprNotationRule {fromEnr          :: String,
                                          toEnr            :: String,
                                          associativityEnr :: String,
                                          precedenceEnr    :: Integer}
                  deriving (Show, Eq, Typeable)

data VarEnv = VarEnv { typeConstructors :: M.Map String TypeConstructor,
                       valConstructors  :: M.Map String ValConstructor,
                       operators        :: M.Map String Env.Operator,
                       predicates       :: M.Map String PredicateEnv,
                       typeVarMap       :: M.Map TypeVar Type,
                       typeValConstructor :: M.Map T ValConstructor,
                       varMap           :: M.Map Var T,
                       preludes        :: [(Var,T)],
                       subTypes         :: [(T,T)],
                       typeCtorNames    :: [String],  -- a global list which contains all the names of types in that env
                       declaredNames    :: [String],  -- a global list which contains all the names of elements declared in that env
                       stmtNotations    :: [StmtNotationRule], -- all the statement notations in the dsll
                       errors           :: String }   -- a string which accumulates all the errors founded during the run of the typechecker
              deriving (Show, Eq, Typeable)

isDeclared :: String -> VarEnv -> Bool
isDeclared name varEnv = name `elem` typeCtorNames varEnv

checkTypeVar :: VarEnv -> TypeVar -> VarEnv
checkTypeVar e v = if M.member v (typeVarMap e)
                   then e
                   else e { errors = errors e ++ ("TypeVar " ++
                    show v ++ "is not in scope \n") }

checkVar :: VarEnv -> Var -> VarEnv
checkVar e v = if M.member v (varMap e)
               then e
               else e { errors = errors e ++ ("Var " ++ show v
               ++ "is not in scope \n") }


checkY :: VarEnv -> Y -> VarEnv
checkY e (TypeVarY y) = checkTypeVar e y
checkY e (VarY y)     = checkVar e y

checkArg :: VarEnv -> Arg -> VarEnv
checkArg e (AVar v) = checkVar e v
checkArg e (AT (TConstr i)) = if nameCons i `elem` declaredNames e
                              then checkVar e (VarConst (nameCons i))
                              else checkT e (TConstr i)
checkArg e (AT t) = checkT e t

checkT :: VarEnv -> T -> VarEnv
checkT e (TTypeVar t) = checkTypeVar e t
checkT e (TConstr c)  = let env1 = checkTypeCtorApp e c
                            env2 = checkDeclaredType env1 (TConstr c)
                        in env2

checkType :: VarEnv -> Type -> VarEnv
checkType e t = e

checkTypeCtorApp :: VarEnv -> TypeCtorApp -> VarEnv
checkTypeCtorApp e const =
  let name = nameCons const
      args = argCons const
      env1 = foldl checkArg e args
      kinds1 = getTypesOfArgs e args
  in case checkAndGet name (typeConstructors e) (constructorInvokerPos const) of
       Right val -> let kinds2 = kindstc val
                     in if kinds1 /= kinds2
                      then env1 { errors = errors env1
                      ++ ("Args do not match: " ++ show kinds1 ++
                      " != " ++ show kinds2 ++ "\n") }
                      else env1
       Left err -> env1 { errors = errors env1 ++ err }

checkDeclaredType :: VarEnv -> T -> VarEnv
checkDeclaredType e (TConstr t) =
   if nameCons t `elem` typeCtorNames e
     then e
     else e { errors = errors e ++ "Type " ++ nameCons t ++
      " does not exsist in the context \n" }

checkDeclaredType e _ = e { errors = errors e ++
 "checkDeclaredType should be called only with type constructors \n" }


checkK :: VarEnv -> K -> VarEnv
checkK e (Ktype t) = checkType e t
checkK e (KT t) = checkT e t
