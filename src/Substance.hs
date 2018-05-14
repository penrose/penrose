-- | "Substance" contains the grammar, parser, and semantic checker for
--   the Substance language. It also contains translators to Alloy and
--   the driver for it.

{-# OPTIONS_HADDOCK prune #-}
module Substance where
-- module Main (main) where -- for debugging purposes
-- TODO split this up + do selective export

--import Utils
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
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L



--------------------------------------- Substance AST ---------------------------------------

data SubType = TypeConst String -- these are all names, e.g. “Set”
        | AllT -- specifically for global selection in Style
    deriving (Show, Eq, Typeable)

data ValConstructor = ValConst String    -- “Cons”, “Times”
    deriving (Show, Eq, Typeable)

data Operator' = OperatorConst String             -- “Intersection” 
    deriving (Show, Eq, Typeable)

data Predicate = PredicateConst String            -- “Intersect”
    deriving (Show, Eq, Typeable)

data TypeVar = TypeVarConst String
    deriving (Show, Eq, Typeable)

data Var = VarConst String
    deriving (Show, Eq, Typeable)



data Type = ApplyT SubType [Arg] | TypeT TypeVar
    deriving (Show, Eq, Typeable)

data Arg = VarA Var | TypeA Type
    deriving (Show, Eq, Typeable)

data Expr = VarE Var | ApplyV Operator' [Expr] | ApplyC ValConstructor [Expr]
    deriving (Show, Eq, Typeable)

data SubStmt = Decl Type Var | Bind Var Expr | ApplyP Predicate [Var]
    deriving (Show, Eq, Typeable)


-- | Program is a sequence of statements
type SubProg = [SubStmt]
type SubObjDiv = ([SubDecl], [SubConstr])

-- | Declaration of Substance objects
data SubDecl
    = SubDeclConst Type Var
    deriving (Show, Eq, Typeable)

-- | Declaration of Substance constaints
data SubConstr
    = SubConstrConst Predicate [Var]
    deriving (Show, Eq, Typeable)

-- | Both declarations and constaints in Substance are regarded as objects, which is possible for Style to select later.
data SubObj = LD SubDecl | LC SubConstr deriving (Show, Eq, Typeable)

-- --------------------------------------- Substance Lexer -------------------------------------
-- | TODO: Some of the lexing functions are alreadt implemented at Utils.hs, Merge it and avoid duplications!
type Parser = Parsec Void String

-- | 'lineComment' and 'blockComment' are the two styles of commenting in Penrose and in the DSLL
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

-- A warper to make sure that we comsume whitespace after every lexeme
symbol :: String -> Parser String
symbol = L.symbol sc


newline' :: Parser ()
newline' = newline >> scn

-- Pill out the left and right parens from the input and roll-out the parsing
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

listOut :: Parser a -> Parser a
listOut = between (symbol "[") (symbol "]")



-- Define characters wich are part of the syntax
colon, arrow, comma, rsbrac, lsbrac, lparen, rparen,eq :: Parser ()
colon  = void (symbol ":")
comma  = void (symbol ",")
arrow  = void (symbol "->")
lsbrac = void (symbol "[")
rsbrac = void (symbol "]")
lparen = void (symbol "(")
rparen = void (symbol ")")
eq  = void (symbol "=")



--Reserverd word handling
rword :: String -> Parser()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)
rws :: [String] -- The list of reserved words in DSLL
rws = [] -- TODO: Doesn't seems like there is any reserved word here, should seperate reserved words in Utils.hs for each PL

--Identifiers handling
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


--------------------------------------- Substance Parser -------------------------------------
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

subType :: Parser SubType
subType = do
    i <- identifier
    return (TypeConst i)

valConstructorParser :: Parser ValConstructor
valConstructorParser = do
    i <- identifier
    return (ValConst i)

operatorParser :: Parser Operator'
operatorParser = do
    i <- identifier
    return (OperatorConst i)

predicateParser :: Parser Predicate
predicateParser = do
    i <- identifier
    return (PredicateConst i)


typeParser, applyT, typeT :: Parser Type
typeParser = try applyT <|> typeT
applyT = do
  tc <- subType
  args <- listOut (argParser `sepBy1` comma)
  return (ApplyT tc args)
typeT = do
  i <- typeVarParser
  return (TypeT i)

argParser, varA , typeA :: Parser Arg
argParser = try varA <|> typeA
varA  = do
  i <- varParser
  return (VarA i)
typeA = do
  i <- typeParser
  return (TypeA i)

exprParser, varE, applyV, applyC :: Parser Expr
exprParser = try varE <|> try applyV <|> applyC
varE = do
  i <- varParser
  return (VarE i)
applyV = do
  tc <- operatorParser
  args <- parens (exprParser `sepBy1` comma)
  return (ApplyV tc args)
applyC = do
  tc <- valConstructorParser
  args <- parens (exprParser `sepBy1` comma)
  return (ApplyC tc args)

subStmt, decl, bind, applyP :: Parser SubStmt
subStmt = try decl <|> try bind <|> applyP
decl = do
  t' <- typeParser
  v' <- varParser
  return (Decl t' v')
bind = do
  v' <- varParser
  eq
  e' <- exprParser
  return (Bind v' e')
applyP = do
  p <- predicateParser
  v' <- parens (varParser `sepBy1` comma)
  return (ApplyP p v')

--------------------------------------------------------------------------------
-- Semantic checker and desugaring

-- | Environment for the semantic checker. As the 'check' function executes, it
-- accumulate information such as symbol tables in the environment.
data SubEnv = SubEnv {
    subObjs :: [SubObj] --,  -- declared Substance objects(including constraints, which is, again, viewed also as objects)
    --subDefs :: M.Map String ([(SubType, String)], FOLExpr), -- all definitions
    --subApps :: [(FOLExpr, M.Map String String, [String])], -- Def, varmap, ids to be solved by alloy
    --subSymbols :: M.Map String SubType, -- symbol table
    --subArgs :: M.Map String [String]
    -- subAppliedDefs :: [FOLExpr]
} deriving (Show, Eq, Typeable)

-- | 'check' is the top-level semantic checking function. It takes a Substance
-- program as the input, checks the validity of the program, and outputs
-- a collection of information.
-- The check is done in two passes. First check all the declarations of
-- stand-alone objects like `Set`, and then check for all other things
check :: SubProg -> SubEnv
check p = let env1 = foldl checkDecls initE p
              env2 = foldl checkReferencess env1 p
              in
            env2 { subObjs = reverse $ subObjs env2 }
          where initE = SubEnv { subObjs = []}--, subDefs = M.empty,  subSymbols = M.empty, subApps = [], subArgs = M.empty }

applyDef (n, m) d = case M.lookup n d of
    Nothing -> error "applyDef: definition not found!"
    Just (_, e) -> e

-- | 'checkDecls' checks the validity of declarations of objects.
checkDecls :: SubEnv -> SubStmt -> SubEnv
checkDecls e (Decl t s)  = e { subObjs = toObj t s : subObjs e} --, subSymbols = checkAndInsert s t $ subSymbols e }
checkDecls e _ = e -- Ignore all other statements

-- 'toObj' translates [Type] + [Identiers] to concrete Substance objects, to be selected by the Style program
toObj :: Type -> Var -> SubObj
toObj t v = LD $ (SubDeclConst t v)

-- 'checkReferencess' checks any statement that refers to other objects. For example,
-- > Subset A B
-- refers to identifiers @A@ and @B@. The function will perform a lookup in the symbol table, make sure the objects exist and are of desired types -- in this case, 'Set' -- and throws an error otherwise.
checkReferencess :: SubEnv -> SubStmt -> SubEnv
checkReferencess e (ApplyP t ss) = e { subObjs = newConstrs : subObjs e }
    where newConstrs = (toConstr t ss)
checkReferencess e _ = e -- Ignore all other statements


-- | Similar to 'toObj'
toConstr :: Predicate -> [Var] ->  SubObj
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
parseSubstance :: String -> String -> IO [SubObj]
parseSubstance subFile subIn = case runParser substanceParser subFile subIn of
     Left err -> error (parseErrorPretty err)
     Right xs -> do
         mapM_ print xs
         --divLine
         let c = check xs
         return (subObjs c)


-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc SubstanceCore.hs; ./SubstanceCore <substance core-file>

--parseFromFile p file = parseTest p file <$> readFile file

parseFromFile p file = runParser p file <$> readFile file

main :: IO ()
main = do
    args <- getArgs
    let subFile = head args
    subIn <- readFile subFile
    parsed <- parseSubstance subFile subIn
    mapM_ print parsed
    return ()

alloyDir = "./"
alloyTempFile = "__temp__.als"
alloyNumSamples = 1
