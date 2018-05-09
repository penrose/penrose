-- | "DSLL" contains the grammers and parser for the 
--    DSLL language
--    Author: Dor Ma'ayan, May 2018

{-# OPTIONS_HADDOCK prune #-}
--module DSLL where
module Main (main) where -- for debugging purposes

import System.Process
import Control.Monad (void)
import Data.Void
import Debug.Trace
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
--import Text.PrettyPrint
--import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
--------------------------------------- DSLL AST ---------------------------------------
type TypeVar = String

type Var = String

type Prop = String

type Type = String

type ConstructorInvoker = (String,[Arg])


data T = TTypeVar TypeVar --TODO: figure out a better name for that...
    | TConstr ConstructorInvoker
    deriving (Show, Eq, Typeable)

data Arg = AVar Var
    | AT T
    deriving (Show, Eq, Typeable)

data K = Ktype Type --TODO: figure out a better name for that...
    | KT T
    deriving (Show, Eq, Typeable)

data Cd = Constructor String [K] Type 
    deriving (Show, Eq, Typeable)

data Od = Operator String [Var] [T] [TypeVar] [Type] [T] T 
    deriving (Show, Eq, Typeable)

data Pd = Predicate String [Var] [T] [TypeVar] [Type] [T] [Prop] Prop 
    deriving (Show, Eq, Typeable)

type DSLLProg = ([Cd], [Od], [Pd]) 

-- --------------------------------------- DSLL Lexer -------------------------------------

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
colon, arrow, comma, rsbrac, lsbrac, lparen, rparen :: Parser ()
colon  = void (symbol ":")
comma  = void (symbol ",")
arrow  = void (symbol "->")
lsbrac = void (symbol "[")
rsbrac = void (symbol "]")
lparen = void (symbol "(")
rparen = void (symbol ")")


-- Reserverd word handling
rword :: String -> Parser()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar) -- make sure that reserved words stands alone

rws :: [String] -- The list of reserved words in DSLL
rws = ["constructor","operator","forvars","fortypes","predicate"]

--Identifiers handling
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


-- --------------------------------------- DSLL Parser -------------------------------------


-- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
dsllParser :: Parser ([Cd], [Od], [Pd])
dsllParser = between scn eof dsllProg -- Parse all the statemnts between the spaces to the end of the input file

-- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
--   operations followed by a collection of predicates
dsllProg :: Parser ([Cd],[Od],[Pd])
dsllProg = do
    listCd <- cdParser `sepEndBy` newline'
    listOd <- odParser `sepEndBy` newline'
    listPd <- pdParser `sepEndBy` newline'
    return (listCd, listOd, listPd)


typeParser :: Parser Type
typeParser = do
    i <- identifier
    return i

varParser :: Parser Var
varParser = do
    i <- identifier
    return i

typeVarParser :: Parser TypeVar
typeVarParser = do
    i <- identifier
    return i

propParser :: Parser Prop
propParser = do
    i <- identifier
    return i


tParser, tConstructorInvokerParser, typeVarParser' :: Parser T
tParser = try tConstructorInvokerParser <|> typeVarParser'
tConstructorInvokerParser = do
    i         <- identifier
    arguments <- parens  ( argParser `sepBy1` comma)
    return (TConstr (i,arguments))
typeVarParser' = do
    i <- typeVarParser
    return (TTypeVar i)



argParser, varParser', tParser'  :: Parser Arg
argParser = try varParser' <|> try tParser'
varParser' = do
     i <- varParser
     return (AVar i)
tParser' = do
    t <- tParser
    return (AT t)


kParser, kTypeParser, tParser'' :: Parser K
kParser = try kTypeParser <|> try tParser''
kTypeParser = do
     i <- typeParser
     return (Ktype i)
tParser'' = do
    t <- tParser
    return (KT t)

cdParser :: Parser Cd
cdParser = do
    rword "constructor"
    name <- identifier
    colon
    k' <- listOut( kParser `sepBy1` comma)
    arrow
    t' <- typeParser
    return (Constructor name k' t')

odParser :: Parser Od
odParser = do
  rword "operator"
  name <- identifier
  colon
  rword "forvars"
  varsList <- listOut (varParser `sepBy1` comma)
  colon
  tList <- listOut (tParser `sepBy1` comma)
  comma
  rword "fortypes"
  typeVarList <- listOut (typeVarParser `sepBy1` comma)
  colon
  typeList <- listOut (typeParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  t <- tParser
  return (Operator name varsList tList typeVarList typeList outerTList t)


pdParser, pd1, pd2 :: Parser Pd
pdParser = try pd1 <|> pd2
pd1 = do
  rword "predicate"
  name <- identifier
  colon
  propList <- listOut (propParser `sepBy1` comma)
  arrow
  prop <- propParser 
  return (Predicate name [] [] [] [] [] propList prop)
pd2 = do
  rword "predicate"
  name <- identifier
  colon
  rword "forvars"
  varsList <- listOut (varParser `sepBy1` comma)
  colon
  tList <- listOut (tParser `sepBy1` comma)
  comma
  rword "fortypes"
  typeVarList <- listOut (typeVarParser `sepBy1` comma)
  colon
  typeList <- listOut (typeParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  prop <- propParser 
  return (Predicate name varsList tList typeVarList typeList outerTList [] prop)

-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Dsll.hs; ./Substance <substance-file>

parseFromFile p file = runParser p file <$> readFile file

main :: IO ()
main = do
    args <- getArgs
    let dsllFile = head args
    dsllIn <- readFile dsllFile
    --parsed <- parseSubstance subFile subIn
    --mapM_ print parsed
    return ()










