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

data T = Constr String [A] 
    | TypeVar
    deriving (Show, Eq, Typeable)

data A = Ta T
    | TVar Var
    deriving (Show, Eq, Typeable)

data K = Ktype Type
    | Kt T
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

-- Define characters wich are part of the syntax
colon, arrow, comma, rsbrac, lsbrac :: Parser ()
colon  = void (symbol ":")
comma  = void (symbol ",")
arrow  = void (symbol "->")
lsbrac = void (symbol "[")
rsbrac = void (symbol "]")

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


-- -- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
-- --    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
-- dsllParser :: Parser ([Cd], [Od], [Pd])
-- dsllParser = between scn eof dsllProg -- Parse all the statemnts between the spaces to the end of the input file

-- -- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
-- --   operations followed by a collection of predicates
-- dsllProg :: Parser ([Cd],[Od],[Pd])


-- cd = do
--   rword "constructor"
--   name <- identifier
--   colon
--   --k' <- parse that list somehow
--   arrow
--   -- t' <- parse the resulting type somehow
--   -- return (Cd name k' t')

-- od = do
--   rword "operator"
--   name <- identifier
--   colon
--   -- check how to handle with the facts that parts of these might not appear
--   rword "forvars"

-- pd = do
--   rword "predicate"
--   name <- identifier
--   -- Figure out how to continue

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










