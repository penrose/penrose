-- | "DSLL" contains the grammers and parser for the 
--    DSLL language
--    Author: Dor Ma'ayan, May 2018

{-# OPTIONS_HADDOCK prune #-}
--module DSLL where
module Main (main) where -- for debugging purposes

import System.Process
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
import Text.PrettyPrint.HughesPJClass hiding (colon, comma, parens, braces)
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

data A = T
		| Var
		deriving (Show, Eq, Typeable)

data K = Type
		| T
		deriving (Show, Eq, Typeable)

data Cd = Constructor String [K] Type 
		deriving (Show, Eq, Typeable)

data Od = Operator String [Var] [T] [TypeVar] [Type] [T] T 
		deriving (Show, Eq, Typeable)

data Pd = Predicate String [Var] [T] [TypeVar] [Type] [T] [Prop] Prop 
		deriving (Show, Eq, Typeable)

data DSLLProg = ([Cd], [Od], [Pd]) 
		deriving (Show, Eq, Typeable)

--------------------------------------- DSLL Lexer -------------------------------------

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
rword w = (lexeme . try) (sting w *> NotFollowedBy alphaNumChar) -- make sure that reserved words stands alone

rws :: [String] -- The list of reserved words in DSLL
rws = ["constructor","operator","forvars","fortypes","predicate"]


--Identifiers handling
identidier :: Parser String
identifiers = (lexeme . try) (p >>= check)      -- Make sure that identifier is not a reserved word
	p = (:) <$> letterChar <*> many alphaNumChar
	check x = if x `elem` rws
				then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x


--------------------------------------- DSLL Parser -------------------------------------


-- | 'DSLLParser' is the top-level parser function. The parser contains a list of functions
--    that parse small parts of the language. When parsing a source program, these functions are invoked in a top-down manner.
dsllParser :: Parser ([Cd], [Od], [Pd])
dsllParser = between scn eof dsllProg -- Parse all the statemnts between the spaces to the end of the input file

-- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
--   operations followed by a collection of predicates
dsllProg :: Parser ([Cd],[Od],[Pd])


cd = do
	rword "constructor"
	name <- identifier
	colon
	--k' <- parse that list somehow
	arrow
	-- t' <- parse the resulting type somehow
	-- return (Cd name k' t')

od = do
	rword "operator"
	name <- identifier
	colon
	-- check how to handle with the facts that parts of these might not appear
	rword "forvars"

pd = do
	rword "predicate"
	name <- identifier
	-- Figure out how to continue











