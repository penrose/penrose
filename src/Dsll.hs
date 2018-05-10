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
--------------------------------------- DSLL AST ---------------------------------------
type TypeVar = String

type Var = String

type Prop = String

type Type = String


data ConstructorInvoker = ConstructorInvoker { nameCons :: String, argCons:: [Arg]}
    deriving (Eq, Typeable)
instance Show ConstructorInvoker where
    show (ConstructorInvoker nameCons argCons) = nString ++ "(" ++ aString ++ ")"
        where nString = show nameCons
              aString = show argCons


data T = TTypeVar TypeVar --TODO: figure out a better name for that...
    | TConstr ConstructorInvoker
    deriving (Show, Eq, Typeable)

data Arg = AVar Var
    | AT T
    deriving (Show, Eq, Typeable)

data K = Ktype Type --TODO: figure out a better name for that...
    | KT T
    deriving (Show, Eq, Typeable)

data Cd = Cd{nameCd :: String, inputCd::[K], outputCd::Type}
    deriving (Eq, Typeable)

instance Show Cd where
    show (Cd nameCd inputCd outputCd) = "(TCon, " ++ nString ++ ", ValOfType " ++ iString ++ ", Output " ++ oString ++")"
        where nString = show nameCd
              iString = show inputCd
              oString = show outputCd
        

-- data Od = Operator String [Var] [T] [TypeVar] [Type] [T] T 
--     deriving (Show, Eq, Typeable)

data Od = Od{ nameOd :: String, forVarsOd :: [Var], typesForVarsOd :: [T], forTypesOd :: [TypeVar], typeForTypesOd :: [Type]
              ,fromOd:: [T], toOd:: T}
    deriving (Eq, Typeable)

instance Show Od where
    show (Od nameOd forVarsOd typesForVarsOd forTypesOd typeForTypesOd fromOd toOd) =
     "(Op, " ++ aString ++ ", forvars " ++ bString ++ ":" ++ cString ++ ", fortypes " ++ dString 
     ++ ":" ++ eString ++ ", inputT " ++ fString ++ ", outputT " ++ gString ++ ")"
        where aString = show nameOd
              bString = show forVarsOd
              cString = show typesForVarsOd
              dString = show forTypesOd
              eString = show typeForTypesOd
              fString = show fromOd
              gString = show toOd



data Pd = Pd{ namePd :: String, forVarsPd :: [Var], typesForVarsPd :: [T], forTypesPd :: [TypeVar], typeForTypesPd :: [Type]
              ,fromTPd:: [T], fromPropPd :: [Prop], toPd:: Prop}
    deriving (Eq, Typeable)

instance Show Pd where
    show (Pd namePd forVarsPd typesForVarsPd forTypesPd typeForTypesPd fromTPd fromPropPd toPd) =
     "(Pd, " ++ aString ++ ", forvars " ++ bString ++ ":" ++ cString ++ ", fortypes " ++ dString 
     ++ ":" ++ eString ++ ", inputT " ++ fString ++ " , " ++ gString ++ ", outputT " ++ hString ++ ")"
        where aString = show namePd
              bString = show forVarsPd
              cString = show typesForVarsPd
              dString = show forTypesPd
              eString = show typeForTypesPd
              fString = show fromTPd
              gString = show fromPropPd
              hString = show toPd



-- data Pd = Predicate String [Var] [T] [TypeVar] [Type] [T] [Prop] Prop 
--     deriving (Show, Eq, Typeable)

data DSLLProg = DSLLProg{ cd :: [Cd], od :: [Od], pd :: [Pd]} 
    deriving (Eq, Typeable)

instance Show DSLLProg where
    show (DSLLProg cd od pd) = "types = " ++ cdString ++ "\n \n" ++ "operations = " ++ odString ++ "\n \n" ++ "predicates = " ++ pdString ++ "\n \n"
        where cdString = show cd
              odString = show od
              pdString = show pd

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
dsllParser :: Parser DSLLProg
dsllParser = between scn eof dsllProg -- Parse all the statemnts between the spaces to the end of the input file

-- |'dsllProg' parses the entire actual DSLL program which is a collection of constructors followed by a collection of
--   operations followed by a collection of predicates
dsllProg :: Parser DSLLProg
dsllProg = do
    listCd <- cdParser `sepEndBy` newline'
    listOd <- odParser `sepEndBy` newline'
    listPd <- pdParser `sepEndBy` newline'
    return DSLLProg {cd = listCd, od =  listOd, pd =  listPd}


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
    arguments <- parens ( argParser `sepBy1` comma)
    return (TConstr (ConstructorInvoker {nameCons = i, argCons = arguments}))
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
     i <- typeParser
     return (Ktype i)
tParser'' = do
    t <- tParser
    return (KT t)

cdParser, cd1, cd2 :: Parser Cd
cdParser = try cd1 <|> cd2
cd1= do
    rword "constructor"
    name <- identifier
    colon
    k' <- listOut (kParser `sepBy1` comma)
    arrow
    t' <- typeParser
    return Cd {nameCd = name, inputCd =  k', outputCd =  t'}
cd2 = do
    rword "constructor"
    name <- identifier
    colon
    t' <- typeParser
    return Cd {nameCd = name, inputCd =  [], outputCd =  t'}

odParser, od1, od2, od3, od4 :: Parser Od
odParser = try od1 <|> try od2 <|> try od3 <|> od4
od1 =  do
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
  return Od{nameOd  = name ,  forVarsOd  = varsList, typesForVarsOd  = tList, forTypesOd  = typeVarList , typeForTypesOd  = typeList 
              ,fromOd = outerTList , toOd = t }
od2 =  do
  rword "operator"
  name <- identifier
  colon
  rword "fortypes"
  typeVarList <- listOut (typeVarParser `sepBy1` comma)
  colon
  typeList <- listOut (typeParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  t <- tParser
  return Od{nameOd  = name ,  forVarsOd  = [], typesForVarsOd  = [], forTypesOd  = typeVarList , typeForTypesOd  = typeList 
              ,fromOd = outerTList , toOd = t }
od3 =  do
  rword "operator"
  name <- identifier
  colon
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  t <- tParser
  return Od{nameOd  = name ,  forVarsOd  = [], typesForVarsOd  = [], forTypesOd  = [] , typeForTypesOd  = [] 
              ,fromOd = outerTList , toOd = t }
od4 =  do
  rword "operator"
  name <- identifier
  colon
  rword "forvars"
  varsList <- listOut (varParser `sepBy1` comma)
  colon
  tList <- listOut (tParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  t <- tParser
  return Od{nameOd  = name ,  forVarsOd  = varsList, typesForVarsOd  = tList, forTypesOd  = [] , typeForTypesOd  = [] 
              ,fromOd = outerTList , toOd = t }

  


pdParser, pd1, pd2, pd3, pd4, pd5 :: Parser Pd
pdParser = try pd1 <|> try pd2 <|> try pd3 <|> try pd4 <|> pd5
pd1 = do
  rword "predicate"
  name <- identifier
  colon
  propList <- listOut (propParser `sepBy1` comma)
  arrow
  prop <- propParser 
  return Pd{ namePd = name, forVarsPd = [], typesForVarsPd =[], forTypesPd = [], typeForTypesPd = []
              ,fromTPd = [], fromPropPd = propList, toPd =  prop}
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
  return Pd{ namePd = name, forVarsPd = varsList, typesForVarsPd =tList, forTypesPd = typeVarList, typeForTypesPd = typeList
              ,fromTPd = outerTList, fromPropPd = [], toPd =  prop}
pd3 = do
  rword "predicate"
  name <- identifier
  colon
  rword "fortypes"
  typeVarList <- listOut (typeVarParser `sepBy1` comma)
  colon
  typeList <- listOut (typeParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  prop <- propParser
  return Pd{ namePd = name, forVarsPd = [], typesForVarsPd = [], forTypesPd = typeVarList, typeForTypesPd = typeList
              ,fromTPd = outerTList, fromPropPd = [], toPd =  prop}
pd4 = do
  rword "predicate"
  name <- identifier
  colon
  rword "forvars"
  varsList <- listOut (varParser `sepBy1` comma)
  colon
  tList <- listOut (tParser `sepBy1` comma)
  comma
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  prop <- propParser
  return Pd{ namePd = name, forVarsPd = varsList, typesForVarsPd =tList, forTypesPd = [], typeForTypesPd = []
              ,fromTPd = outerTList, fromPropPd = [], toPd =  prop}
pd5 = do
  rword "predicate"
  name <- identifier
  colon
  outerTList <- listOut (tParser `sepBy1` comma)
  arrow
  prop <- propParser 
  return Pd{ namePd = name, forVarsPd = [], typesForVarsPd = [], forTypesPd = [], typeForTypesPd = []
              ,fromTPd = outerTList, fromPropPd = [], toPd =  prop}

-- --------------------------------------- Test Driver -------------------------------------
-- | For testing: first uncomment the module definition to make this module the
-- Main module. Usage: ghc Dsll.hs; ./Substance <substance-file>

--parseFromFile p file = parseTest p file <$> readFile file

main :: IO ()
main = do
    [dsllFile, outputFile] <- getArgs
    dsllIn <- readFile dsllFile
    case (parse dsllParser dsllFile dsllIn) of
        Left err -> putStr (parseErrorPretty err)
        Right xs -> writeFile outputFile (show xs)
    putStrLn "Parsing Done!"  
    --mapM_ print parsed
    return ()


    






