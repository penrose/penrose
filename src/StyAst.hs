-- module StyAst where
module Main (main) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import System.Environment
import qualified Text.Megaparsec.Lexer as L


-- Style grammar (relies on the Substance grammar, specifically SubObj)

data SubType
    = Set
    | Pt
    | Map
    deriving (Show)

data StyProg
    = Block Block
    deriving (Show)

data Block
    = BlockSeq [Block]
    | GlobalBlock Stmt
    | TypeBlock SubType Stmt
    | ObjBlock String Stmt
    | ConstraintBlock Stmt
    deriving (Show)

data Stmt
    = StmtSeq [Stmt]
    | Assign String Expr
    | ConstrFn String [Expr]
    deriving (Show)

data Expr
    = Id String
    | Shape SubShape
    deriving (Show)

data SubShape = SS SetShape | SP PtShape | SM MapShape
     deriving (Show, Eq)

data SetShape = SetCircle | Box
     deriving (Show, Eq)

data PtShape = SolidDot | HollowDot | Cross
     deriving (Show, Eq)

data MapShape = SolidArrow
     deriving (Show, Eq)

data Direction = Horiz | Vert | Angle Float
     deriving (Show, Eq)

data LineType = Solid | Dotted
     deriving (Show, Eq, Read)

data Color = Red | Blue | Black | Yellow -- idk
     deriving (Show, Eq, Read)


sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/"

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- noop :: Parser a
-- noop = void

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws, attribs, shapes :: [String] -- list of reserved words
rws = ["global"]
attribs = ["shape"]
shapes = ["Circle","Box","SolidArrow"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x
attribute :: Parser String
attribute = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` attribs
                then return x
                else fail $ "keyword " ++ show x ++ " is not a valid attribute"

subshape :: Parser Expr
subshape = (lexeme . try) (p >>= check)
  where
    p       = some letterChar
    check x = if x `elem` shapes
                then return (Shape $ getShape x)
                else fail $ "Shape " ++ show x ++ " is not a valid shape"

getShape :: String -> SubShape
getShape str = case str of
    "Circle"     -> SS SetCircle
    "Box"        -> SS Box
    "SolidArrow" -> SM SolidArrow

term :: Parser Expr
term = expr
  <|> subshape

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators = []

assignStmt :: Parser Stmt
assignStmt = do
  var  <- attribute
  void (symbol "=")
  e    <- subshape
  return (Assign var e)

-- stmtSeq :: Parser Stmt
-- stmtSeq = f <$> sepBy1 stmt' semi
--   -- if there's only one stmt return it without using ‘Seq’
--   where f l = if length l == 1 then head l else Seq l

stmt :: Parser Stmt
stmt = assignStmt

globalBlock :: Parser Block
globalBlock = do
    void (symbol "global")
    stmts <- braces stmt
    return $ GlobalBlock stmts

styProg :: Parser Block
styProg = globalBlock

styleParser :: Parser Block
styleParser = between sc eof styProg
-- styleParser = between (symbol "[") (symbol "]") styProg

-- TODO
styPrettyPrint :: Block -> String
styPrettyPrint b = "HAHAHAHA"

parseFromFile p file = runParser p file <$> readFile file

main :: IO ()
main = do
    args <- getArgs
    let styFile = head args
    styIn <- readFile styFile
    -- putStrLn styIn
    parseTest styleParser styIn
    case (runParser styleParser "" styIn) of
         Left err -> putStr (parseErrorPretty err)
         Right xs -> putStrLn $ show xs
    return ()
