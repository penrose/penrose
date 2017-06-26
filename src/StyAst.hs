-- module StyAst where
-- module Main (main) where
module StyAst where

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
    deriving (Show, Eq)

data StySpec = StySpec {
    spType :: SubType,
    spId :: String,
    spShape :: SubShape,
    spColor :: Color
} deriving (Show)

type StyProg = [Block]

data Block
    = GlobalBlock [Stmt]
    | TypeBlock SubType [Stmt]
    | ObjBlock String [Stmt]
    | ConstraintBlock [Stmt]
    deriving (Show)

--     = GlobalBlock [Stmt]
--     | GeneralBlock Selector [Stmt]
-- type Selector = SubType | ID |

data Stmt
    = Assign String Expr
    | ConstrFn String [Expr]
    deriving (Show)

data Expr
    = Id String
    | Shape SubShape
    | Color Color
    | Dir Direction
    | IntLit Int
    | FloatLit Float
    deriving (Show)

data SubShape = SS SetShape | SP PtShape | SM MapShape | NoShape
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

data Color = Red | Blue | Black | Yellow | Random -- idk
     deriving (Show, Eq, Read)

-- Lexer functions
sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--"
        blockCmnt = L.skipBlockComment "/*" "*/"

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

emptyProg :: Parser [Block]
emptyProg = return []

-- noStmt :: Parser Stmt
-- noStmt = return (StmtSeq [])

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

comma :: Parser String
comma = symbol ","

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

-- float :: Parser Float
-- float = realToFrac $ lexeme L.float -- TODO: parsing without sign?

-- Reserved words
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws, attribs, shapes, types :: [String] -- list of reserved words
rws =     ["global"]
types =   ["Set", "Map", "Point"]
attribs = ["shape", "color", "label", "scale", "position"]
shapes =  ["None", "Circle", "Box", "SolidArrow", "SolidDot", "HollowDot", "Cross"]
colors =  ["Random", "Black", "Red", "Blue", "Yellow"]

getShape :: String -> SubShape
getShape str = case str of
    "None"       -> NoShape
    "Circle"     -> SS SetCircle
    "Box"        -> SS Box
    "SolidArrow" -> SM SolidArrow
    "SolidDot"   -> SP SolidDot
    "HollowDot"  -> SP HollowDot
    "Cross"      -> SP Cross

getColor :: String -> Color
getColor str = case str of
    "Red"    -> Red
    "Yellow" -> Yellow
    "Blue"   -> Blue
    "Black"  -> Black
    "Random" -> Random

getType :: String -> SubType
getType str = case str of
    "Set"   -> Set
    "Map"   -> Map
    "Point" -> Pt

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

color :: Parser Expr
color = (lexeme . try) (p >>= check)
  where
    p       = some letterChar
    check x = if x `elem` colors
                then return (Color $ getColor x)
                else fail $ "Color " ++ show x ++ " is not a valid color"

typ :: Parser SubType
typ = (lexeme . try) (p >>= check)
  where
    p       = some letterChar
    check x = if x `elem` types
                then return (getType x)
                else fail $ "Type " ++ show x ++ " is not a valid type"

term :: Parser Expr
term = expr
  <|> subshape
  <|> color
  -- <|> integer
  -- <|> float

expr :: Parser Expr
expr = makeExprParser term operators

operators :: [[Operator Parser Expr]]
operators = []

assignStmt :: Parser Stmt
assignStmt = do
  var  <- attribute
  void (symbol "=")
  e    <- subshape <|> color
  return (Assign var e)

stmtSeq :: Parser [Stmt]
stmtSeq = sepBy1 stmt comma

stmt :: Parser Stmt
stmt = assignStmt

globalBlock :: Parser Block
globalBlock = do
    void (symbol "global")
    stmts <- braces stmtSeq
    return $ GlobalBlock stmts

typeBlock :: Parser Block
typeBlock = do
    t     <- typ
    stmts <- braces stmtSeq
    return $ TypeBlock t stmts

objBlock :: Parser Block
objBlock = do
    i     <- identifier
    stmts <- braces stmtSeq
    return $ ObjBlock i stmts

-- TODO: required global and/or style block or not?
-- TODO: How can I write something like noop???
-- NOTE: sequence matters here
styProg :: Parser [Block]
styProg = (some $ globalBlock <|> typeBlock <|> objBlock)
        <|> emptyProg

styleParser :: Parser [Block]
styleParser = between sc eof styProg

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
    -- parseTest styleParser styIn
    case runParser styleParser styFile styIn of
         Left err -> putStr (parseErrorPretty err)
         Right xs -> mapM_ print xs
    return ()
