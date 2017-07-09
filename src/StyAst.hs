module StyAst where
-- module Main (main) where

import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import System.Environment
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Lexer as L


-- Style grammar (relies on the Substance grammar, specifically SubObj)

data SubType = Set | Pt | Map | Intersect | NoIntersect | NoSubset | Subset | PointIn | PointNotIn | AllTypes deriving (Show, Eq)

data StyObj = Circle | Box | Dot | Arrow | NoShape | Color | Text | Auto
    deriving (Show)

type StyObjInfo
    = (StyObj, M.Map String Expr)

data StySpec = StySpec {
    spType :: SubType,
    spId :: String,
    spArgs :: [String],
    spShape :: StyObjInfo,
    spShpMap :: M.Map String StyObjInfo
} deriving (Show)

type StyProg = [Block]

type Block = ([Selector], [Stmt])

data Selector = Selector
              { selTyp :: SubType
              , selPatterns :: [Pattern]
            --   , selIds :: [String]
          }
              deriving (Show)
data Pattern
    = RawID String
    | WildCard String
    deriving (Show)

data Stmt
    = Assign String Expr
    | ObjFn String [Expr]
    | ConstrFn String [Expr]
    | Avoid String [Expr]
    deriving (Show)

data Expr
    = IntLit Integer
    | FloatLit Float
    | Id String
    | BinOp BinaryOp Expr Expr
    | Cons StyObj [Stmt] -- Constructors for objects
    deriving (Show)

data BinaryOp = Access deriving (Show)

data Color = RndColor | Colo
          { r :: Float
          , g :: Float
          , b :: Float
          , a :: Float }
          deriving (Show)
--
-- data Shape
--     = Circle [(String, Expr)]
--     | Box [(String, Expr)]
--     | Dot [(String, Expr)]
--     | Arrow [(String, Expr)]
--     | NoShape
--     deriving (Show)

---- Style program
styleParser :: Parser [Block]
styleParser = between sc eof styProg

-- TODO: required global and/or style block or not?
-- TODO: How can I write something like noop???
-- NOTE: sequence matters here
styProg :: Parser [Block]
styProg = some block
-- globalBlock <|> typeBlock <|> objBlock
-- <|> emptyProg

---- Style blocks
block :: Parser Block
block = do
    sel <- selector `sepBy1` comma
    void (symbol "{")
    try newline'
    sc
    stmts <- try stmtSeq
    -- res <- try stmtSeq
    -- let stmts = case res of
    --                 Just a -> a
    --                 Nothing -> []
    void (symbol "}")
    newline'
    return (sel, stmts)
    -- return (sel, [])

globalSelect :: Parser Selector
globalSelect = do
    i <- WildCard <$> identifier
    -- return $ Selector AllTypes [] []
    return $ Selector AllTypes [i]

constructorSelect :: Parser Selector
constructorSelect = do
    typ <- subtype
    -- sc
    pat <- patterns
    -- void sc <|> void (symbol ":")
    -- ids <- many identifier
    -- res <- optional $ rword "as" *> some identifier
    -- let ids = case res of
    --             Just a -> a
    --             Nothing -> []
    -- return $ Selector typ pat ids
    return $ Selector typ pat

selector :: Parser Selector
selector = constructorSelect <|> globalSelect

subtype :: Parser SubType
subtype = do
    str <- symbol "Set"
       <|> symbol "Point"
       <|> symbol "Map"
       <|> symbol "Subset"
       <|> symbol "NoSubset"
       <|> symbol "Intersect"
       <|> symbol "NoIntersect"
       <|> symbol "PointIn"
       <|> symbol "PointNotIn"

    return (convert str)
    where convert s
            | s == "Set" = Set
            | s == "Point" = Pt
            | s == "Map" = Map
            | s == "Subset" = Subset
            | s == "NoSubset" = NoSubset
            | s == "Intersect" = Intersect
            | s == "NoIntersect" = NoIntersect
            | s == "PointIn" = PointIn
            | s == "PointNotIn" = PointNotIn

styObj :: Parser StyObj
styObj = do
    str <- symbol "Color"
       <|> symbol "None"
       <|> symbol "Arrow"
       <|> symbol "Text"
       <|> symbol "Circle"
       <|> symbol "Box"
       <|> symbol "Dot"
    return (convert str)
    where convert s
            | s == "Color" = Color
            | s == "Arrow" = Arrow
            | s == "None"  = NoShape
            | s == "Text"  = Text
            | s == "Circle"  = Circle
            | s == "Box"  = Box
            | s == "Dot"  = Dot

patterns :: Parser [Pattern]
patterns = many pattern
    where pattern = (WildCard <$> identifier <|> RawID <$> backticks identifier)

    -- manyTill anyChar (symbol "{")
---- Statements
stmtSeq :: Parser [Stmt]
stmtSeq = endBy stmt newline'

stmt :: Parser Stmt
stmt =
    try objFn
    <|> try assignStmt
    <|> try avoidObjFn
    <|> try constrFn
--
assignStmt :: Parser Stmt
assignStmt = do
    var  <- attribute
    sc
    void (symbol "=")
    e    <- expr
    return (Assign var e)
--
objFn :: Parser Stmt
objFn = do
    rword "objective"
    fname  <- identifier
    void (symbol "(")
    params <- expr `sepBy` comma
    void (symbol ")")
    -- params <- sepBy expr comma
    return (ObjFn fname params)

avoidObjFn :: Parser Stmt
avoidObjFn = do
    rword "avoid"
    fname  <- identifier
    lbrac
    params <- expr `sepBy` comma
    rbrac
    return (Avoid fname params)

constrFn :: Parser Stmt
constrFn = do
    rword "constraint"
    fname  <- identifier
    void (symbol "(")
    params <- expr `sepBy` comma
    void (symbol ")")
    -- params <- sepBy expr comma
    return (ConstrFn fname params)

expr :: Parser Expr
expr =  try objConstructor
    <|> makeExprParser term operators
    <|> none
    <|> auto
    <|> number

term :: Parser Expr
term = Id <$> identifier

operators :: [[Operator Parser Expr]]
operators = [ [ InfixL (BinOp Access <$ symbol ".")] ]

none :: Parser Expr
none = do
    rword "None"
    return $ Cons NoShape []

auto :: Parser Expr
auto = do
    rword "Auto"
    return $ Cons Auto []

objConstructor :: Parser Expr
objConstructor = do
    typ <- styObj
    lbrac >> newline'
    stmts <- stmtSeq
    rbrac  -- NOTE: not consuming the space because stmt already does
    return $ Cons typ stmts

number :: Parser Expr
number =  FloatLit <$> try float <|> IntLit <$> integer

attribute :: Parser String
attribute = many alphaNumChar

--
--
--
-- typeBlock :: Parser Block
-- typeBlock = do
--     t     <- typ
--     stmts <- braces stmtSeq
--     return $ TypeBlock t stmts
--
-- objBlock :: Parser Block
-- objBlock = do
--     i     <- identifier
--     stmts <- braces stmtSeq
--     return $ ObjBlock i stmts


---- Lexer functions
sc :: Parser ()
sc = L.space (void separatorChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--" >> newline'
        blockCmnt = L.skipBlockComment "/*" "*/" >> newline'

newline' :: Parser ()
newline' = void sc >> void (many newline) >> void sc

backticks :: Parser a -> Parser a
backticks = between (symbol "`") (symbol "`")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lparen, rparen, lbrac, rbrac :: Parser ()
lbrac = void (symbol "{")
rbrac = void (symbol "}")
lparen = void (symbol "(")
rparen = void (symbol ")")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

comma :: Parser String
comma = symbol ","

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Float
float = realToFrac <$> lexeme L.float -- TODO: parsing without sign?

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

-- Reserved words
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc

rws, attribs, attribVs, shapes, types :: [String] -- list of reserved words
rws =     ["avoid", "global", "as"] ++ types ++ shapes
-- ++ types ++ attribs ++ shapes ++ colors

types =   ["Set", "Map", "Point"]
attribs = ["shape", "color", "label", "scale", "position"]
attribVs = shapes ++ colors
shapes =  ["Auto", "None", "Circle", "Box", "SolidArrow", "SolidDot", "HollowDot", "Cross"]
colors =  ["Random", "Black", "Red", "Blue", "Yellow"]
--
-- getType :: String -> SubType
-- getType str = case str of
--     "Set"   -> Set
--     "Map"   -> Map
--     "Point" -> Pt
--     "Subset" -> Subset
--
--
-- getAttribute :: String -> Attribute
-- getAttribute str = case str of
--     "shape" -> Shape
--     "color" -> Color
--     "bordor" -> LineType
--
--
-- getAttributeV :: String -> AttributeV
-- getAttributeV str = case str of
--     "None"       -> ShapeV   NoShape
--     "Circle"     -> ShapeV $ SS SetCircle
--     "Box"        -> ShapeV $ SS Box
--     "SolidArrow" -> ShapeV $ SM SolidArrow
--     "SolidDot"   -> ShapeV $ SP SolidDot
--     "HollowDot"  -> ShapeV $ SP HollowDot
--     "Cross"      -> ShapeV $ SP Cross
--     "Red"        -> ColorV Red
--     "Yellow"     -> ColorV Yellow
--     "Blue"       -> ColorV Blue
--     "Black"      -> ColorV Black
--     "Random"     -> ColorV Random
--
-- attribute :: Parser Attribute
-- attribute = (lexeme . try) (p >>= check)
--   where
--     p       = many letterChar
--     check x = if x `elem` attribs
--                 then return (getAttribute x)
--                 else fail $ "keyword " ++ show x ++ " cannot be an attribute"
--
-- attributeValue :: Parser AttributeV
-- attributeValue = (lexeme . try) (p >>= check)
--   where
--     p       = many letterChar
--     check x = if x `elem` attribVs
--                 then return (getAttributeV x)
--                 else fail $ "keyword " ++ show x ++ " cannot be an attribute value"
--
-- typ :: Parser SubType
-- typ = (lexeme . try) (p >>= check)
--   where
--     p       = some letterChar
--     check x = if x `elem` types
--                 then return (getType x)
--                 else fail $ "Type " ++ show x ++ " is not a valid type"
--
-- -- access :: Parser Expr
-- -- access = do
-- --     i <- identifier
-- --     void (symbol ".")
-- --     a <- attributeName
-- --     return (Access i a)
--
--
--
--

-- -- TODO
-- styPrettyPrint :: Block -> String
-- styPrettyPrint b = "HAHAHAHA"

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
