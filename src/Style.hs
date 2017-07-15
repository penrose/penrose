-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST, which are used by "Runtime"
module Style where
-- module Main (main) where -- for debugging purposes

import Functions
import Shapes
import Utils
import Control.Monad (void)
import Data.Either (partitionEithers)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import System.Environment
import qualified Substance as C
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Lexer as L

--------------------------------------------------------------------------------
-- Style AST

data SubType = Set | Point | Map | Intersect | NoIntersect | NoSubset | Subset | PointIn | PointNotIn | AllTypes deriving (Show, Eq)

data StyObj = Ellip | Circle | Box | Dot | Arrow | NoShape | Color | Text | Auto
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

data Numeric = I Integer | F Float

--
-- data Shape
--     = Circle [(String, Expr)]
--     | Box [(String, Expr)]
--     | Dot [(String, Expr)]
--     | Arrow [(String, Expr)]
--     | NoShape
--     deriving (Show)

--------------------------------------------------------------------------------
-- Style Parser

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
       <|> symbol "In"
       <|> symbol "PointNotIn"

    return (convert str)
    where convert s
            | s == "Set" = Set
            | s == "Point" = Point
            | s == "Map" = Map
            | s == "Subset" = Subset
            | s == "NoSubset" = NoSubset
            | s == "Intersect" = Intersect
            | s == "NoIntersect" = NoIntersect
            | s == "In" = PointIn
            | s == "PointNotIn" = PointNotIn

-- TODO: maybe simplify this pattern using some Monad magic? An alternative is:
-- `try $ symbol "Color" >> return Color` to avoid this `convert` function
styObj :: Parser StyObj
styObj = do
    str <- symbol "Color"
       <|> symbol "None"
       <|> symbol "Arrow"
       <|> symbol "Text"
       <|> symbol "Circle"
       <|> symbol "Ellipse"
       <|> symbol "Box"
       <|> symbol "Dot"
    return (convert str)
    where convert s
            | s == "Color" = Color
            | s == "Arrow" = Arrow
            | s == "None"  = NoShape
            | s == "Text"  = Text
            | s == "Circle"  = Circle
            | s == "Ellipse"  = Ellip
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
attribVs = shapes
shapes =  ["Auto", "None", "Circle", "Box", "SolidArrow", "SolidDot", "HollowDot", "Cross"]
-- colors =  ["Random", "Black", "Red", "Blue", "Yellow"]
--
-- getType :: String -> SubType
-- getType str = case str of
--     "Set"   -> Set
--     "Map"   -> Map
--     "Point" -> Point
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
-- styPrettyPrint b = ""

--------------------------------------------------------------------------------
-- Functions used by "Runtime"

----- Parser for Style design
--- TODO: move these to the Style module

-- Type aliases for readability in this section
type StyDict = M.Map Name StySpec
-- type ObjFn a = M.Map Name (Obj' a) -> a
-- type ConstrFn a = [Obj' a] -> a
-- type ObjFn a    = [Obj' a] -> a
-- A VarMap matches lambda ids in the selector to the actual selected id
type VarMap  = M.Map Name Name

initSpec :: StySpec
initSpec = StySpec { spType = Point, spId = "", spShape = (NoShape, M.empty),  spArgs = [], spShpMap = M.empty}

-- | `getDictAndFns` is the top-level function used by "Runtime", which returns a dictionary of Style configuration and all objective and constraint fucntions generated by Style
-- TODO: maybe generate objects directly?
getDictAndFns :: (Floating a, Real a, Show a, Ord a) =>
    ([C.SubDecl], [C.SubConstr]) -> StyProg
    -> (StyDict, [(ObjFnOn a, Weight a, [Name], [a])], [(ConstrFnOn a, Weight a, [Name], [a])])
getDictAndFns (decls, constrs) blocks = foldl procBlock (initDict, [], []) blocks
    where
        res = getSubTuples decls ++ getConstrTuples constrs
        ids = map (\(x, y, z) -> (x, y)) res
        -- args = map (\(_, _, z) -> z) res
        initDict = foldl (\m (t, n, a) ->
                        M.insert n (initSpec { spId = n, spType = t, spArgs = a }) m) M.empty res
        -- applyConfig f d = foldl f d prog

procBlock :: (Floating a, Real a, Show a, Ord a) =>
    (StyDict, [(ObjFnOn a, Weight a, [Name], [a])], [(ConstrFnOn a, Weight a, [Name], [a])])
    -> Block
    -> (StyDict, [(ObjFnOn a, Weight a, [Name], [a])], [(ConstrFnOn a, Weight a, [Name], [a])])
procBlock (dict, objFns, constrFns) (selectors, stmts) = (newDict, objFns ++ newObjFns, constrFns ++ newConstrFns)
    where
        select s = M.elems $ M.filter (matchSel s) dict
        -- selectedSpecs :: [[(VarMap, StySpec)]]
        selectedSpecs = map
            (\s -> let xs = select s
                       vs = map (allOtherVars . getVarMap s) xs in zip vs xs) selectors
        -- TODO: scoping - now every block has access to everyone else
        allOtherVars = M.union (M.fromList $ zip k k) where k = M.keys dict
        -- Combination of all selected (spec. varmap)
        allCombs = filter (\x -> length x == length selectedSpecs) $ cartesianProduct (map (map fst) selectedSpecs)
        mergedMaps =
            -- let allMaps = map (map fst) allCombs in
            -- map M.unions (tr "allMaps: " allMaps)
            -- tr "allmaps: " $
            map M.unions allCombs
        -- Only process assignment statements on matched specs, not the cartesion product of them
        updateSpec d (vm, sp) =
            let newSpec = foldl (procAssign vm) sp stmts in
            M.insert (spId newSpec) newSpec d
        newDict = foldl updateSpec dict $ concat selectedSpecs
        -- (zip varMaps selected)
        genFns f vm = foldl (f vm) [] stmts
        newObjFns    = concatMap (genFns procObjFn) mergedMaps
        newConstrFns = concatMap (genFns procConstrFn) mergedMaps

cartesianProduct = foldr f [[]] where f l a = [ x:xs | x <- l, xs <- a ]

-- Returns a map from placeholder ids to actual matched ids
getVarMap :: Selector -> StySpec -> VarMap
getVarMap sel spec = foldl add M.empty patternNamePairs
    where
        patternNamePairs = zip (selPatterns sel) (spArgs spec)
        add d (p, n) = case p of
            RawID _    -> d
            WildCard i -> M.insert i n d


-- Returns true of an object matches the selector
matchSel :: Selector -> StySpec -> Bool
matchSel sel spec = all test (zip args patterns) &&
                selTyp sel == spType spec &&
                length args == length patterns
    where
        patterns = selPatterns sel
        args = spArgs spec
        -- dummies = selIds sel
        test (a, p) = case p of
            RawID i -> a == i
            WildCard _ -> True

procConstrFn :: (Floating a, Real a, Show a, Ord a) =>
    VarMap -> [(ConstrFnOn a, Weight a, [Name], [a])] -> Stmt
    -> [(ConstrFnOn a, Weight a, [Name], [a])]
procConstrFn varMap fns (ConstrFn fname es) =
    -- trStr ("New Constraint function: " ++ fname ++ " " ++ (show names)) $
    fns ++ [(func, defaultWeight, names, [])]
    where
        (func, names) = case M.lookup fname constrFuncDict of
            Just f -> (f, map (procExpr varMap) es)
            Nothing -> error "procConstrFn: constraint function not known"
procConstrFn varMap fns _ = fns -- TODO: avoid functions

procObjFn :: (Floating a, Real a, Show a, Ord a) =>
    VarMap -> [(ObjFnOn a, Weight a, [Name], [a])] -> Stmt
    -> [(ObjFnOn a, Weight a, [Name], [a])]
procObjFn varMap fns (ObjFn fname es) =
    trStr ("New Objective function: " ++ fname ++ " " ++ (show names)) $
    fns ++ [(func, defaultWeight, names, [])]
    where
        (func, names) = case M.lookup fname objFuncDict of
            Just f -> (f, tr "Args: " args)
            Nothing -> error "procObjFn: objective function not known"
        args = map (procExpr varMap) es
procObjFn varMap fns (Avoid fname es) = fns -- TODO: avoid functions
procObjFn varMap fns _ = fns -- TODO: avoid functions

-- TODO: Have a more principled expr look up routine
lookupVarMap s varMap= case M.lookup s varMap of
    Just s -> s
    Nothing  -> (error $ "lookupVarMap: incorrect variable mapping from " ++ s)

-- procExpr :: VarMap -> Expr -> Either String Numeric
-- procExpr d (IntLit i) = Right $ I i
-- procExpr d (FloatLit f) = Right $ F f
-- procExpr d (Id s)  = Left $ lookupVarMap s d
-- -- FIXME: properly resolve access by doing lookups
-- procExpr d (BinOp Access (Id i) (Id "label"))  = Left $ labelName $ lookupVarMap i d
-- procExpr d (BinOp Access (Id i) (Id "shape"))  = Left $ lookupVarMap i d
procExpr :: VarMap -> Expr -> String
procExpr d (Id s)  = lookupVarMap s d
-- FIXME: properly resolve access by doing lookups
procExpr d (BinOp Access (Id i) (Id "label"))  = labelName $ lookupVarMap i d
procExpr d (BinOp Access (Id i) (Id "shape"))  = lookupVarMap i d
procExpr _ _  = error "expr: argument unsupported!"

procAssign :: VarMap -> StySpec -> Stmt -> StySpec
procAssign varMap spec (Assign n (Cons typ stmts)) =
    if n == "shape" then spec { spShape = (typ, configs) } -- primary shape
        else spec { spShpMap = M.insert n (typ, configs) $ spShpMap spec } -- secondary shapes
    where
        configs = foldl addSpec M.empty stmts
        -- FIXME: this is incorrect, we should resolve the variables earlier
        addSpec dict (Assign s e@(Cons NoShape _)) = M.insert s (Id "None") dict
        addSpec dict (Assign s e@(Cons Auto _)) = M.insert s (Id "Auto") dict
        addSpec dict (Assign s e) = M.insert s (Id (procExpr varMap e)) dict
        addSpec _ _ = error "procAssign: only support assignments in constructors!"
procAssign _ spec  _  = spec -- TODO: ignoring assignment for all others

getConstrTuples :: [C.SubConstr] -> [(SubType, String, [String])]
getConstrTuples = map getType
    where getType c = case c of
            C.Intersect a b -> (Intersect, "Intersect" ++ a ++ b, [a, b])
            C.NoIntersect a b -> (NoIntersect, "NoIntersect" ++ a ++ b, [a, b])
            C.Subset a b -> (Subset, "Subset" ++ a ++ b, [a, b])
            C.NoSubset a b -> (NoSubset, "NoSubset" ++ a ++ b, [a, b])
            C.PointIn a b -> (PointIn, "In" ++ a ++ b, [a, b])
            C.PointNotIn a b -> (PointNotIn, "PointNotIn" ++ a ++ b, [a, b])

getSubTuples :: [C.SubDecl] -> [(SubType, String, [String])]
getSubTuples = map getType
    where getType (C.Decl d) = case d of
            C.OS (C.Set' n _) -> (Set, n, [n])
            C.OP (C.Pt' n) -> (Point, n, [n])
            C.OM (C.Map' n a b) -> (Map, n, [n, a, b])

getAllIds :: ([C.SubDecl], [C.SubConstr]) -> [String]
getAllIds (decls, constrs) = map (\(_, x, _) -> x) $ getSubTuples decls ++ getConstrTuples constrs


--------------------------------------------------------------------------------
-- DEBUG: takes an input file and prints the parsed AST

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
