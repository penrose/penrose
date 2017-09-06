-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST, which are used by "Runtime"
module Style where
-- module Main (main) where -- for debugging purposes

import Shapes
import Utils
import Control.Monad (void)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromLeft)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import System.Environment
import qualified Substance as C
import Functions (objFuncDict, constrFuncDict, ObjFnOn, Weight, ConstrFnOn)
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Lexer as L



--------------------------------------------------------------------------------
-- Style AST

data StyObj = Ellip | Circle | Box | Dot | Arrow | NoShape | Color | Text | Auto
    deriving (Show)

type StyObjInfo
    = (StyObj, M.Map String Expr)

-- | Style specification for a particular object declared in Substance
data StySpec = StySpec {
    spType :: C.SubType,
    spId :: String,
    spArgs :: [String],
    spShape :: StyObjInfo,
    spShpMap :: M.Map String StyObjInfo
} deriving (Show)

type StyProg = [Block]

type Block = ([Selector], [Stmt])

data Selector = Selector
              { selTyp :: C.SubType
              , selPatterns :: [Pattern]
          } deriving (Show)

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

--------------------------------------------------------------------------------
-- Style Parser

-- | 'styleParser' is the top-level function that parses a Style proram
styleParser :: Parser [Block]
styleParser = between sc eof styProg

-- TODO: required global and/or style block or not?
-- TODO: How can I write something like noop???
-- NOTE: sequence matters here
styProg :: Parser [Block]
styProg = some block

---- Style blocks
block :: Parser Block
block = do
    sel <- selector `sepBy1` comma
    void (symbol "{")
    try newline'
    sc
    stmts <- try stmtSeq
    void (symbol "}")
    newline'
    return (sel, stmts)

globalSelect :: Parser Selector
globalSelect = do
    -- i <- WildCard <$> identifier
    rword "global"
    return $ Selector C.AllT []
    -- return $ Selector AllTypes [i]

constructorSelect :: Parser Selector
constructorSelect = do
    typ <- C.subtype
    pat <- patterns
    return $ Selector typ pat

selector :: Parser Selector
selector = constructorSelect <|> globalSelect


styObj :: Parser StyObj
styObj =
       (rword "Color"   >> return Color)   <|>
       (rword "None"    >> return NoShape) <|>
       (rword "Arrow"   >> return Arrow)   <|>
       (rword "Text"    >> return Text)    <|>
       (rword "Circle"  >> return Circle)  <|>
       (rword "Ellipse" >> return Ellip)   <|>
       (rword "Box"     >> return Box)     <|>
       (rword "Dot"     >> return Dot)

patterns :: Parser [Pattern]
patterns = many pattern
    where pattern = (WildCard <$> identifier <|> RawID <$> backticks identifier)

--- Statements
stmtSeq :: Parser [Stmt]
stmtSeq = endBy stmt newline'

stmt :: Parser Stmt
stmt =
    try objFn
    <|> try assignStmt
    <|> try avoidObjFn
    <|> try constrFn
    <|> try constrFnInfix
    <|> try objFnInfix
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
    return (ObjFn fname params)

objFnInfix :: Parser Stmt
objFnInfix = do
    rword "objective"
    arg1  <- expr
    fname <- identifier
    arg2  <- expr
    return (ObjFn fname [arg1, arg2])

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
    return (ConstrFn fname params)

constrFnInfix :: Parser Stmt
constrFnInfix = do
    rword "constraint"
    arg1  <- expr
    fname <- identifier
    arg2  <- expr
    return (ConstrFn fname [arg1, arg2])

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

-- TODO: use the PrettyPrint library
-- styPrettyPrint :: Block -> String
-- styPrettyPrint b = ""

--------------------------------------------------------------------------------
-- Functions used by "Runtime"

----- Parser for Style design

-- Type aliases for readability in this section
type StyDict = M.Map Name StySpec
-- A VarMap matches lambda ids in the selector to the actual selected id
type VarMap  = M.Map Name Name

initSpec :: StySpec
initSpec = StySpec { spType = C.PointT, spId = "", spShape = (NoShape, M.empty),  spArgs = [], spShpMap = M.empty}

-- | `getDictAndFns` is the top-level function used by "Runtime", which returns a dictionary of Style configuration and all objective and constraint fucntions generated by Style
-- TODO: maybe generate objects directly?
getDictAndFns :: (Floating a, Real a, Show a, Ord a) =>
    ([C.SubDecl], [C.SubConstr]) -> StyProg
    -> (StyDict, [(ObjFnOn a, Weight a, [Name], [a])], [(ConstrFnOn a, Weight a, [Name], [a])])
getDictAndFns (decls, constrs) blocks = foldl procBlock (initDict, [], []) blocks
    where
        res = getSubTuples decls ++ getConstrTuples constrs
        ids = map (\(x, y, z) -> (x, y)) res
        initDict = foldl (\m (t, n, a) ->
                        M.insert n (initSpec { spId = n, spType = t, spArgs = a }) m) M.empty res

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
                       vs = map (getVarMap s) xs in zip vs xs) selectors
        allVars = M.fromList $ zip k k where k = M.keys dict
        -- Combination of all selected (spec. varmap)
        allCombs = filter (\x -> length x == length selectedSpecs) $ cartesianProduct (map (map fst) selectedSpecs)
        -- $ trStr (concatMap (\x -> show x ++ "\n") $ map (map fst) selectedSpecs)
        mergedMaps = if length selectors == 1 && (selTyp . head) selectors == C.AllT then [allVars] else map M.unions (filter noDup allCombs)
        noDup ms = validMap $ concatMap M.toAscList ms
        validMap ts = and . tr "result" . fst $ foldl
            (\(l, m) (x, y) -> case M.lookup x m of
                Nothing -> (True:l, M.insert x y m)
                Just y' -> ((tr ("compare: " ++ y ++ " " ++ y' ++ ": ") $ y == y') : l, m))
            ([], M.empty) (tr "ts" ts)
        -- Only process assignment statements on matched specs, not the cartesion product of them
        updateSpec d (vm, sp) =
            let newSpec = foldl (procAssign vm) sp stmts in
            M.insert (spId newSpec) newSpec d
        newDict = foldl updateSpec dict $ concat selectedSpecs
        genFns f vm = foldl (f vm) [] stmts
        newObjFns    = concatMap (genFns procObjFn) $ tr "mergedMaps: " mergedMaps
        newConstrFns = concatMap (genFns procConstrFn) mergedMaps


-- removeDups :: [[(VarMap, StySpec)]]
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
    trStr ("New Constraint function: " ++ fname ++ " " ++ (show names)) $
    fns ++ [(func, defaultWeight, names, nums)]
    where
        func = case M.lookup fname constrFuncDict of
            Just f -> f
            Nothing -> error "procConstrFn: constraint function not known"
        (names, nums) = partitionEithers $ map (procExpr varMap) es
procConstrFn varMap fns _ = fns -- TODO: avoid functions

procObjFn :: (Floating a, Real a, Show a, Ord a) =>
    VarMap -> [(ObjFnOn a, Weight a, [Name], [a])] -> Stmt
    -> [(ObjFnOn a, Weight a, [Name], [a])]
procObjFn varMap fns (ObjFn fname es) =
    trStr ("New Objective function: " ++ fname ++ " " ++ (show names)) $
    fns ++ [(func, defaultWeight, names, nums)]
    where
        func = case M.lookup fname objFuncDict of
            Just f -> f
            Nothing -> error "procObjFn: objective function not known"
        (names, nums) = partitionEithers $ map (procExpr varMap) es
procObjFn varMap fns (Avoid fname es) = fns -- TODO: avoid functions
procObjFn varMap fns _ = fns -- TODO: avoid functions

-- TODO: Have a more principled expr look up routine
lookupVarMap s varMap= case M.lookup s varMap of
    Just s -> s
    Nothing  -> (error $ "lookupVarMap: incorrect variable mapping from " ++ s)

procExpr :: (Floating a, Real a, Show a, Ord a) =>
    VarMap -> Expr -> Either String a
procExpr d (Id s)  = Left $ lookupVarMap s d
-- FIXME: properly resolve access by doing lookups
procExpr d (BinOp Access (Id i) (Id "label"))  = Left $ labelName $ lookupVarMap i d
procExpr d (BinOp Access (Id i) (Id "shape"))  = Left $ lookupVarMap i d
procExpr _ (IntLit i) = Right $ r2f i
procExpr _ (FloatLit i) = Right $ r2f i
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
        -- FIXME: wrap fromleft inside a function!
        addSpec dict (Assign s e) = M.insert s (Id (fromLeft (error "Unexpected ID")$ procExpr varMap e)) dict
        addSpec _ _ = error "procAssign: only support assignments in constructors!"
procAssign _ spec  _  = spec -- TODO: ignoring assignment for all others

-- FIXME: make sure these names are unique and make sure users cannot start ids
-- with underscores
getConstrTuples :: [C.SubConstr] -> [(C.SubType, String, [String])]
getConstrTuples = map getType
    where getType c = case c of
            C.Intersect a b -> (C.IntersectT, "_Intersect" ++ a ++ b, [a, b])
            C.NoIntersect a b -> (C.NoIntersectT, "_NoIntersect" ++ a ++ b, [a, b])
            C.Subset a b -> (C.SubsetT, "_Subset" ++ a ++ b, [a, b])
            C.NoSubset a b -> (C.NoSubsetT, "_NoSubset" ++ a ++ b, [a, b])
            C.PointIn a b -> (C.PointInT, "_In" ++ a ++ b, [a, b])
            C.PointNotIn a b -> (C.PointNotInT, "_PointNotIn" ++ a ++ b, [a, b])

getSubTuples :: [C.SubDecl] -> [(C.SubType, String, [String])]
getSubTuples = map getType
    where getType d = case d of
            C.Set n -> (C.SetT, n, [n])
            C.Point n -> (C.PointT, n, [n])
            C.Map n a b -> (C.MapT, n, [n, a, b])
            C.Value n a b -> (C.ValueT, "_Value" ++ n ++ a ++ b, [n, a, b])

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
