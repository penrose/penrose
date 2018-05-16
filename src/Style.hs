-- | The "Style" module contains the compiler for the Style language,
-- and functions to traverse the Style AST, which are used by "Runtime"

{-# OPTIONS_HADDOCK prune #-}
module Style where
-- module Main (main) where -- for debugging purposes

import Shapes
import Utils
import Control.Monad (void)
import Data.Function (on)
import Data.Either (partitionEithers)
import Data.Either.Extra (fromLeft)
import Data.Maybe (fromMaybe)
import Data.List (nubBy, nub, intercalate)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import System.Environment
import Debug.Trace
import qualified Substance as C
import Functions (objFuncDict, constrFuncDict, ObjFnOn, Weight, ConstrFnOn, ConstrFnInfo, ObjFnInfo)
import Computation
import qualified Data.Map.Strict as M
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Dynamic
import Data.Typeable

--------------------------------------------------------------------------------
-- Style AST

-- | Type annotation for all geometries supported by Style so far.
data StyType = Ellip | Circle | Box | Rectangle | Dot | Arrow | NoShape | Color | Text | Curve | Auto 
               | Line2 -- two points
    deriving (Show, Eq, Ord, Typeable) -- Ord for M.toList in Runtime

-- | A Style program is a collection of blocks
type StyProg = [Block]

-- | A Style block contains a list of selectors and statements
type Block = ([Selector], [Stmt])

-- | A selector is some pattern annotated by a __Substance type__.
data Selector = Selector
              { selTyp :: C.SubType -- type of Substance object
              , selPatterns :: [Pattern] -- a list of patterns: ids or wildcards
          } deriving (Show, Typeable)

-- | So far we have two kinds of patterns:
--
-- * Raw IDs: 'Set `A`', referring to actual IDs from Substance
-- * WildCard: `Set A`, which can be anything with the corresponding type
--
-- They can also be mixed, yielding to partial selectors like 'Subset `A` B', referring to all supersets of 'A'.
data Pattern
    = RawID String
    | WildCard String
    deriving (Show, Typeable)

-- | A Style statement
data Stmt
    = Assign String Expr -- binding to geometric primitives: 'shapeName = ShapeType { ... }'
    | ObjFn String [Expr] -- adding an objective function
    | ConstrFn String [Expr] -- adding a constraint function
    | Avoid String [Expr] -- to be implemented, stating an objective that we would like to avoid
    deriving (Show, Typeable)

-- | A Style expression, typically appears on the righthand side of assignment statements
data Expr
    = IntLit Integer
    | FloatLit Float
    | StringLit String
    | Id String
    | BinOp BinaryOp Expr Expr
    | Cons StyType [Stmt] -- | Constructors for objects
    | CompArgs String [Expr]
    deriving (Show, Typeable)

-- TODO: this feature is NOT fully implemented. As if now, we do not support chained dot-access to arbitrary elements in the environment.
-- Difficulty: the return type of each access might be different. What is a good way to resolve this?
data BinaryOp = Access deriving (Show, Typeable)

data Color = RndColor | Colo
          { r :: Float
          , g :: Float
          , b :: Float
          , a :: Float }
          deriving (Show, Typeable)

--------------------------------------------------------------------------------
-- Style Parser

-- | 'styleParser' is the top-level function that parses a Style proram
styleParser :: Parser [Block]
styleParser = between scn eof styProg

-- | `styProg` parses a Style program, consisting of a collection of one or
-- more blocks
styProg :: Parser [Block]
styProg = some block

-- | Style blocks
block :: Parser Block
block = do
    sel <- selector `sepBy1` comma
    lbrac >> scn
    stmts <- try stmtSeq
    rbrac >> scn
    return (sel, stmts)

-- | a selector can be either a global selector or constructor selector
selector :: Parser Selector
selector = constructorSelect <|> globalSelect

-- | a global selector matches on all types of objects. Normally used as the only selector in a "global block", where all Substance identifiers are visible
globalSelect :: Parser Selector
globalSelect = do
    rword "global"
    return $ Selector C.AllT []

-- | a constructor selector selects Substance objects in a similar syntax as they were declared in Substance
-- TODO: with the exception of some new grammars that we developed, such as 'f: A -> B'. You still have to do 'Map A B' to select this object.
constructorSelect :: Parser Selector
constructorSelect = do
    typ <- C.subType
    pat <- patterns
    return $ Selector typ pat

-- | a pattern can be a list of the mixture of wildcard bindings and concrete IDs
patterns :: Parser [Pattern]
patterns = many pattern
    where pattern = (WildCard <$> identifier <|> RawID <$> backticks identifier)

-- | parses the type of Style object
styObj :: Parser StyType
styObj =
       (rword "Color"   >> return Color)   <|>
       (rword "None"    >> return NoShape) <|>
       (rword "Arrow"   >> return Arrow)   <|>
       (rword "Text"    >> return Text)    <|>
       (rword "Circle"  >> return Circle)  <|>
       (rword "Curve"   >> return Curve)   <|>
       (rword "Ellipse" >> return Ellip)   <|>
       (rword "Box"     >> return Box)     <|>
       (rword "Rect"    >> return Rectangle)     <|>
       (rword "Dot"     >> return Dot) <|>
       (rword "Line"     >> return Line2)

-- | a sequence of Style statements
stmtSeq :: Parser [Stmt]
stmtSeq = stmt `sepEndBy` newline'

-- | a Style statement can be objective/constraint function calls or assignment statements
stmt :: Parser Stmt
stmt =  try objFn
    <|> try assignStmt -- TODO: redundant `try`s?
    <|> try avoidObjFn
    <|> try constrFn
    <|> try constrFnInfix
    <|> try objFnInfix

-- TODO: Currently, "start = p" or "color = computeColor(...)" (not sure about args yet)
-- TODO: How does it deal with nested assignments? Also, add ~ and *
assignStmt :: Parser Stmt
assignStmt = do
    var  <- attribute
    void (symbol "=")
    e    <- (try parseComp <|> expr) -- TODO: right order? try on which/both/neither?
    return (Assign var e)

-- | modeled off of objFn
parseComp :: Parser Expr
parseComp = do
    fname <- identifier
    lparen
    params <- expr `sepBy` comma
    rparen
    return (CompArgs fname params)

-- | objective function call
objFn :: Parser Stmt
objFn = do
    rword "objective"
    fname  <- identifier
    lparen
    params <- expr `sepBy` comma
    rparen
    return (ObjFn fname params)

-- | objective function call - infix version
objFnInfix :: Parser Stmt
objFnInfix = do
    rword "objective"
    arg1  <- expr
    fname <- identifier
    arg2  <- expr
    return (ObjFn fname [arg1, arg2])

-- | (TODO: to be implemented) "avoid" objective function call
avoidObjFn :: Parser Stmt
avoidObjFn = do
    rword "avoid"
    fname  <- identifier
    lbrac
    params <- expr `sepBy` comma
    rbrac
    return (Avoid fname params)

-- | constraint function call
constrFn :: Parser Stmt
constrFn = do
    rword "constraint"
    fname  <- identifier
    lparen
    params <- expr `sepBy` comma
    rparen
    return (ConstrFn fname params)

-- | constraint function call -- infix version
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
    <|> stringLit

term :: Parser Expr
term = Id <$> identifier

operators :: [[Operator Parser Expr]]
operators = [ [ InfixL (BinOp Access <$ symbol ".")] ]

-- | Special keyword @None@, normally written as @shape = None@, making the system not render the Substance object
none :: Parser Expr
none = do
    rword "None"
    return $ Cons NoShape []

-- | Special keyword @Auto@, currently used on labels. When specified, the system automatically generates a label using the object's Substance ID.
auto :: Parser Expr
auto = do
    rword "Auto"
    return $ Cons Auto []

objConstructor :: Parser Expr
objConstructor = do
    typ <- styObj
    lbrac >> scn
    stmts <- try stmtSeq
    rbrac >> sc -- NOTE: does NOT consume newline because this is an expression
    return $ Cons typ stmts

number :: Parser Expr
number =  FloatLit <$> try float <|> IntLit <$> integer

stringLit :: Parser Expr
stringLit = StringLit <$> (char '"' >> manyTill L.charLiteral (char '"'))

attribute :: Parser String
-- attribute = many alphaNumChar
attribute = identifier -- TODO: Naming convention - same as identifiers?

-- TODO: use the PrettyPrint library
-- styPrettyPrint :: Block -> String
-- styPrettyPrint b = ""

--------------------------------------------------------------------------------
-- Functions used by "Runtime" -- Style Translator
-- Translates from a Style AST to:
-- * a list of geometric primitives
-- * a list of constraints
-- * a list of objectives

-- Type aliases for readability in this section
-- | 'StyContext' maintains the current output of the translater
type StyContext a =
    (StyDict,         -- | dictionary mapping Substance ID to Style objects
    [ObjFnInfo a],    -- | List of objective functions
    [ConstrFnInfo a]) -- | List of constraints

-- | A dictionary storing properties of a Style object, e.g. "start" for 'Arrow'
type Properties = M.Map String Expr

-- | A Style object is a type annotation ('StyType') and a collection of
-- properties. E.g. an arrow is 'Arrow' and properties: "start", "end".
type StyObj = (StyType, Properties)

-- | a Style Dictionary maps from a Substance id to a 'StySpec' -- collection of Style objects and some metadata about the Substance object
type StyDict = M.Map Name StySpec

-- | Style specification for a particular object declared in Substance (declarations and constraints)
-- (TODO: maybe this is not the best model, since there is no difference between "primary" and "secondary" shapes)
-- NOTE: for Substance constraints such as `Subset A B`, the 'spId' will be '_Subset_A_B' and the 'spArgs' will be '['A', 'B']'
data StySpec = StySpec {
    spType   :: C.SubType,  -- | The Substance type of the object
    spId     :: String,     -- | The Substance ID of the object
    spArgs   :: [String],   -- | the "arguments" following the type.  The idea is to capture @f@, @A@ and @B@ in the case of @Map f A B@, which is needed for pattern matching (TODO: Maybe not the best term here.)
    spShpMap :: M.Map String StyObj -- | shapes associated with the substance object, e.g. "shapeName = shapeType { ... }; otherShape = otherType { ... }"
} deriving (Show, Typeable)

-- | A VarMap matches lambda ids in the selector to the actual selected id
type VarMap  = M.Map Name Name

initSpec :: StySpec
initSpec = StySpec { spType = (C.TypeConst "PointT" ),
                     spId = "",
                     spArgs = [],
                     spShpMap = M.empty
                   }

-- | 'getDictAndFns' is the top-level function used by "Runtime", which returns a dictionary of Style objects and all objective and constraint functions generated by Style
-- TODO: maybe generate objects directly?
getDictAndFns :: (Autofloat a) =>
    ([C.SubDecl], [C.SubConstr]) -> StyProg -> StyContext a
getDictAndFns (decls, constrs) blocks =
    foldl procBlock (initDict, [], []) blocks
    where
        -- idsWithArgs :: [(C.SubType, String, [String])]
        idsWithArgs = getSubTuples decls ++ getConstrTuples constrs
        -- initDict :: M.Map String StySpec
        initDict = foldl (\m (t, n, a) ->
                     M.insert n (initSpec { spId = n, spType = t, spArgs = a }) m) M.empty idsWithArgs

--------------------------------------------------------------------------------
-- Selector implementation
-- GOAL: given a list of selectors and all Substance identifiers, output a
-- variable map (from aliases to ids) and a list of selected specs

-- | Returns true if an object (declaration or constraint) matches the selector. A match is made when all of the conditions are met:
-- 1. The number of arguments match,
-- 2. The types match,
-- 3. Identifier and arguments match. A wildcard matches with anything
matched :: StySpec -> Selector -> Bool
matched spec selector =
    let patterns = selPatterns selector
        args     = spArgs spec
    in  length args      == length patterns &&  -- 1
        selTyp selector  == spType spec     &&  -- 2
        all match (zip args patterns)           -- 3
    where
        match (a, p) = case p of
            RawID    i -> a == i
            WildCard _ -> True

-- | Return all StySpecs and associated variable maps in the StyDict that
-- match a selector.
matchWith :: StyDict -> Selector -> [(VarMap, StySpec)]
matchWith dict selector = M.foldlWithKey getSpecAndVarMap [] dict
    where getSpecAndVarMap mapsAndSpecs name spec =
            if spec `matched` selector
                then (getVarMap selector spec, spec) : mapsAndSpecs
                else mapsAndSpecs

-- | Match all entries in the 'StyDict' with all selectors. Each selector can
-- potentially have a list of (varmap, spec) pairs, and thus we have a list of
-- list of (varmap, spec) for a list of selectors.
matchWithAll :: StyDict -> [Selector] -> [[(VarMap, StySpec)]]
matchWithAll dict selectors = map (matchWith dict) selectors

-- | Given a selector and a Style spec that matches with it, returns a map from placeholder ids to actual matched ids.
-- TODO: the underlying logic is the same as `matched`. Think of a cleaner
-- way to both (1) check if there is a match and (2) collect a list of `VarMap`s
getVarMap :: Selector -> StySpec -> VarMap
getVarMap sel spec = foldl add M.empty patternNamePairs
    where
        -- patternNamePairs :: [(Pattern, String)]
        patternNamePairs = zip (selPatterns sel) (spArgs spec)
        -- add :: VarMap -> (Pattern, Name) -> VarMap
        add dict (patt, name) = case patt of
            RawID _    -> dict
            WildCard wc -> M.insert wc name dict

-- Given either 'procConstrFn' or 'procObjFn' and its arguments, returns
-- a #map@pable function that can get @map@ped onto a list of variable maps
genFns :: (VarMap -> [t] -> Stmt -> [t]) -> [Stmt] -> VarMap -> [t]
genFns f stmts varmap = foldl (f varmap) [] stmts

-- | 'procBlock' is called by 'getDictAndFns'. 'getDictAndFns' would fold this function on a list of blocks, a.k.a. a Style program, and accumulate objective/constraint functions, and a dictionary of geometries to be rendered.
-- TODO: first split `stmts` into assigns, objs, and contrs and call procConstrFn and procObjFn with `map`?
procBlock :: (Autofloat a) =>
    StyContext a ->
    Block ->
    StyContext a
-- Case 1: @global@ block - only objectives and constraints are allowed, and
--- there is no need to compute any variable map
procBlock (dict, objFns, constrFns) ((Selector C.AllT []) : [], stmts) =
    let keys          = M.keys dict
        identityMap   = M.fromList $ zip keys keys
        newObjFns     = concatMap (procObjFn identityMap [])    stmts
        newConstrFns  = concatMap (procConstrFn identityMap []) stmts
    in (dict, objFns ++ newObjFns, constrFns ++ newConstrFns)

-- Case 2: only one selector, no need to compute combinations.
-- this function will process: (1) objectives, (2) constraints and (3)
-- assignments
procBlock (dict, objFns, constrFns) (selector : [], stmts) =
    let mapsAndSpecs  = dict `matchWith` selector
        newDict       = foldl (addShapes stmts) dict mapsAndSpecs
        varmaps       = map fst mapsAndSpecs
        newObjFns     = concatMap (genFns procObjFn stmts)    varmaps
        newConstrFns  = concatMap (genFns procConstrFn stmts) varmaps
    in (newDict, objFns ++ newObjFns, constrFns ++ newConstrFns)
    where
        -- For the block, add each statement to the spec and insert the spec in the overall stydict
        addShapes :: [Stmt] -> StyDict -> (VarMap, StySpec) -> StyDict
        addShapes statements dct (varmap, spec) =
            let newSpec = foldl (procAssign varmap) spec statements
            in M.insert (spId newSpec) newSpec dct

-- Case 3: multiple selectors, which in the current Style languages means
-- we would take a subset of the cartesian product among the selectors.
-- TEMP: the system does not support assignment statements in these blocks,
-- because it is not clear that which Substance object the shapes associate with
procBlock (dict, objFns, constrFns) (selectors, stmts) =
    let mapsAndSpecs  = dict `matchWithAll` selectors
        mergedMaps    = tr "mergedMaps: " $ mergeMaps $ map (map fst) mapsAndSpecs
        newObjFns     = concatMap (genFns procObjFn stmts)    mergedMaps
        newConstrFns  = concatMap (genFns procConstrFn stmts) mergedMaps
    in (dict, objFns ++ newObjFns, constrFns ++ newConstrFns)
    where
        -- makes sure we don't bind same name with multiple Substance ids
        -- in a list of comma-separated selectors
        isOneToOne :: [VarMap] -> Bool
        isOneToOne varmaps =
            let flatMap      = nub $ concatMap M.toAscList varmaps
                bijection    = bijectify flatMap
            in  flatMap == bijection
            where bijectify = nubBy ((==) `on` snd) . nubBy ((==) `on` fst)

        -- allCombinationsOf :: [[VarMap]] -> [[VarMap]]
        allCombinationsOf varmaps = filter
            (\x -> length x == length varmaps) $
            cartesianProduct varmaps
        -- mergeMaps :: [[VarMap]] -> [VarMap]
        mergeMaps varmaps =  map M.unions (filter isOneToOne $
            allCombinationsOf varmaps)


-- | Called repeatedly by 'procBlock', 'procConstrFn' would look up and generate constraint functions if the input is a constraint function call. It ignores all other inputs
procConstrFn :: (Autofloat a) =>
    VarMap ->
    [ConstrFnInfo a] ->
    Stmt ->
    [ConstrFnInfo a]
procConstrFn varMap fns (ConstrFn fname es) =
    trStr ("New Constraint function: " ++ fname ++ " " ++ (show names)) $
    (func, defaultWeight, names, nums) : fns
    where
        func = case M.lookup fname constrFuncDict of
            Just f -> f
            Nothing -> error ("procConstrFn: constraint function " ++ fname ++ " not known")
        (names, nums) = partitionEithers $ map (procExpr varMap) es
procConstrFn varMap fns _ = fns -- TODO: avoid functions

-- | Similar to `procConstrFn` but for objective functions
procObjFn :: (Autofloat a) =>
    VarMap ->
    [ObjFnInfo a] ->
    Stmt ->
    [ObjFnInfo a]
procObjFn varMap fns (ObjFn fname es) =
    trStr ("New Objective function: " ++ fname ++ " " ++ (show names)) $
    (func, defaultWeight, names, nums) : fns
    where
        func = case M.lookup fname objFuncDict of
            Just f -> f
            Nothing -> error ("procObjFn: objective function '" ++ fname ++ "' not known")
        (names, nums) = partitionEithers $ map (procExpr varMap) es
procObjFn varMap fns (Avoid fname es) = fns -- TODO: `avoid` functions
procObjFn varMap fns _ = fns -- TODO: `avoid` functions

-- TODO: Have a more principled expr look up routine
lookupVarMap :: String -> VarMap -> String
lookupVarMap s varMap = case M.lookup s varMap of
    Just s' -> s'
    Nothing -> case M.lookup s computationDict of -- TODO remove this case
               Just f -> trace ("found function named: " ++ s) $ s
               Nothing -> s
               -- TODO: there is a possibility of accessing unselected Substance variables here. As written here, we are assuming all ids from Substance are accessible in Style GLOBALLY. Is this okay?
               -- error $ "lookupVarMap: incorrect variable mapping from " ++ s ++ " or no computation"

-- | Resolve a Style expression, which could be operations among expressions such as a chained dot-access for an attribute through a couple of layers of indirection

-- An expression can be either a string (variable name) or float (literal)? TODO revise
-- Example: context (VarMap) [X ~> A] would arise with Sub: "Set A", Sty: "Set X { ... }"
-- TODO: this function is not done/tested yet
-- TODO: handle properties (e.g. `X.yaxis:length`, `X.yaxis.label:width`)
-- TODO: replace underscores with spaces
-- TODO: generalize this function to return Exprs? (so objectives/constraints/computations can handle gets)
procExpr :: (Autofloat a) => VarMap -> Expr -> Either String a

-- in context [X ~> A], look up "X", return "A"
-- TODO: or a list of all shapes: "A xaxis", "A yaxis", ... (lookupAll does this for now)
procExpr ctx (Id subObjPattern) = Left $ lookupVarMap subObjPattern ctx

-- "ID.label" is a synonym for "ID.shape.label"
procExpr ctx r@(BinOp Access (Id subObjName) (Id "label")) = error ("cannot access label of non-shape:\n" ++ show r)

-- Shapes are given their unique names (for lookup) in Runtime (so far)
-- in context [X ~> A], look up "X.yaxis.label", return "A yaxis label"
procExpr ctx (BinOp Access (BinOp Access (Id subObjPattern) (Id styShapeName)) (Id "label")) = 
             let subObjName = lookupVarMap subObjPattern ctx in
             Left $ labelName $ uniqueShapeName subObjName styShapeName

-- in context [X ~> A], look up "X.yaxis", return "A yaxis"
procExpr ctx (BinOp Access (Id subObjPattern) (Id styShapeName)) = 
         let subObjName = lookupVarMap subObjPattern ctx in
         Left $ uniqueShapeName subObjName styShapeName

-- disallow deeper binops (e.g. "X.yaxis.zaxis")
procExpr ctx r@(BinOp Access (BinOp Access _ _) _) = error ("nested non-label accesses not allowed:\n" ++ show r)
procExpr ctx r@(BinOp Access _ _) = error ("incorrect binop access pattern:\n" ++ show r)

procExpr _ (IntLit i) = Right $ r2f i -- TODO this shouldn't flatten ints for computations
procExpr _ (FloatLit i) = Right $ r2f i
procExpr _ (StringLit s) = Left s -- TODO: distinguish strings from ids?

procExpr v e  = error ("expr: argument unsupported! v: " ++ show v ++ " | e: " ++ show e)
-- Unsupported: Cons, and Comp seems to be (hackily) handled in procAssign


-- temp. hack to convert procExpr output to computation input
backToExpr :: (Autofloat a) => Either String a -> Expr
backToExpr (Left s) = Id s -- TODO write new procExpr
backToExpr (Right x) = FloatLit $ r2f x

-- FIXME: this (?) is incorrect, we should resolve the variables earlier
addSpec :: VarMap -> Properties -> Stmt -> Properties
addSpec _ dict (Assign s e@(Cons NoShape _)) = M.insert s (Id "None") dict
addSpec _ dict (Assign s e@(Cons Auto _)) = M.insert s (Id "Auto") dict
-- FIXME: wrap fromleft inside a function!
addSpec varMap dict (Assign s e) =
        case e of
        CompArgs fname params -> let resolvedParams = map (backToExpr . procExpr varMap) params in
                                 M.insert s (CompArgs fname resolvedParams) dict
        StringLit p -> M.insert s (StringLit p) dict
        _ -> M.insert s (Id (fromLeft (error "Unexpected ID") $ procExpr varMap e)) dict
addSpec _ _ _ = error "addSpec: only support assignments in constructors!"

-- | Given a variable mapping and spec, if the statement is an assignment,
-- fold over the list of statements in the assignments (e.g. shapeName = ShapeType { key = val... } )
-- and add them to the configuration in the object's spec.
procAssign :: VarMap -> StySpec -> Stmt -> StySpec
procAssign varMap spec (Assign n (Cons typ stmts)) =
    spec { spShpMap = M.insert n (typ, configs) $ spShpMap spec }
    where
        configs :: M.Map String Expr
        configs = foldl (addSpec varMap) M.empty stmts
procAssign _ spec _ = spec -- TODO: ignoring assignment for all others; what kinds are invalid?

--------------------------------------------------------------------------------

-- | Generate a unique id for a Substance constraint
-- FIXME: make sure these names are unique and make sure users cannot start ids
-- with underscores

varListToString :: [C.Var] -> [String]
varListToString = map conv
    where conv (C.VarConst s)  = s 

varArgsToString :: [C.Arg] -> [String]
varArgsToString = map conv
    where conv c = case c of
            C.VarA (C.VarConst s) -> s
            _ -> ""

getConstrTuples :: [C.SubConstr] -> [(C.SubType, String, [String])]
getConstrTuples = map getType
    where getType (C.SubConstrConst (C.PredicateConst p) vs)  = ((C.TypeConst p), "_" ++ p ++ (intercalate "" (varListToString vs)), (varListToString vs))

getSubTuples :: [C.SubDecl] -> [(C.SubType, String, [String])]
getSubTuples = map getType
    where getType d = case d of
            C.SubDeclConst (C.ApplyT t xls) (C.VarConst v) -> (t, v , (v : (varArgsToString xls)))
            C.SubDeclConst (C.TypeT (C.TypeVarConst t)) (C.VarConst v)     -> ((C.TypeConst t), v , [v])




           

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
