{-# LANGUAGE BangPatterns #-}
-- | The GenOptProblem module performs several passes on the translation generated
-- by the Style compiler to generate the initial state (fields and GPIs) and optimization problem
-- (objectives, constraints, and computations) specified by the Substance/Style pair.

{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}
-- Mostly for autodiff

module GenOptProblem where

import Utils
import Shapes
import qualified Substance as C
import Env
import Style
import Functions
import Text.Show.Pretty (ppShow, pPrint)
import System.Random
import Debug.Trace
import qualified Data.Map.Strict as M
import Control.Monad (foldM, forM_)
import Data.List (foldl', minimumBy, intercalate, partition)
import Data.Array (assocs)
import Data.Either (partitionEithers)
import           System.Console.Pretty (Color (..), Style (..), bgColor, color, style, supportsPretty)
import qualified Data.Set as Set
import qualified Data.Graph as Graph
import GHC.Float (float2Double, double2Float)

-------------------- Type definitions

type StyleOptFn = (String, [Expr]) -- Objective or constraint

data OptType = Objfn | Constrfn
     deriving (Show, Eq)

data Fn = Fn { fname :: String,
               fargs :: [Expr],
               optType :: OptType }
     deriving (Show, Eq)

data FnDone a = FnDone { fname_d :: String,
                         fargs_d :: [ArgVal a],
                         optType_d :: OptType }
     deriving (Show, Eq)

-- A map from the varying path to its value, used to look up values in the translation
type VaryMap a = M.Map Path (TagExpr a)

------- State type definitions

-- Stores the last EP varying state (that is, the state when the unconstrained opt last converged)
type LastEPstate = [Float] -- Note: NOT polymorphic (due to system slowness with polymorphism)

data OptStatus = NewIter
               | UnconstrainedRunning LastEPstate
               | UnconstrainedConverged LastEPstate
               | EPConverged

instance Show OptStatus where
         show NewIter = "New iteration"
         show (UnconstrainedRunning lastEPstate) =
              "Unconstrained running" -- with last EP state:\n" ++ show lastEPstate
         show (UnconstrainedConverged lastEPstate) =
              "Unconstrained converged" -- with last EP state:\n" ++ show lastEPstate
         show EPConverged = "EP converged"

instance Eq OptStatus where
         x == y = case (x, y) of
                  (NewIter, NewIter) -> True
                  (EPConverged, EPConverged) -> True
                  (UnconstrainedRunning a, UnconstrainedRunning b) -> a == b
                  (UnconstrainedConverged a, UnconstrainedConverged b) -> a == b
                  (_, _) -> False

data Params = Params { weight :: Float,
                       optStatus :: OptStatus,
                       overallObjFn :: forall a . (Autofloat a) => StdGen -> a -> [a] -> a
                     }

instance Show Params where
         show p = "Weight: " ++ show (weight p) ++ " | Opt status: " ++ show (optStatus p)

data State = State { shapesr :: forall a . (Autofloat a) => [Shape a],
                     shapeNames :: [(String, Field)], -- TODO Sub name type
                     shapeOrdering :: [String],
                     shapeProperties :: [(String, Field, Property)],
                     transr :: forall a . (Autofloat a) => Translation a,
                     varyingPaths :: [Path],
                     uninitializedPaths :: [Path],
                     varyingState :: [Float], -- Note: NOT polymorphic
                     paramsr :: Params,
                     objFns :: [Fn],
                     constrFns :: [Fn],
                     rng :: StdGen,
                     autostep :: Bool }

instance Show State where
         show s = "Shapes: \n" ++ ppShow (shapesr s) ++
                  "\nShape names: \n" ++ ppShow (shapeNames s) ++
                  "\nTranslation: \n" ++ ppShow (transr s) ++
                  "\nVarying paths: \n" ++ ppShow (varyingPaths s) ++
                  "\nUninitialized paths: \n" ++ ppShow (uninitializedPaths s) ++
                  "\nVarying state: \n" ++ ppShow (varyingState s) ++
                  "\nParams: \n" ++ ppShow (paramsr s) ++
                  "\nObjective Functions: \n" ++ ppShowList (objFns s) ++
                  "\nConstraint Functions: \n" ++ ppShowList (constrFns s) ++
                  "\nAutostep: \n" ++ ppShow (autostep s)

-- Reimplementation of 'ppShowList' from pretty-show. Not sure why it cannot be imported at all
ppShowList = concatMap ((++) "\n" . ppShow)

--------------- Constants

-- For evaluating expressions
startingIteration, maxEvalIteration :: Int
startingIteration = 0
maxEvalIteration  = 500 -- Max iteration depth in case of cycles

evalIterRange :: (Int, Int)
evalIterRange = (startingIteration, maxEvalIteration)

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 17 -- deterministic RNG with seed

--------------- Parameters used in optimization
-- Should really be in Optimizer, but need to fix module import structure

constrWeight :: Floating a => a
constrWeight = 10 ^ 4

-- for use in barrier/penalty method (interior/exterior point method)
-- seems if the point starts in interior + weight starts v small and increases, then it converges
-- not quite: if the weight is too small then the constraint will be violated
initWeight :: Autofloat a => a
-- initWeight = 10 ** (-5)

-- Converges very fast w/ constraints removed (function-composition.sub)
-- initWeight = 0

-- Steps very slowly with a higher weight; does not seem to converge but looks visually OK (function-composition.sub)
-- initWeight = 1
initWeight = 10 ** (-3)

--------------- Utility functions

declaredVarying :: (Autofloat a) => TagExpr a -> Bool
declaredVarying (OptEval (AFloat Vary)) = True
declaredVarying _                       = False

sumMap :: Floating b => (a -> b) -> [a] -> b -- common pattern in objective functions
sumMap f l = sum $ map f l

-- TODO: figure out what to do with sty vars
mkPath :: [String] -> Path
mkPath [name, field]           = FieldPath (BSubVar (VarConst name)) field
mkPath [name, field, property] = PropertyPath (BSubVar (VarConst name)) field property

pathToList :: Path -> [String]
pathToList (FieldPath (BSubVar (VarConst name)) field)             = [name, field]
pathToList (PropertyPath (BSubVar (VarConst name)) field property) = [name, field, property]
pathToList _ = error "pathToList should not handle Sty vars"

isFieldPath :: Path -> Bool
isFieldPath (FieldPath _ _)      = True
isFieldPath (PropertyPath _ _ _) = False

bvarToString :: BindingForm -> String
bvarToString (BSubVar (VarConst s)) = s
bvarToString (BStyVar (StyVar' s)) = s -- For namespaces
             -- error ("bvarToString: cannot handle Style variable: " ++ show v)

getShapeName :: String -> Field -> String
getShapeName subName field = subName ++ "." ++ field

-- For varying values to be inserted into varyMap
floatToTagExpr :: (Autofloat a) => a -> TagExpr a
floatToTagExpr n = Done (FloatV n)

-- | converting from Value to TagExpr
toTagExpr :: (Autofloat a) => Value a -> TagExpr a
toTagExpr v = Done v

-- | converting from TagExpr to Value
toVal :: (Autofloat a) => TagExpr a -> Value a
toVal (Done v)    = v
toVal (OptEval _) = error "Shape properties were not fully evaluated"

toFn :: OptType -> StyleOptFn -> Fn
toFn otype (name, args) = Fn { fname = name, fargs = args, optType = otype }

toFns :: ([StyleOptFn], [StyleOptFn]) -> ([Fn], [Fn])
toFns (objfns, constrfns) = (map (toFn Objfn) objfns, map (toFn Constrfn) constrfns)

list2 (a, b) = [a, b]

mkVaryMap :: (Autofloat a) => [Path] -> [a] -> VaryMap a
mkVaryMap varyPaths varyVals = M.fromList $ zip varyPaths (map floatToTagExpr varyVals)

------------------- Translation helper functions

------ Generic functions for folding over a translation

foldFields :: (Autofloat a) => (String -> Field -> FieldExpr a -> [b] -> [b]) ->
                                       Name -> FieldDict a -> [b] -> [b]
foldFields f name fieldDict acc =
    let name' = nameStr name in -- TODO do we need do anything with Sub vs Gen names?
    let res = M.foldrWithKey (f name') [] fieldDict in
    res ++ acc

foldSubObjs :: (Autofloat a) => (String -> Field -> FieldExpr a -> [b] -> [b]) -> Translation a -> [b]
foldSubObjs f trans = M.foldrWithKey (foldFields f) [] (trMap trans)

------- Inserting into a translation

insertGPI :: (Autofloat a) =>
    Translation a -> String -> Field -> ShapeTypeStr -> PropertyDict a
    -> Translation a
insertGPI trans n field t propDict = case M.lookup (Sub n) $ trMap trans of
    Nothing        -> error "Substance ID does not exist"
    Just fieldDict ->
        let fieldDict' = M.insert field (FGPI t propDict) fieldDict
            trMap'     = M.insert (Sub n) fieldDict' $ trMap trans
        in trans { trMap = trMap' }

insertPath :: (Autofloat a) => Translation a -> (Path, TagExpr a) -> Either [Error] (Translation a)
insertPath trans (path, expr) =
         let overrideFlag = False in -- These paths should not exist in trans
         addPath overrideFlag trans path expr

insertPaths :: (Autofloat a) => [Path] -> [TagExpr a] -> Translation a -> Translation a
insertPaths varyingPaths varying trans =
         if length varying /= length varyingPaths
         then error "not the same # varying paths as varying variables"
         else case foldM insertPath trans (zip varyingPaths varying) of
              Left errs -> error $ "Error while adding varying paths: " ++ intercalate "\n" errs
              Right tr -> tr

------- Looking up fields/properties in a translation

-- First check if the path is a varying path. If so then use the varying value
-- (The value in the translation is stale and should be ignored)
-- If not then use the expr in the translation
lookupFieldWithVarying :: (Autofloat a) => BindingForm -> Field -> Translation a -> VaryMap a -> FieldExpr a
lookupFieldWithVarying bvar field trans varyMap =
    case M.lookup (mkPath [bvarToString bvar, field]) varyMap of
    Just varyVal -> {-trace "field lookup was vary" $ -} FExpr varyVal
    Nothing -> {-trace "field lookup was not vary" $ -} lookupField bvar field trans

lookupPropertyWithVarying :: (Autofloat a) => BindingForm -> Field -> Property
                                              -> Translation a -> VaryMap a -> TagExpr a
lookupPropertyWithVarying bvar field property trans varyMap =
    case M.lookup (mkPath [bvarToString bvar, field, property]) varyMap of
    Just varyVal -> {-trace "property lookup was vary" $ -} varyVal
    Nothing -> {- trace "property lookup was not vary" $ -} lookupProperty bvar field property trans

-- TODO move lookups to utils
lookupField :: (Autofloat a) => BindingForm -> Field -> Translation a -> FieldExpr a
lookupField bvar field trans =
    let name = trName bvar in
    let trn = trMap trans in
    case M.lookup name trn of
    Nothing -> error ("path '" ++ pathStr2 name field ++ "''s name doesn't exist in trans")
               -- TODO improve error messages and return error messages (Either [Error] (TagExpr a))
    Just fieldDict ->
         case M.lookup field fieldDict of
         Nothing -> error ("path '" ++ pathStr2 name field ++ "'s field doesn't exist in trans")
         Just fexpr -> fexpr

lookupProperty :: (Autofloat a) => BindingForm -> Field -> Property -> Translation a -> TagExpr a
lookupProperty bvar field property trans =
    let name = trName bvar in
    case lookupField bvar field trans of
    FExpr e ->
        -- to deal with path synonyms, e.g. `y.f = some GPI with property p; z.f = y.f; z.f.p = some value`
        -- if we're looking for `z.f.p` and we find out that `z.f = y.f`, then look for `y.f.p` instead
        -- NOTE: this makes a recursive call!
        case e of
        OptEval (EPath (FieldPath bvarSynonym fieldSynonym)) ->
                if bvar == bvarSynonym && field == fieldSynonym
                then error ("nontermination in lookupProperty with path '" ++ pathStr3 name field property ++ "' set to itself")
                else lookupProperty bvarSynonym fieldSynonym property trans
        -- the only thing that might have properties is another field path
        _ -> error ("path '" ++ pathStr3 name field property ++ "' has no properties")
    FGPI ctor properties ->
        case M.lookup property properties of
        Nothing -> error ("path '" ++ pathStr3 name field property ++ "'s property does not exist")
        Just texpr -> texpr

lookupPaths :: (Autofloat a) => [Path] -> Translation a -> [a]
lookupPaths paths trans = map lookupPath paths
    where
        lookupPath p@(FieldPath v field) = case lookupField v field trans of
            FExpr (OptEval (AFloat (Fix n))) -> r2f n
            FExpr (Done (FloatV n))          -> r2f n
            xs -> error ("varying path \"" ++ pathStr p ++ "\" is invalid: is '" ++ show xs ++ "'")
        lookupPath p@(PropertyPath v field pty) = case lookupProperty v field pty trans of
            OptEval (AFloat (Fix n)) -> r2f n
            Done (FloatV n) -> n
            xs -> error ("varying path \"" ++ pathStr p ++ "\" is invalid: is '" ++ show xs ++ "'")

-- TODO: resolve label logic here?
shapeExprsToVals :: (Autofloat a) => (String, Field) -> PropertyDict a -> Properties a
shapeExprsToVals (subName, field) properties =
          let shapeName   = getShapeName subName field
              properties' = M.map toVal properties
          in M.insert "name" (StrV shapeName) properties'

getShapes :: (Autofloat a) => [(String, Field)] -> Translation a -> [Shape a]
getShapes shapenames trans = map (getShape trans) shapenames
          -- TODO: fix use of Sub/Sty name here
          where getShape trans (name, field) =
                    let fexpr = lookupField (BSubVar $ VarConst name) field trans in
                    case fexpr of
                    FExpr _ -> error "expected GPI, got field"
                    FGPI ctor properties -> (ctor, shapeExprsToVals (name, field) properties)

----- GPI helper functions

shapes2vals :: (Autofloat a) => [Shape a] -> [Path] -> [Value a]
shapes2vals shapes paths = reverse $ foldl (lookupPath shapes) [] paths
    where
        lookupPath shapes acc (PropertyPath s field property) =
            let subID = bvarToString s
                shapeName = getShapeName subID field in
            get (findShape shapeName shapes) property : acc
        lookupPath _ acc (FieldPath _ _) = acc

-- Given a set of new shapes (from the frontend) and a varyMap (for varying field values):
-- look up property values in the shapes and field values in the varyMap
-- NOTE: varyState is constructed using a foldl, so to preserve its order, we must reverse the list of values!
shapes2floats :: (Autofloat a) => [Shape a] -> VaryMap a -> [Path] -> [a]
shapes2floats shapes varyMap varyingPaths = reverse $ foldl (lookupPathFloat shapes varyMap) [] varyingPaths
    where
        lookupPathFloat :: (Autofloat a) => [Shape a] -> VaryMap a -> [a] -> Path -> [a]
        lookupPathFloat shapes _ acc (PropertyPath s field property) =
            let subID = bvarToString s
                shapeName = getShapeName subID field in
            getNum (findShape shapeName shapes) property : acc
        lookupPathFloat _ varyMap acc fp@(FieldPath _ _) =
             case M.lookup fp varyMap of
             Just (Done (FloatV num)) -> num : acc
             Just _ -> error ("wrong type for varying field path (expected float): " ++ show fp)
             Nothing -> error ("could not find varying field path '" ++ show fp ++ "' in varyMap")

--------------------------------- Analyzing the translation

--- Find varying (float) paths

-- For now, don't optimize these float-valued properties of a GPI 
-- (use whatever they are initialized to in Shapes or set to in Style)
unoptimizedFloatProperties :: [String]
unoptimizedFloatProperties = ["rotation", "strokeWidth", "thickness"]

-- If any float property is not initialized in properties,
-- or it's in properties and declared varying, it's varying
findPropertyVarying :: (Autofloat a) => String -> Field -> M.Map String (TagExpr a) ->
                                                                 String -> [Path] -> [Path]
findPropertyVarying name field properties floatProperty acc =
    if floatProperty `elem` unoptimizedFloatProperties then acc else
    case M.lookup floatProperty properties of
    Nothing -> mkPath [name, field, floatProperty] : acc
    Just expr -> if declaredVarying expr then mkPath [name, field, floatProperty] : acc else acc

findFieldVarying :: (Autofloat a) => String -> Field -> FieldExpr a -> [Path] -> [Path]
findFieldVarying name field (FExpr expr) acc =
    if declaredVarying expr
    then mkPath [name, field] : acc -- TODO: deal with StyVars
    else acc
findFieldVarying name field (FGPI typ properties) acc =
    let ctorFloats    = propertiesOf FloatT typ
        varyingFloats = filter (not . isPending typ) ctorFloats
        vs = foldr (findPropertyVarying name field properties) [] varyingFloats
    in vs ++ acc

findVarying :: (Autofloat a) => Translation a -> [Path]
findVarying = foldSubObjs findFieldVarying

--- Find uninitialized (non-float) paths

findPropertyUninitialized :: (Autofloat a) => String -> Field -> M.Map String (TagExpr a) ->
                                                                       String -> [Path] -> [Path]
findPropertyUninitialized name field properties nonfloatProperty acc =
    case M.lookup nonfloatProperty properties of
    -- nonfloatProperty is a non-float property that is NOT set by the user and thus we can sample it
    Nothing   -> mkPath [name, field, nonfloatProperty] : acc
    Just expr -> acc

findFieldUninitialized :: (Autofloat a) => String -> Field -> FieldExpr a -> [Path] -> [Path]
-- NOTE: we don't find uninitialized field because you can't leave them uninitialized. Plus, we don't know what types they are
findFieldUninitialized name field (FExpr expr) acc = acc
findFieldUninitialized name field (FGPI typ properties) acc =
    let ctorNonfloats  = filter (/= "name") $ propertiesNotOf FloatT typ in
    -- TODO: add a separate field (e.g. pendingPaths) in State to store these special properties that needs frontend updates
    let uninitializedProps = pendingProperties typ ++ ctorNonfloats in
    let vs = foldr (findPropertyUninitialized name field properties) [] uninitializedProps in
    vs ++ acc

-- | Find the paths to all uninitialized, non-float, non-name properties
findUninitialized :: (Autofloat a) => Translation a -> [Path]
findUninitialized = foldSubObjs findFieldUninitialized

--- Find various kinds of functions

findObjfnsConstrs :: (Autofloat a) => Translation a -> [Either StyleOptFn StyleOptFn]
findObjfnsConstrs = foldSubObjs findFieldFns
    where findFieldFns :: (Autofloat a) => String -> Field -> FieldExpr a -> [Either StyleOptFn StyleOptFn]
                                        -> [Either StyleOptFn StyleOptFn]
          findFieldFns name field (FExpr (OptEval expr)) acc =
              case expr of
              ObjFn fname args -> Left (fname, args) : acc
              ConstrFn fname args -> Right (fname, args) : acc
              _ -> acc -- Not an optfn
          -- COMBAK: what should we do if there's a constant field?
          findFieldFns name field (FExpr (Done _)) acc = acc
          findFieldFns name field (FGPI _ _) acc = acc

findDefaultFns :: (Autofloat a) => Translation a -> [Either StyleOptFn StyleOptFn]
findDefaultFns = foldSubObjs findFieldDefaultFns
    where findFieldDefaultFns :: (Autofloat a) => String -> Field -> FieldExpr a ->
                                            [Either StyleOptFn StyleOptFn] -> [Either StyleOptFn StyleOptFn]
          findFieldDefaultFns name field gpi@(FGPI typ props) acc =
              let args    = [EPath $ FieldPath (BSubVar (VarConst name)) field]
                  objs    = map (Left . addArgs args) $ defaultObjFnsOf typ
                  constrs = map (Right . addArgs args) $ defaultConstrsOf typ
              in constrs ++ objs ++ acc
              where addArgs arguments f = (f, arguments)
          findFieldDefaultFns _ _ _ acc = acc

--- Find shapes and their properties

findShapeNames :: (Autofloat a) => Translation a -> [(String, Field)]
findShapeNames = foldSubObjs findGPIName
    where findGPIName :: (Autofloat a) => String -> Field -> FieldExpr a ->
                                          [(String, Field)] -> [(String, Field)]
          findGPIName name field (FGPI _ _) acc = (name, field) : acc
          findGPIName _ _ (FExpr _) acc = acc

findShapesProperties :: (Autofloat a) => Translation a -> [(String, Field, Property)]
findShapesProperties = foldSubObjs findShapeProperties
    where findShapeProperties :: (Autofloat a) => String -> Field -> FieldExpr a -> [(String, Field, Property)]
                                                  -> [(String, Field, Property)]
          findShapeProperties name field (FGPI ctor properties) acc =
               let paths = map (\property -> (name, field, property)) (M.keys properties)
               in paths ++ acc
          findShapeProperties _ _ (FExpr _) acc = acc

------------------------------ Evaluating the translation and expressions/GPIs in it

-- TODO: write a more general typechecking mechanism
evalUop :: (Autofloat a) => UnaryOp -> ArgVal a -> Value a
evalUop UMinus v = case v of
                  Val (FloatV a) -> FloatV (-a)
                  Val (IntV i) -> IntV (-i)
                  GPI _ -> error "cannot negate a GPI"
                  Val _ -> error "wrong type to negate"
evalUop UPlus v = error "unary + doesn't make sense" -- TODO remove from parser

evalBinop :: (Autofloat a) => BinaryOp -> ArgVal a -> ArgVal a -> Value a
evalBinop op v1 v2 =
        case (v1, v2) of
        (Val (FloatV n1), Val (FloatV n2)) ->
                  case op of
                  BPlus -> FloatV $ n1 + n2
                  BMinus -> FloatV $ n1 - n2
                  Multiply -> FloatV $ n1 * n2
                  Divide -> if n2 == 0 then error "divide by 0!" else FloatV $ n1 / n2
                  Exp -> FloatV $ n1 ** n2
        (Val (IntV n1), Val (IntV n2)) ->
                  case op of
                  BPlus -> IntV $ n1 + n2
                  BMinus -> IntV $ n1 - n2
                  Multiply -> IntV $ n1 * n2
                  Divide -> if n2 == 0 then error "divide by 0!" else IntV $ n1 `quot` n2 -- NOTE: not float
                  Exp -> IntV $ n1 ^ n2
        -- Cannot mix int and float
        (Val _, Val _) -> error ("wrong field types for binary op: " ++ show v1 ++ show op ++ show v2)
        (GPI _, Val _) -> error "binop cannot operate on GPI"
        (Val _, GPI _) -> error "binop cannot operate on GPI"
        (GPI _, GPI _) -> error "binop cannot operate on GPIs"

evalProperty :: (Autofloat a)
    => (Int, Int) -> BindingForm -> Field -> VaryMap a -> ([(Property, TagExpr a)], Translation a, StdGen) -> (Property, TagExpr a)
    -> ([(Property, TagExpr a)], Translation a, StdGen)
evalProperty (i, n) bvar field varyMap (propertiesList, trans, g) (property, expr) =
    let path = EPath $ PropertyPath bvar field property in -- factor out?
    let (res, trans', g') = evalExpr (i, n) path trans varyMap g in
    -- This check might be redundant with the later GPI conversion in evalExpr, TODO factor out
    case res of
        Val val -> {-trace ("Evaled property " ++ show path)-} ((property, Done val) : propertiesList, trans', g')
        GPI _ -> error "GPI property should not evaluate to GPI argument" -- TODO: true later? references?

evalGPI_withUpdate :: (Autofloat a)
    => (Int, Int) -> BindingForm -> Field -> (GPICtor, PropertyDict a) -> Translation a -> VaryMap a -> StdGen
    -> ((GPICtor, PropertyDict a), Translation a, StdGen)
evalGPI_withUpdate (i, n) bvar field (ctor, properties) trans varyMap g =
        -- Fold over the properties, evaluating each path, which will update the translation each time,
        -- and accumulate the new property-value list (WITH varying looked up)
        let (propertyList', trans', g') = foldl (evalProperty (i, n) bvar field varyMap) ([], trans, g) (M.toList properties) in
        let properties' = M.fromList propertyList' in
        {-trace ("Start eval GPI: " ++ show properties ++ " " ++ "\n\tctor: " ++ "\n\tfield: " ++ show field)-}
        ((ctor, properties'), trans', g')

-- recursively evaluate, tracking iteration depth in case there are cycles in graph
evalExpr :: (Autofloat a) => (Int, Int) -> Expr -> Translation a -> VaryMap a -> StdGen -> (ArgVal a, Translation a, StdGen)
evalExpr (i, n) arg trans varyMap g =
    if i >= n then error ("evalExpr: iteration depth exceeded (" ++ show n ++ ")")
        else {- trace ("Evaluating expression: " ++ show arg ++ "\n(i, n): " ++ show i ++ ", " ++ show n) -}
                   argResult
    where limit = (i + 1, n)
          argResult = case arg of
            -- Already done values; don't change trans
            IntLit i -> (Val $ IntV i, trans, g)
            StringLit s -> (Val $ StrV s, trans, g)
            BoolLit b -> (Val $ BoolV b, trans, g)
            AFloat (Fix f) -> (Val $ FloatV (r2f f), trans, g) -- TODO: note use of r2f here. is that ok?
            AFloat Vary -> error "evalExpr should not encounter an uninitialized varying float!"

            -- Inline computation, needs a recursive lookup that may change trans, but not a path
            -- TODO factor out eval / trans computation?
            UOp op e ->
                let (val, trans', g') = evalExpr limit e trans varyMap g in
                let compVal = evalUop op val in
                (Val compVal, trans', g')
            BinOp op e1 e2 ->
                let ([v1, v2], trans', g') = evalExprs limit [e1, e2] trans varyMap g in
                let compVal = evalBinop op v1 v2 in
                (Val compVal, trans', g')
            CompApp fname args ->
                -- NOTE: the goal of all the rng passing in this module is for invoking computations with randomization
                let (vs, trans', g') = evalExprs limit args trans varyMap g
                    (compRes, g'')   = invokeComp fname vs compSignatures g'
                in (compRes, trans', g'')
                -- -- TODO: invokeComp should be used here
                -- case M.lookup fname compDict of
                -- Nothing -> error ("computation '" ++ fname ++ "' doesn't exist")
                -- Just f -> let res = f vs in
                --           (res, trans')
            List es -> error "TODO lists"
                -- let (vs, trans') = evalExprs es trans in
                -- (vs, trans')
            ListAccess p i -> error "TODO lists"

            -- Needs a recursive lookup that may change trans. The path case is where trans is actually changed.
            EPath p ->
                  case p of
                  FieldPath bvar field ->
                     -- Lookup field expr, evaluate it if necessary, cache the evaluated value in the trans,
                     -- return the evaluated value and the updated trans
                     let fexpr = lookupFieldWithVarying bvar field trans varyMap in
                     case fexpr of
                     FExpr (Done v) -> (Val v, trans, g)
                     FExpr (OptEval e) ->
                         let (v, trans', g') = evalExpr limit e trans varyMap g in
                         case v of
                             Val fval ->
                                 case insertPath trans' (p, Done fval) of
                                 Right trans' -> (v, trans', g')
                                 Left err -> error $ concat err
                             gpiVal@(GPI _) -> (gpiVal, trans', g') -- to deal with path synonyms, e.g. "y.f = some GPI; z.f = y.f"
                     FGPI ctor properties ->
                     -- Eval each property in the GPI, storing each property result in a new dictionary
                     -- No need to update the translation because each path should update the translation
                         let (gpiVal@(ctor', propertiesVal), trans', g') =
                                 evalGPI_withUpdate limit bvar field (ctor, properties) trans varyMap g in
                         (GPI (ctor', shapeExprsToVals (bvarToString bvar, field) propertiesVal), trans', g')

                  PropertyPath bvar field property ->
                      let texpr = lookupPropertyWithVarying bvar field property trans varyMap in
                      case texpr of
                      Done v -> (Val v, trans, g)
                      OptEval e ->
                         let (v, trans', g') = evalExpr limit e trans varyMap g in
                         case v of
                         Val fval ->
                             case insertPath trans' (p, Done fval) of
                             Right trans' -> (v, trans', g')
                             Left err -> error $ concat err
                         GPI _ -> error ("path to property expr '" ++ pathStr p ++ "' evaluated to a GPI")

            -- GPI argument
            Ctor ctor properties -> error "no anonymous/inline GPIs allowed as expressions!"

            -- Error
            Layering _ _ -> error "layering should not be an objfn arg (or in the children of one)"
            ObjFn _ _ -> error "objfn should not be an objfn arg (or in the children of one)"
            ConstrFn _ _ -> error "constrfn should not be an objfn arg (or in the children of one)"
            AvoidFn _ _ -> error "avoidfn should not be an objfn arg (or in the children of one)"
            -- xs -> error ("unmatched case in evalExpr with argument: " ++ show xs)

-- Any evaluated exprs are cached in the translation for future evaluation
-- The varyMap is not changed because its values are final (set by the optimization)
evalExprs :: (Autofloat a)
    => (Int, Int) -> [Expr] -> Translation a -> VaryMap a -> StdGen
    -> ([ArgVal a], Translation a, StdGen)
evalExprs limit args trans varyMap g =
    foldl (evalExprF limit varyMap) ([], trans, g) args
    where evalExprF :: (Autofloat a) => (Int, Int) -> VaryMap a -> ([ArgVal a], Translation a, StdGen) -> Expr -> ([ArgVal a], Translation a, StdGen)
          evalExprF limit varyMap (argvals, trans, rng) arg =
                       let (argVal, trans', rng') = evalExpr limit arg trans varyMap rng in
                       (argvals ++ [argVal], trans', rng') -- So returned exprs are in same order

------------------- Generating and evaluating the objective function

evalFnArgs :: (Autofloat a) => (Int, Int) -> VaryMap a -> ([FnDone a], Translation a, StdGen) -> Fn -> ([FnDone a], Translation a, StdGen)
evalFnArgs limit varyMap (fnDones, trans, g) fn =
    let args = fargs fn in
    let (argsVal, trans', g') = evalExprs limit (fargs fn) trans varyMap g in
    let fn' = FnDone { fname_d = fname fn, fargs_d = argsVal, optType_d = optType fn } in
    (fnDones ++ [fn'], trans', g') -- TODO factor out this pattern

evalFns :: (Autofloat a)
    => (Int, Int) -> [Fn] -> Translation a -> VaryMap a -> StdGen
    -> ([FnDone a], Translation a, StdGen)
evalFns limit fns trans varyMap g = foldl (evalFnArgs limit varyMap) ([], trans, g) fns

applyOptFn :: (Autofloat a) =>
    M.Map String (OptFn a) -> OptSignatures -> FnDone a -> a
applyOptFn dict sigs finfo =
    let (name, args) = (fname_d finfo, fargs_d finfo)
    in invokeOptFn dict name args sigs

applyCombined :: (Autofloat a) => a -> [FnDone a] -> a
applyCombined penaltyWeight fns =
        let (objfns, constrfns) = partition (\f -> optType_d f == Objfn) fns in
        sumMap (applyOptFn objFuncDict objSignatures) objfns
               + constrWeight * penaltyWeight * sumMap (applyOptFn constrFuncDict constrSignatures) constrfns

-- Main function: generates the objective function, partially applying it with some info

genObjfn :: (Autofloat a)
    => Translation a -> [Fn] -> [Fn] -> [Path]
    -> StdGen -> a -> [a]
    -> a
genObjfn trans objfns constrfns varyingPaths =
     \rng penaltyWeight varyingVals ->
         let varyMap = tr "varyingMap: " $ mkVaryMap varyingPaths varyingVals in
         let (fnsE, transE, rng') = evalFns evalIterRange (objfns ++ constrfns) trans varyMap rng in
         let overallEnergy = applyCombined penaltyWeight (tr "Completed evaluating function arguments" fnsE) in
         tr "Completed applying optimization function" overallEnergy

--------------- Generating an initial state (concrete values for all fields/properties needed to draw the GPIs)
-- 1. Initialize all varying fields
-- 2. Initialize all properties of all GPIs

-- NOTE: since we store all varying paths separately, it is okay to mark the default values as Done -- they will still be optimized, if needed.
-- TODO: document the logic here (e.g. only sampling varying floats) and think about whether to use translation here or [Shape a] since we will expose the sampler to users later
initProperty :: (Autofloat a) => (PropertyDict a, StdGen) -> String ->
                                               (ValueType, SampledValue a) -> (PropertyDict a, StdGen)
initProperty (properties, g) pID (typ, sampleF) =
    let (v, g')    = sampleF g
        autoRndVal = Done v in
    case M.lookup pID properties of
      Just (OptEval (AFloat Vary)) -> (M.insert pID autoRndVal properties, g')
      Just (OptEval e) -> (properties, g)
      Just (Done v)    -> (properties, g)
      Nothing          -> (M.insert pID autoRndVal properties, g')

initShape :: (Autofloat a) => (Translation a, StdGen) -> (String, Field) -> (Translation a, StdGen)
initShape (trans, g) (n, field) =
    case lookupField (BSubVar (VarConst n)) field trans of
        FGPI shapeType propDict ->
            let def = findDef shapeType
                (propDict', g') = foldlPropertyMappings initProperty (propDict, g) def
                -- NOTE: getShapes resolves the names + we don't use the names of the shapes in the translation
                -- The name-adding logic can be removed but is left in for debugging
                shapeName = getShapeName n field
                propDict'' =  M.insert "name" (Done $ StrV shapeName) propDict'
            in (insertGPI trans n field shapeType propDict'', g')
        _   -> error "expected GPI but got field"

initShapes :: (Autofloat a) =>
    Translation a -> [(String, Field)] -> StdGen -> (Translation a, StdGen)
initShapes trans shapePaths gen = foldl initShape (trans, gen) shapePaths

resampleFields :: (Autofloat a) => [Path] -> StdGen -> ([a], StdGen)
resampleFields varyingPaths g =
    let varyingFields = filter isFieldPath varyingPaths in
    Functions.randomsIn g (fromIntegral $ length varyingFields) Shapes.canvasDims

-- sample varying fields only (from the range defined by canvas dims) and store them in the translation
-- example: A.val = OPTIMIZED
initFields :: (Autofloat a) => [Path] -> Translation a -> StdGen -> (Translation a, StdGen)
initFields varyingPaths trans g =
    let varyingFields = filter isFieldPath varyingPaths
        (sampledVals, g') = Functions.randomsIn g (fromIntegral $ length varyingFields) Shapes.canvasDims
        trans' = insertPaths varyingFields (map (Done . FloatV) sampledVals) trans in
    (trans', g')

------------- Evaluating all shapes in a translation

evalShape :: (Autofloat a) =>
    (Int, Int) -> VaryMap a
    -> ([Shape a], Translation a, StdGen) -> Path
    -> ([Shape a], Translation a, StdGen)
evalShape limit varyMap (shapes, trans, g) shapePath =
    let (res, trans', g') = evalExpr limit (EPath shapePath) trans varyMap g in
    case res of
    GPI shape -> (shape : shapes, trans', g')
    _ -> error "evaluating a GPI path did not result in a GPI"

-- recursively evaluate every shape property in the translation
evalShapes :: (Autofloat a) => (Int, Int) -> [Path] -> Translation a -> VaryMap a -> StdGen -> ([Shape a], Translation a, StdGen)
evalShapes limit shapeNames trans varyMap rng =
           let (shapes, trans', rng') = foldl (evalShape limit varyMap) ([], trans, rng) shapeNames in
           (reverse shapes, trans', rng')

-- Given the shape names, use the translation and the varying paths/values in order to evaluate each shape
-- with respect to the varying values
evalTranslation :: (Autofloat a) => State -> ([Shape a], Translation a, StdGen)
evalTranslation s =
    let varyMap = mkVaryMap (varyingPaths s) (map r2f $ varyingState s) in
    evalShapes evalIterRange (map (mkPath . list2) $ shapeNames s) (transr s) varyMap (rng s)

------------- Compute global layering of GPIs

lookupGPIName :: (Autofloat a) => Path -> Translation a -> String
lookupGPIName path@(FieldPath v field) trans =
    case lookupField v field trans of
        FExpr e  -> 
           -- to deal with path synonyms in a layering statement (see `lookupProperty` for more explanation)
           case e of
               OptEval (EPath pathSynonym@(FieldPath vSynonym fieldSynonym)) ->
                   if v == vSynonym && field == fieldSynonym
                   then error ("nontermination in lookupGPIName w/ path '" ++ show path ++ "' set to itself")
                   else lookupGPIName pathSynonym trans
               _ -> notGPIError
        FGPI _ _ -> getShapeName (bvarToString v) field

lookupGPIName _ _ = notGPIError
notGPIError = error "Layering expressions can only operate on GPIs."

-- | Walk the translation to find all layering statements.
findLayeringExprs :: (Autofloat a) => Translation a -> [Expr]
findLayeringExprs t = foldSubObjs findLayeringExpr t
  where findLayeringExpr :: (Autofloat a) => String -> Field -> FieldExpr a -> [Expr] -> [Expr]
        findLayeringExpr name field fexpr acc =
          case fexpr of
          FExpr (OptEval x@(Layering _ _)) -> x : acc
          _ -> acc

-- | Calculates all the nodes that are part of cycles in a graph.
cyclicNodes :: Graph.Graph -> [Graph.Vertex]
cyclicNodes graph =
  map fst . filter isCyclicAssoc . assocs $ graph
  where
    isCyclicAssoc = uncurry $ reachableFromAny graph

-- | In the specified graph, can the specified node be reached, starting out
-- from any of the specified vertices?
reachableFromAny :: Graph.Graph -> Graph.Vertex -> [Graph.Vertex] -> Bool
reachableFromAny graph node =
  elem node . concatMap (Graph.reachable graph)

-- | 'computeLayering' takes in a list of all GPI names and a list of directed edges [(a -> b)] representing partial layering orders as input and outputs a linear layering order of GPIs
topSortLayering :: [String] -> [(String, String)] -> [String]
topSortLayering names partialOrderings =
    let orderedNodes = nodesFromEdges partialOrderings
        freeNodes = Set.difference (Set.fromList names) orderedNodes
        edges = map (\(x, y) -> (x, x, y)) $ adjList partialOrderings
                    ++ (map (\x -> (x, [])) $ Set.toList freeNodes)
        (graph, nodeFromVertex, vertexFromKey) = Graph.graphFromEdges edges
        cyclic = not . null $ cyclicNodes graph
    in if cyclic then error "The graph is cyclic!" else map (getNodePart . nodeFromVertex) $ Graph.topSort graph
    where
        getNodePart (n, _, _) = n

nodesFromEdges edges = Set.fromList $ concatMap (\(a, b) -> [a, b]) edges

adjList :: [(String, String)] -> [(String, [String])]
adjList edges =
    let nodes = Set.toList $ nodesFromEdges edges
    in map (\x -> (x, findNeighbors x)) nodes
    where findNeighbors node = map snd $ filter ((==) node . fst) edges

computeLayering :: (Autofloat a) => Translation a -> [String]
computeLayering trans =
    let layeringExprs = findLayeringExprs trans
        partialOrderings = map findNames layeringExprs
        gpiNames  = map (uncurry getShapeName) $ findShapeNames trans
    in topSortLayering gpiNames partialOrderings
    where
        unused = -1
        substitute res (block, substs) =
            let block'  = (block, unused)
                substs' = map (\s -> (s, unused)) substs
            in res ++ map (`substituteBlock` block') substs'
        findNames (Layering path1 path2) = (lookupGPIName path1 trans, lookupGPIName path2 trans)

------------- Main function: what the Style compiler generates

genOptProblemAndState :: (forall a. (Autofloat a) => Translation a) -> State
genOptProblemAndState trans =
    -- Save information about the translation
    let !varyingPaths       = findVarying trans in
    -- NOTE: the properties in uninitializedPaths are NOT floats. Floats are included in varyingPaths already
    let uninitializedPaths = findUninitialized trans in
    let shapeNames         = findShapeNames trans in

    -- sample varying fields
    let (transInitFields, g') = initFields varyingPaths trans initRng in

    -- sample varying vals and instantiate all the non-float base properties of every GPI in the translation
    let (!transInit, g'') = initShapes transInitFields shapeNames g' in
    let shapeProperties  = transInit `seq` findShapesProperties transInit in

    let (objfns, constrfns) = (toFns . partitionEithers . findObjfnsConstrs) transInit in
    let (defaultObjFns, defaultConstrs) = (toFns . partitionEithers . findDefaultFns) transInit in
    let (!objFnsWithDefaults, !constrsWithDefaults) = (objfns ++ defaultObjFns, constrfns ++ defaultConstrs) in
    let overallFn = genObjfn transInit objFnsWithDefaults constrsWithDefaults varyingPaths in
    -- NOTE: this does NOT use transEvaled because it needs to be re-evaled at each opt step
    -- the varying values are re-inserted at each opt step

    -- Evaluate all expressions once to get the initial shapes
    let initVaryingMap = M.empty in -- No optimization has happened. Sampled varying vals are in transInit
    let (initialGPIs, transEvaled, _) = evalShapes evalIterRange (map (mkPath . list2) shapeNames) transInit initVaryingMap g'' in -- intentially discarding the new random feed, since we want the computation result to be consistent within one optimization session
    let initState = lookupPaths varyingPaths transEvaled in

    if null initState then error "empty state in genopt" else

    -- This is the final Style compiler output
    let initFullState = trace "genOptProblem init state: " $
                        State { shapesr = initialGPIs,
                                 shapeNames = shapeNames,
                                 shapeProperties = shapeProperties,
                                 shapeOrdering = [], -- NOTE: to be populated later
                                 transr = transInit, -- note: NOT transEvaled
                                 varyingPaths = varyingPaths,
                                 uninitializedPaths = uninitializedPaths,
                                 varyingState = initState,
                                 objFns = objFnsWithDefaults,
                                 constrFns = constrsWithDefaults,
                                 paramsr = Params { weight = initWeight,
                                                    optStatus = NewIter,
                                                    overallObjFn = overallFn },
                                 rng = g'',
                                 autostep = False -- default
                               } in

    initFullState
    -- NOTE: we do not resample the very first initial state. Not sure why the shapes / labels are rendered incorrectly.
    -- resampleBest numStateSamples initFullState

-- | 'compileStyle' runs the main Style compiler on the AST of Style and output from the Substance compiler and outputs the initial state for the optimization problem. This function is a top-level function used by "Server" and "ShadowMain"
-- NOTE: this function also print information out to stdout
-- TODO: enable logger
compileStyle :: StyProg -> C.SubOut -> IO State
compileStyle styProg (C.SubOut subProg (subEnv, eqEnv) labelMap) = do
   putStrLn "Running Style semantics\n"
   let selEnvs = checkSels subEnv styProg

   putStrLn "Selector static semantics and local envs:\n"
   forM_ selEnvs pPrint
   divLine

   let subss = find_substs_prog subEnv eqEnv subProg styProg selEnvs
   putStrLn "Selector matches:\n"
   forM_ subss pPrint
   divLine

   let !trans = translateStyProg subEnv eqEnv subProg styProg labelMap
                       :: Either [Error] (Translation Float)
                       -- NOT :: forall a . (Autofloat a) => Either [Error] (Translation a)
                       -- We intentionally specialize/monomorphize the translation to Float so it can be fully evaluated
                       -- and is not trapped under the lambda of the typeclass (Autofloat a) => ...
                       -- This greatly improves the performance of the system. See #166 for more details.
   let transAuto = castTranslation $ fromRight trans
                       :: forall a . (Autofloat a) => Translation a
   putStrLn "Translated Style program:\n"
   pPrint trans
   divLine

   let initState = genOptProblemAndState transAuto
   putStrLn "Generated initial state:\n"
   print initState
   divLine

   -- global layering order computation
   let gpiOrdering = computeLayering transAuto
   putStrLn "Generated GPI global layering:\n"
   print gpiOrdering
   divLine

   let initState' = initState { shapeOrdering = gpiOrdering }

   putStrLn (bgColor Cyan $ style Italic "   Style program warnings   ")
   let warns = warnings transAuto
   putStrLn (color Red $ intercalate "\n" warns ++ "\n")
   return initState'

-- | After monomorphizing the translation's type (to make sure it's computed), we generalize the type again, which means
-- | it's again under a typeclass lambda. (#166)
castTranslation :: Translation Float -> (forall a . Autofloat a => Translation a)
castTranslation t =
      let res = M.map castFieldDict (trMap t) in
      t { trMap = res }
      where
        castFieldDict :: FieldDict Float -> (forall a . Autofloat a => FieldDict a)
        castFieldDict dict = M.map castFieldExpr dict

        castFieldExpr :: FieldExpr Float -> (forall a . (Autofloat a) => FieldExpr a)
        castFieldExpr e =
          case e of
             FExpr te -> FExpr $ castTagExpr te
             FGPI n props -> FGPI n $ M.map castTagExpr props

        castTagExpr :: TagExpr Float -> (forall a . Autofloat a => TagExpr a)
        castTagExpr e =
           case e of
             Done v ->
                let res = case v of
                          FloatV x -> FloatV (r2f x)
                          PtV (x, y) -> PtV (r2f x, r2f y)
                          PtListV pts -> PtListV $ map (app2 r2f) pts
                          PathDataV d -> PathDataV $ map castPath d
                          -- More boilerplate not involving floats
                          IntV x -> IntV x
                          BoolV x -> BoolV x
                          StrV x -> StrV x
                          FileV x -> FileV x
                          StyleV x -> StyleV x
                in Done res
             OptEval e -> OptEval e -- Expr only contains floats

        castPath :: Path' Float -> (forall a . Autofloat a => Path' a)
        castPath p = case p of
                     Closed elems -> Closed $ map castElem elems
                     Open elems -> Open $ map castElem elems

        castElem :: Elem Float -> (forall a . Autofloat a => Elem a)
        castElem e = case e of
                     Pt pt -> Pt $ app2 r2f pt
                     CubicBez pts -> CubicBez $ app3 (app2 r2f) pts
                     CubicBezJoin pts -> CubicBezJoin $ app2 (app2 r2f) pts
                     QuadBez pts -> QuadBez $ app2 (app2 r2f) pts
                     QuadBezJoin pt -> QuadBezJoin $ app2 r2f pt

-------------------------------
-- Sampling code
-- TODO: should this code go in the optimizer?

numStateSamples :: Int
numStateSamples = 1000

-- | Resample the varying state.
-- | We are intentionally using a monomorphic type (float) and NOT using the translation, to avoid slowness.
resampleVState :: [Path] -> [Shape Double] -> StdGen -> (([Shape Double], [Double], [Double]), StdGen)
resampleVState varyPaths shapes g =
    let (resampledShapes, rng') = sampleShapes g shapes
        (resampledFields, rng'') = resampleFields varyPaths rng'
        -- make varying map using the newly sampled fields (we do not need to insert the shape paths)
        varyMapNew = mkVaryMap (filter isFieldPath $ varyPaths) resampledFields
        varyingState = shapes2floats resampledShapes varyMapNew $ varyPaths
    in ((resampledShapes, varyingState, resampledFields), rng'')

-- | Update the translation to get the full state.
updateVState :: State -> (([Shape Double], [Double], [Double]), StdGen) -> State
updateVState s ((resampledShapes, varyingState', fields'), g) =
    let polyShapes = toPolymorphics resampledShapes
        uninitVals = map toTagExpr $ shapes2vals polyShapes $ uninitializedPaths s
        trans' = insertPaths (uninitializedPaths s) uninitVals (transr s)
                    -- TODO: shapes', rng' = sampleConstrainedState (rng s) (shapesr s) (constrs s)
        varyMapNew = mkVaryMap (filter isFieldPath $ varyingPaths s) fields'
    in s { shapesr = polyShapes,
           rng = g,
           transr = trans' { warnings = [] }, -- Clear the warnings, since they aren't relevant anymore
           varyingState = map r2f varyingState',
           paramsr = (paramsr s) { weight = initWeight, optStatus = NewIter } }
    -- NOTE: for now we do not update the new state with the new rng from eval.
    -- The results still look different because resampling updated the rng.
    -- Therefore, we do not have to update rng here.

-- | Iterate a function that uses a generator, generating an infinite list of results with their corresponding updated generators.
iterateS :: (a -> (b, a)) -> a -> [(b, a)]
iterateS f g = let (res, g') = f g in
               (res, g') : iterateS f g'

-- | Compare two states and return the one with less energy.
lessEnergyOn :: ([Double] -> Double) -> (([Shape Double], [Double], [Double]), StdGen)
                             -> (([Shape Double], [Double], [Double]), StdGen) -> Ordering
lessEnergyOn f ((_, vs1, _), _) ((_, vs2, _), _) = compare (f vs1) (f vs2)

-- | Resample the varying state some number of times (sampling each new state from the original state, but with an updated rng).
-- | Pick the one with the lowest energy and update the original state with the lowest-energy-state's info.
resampleBest :: Int -> State -> State
resampleBest n s =
          if n < 2 then error "Need to sample at least two states" else
          let optInfo = paramsr s
              -- Take out the relevant information for resampling
              f       = (overallObjFn optInfo) (rng s) (float2Double $ weight optInfo)
              (varyPaths, shapes, g) = (varyingPaths s, shapesr s, rng s)
              -- Partially apply resampleVState with the params that don't change over a resampling
              resampleVStateConst = resampleVState varyPaths shapes
              sampledResults = take n $ iterateS resampleVStateConst g
              res = minimumBy (lessEnergyOn f) sampledResults
              {- (trace ("energies: " ++ (show $ map (\((_, x, _), _) -> f x) sampledResults)) -}
          in updateVState s res

------- Other possibly-useful utility functions (not currently used)

-- | Evaluate the objective function on the varying state (with the penalty weight, which should be the same between state).
evalFnOn :: State -> Double
evalFnOn s = let optInfo = paramsr s
                 f       = (overallObjFn optInfo) (rng s) (float2Double $ weight optInfo)
                 args    = map float2Double $ varyingState s
             in f args

-- | Compare two states and return the one with less energy.
lessEnergy :: State -> State -> Ordering
lessEnergy s1 s2 = compare (evalFnOn s1) (evalFnOn s2)
