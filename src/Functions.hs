-- {-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module Functions where

import Utils
import System.Random
import Debug.Trace
import Shapes
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import           Data.List                          (nub, sort, findIndex, find)
import           System.Random.Shuffle
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM

-- genShapeType $ shapeTypes shapeDefs
-- deriving instance Show ShapeType

debugOpt = True

trOpt :: Show a => String -> a -> a
trOpt s x = if debugOpt then trace "---" $ trace s $ traceShowId x else x

--------------------------------------------------------------------------------
-- Types

-- | possible values in the argument of computation, constraint, or objectives
data ArgVal a = GPI (Shape a) | Val (Value a)
     deriving (Eq, Show)

 -- | possible types in the argument of computation, constraint, or objectives.
 -- Used for type checking functions
data ArgType
    = GPIType ShapeTypeStr
    | ValueT ValueType
    | OneOf [ShapeTypeStr]
    | AnyGPI
    deriving (Eq, Show)

type FuncName  = String
type OptSignatures  = MM.MultiMap String [ArgType]
-- TODO: should computations be overloaded?
type CompSignatures = M.Map String ([ArgType], ArgType)

type OptFn      a = [ArgVal a] -> a
type ObjFnOn    a = [ArgVal a] -> a
type ConstrFnOn a = [ArgVal a] -> a
type CompFnOn   a = [ArgVal a] -> StdGen -> (ArgVal a, StdGen)
type ObjFn    = forall a. (Autofloat a) => [ArgVal a] -> a
type ConstrFn = forall a. (Autofloat a) => [ArgVal a] -> a
type CompFn   = forall a. (Autofloat a) => [ArgVal a] -> StdGen -> (ArgVal a, StdGen)

-- | computations that do not use randomization
type ConstCompFn = forall a. (Autofloat a) => [ArgVal a] -> ArgVal a

-- TODO: are the Info types still needed?
type Weight       a = a
type ObjFnInfo    a = (ObjFnOn    a, Weight a, [Value a])
type ConstrFnInfo a = (ConstrFnOn a, Weight a, [Value a])
data FnInfo a = ObjFnInfo a | ConstrFnInfo a

-- TODO: the functions can just be looked up and checked once, don't need to repeat
invokeOptFn :: (Autofloat a) =>
    M.Map String (OptFn a) -> FuncName -> [ArgVal a] -> OptSignatures -> a
invokeOptFn dict n args signatures =
    let sigs = case signatures MM.! n of
                   [] -> noSignatureError n
                   l  -> l
        args'  = checkArgsOverload args sigs n
        f      = fromMaybe (noFunctionError n) (M.lookup n dict)
    in f args
    -- in f args'

-- For a very limited form of supertyping...
linelike :: Autofloat a => Shape a -> Bool
linelike shape = fst shape == "Line" || fst shape == "Arrow"

--------------------------------------------------------------------------------
-- Computations
-- compDict :: forall a. (Autofloat a) => M.Map String (CompFnOn a)
compDict :: (Autofloat a) => M.Map String (CompFnOn a)
compDict = M.fromList
    [
        ("rgba", constComp rgba),
        ("atan", constComp arctangent),
        ("calcVectorsAngle", constComp calcVectorsAngle),
        ("calcVectorsAngleWithOrigin", constComp calcVectorsAngleWithOrigin),
        ("generateRandomReal", constComp generateRandomReal),
        ("calcNorm", constComp calcNorm),
        ("bboxWidth", constComp bboxWidth),
        ("bboxHeight", constComp bboxHeight),
        ("intersectionX", constComp intersectionX),
        ("intersectionY", constComp intersectionY),
        ("midpointX", constComp midpointX),
        ("midpointY", constComp midpointY),
        ("average", constComp average),
        ("len", constComp len),
        ("computeSurjectionLines", computeSurjectionLines),
        ("lineLeft", constComp lineLeft),
        ("lineRight", constComp lineRight),
        ("interpolate", constComp interpolate),
        ("sampleFunction", sampleFunction),
        ("fromDomain", fromDomain),
        ("applyFn", constComp applyFn),
        ("norm_", constComp norm_), -- type: any two GPIs with centers (getX, getY)
        ("midpointPathX", constComp midpointPathX),
        ("midpointPathY", constComp midpointPathY),
        ("sizePathX", constComp sizePathX),
        ("sizePathY", constComp sizePathY),
        ("makeRegionPath", constComp makeRegionPath),
        ("sampleFunctionArea", sampleFunctionArea),
        ("makeCurve", makeCurve),
        ("tangentLineSX", constComp tangentLineSX),
        ("tangentLineSY", constComp tangentLineSY),
        ("tangentLineEX", constComp tangentLineEX),
        ("tangentLineEY", constComp tangentLineEY),
        ("polygonizeCurve", constComp polygonizeCurve),
        ("setOpacity", constComp setOpacity),
        ("bbox", constComp bbox'),

        ("midpoint", noop), -- TODO
        ("sampleMatrix", noop), -- TODO
        ("sampleReal", noop), -- TODO
        ("sampleVectorIn", noop), -- TODO
        ("intersection", noop), -- TODO
        ("determinant", noop), -- TODO
        ("apply", noop) -- TODO
    ] -- TODO: port existing comps

compSignatures :: CompSignatures
compSignatures = M.fromList
    [
        ("rgba",
            ([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],
              ValueT ColorT)),
        ("atan",([ValueT FloatT],ValueT FloatT)),
        ("calcVectorsAngle",([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT,
            ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],ValueT FloatT)),
        ("calcVectorsAngleCos",([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT,
                ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],ValueT FloatT)),
        ("calcVectorsAngleWithOrigin",([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT,
                ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],ValueT FloatT)),
        ("generateRandomReal",([],ValueT FloatT)),
        ("calcNorm",([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],ValueT FloatT)),
        ("intersectionX", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("intersectionY", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("bboxHeight", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("bboxWidth", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("len", ([GPIType "Arrow"], ValueT FloatT)),
        ("computeSurjectionLines", ([ValueT IntT, GPIType "Line", GPIType "Line", GPIType "Line", GPIType "Line"], ValueT PtListT)),
        ("lineLeft", ([ValueT FloatT, GPIType "Arrow", GPIType "Arrow"], ValueT PtListT)),
        ("interpolate", ([ValueT PtListT, ValueT StrT], ValueT PathDataT)),
        ("sampleFunction", ([ValueT IntT, AnyGPI, AnyGPI], ValueT PtListT)),
        ("midpointX", ([AnyGPI], ValueT FloatT)),
        ("midpointY", ([AnyGPI], ValueT FloatT)),
        ("fromDomain", ([ValueT PtListT], ValueT FloatT)),
        ("applyFn", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)),
        ("norm_", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)), -- type: any two GPIs with centers (getX, getY)
        ("midpointPathX", ([ValueT PtListT], ValueT FloatT)),
        ("midpointPathY", ([ValueT PtListT], ValueT FloatT)),
        ("sizePathX", ([ValueT PtListT], ValueT FloatT)),
        ("sizePathY", ([ValueT PtListT], ValueT FloatT)),
        ("tangentLineSX", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)),
        ("tangentLineSY", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)),
        ("tangentLineEX", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)),
        ("tangentLineEY", ([ValueT PtListT, ValueT FloatT], ValueT FloatT)),
        ("makeRegionPath", ([GPIType "Curve", GPIType "Line"], ValueT PathDataT))
        -- ("len", ([GPIType "Arrow"], ValueT FloatT))
        -- ("bbox", ([GPIType "Arrow", GPIType "Arrow"], ValueT StrT)), -- TODO
        -- ("sampleMatrix", ([], ValueT StrT)), -- TODO
        -- ("sampleVectorIn", ([], ValueT StrT)), -- TODO
        -- ("intersection", ([], ValueT StrT)), -- TODO
        -- ("determinant", ([], ValueT StrT)), -- TODO
        -- ("apply", ([], ValueT StrT)) -- TODO
    ]

invokeComp :: (Autofloat a) =>
    FuncName -> [ArgVal a] -> CompSignatures -> StdGen
    -> (ArgVal a, StdGen)
invokeComp n args sigs g =
    -- TODO: Improve computation function typechecking to allow for genericity #164
    let -- (argTypes, retType) =
            -- fromMaybe (noSignatureError n) (M.lookup n compSignatures)
        -- args'  = checkArgs args argTypes n
        f      = fromMaybe (noFunctionError n) (M.lookup n compDict)
        (ret, g') = f args g
    in (ret, g')
       -- if checkReturn ret retType then (ret, g') else
       --  error ("invalid return value \"" ++ show ret ++ "\" of computation \"" ++ show n ++ "\". expected type is \"" ++ show retType ++ "\"")

-- | 'constComp' is a wrapper for computation functions that do not use randomization
constComp :: ConstCompFn -> CompFn
constComp f = \args g -> (f args, g) -- written in the lambda fn style to be more readable

--------------------------------------------------------------------------------
-- Objectives

-- Weights
repelWeight :: (Autofloat a) => a
repelWeight = 10000000

-- | 'objFuncDict' stores a mapping from the name of objective functions to the actual implementation
objFuncDict :: forall a. (Autofloat a) => M.Map String (ObjFnOn a)
objFuncDict = M.fromList
    [
        ("near", near),
        ("center", center),
        ("centerX", centerX),
        ("centerLabel", centerLabel),
        ("centerArrow", centerArrow),
        ("repel", (*) repelWeight . repel),
        ("nearHead", nearHead),
        ("topRightOf", topRightOf),
        ("nearEndVert", nearEndVert),
        ("nearEndHoriz", nearEndHoriz),
        ("topLeftOf", topLeftOf),
        ("above", above),
        ("equal", equal),
        ("distBetween", distBetween)
{-      ("centerLine", centerLine),
        ("increasingX", increasingX),
        ("increasingY", increasingY),
        ("horizontal", horizontal),
        ("upright", upright),
        ("xInRange", xInRange),
        ("yInRange", yInRange),
        ("orthogonal", orthogonal),
        ("toLeft", toLeft),
        ("between", between),
        ("sameX", sameX),
        ("ratioOf", ratioOf),
        ("sameY", sameY),
        -- ("sameX", (*) 0.6 `compose2` sameX),
        -- ("sameX", (*) 0.2 `compose2` sameX),
        ("sameCenter", sameCenter),
        ("repel", (*)  900000  `compose2` repel),
        -- ("repel", (*)  1000000  `compose2` repel),
        -- ("repel", (*)  10000  `compose2` repel),
        -- ("repel", repel),
        ("outside", outside),
        -}
    ]

objSignatures :: OptSignatures
objSignatures = MM.fromList
    [
        ("near", [GPIType "Circle", GPIType "Circle"]),
        ("near", [GPIType "Image", GPIType "Text", ValueT FloatT, ValueT FloatT]),
        ("nearHead",
            [GPIType "Arrow", GPIType "Text", ValueT FloatT, ValueT FloatT]),
        ("center", [AnyGPI]),
        ("centerX", [ValueT FloatT]),
        ("repel", [AnyGPI, AnyGPI]),
        ("centerLabel", [AnyGPI, GPIType "Text"]),
        ("centerArrow", [GPIType "Arrow", GPIType "Square", GPIType "Square"]),
        ("centerArrow", [GPIType "Arrow", GPIType "Circle", GPIType "Circle"]),
        ("centerArrow", [GPIType "Arrow", GPIType "Text", GPIType "Text"]),
        ("topLeftOf", [GPIType "Text", GPIType "Square"]),
        ("topLeftOf", [GPIType "Text", GPIType "Rectangle"]),
        ("topRightOf", [GPIType "Text", GPIType "Square"]),
        ("topRightOf", [GPIType "Text", GPIType "Rectangle"]),
        ("nearEndVert", [GPIType "Line", GPIType "Text"]),
        ("nearEndHoriz", [GPIType "Line", GPIType "Text"])
        -- ("centerArrow", []) -- TODO
    ]


--------------------------------------------------------------------------------
-- Constraints

-- exterior point method: penalty function
penalty :: (Ord a, Floating a, Show a) => a -> a
penalty x = max x 0 ^ q -- weights should get progressively larger in cr_dist
            where q = 2 -- also, may need to sample OUTSIDE feasible set
            -- where q = 3
{-# INLINE penalty #-}

-- | 'constrFuncDict' stores a mapping from the name of constraint functions to the actual implementation
constrFuncDict :: forall a. (Autofloat a) => M.Map FuncName (ConstrFnOn a)
constrFuncDict = M.fromList $ map toPenalty flist
    where
        toPenalty (n, f) = (n, penalty . f)
        flist =
            [
                ("at", at),
                ("contains", contains),
                ("sameHeight", sameHeight),
                ("nearHead", nearHead),
                ("smallerThan", smallerThan),
                ("minSize", minSize),
                ("maxSize", maxSize),
                ("outsideOf", outsideOf),
                ("disjoint", disjoint),
                ("inRange", inRange'),
                ("lessThan", lessThan)
            ]

constrSignatures :: OptSignatures
constrSignatures = MM.fromList
    [
        ("at", [AnyGPI, ValueT FloatT, ValueT FloatT]),
        ("minSize", [AnyGPI]),
        ("maxSize", [AnyGPI]),
        ("smallerThan", [GPIType "Circle", GPIType "Circle"]),
        ("smallerThan", [GPIType "Circle", GPIType "Square"]),
        ("smallerThan", [GPIType "Square", GPIType "Circle"]),
        ("smallerThan", [GPIType "Square", GPIType "Square"]),
        ("outsideOf", [GPIType "Text", GPIType "Circle"]),
        ("contains", [GPIType "Circle", GPIType "Circle"]),
        ("contains", [GPIType "Square", GPIType "Arrow"]),
        ("contains", [GPIType "Circle", GPIType "Circle", ValueT FloatT]),
        ("contains", [GPIType "Circle", GPIType "Text"]),
        ("contains", [GPIType "Square", GPIType "Text"]),
        ("contains", [GPIType "Rectangle", GPIType "Text"]),
        ("contains", [GPIType "Square", GPIType "Circle", ValueT FloatT]),
        ("contains", [GPIType "Square", GPIType "Circle"]),
        ("contains", [GPIType "Circle", GPIType "Square"]),
        ("contains", [GPIType "Circle", GPIType "Rectangle"]),
        ("overlapping", [GPIType "Circle", GPIType "Circle"]),
        ("overlapping", [GPIType "Square", GPIType "Circle"]),
        ("overlapping", [GPIType "Circle", GPIType "Square"]),
        ("overlapping", [GPIType "Square", GPIType "Square"]),
        ("disjoint", [GPIType "Circle", GPIType "Circle"]),
        ("disjoint", [GPIType "Square", GPIType "Square"])
        -- ("lessThan", []) --TODO
    ]

--------------------------------------------------------------------------------
-- Type checker for objectives and constraints

checkArg :: (Autofloat a) => ArgVal a -> ArgType -> Bool
-- TODO: add warnings/errors (?)
checkArg (GPI _) AnyGPI = True
checkArg _ AnyGPI = False
checkArg (GPI (t, _)) (OneOf l) = t `elem` l
checkArg (GPI (t1, _)) (GPIType t2) = t1 == t2
checkArg (Val v) (ValueT t) = typeOf v == t
checkArg _ _ = False

matchWith args sig = length args == length sig && and (zipWith checkArg args sig)

checkArgs :: (Autofloat a) => [ArgVal a] -> [ArgType] -> String -> [ArgVal a]
checkArgs arguments signature n =
    if arguments `matchWith` signature
    then arguments
    else sigMismatchError n signature arguments

checkArgsOverload :: (Autofloat a) => [ArgVal a] -> [[ArgType]] -> String -> [ArgVal a]
checkArgsOverload arguments signatures n =
    if any (arguments `matchWith`) signatures
        then arguments
        else noMatchedSigError n signatures arguments

checkReturn :: (Autofloat a) => ArgVal a -> ArgType -> Bool
-- TODO: add warning
checkReturn ret@(Val v) (ValueT t) = typeOf v == t
checkReturn (GPI v) _ = error "checkReturn: Computations cannot return GPIs"

--------------------------------------------------------------------------------
-- Computation Functions

type Interval = (Float, Float)

-- Generate n random values uniformly randomly sampled from interval and return generator.
-- NOTE: I'm not sure how backprop works WRT randomness, so the gradients might be inconsistent here.
-- Interval is not polymorphic because I want to avoid using the Random typeclass (Random a)
   -- which causes type inference problems in Style for some reason.
-- Also apparently using Autofloat here with typeable causes problems for generality of returned StdGen.
-- But it works fine without Typeable.
randomsIn :: (Autofloat a) => StdGen -> Integer -> Interval -> ([a], StdGen)
randomsIn g 0 _        =  ([], g)
randomsIn g n interval = let (x, g') = randomR interval g -- First value
                             (xs, g'') = randomsIn g' (n - 1) interval in -- Rest of values
                         (r2f x : xs, g'')

sampleFunction :: CompFn
-- Assuming domain and range are lines or arrows, TODO deal w/ points
-- TODO: discontinuous functions? not sure how to sample/model/draw consistently
sampleFunction [Val (IntV n), GPI domain, GPI range] g =
               let (dsx, dsy, dex, dey) = (getNum domain "startX", getNum domain "startY",
                                           getNum domain "endX", getNum domain "endY")
                   (rsx, rsy, rex, rey) = (getNum range "startX", getNum range "startY",
                                           getNum range "endX", getNum range "endY")
                   lower_left = (min dsx dex, min rsy rey)
                   top_right  = (max dsx dex, max rsy rey)
                   (pts, g')  = computeSurjection g n lower_left top_right
              in (Val $ PtListV pts, g')

-- Computes the surjection to lie inside a bounding box defined by the corners of a box
-- defined by four straight lines, assuming their lower/left coordinates come first.
-- Their intersections give the corners.
computeSurjectionLines :: CompFn
computeSurjectionLines args g =
    let (pts, g') = computeSurjectionLines' g args
    in (Val $ PtListV pts, g')

computeSurjectionLines' :: (Autofloat a) => StdGen -> [ArgVal a] -> ([Pt2 a], StdGen)
computeSurjectionLines' g args@[Val (IntV n), GPI left@("Line", _), GPI right@("Line", _), GPI bottom@("Line", _), GPI top@("Line", _)] =
    let lower_left = (getNum left "startX", getNum bottom "startY") in
    let top_right = (getNum right "startX", getNum top "startY") in
    computeSurjection g n lower_left top_right
-- Assuming left and bottom are perpendicular and share one point
computeSurjectionLines' g [Val (IntV n), GPI left@("Arrow", _), GPI bottom@("Arrow", _)] =
    let lower_left = (getNum left "startX", getNum left "startY") in
    let top_right = (getNum bottom "endX", getNum left "endY") in
    computeSurjection g n lower_left top_right

computeSurjection :: Autofloat a => StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen)
computeSurjection g numPoints (lowerx, lowery) (topx, topy) =
    if numPoints < 2 then error "Surjection needs to have >= 2 points"
    else
        let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
            xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
            xs_increasing = sort xs
            (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f lowery, r2f topy)
            ys = lowery : ys_inner ++ [topy] -- Include endpts so function is onto
            ys_perm = shuffle' ys (length ys) g'' -- Random permutation. TODO return g3?
        -- in (zip xs_increasing ys_perm, g'') -- len xs == len ys
        in (zip xs_increasing ys_perm, g'')-- len xs == len ys

-- calculates a line (of two points) intersecting the first axis, stopping before it leaves bbox of second axis
-- TODO rename lineLeft and lineRight
-- assuming a1 horizontal and a2 vertical, respectively
lineLeft :: ConstCompFn
lineLeft [Val (FloatV lineFrac), GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let a1_start = getNum a1 "startX" in
    let a1_len = abs (getNum a1 "endX" - a1_start) in
    let xpos = a1_start + lineFrac * a1_len in
    Val $ PtListV [(xpos, getNum a1 "startY"), (xpos, getNum a2 "endY")]

-- assuming a1 vert and a2 horiz, respectively
-- can this be written in terms of lineLeft?
lineRight :: ConstCompFn
lineRight [Val (FloatV lineFrac), GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let a1_start = getNum a1 "startY" in
    let a1_len = abs (getNum a1 "endY" - a1_start) in
    let ypos = a1_start + lineFrac * a1_len in
    Val $ PtListV [(getNum a2 "startX", ypos), (getNum a2 "endX", ypos)]

rgba :: ConstCompFn
rgba [Val (FloatV r), Val (FloatV g), Val (FloatV b), Val (FloatV a)] =
    Val (ColorV $ makeColor' r g b a)

arctangent :: ConstCompFn
arctangent [Val (FloatV d)] = Val (FloatV $ (atan d) / pi * 180)

calcVectorsAngle :: ConstCompFn
calcVectorsAngle [Val (FloatV sx1), Val (FloatV sy1), Val (FloatV ex1),
      Val (FloatV ey1), Val (FloatV sx2), Val (FloatV sy2),
       Val (FloatV ex2), Val (FloatV ey2)] =
         let (ax,ay) = (ex1 - sx1, ey1 - sy1)
             (bx,by) = (ex2 - sx2, ey2 - sy2)
             ab = ax*bx + ay*by
             na = sqrt (ax^2 + ay^2)
             nb = sqrt (bx^2 + by^2)
             angle = acos (ab / (na*nb)) / pi*180.0
         in Val (FloatV angle)

calcVectorsAngleWithOrigin :: ConstCompFn
calcVectorsAngleWithOrigin [Val (FloatV sx1), Val (FloatV sy1), Val (FloatV ex1),
     Val (FloatV ey1), Val (FloatV sx2), Val (FloatV sy2),
      Val (FloatV ex2), Val (FloatV ey2)] =
        let (ax,ay) = (ex1 - sx1, ey1 - sy1)
            (bx,by) = (ex2 - sx2, ey2 - sy2)
            (cx,cy) = ((ax + bx)/2.0,(ay + by)/2.0)
            angle =  if cy < 0 then  (atan (cy / cx) / pi*180.0) * 2.0 else 180.0 + (atan (cy / cx) / pi*180.0) * 2.0
        in Val (FloatV $ -1.0 * angle)
        --     angle1 =  if ay < 0 then  (atan (ay / ax) / pi*180.0) else 180.0  +  (atan (ay / ax) / pi*180.0)
        --     angle2 =  if by < 0 then  (atan (by / bx) / pi*180.0) else 180.0 +   (atan  (by / bx) / pi*180.0)
        -- in if traceShowId angle1 > traceShowId angle2 then Val (FloatV $ -1 * angle1) else Val (FloatV $ -1 * angle2)

generateRandomReal :: ConstCompFn
generateRandomReal [] = let g1 = mkStdGen 16
                            (x,g2) = (randomR (1, 15) g1) :: (Int,StdGen)
                            y = fst(randomR (1, 15) g2) ::  Int
                         in Val (FloatV ((fromIntegral x)/(fromIntegral y)))

calcNorm :: ConstCompFn
calcNorm [Val (FloatV sx1), Val (FloatV sy1), Val (FloatV ex1),Val (FloatV ey1)] =
  let nx = (ex1 - sx1) ** 2.0
      ny = (ey1 - sy1) ** 2.0
      norm = sqrt (nx + ny + 0.5)
  in Val (FloatV norm)

linePts, arrowPts :: (Autofloat a) => Shape a -> (a, a, a, a)
linePts = arrowPts
arrowPts a = (getNum a "startX", getNum a "startY", getNum a "endX", getNum a "endY")

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)

intersectionX :: ConstCompFn
intersectionX [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a1
        (x2, y2, x3, y3) = arrowPts a2
        det = (x0 - x1) * (y2 - y3) - (y0 - y1) * (x2 - x3)
    in Val $ FloatV $
       if det == 0 then infinity
       else (x0*y1 - y0*x1)*(x2 - x3) - (x0 - x1)*(x2*x3 - y2*y3) / det
intersectionY :: ConstCompFn
intersectionY [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a1
        (x2, y2, x3, y3) = arrowPts a2
        det = (x0 - x1) * (y2 - y3) - (y0 - y1) * (x2 - x3)
    in Val $ FloatV $
       if det == 0 then infinity
       else (x0*y1 - y0*x1)*(x2 - x3) - (y0 - y1)*(x2*x3 - y2*y3) / det

len :: ConstCompFn
len [GPI a@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a
    in Val $ FloatV $ dist (x0, y0) (x1, y1)

midpointX :: ConstCompFn
midpointX [GPI l] =
    if linelike l
    then let (x0, x1) = (getNum l "startX", getNum l "endX")
         in Val $ FloatV $ (x1 + x0) / 2
    else error "GPI type must be line-like"

midpointY :: ConstCompFn
midpointY [GPI l] =
    if linelike l
    then let (y0, y1) = (getNum l "startY", getNum l "endY")
         in Val $ FloatV $ (y1 + y0) / 2
    else error "GPI type must be line-like"

average :: ConstCompFn
average [Val (FloatV x), Val (FloatV y)] =
    let res = (x + y) / 2
    in Val $ FloatV res

norm_ :: ConstCompFn
norm_ [Val (FloatV x), Val (FloatV y)] = Val $ FloatV $ norm [x, y]

-- | Catmull-Rom spline interpolation algorithm
interpolateFn :: Autofloat a => [Pt2 a] -> [Elem a]
interpolateFn pts =
    let k  = 1.5
        p0 = head pts
        chunks = repeat4 $ head pts : pts ++ [last pts]
        paths = map (chain k) chunks
        finalPath = Pt p0 : paths
    in finalPath
    where repeat4 xs = [ take 4 . drop n $ xs | n <- [0..length xs - 4] ]

-- Wrapper for interpolateFn
interpolate :: ConstCompFn
interpolate [Val (PtListV pts)] =
    let pathRes = interpolateFn pts
    in Val $ PathDataV $ [Open pathRes]

chain :: Autofloat a => a -> [(a, a)] -> Elem a
chain k [(x0, y0), (x1, y1), (x2, y2), (x3, y3)] =
    let cp1x = x1 + (x2 - x0) / 6 * k
        cp1y = y1 + (y2 - y0) / 6 * k
        cp2x = x2 - (x3 - x1) / 6 * k
        cp2y = y2 - (y3 - y1) / 6 * k
    in CubicBez ((cp1x, cp1y), (cp2x, cp2y), (x2, y2))

-- COMBAK: finish this
-- sampleCurve :: Autofloat a =>
--     StdGen -> Integer -> Pt2 a -> Pt2 a -> Bool -> Bool -> [Pt2 a]
-- sampleCurve g numPoints (lowerx, lowery) (topx, topy) =
--     if numPoints < 2 then error "Surjection needs to have >= 2 points"
--     else
--         let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
--             xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
--             (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f lowery, r2f topy)
--             ys = lowery : ys_inner ++ [topy] -- Include endpts so function is onto
--             xs_increasing = sort xs
--             ys_perm = shuffle' ys (length ys) g'' -- Random permutation. TODO return g3?
--         -- in (zip xs_increasing ys_perm, g'') -- len xs == len ys
--         in zip xs_increasing ys_perm -- len xs == len ys

-- From Shapes.hs, TODO factor out
sampleList :: (Autofloat a) => [a] -> StdGen -> (a, StdGen)
sampleList list g =
    let (idx, g') = randomR (0, length list - 1) g
    in (list !! idx, g')

-- sample random element from domain of function (relation)
fromDomain :: CompFn
fromDomain [Val (PtListV path)] g =
           let (x, g') = sampleList (middle $ map fst path) g in
           -- let x = fst $ path !! 1 in
           (Val $ FloatV x, g')
    where middle = init . tail

-- lookup element in function (relation) by making a Map
applyFn :: ConstCompFn
applyFn [Val (PtListV path), Val (FloatV x)] =
        case M.lookup x (M.fromList path) of
        Just y -> Val (FloatV y)
        Nothing -> error "x element does not exist in function"

-- TODO: remove the unused functions in the next four
midpointPathX :: ConstCompFn
midpointPathX [Val (PtListV path)] =
              let xs = map fst path
                  res = (maximum xs + minimum xs) / 2 in
              Val $ FloatV res

midpointPathY :: ConstCompFn
midpointPathY [Val (PtListV path)] =
              let ys = map snd path
                  res = (maximum ys + minimum ys) / 2 in
              Val $ FloatV res

sizePathX :: ConstCompFn
sizePathX [Val (PtListV path)] =
              let xs = map fst path
                  res = maximum xs - minimum xs in
              Val $ FloatV res

sizePathY :: ConstCompFn
sizePathY [Val (PtListV path)] =
              let ys = map snd path
                  res = maximum ys - minimum ys in
              Val $ FloatV res

-- Compute path for an integral's shape (under a function, above the interval on which the function is defined)
makeRegionPath :: ConstCompFn
makeRegionPath [GPI fn@("Curve", _), GPI intv@("Line", _)] =
               let pt1   = Pt $ getPoint "start" intv
                   pt2   = Pt $ getPoint "end" intv
                   -- Assume the function is a single open path consisting of a Pt elem, followed by CubicBez elements
                   -- (i.e. produced by `interpolate` with parameter "open")
                   curve = case (getPathData fn) !! 0 of
                           Open elems -> elems
                           Closed elems -> error "makeRegionPath not implemented for closed paths"
                   path  = Closed $ pt1 : curve ++ [pt2]
               in Val (PathDataV [path])

-- Make determinant region
makeRegionPath [GPI a1, GPI a2] =
               if not (linelike a1 && linelike a2) then error "expected two linelike GPIs" else
               let xs@[sx1, ex1, sx2, ex2] = getXs a1 ++ getXs a2
                   ys@[sy1, ey1, sy2, ey2] = getYs a1 ++ getYs a2
                   -- Third coordinate is the vector addition, normalized for an origin that may not be (0, 0)
                   regionPts = [(sx1, sy1), (ex1, ey1), (ex1 + ex2 - sx1, ey1 + ey2 - sy1), (ex2, ey2)]
                   path = Closed $ map Pt regionPts
               in Val (PathDataV [path])
               where getXs a = [getNum a "startX", getNum a "endX"]
                     getYs a = [getNum a "startY", getNum a "endY"]

-- | Draws a filled region from domain to range with in-curved sides, assuming domain is above range
-- | Directionality: domain's right, then range's right, then range's left, then domain's left

-- TODO: when range's x is a lot smaller than the domain's x, or |range| << |domain|, the generated region crosses itself
-- You can see this by adjusting the interval size by *dragging the labels*
-- TODO: should account for thickness of domain and range
-- Assuming horizontal GPIs
sampleFunctionArea :: CompFn
sampleFunctionArea [x@(GPI domain), y@(GPI range)] g =
                   -- Apply with default offsets
                   sampleFunctionArea [x, y, Val (FloatV 0.3), Val (FloatV 0.0)] g
sampleFunctionArea [GPI domain, GPI range, Val (FloatV xFrac), Val (FloatV yFrac)] g =
               if linelike domain && linelike range
               then let pt_tl = getPoint "start" domain
                        pt_tr = getPoint "end" domain

                        pt_br = getPoint "end" range
                        pt_bl = getPoint "start" range

                        -- Compute dx (the x offset for the function's shape) as a fraction of width of the smaller interval
                        width = min (abs (fst pt_tl - fst pt_tr)) (abs (fst pt_br - fst pt_bl))
                        height = abs $ snd pt_bl - snd pt_tl
                        dx = xFrac * width
                        dy = yFrac * height
                        x_offset = (dx, 0)
                        y_offset = (0, dy)
                        pt_midright = midpoint pt_tr pt_br -: x_offset +: y_offset
                        pt_midleft = midpoint pt_bl pt_tl +: x_offset +: y_offset

                        right_curve = interpolateFn [pt_tr, pt_midright, pt_br]
                        left_curve = interpolateFn [pt_bl, pt_midleft, pt_tl]

                        -- TODO: not sure if this is right. do any points need to be included in the path?
                        path = Closed $ [Pt pt_tl, Pt pt_tr] ++ right_curve ++ [Pt pt_br, Pt pt_bl] ++ left_curve
                    in  (Val $ PathDataV [path], g)
               else error "expected two linelike shapes"

-- Draw a curve from (x1, y1) to (x2, y2) with some point in the middle defining curvature
makeCurve :: CompFn
makeCurve [Val (FloatV x1), Val (FloatV y1), Val (FloatV x2), Val (FloatV y2), Val (FloatV dx), Val (FloatV dy)] g =
          let offset = (dx, dy)
              midpt = midpoint (x1, y1) (x2, y2) +: offset
              path = Open $ interpolateFn [(x1, y1), midpt, (x2, y2)]
          in (Val $ PathDataV [path], g)

-- NOTE: assumes that the curve has at least 3 points
tangentLine :: Autofloat a => a -> [Pt2 a] -> a -> (Pt2 a, Pt2 a)
tangentLine x ptList len =
    let i  = fromMaybe 1 $ findIndex (\(a, _) -> abs (x - a) <= 1) ptList
        p0@(px, py) = ptList !! i
        p1 = nextPoint p0 $ drop (i - 1) ptList
        k = slope p0 p1
        dx = d / sqrt (1 + k^2)
        dy = k * dx
        d = len / 2
    in ((px - dx, py - dy), (px + dx, py + dy))
    where
        -- NOTE: instead of getting the immediate next point in the list, we search the rest of the list until a point that is numerically far enough from (x, y) is found, in order to compute the slope.
        nextPoint (x, y) l = fromMaybe
            (error "tangentLine: cannot find next point") $
            find (\(x', y') -> abs (x - x') > epsd || abs (y - y') > epsd) l
        slope (x0, y0) (x1, y1) = (y1 - y0) / (x1 - x0)

tangentLineSX :: ConstCompFn
tangentLineSX [Val (PtListV curve), Val (FloatV x), Val (FloatV len)] =
    Val $ FloatV $ fst $ fst $ tangentLine x curve len
tangentLineSY :: ConstCompFn
tangentLineSY [Val (PtListV curve), Val (FloatV x), Val (FloatV len)] =
    Val $ FloatV $ snd $ fst $ tangentLine x curve len
tangentLineEX :: ConstCompFn
tangentLineEX [Val (PtListV curve), Val (FloatV x), Val (FloatV len)] =
    Val $ FloatV $ fst $ snd $ tangentLine x curve len
tangentLineEY :: ConstCompFn
tangentLineEY [Val (PtListV curve), Val (FloatV x), Val (FloatV len)] =
    Val $ FloatV $ snd $ snd $ tangentLine x curve len

polygonizeCurve :: ConstCompFn
polygonizeCurve [Val (IntV maxIter), Val (PathDataV curve)] =
    Val $ PtListV $ head $ polygonize (fromIntegral maxIter) curve

polygonize :: Autofloat a => Int -> PathData a -> [[Pt2 a]]
polygonize maxIter = map go
    where
        go (Closed path) = error "TODO"
        go (Open path) = concatMap (polyCubicBez 0 maxIter) $ expandCurves path

type CubicBezCoeffs a = (Pt2 a, Pt2 a, Pt2 a, Pt2 a)

expandCurves :: Autofloat a => [Elem a] -> [CubicBezCoeffs a]
expandCurves elems = zipWith attach elems $ tail elems
    where
        attach (Pt a) (CubicBez (b, c, d)) = (a, b, c, d)
        attach (CubicBez (_, _, a)) (CubicBez (b, c, d)) = (a, b, c, d)

-- | implements http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.162&rep=rep1&type=pdf
polyCubicBez :: Autofloat a => Int -> Int -> CubicBezCoeffs a -> [Pt2 a]
polyCubicBez count maxCount curve@(a, b, c, d) =
    if (tr "count" count) >= maxCount then [a, b, c, d] else
        concatMapTuple (polyCubicBez (count + 1) maxCount) $ divideCubicBezier curve
    where concatMapTuple f (a1, a2) = f a1 ++ f a2

isFlat :: Autofloat a => CubicBezCoeffs a -> Bool
isFlat (a, b, c, d) = True

divideCubicBezier :: Autofloat a => CubicBezCoeffs a -> (CubicBezCoeffs a, CubicBezCoeffs a)
divideCubicBezier bezier@(a, _, _, d) = (left, right) where
    left = (a, ab, abbc, abbcbccd)
    right = (abbcbccd, bccd, cd, d)
    (ab, _bc, cd, abbc, bccd, abbcbccd) = splitCubicBezier bezier

--                     BC
--         B X----------X---------X C
--    ^     /      ___/   \___     \     ^
--   u \   /   __X------X------X_   \   / v
--      \ /___/ ABBC       BCCD  \___\ /
--    AB X/                          \X CD
--      /                              \
--     /                                \
--    /                                  \
-- A X                                    X D
splitCubicBezier :: Autofloat a => CubicBezCoeffs a -> (Pt2 a, Pt2 a, Pt2 a, Pt2 a, Pt2 a, Pt2 a)
splitCubicBezier (a, b, c, d) = (ab, bc, cd, abbc, bccd, abbcbccd)
    where
        ab = a `midpoint` b
        bc = b `midpoint` c
        cd = c `midpoint` d
        abbc = ab `midpoint` bc
        bccd = bc `midpoint` cd
        abbcbccd = abbc `midpoint` bccd

bezierBbox :: (Autofloat a) => Shape a -> ((a, a), (a, a)) -- poly Point type?
bezierBbox cb = let path = getPath cb
                    (xs, ys) = (map fst path, map snd path)
                    lower_left = (minimum xs, minimum ys)
                    top_right = (maximum xs, maximum ys) in
                (lower_left, top_right)

bboxHeight :: ConstCompFn
bboxHeight [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let ys@[y0, y1, y2, y3] = getYs a1 ++ getYs a2
        (ymin, ymax) = (minimum ys, maximum ys)
    in Val $ FloatV $ abs $ ymax - ymin
    where getYs a = [getNum a "startY", getNum a "endY"]

bboxWidth :: ConstCompFn
bboxWidth [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let xs@[x0, x1, x2, x3] = getXs a1 ++ getXs a2
        (xmin, xmax) = (minimum xs, maximum xs)
    in Val $ FloatV $ abs $ xmax - xmin
    where getXs a = [getNum a "startX", getNum a "endX"]

bbox :: (Autofloat a) => Shape a -> Shape a -> [(a, a)]
bbox a1 a2 = 
    if not (linelike a1 && linelike a2) then error "expected two linelike GPIs" else
    let xs@[x0, x1, x2, x3] = getXs a1 ++ getXs a2
        (xmin, xmax) = (minimum xs, maximum xs)
        ys@[y0, y1, y2, y3] = getYs a1 ++ getYs a2
        (ymin, ymax) = (minimum ys, maximum ys)
    -- Four bbox points in clockwise order (bottom left, bottom right, top right, top left)
    in [(xmin, ymin), (xmax, ymin), (xmax, ymax), (xmin, ymax)]
    where getXs a = [getNum a "startX", getNum a "endX"]
          getYs a = [getNum a "startY", getNum a "endY"]

bbox' :: ConstCompFn
bbox' [GPI a1, GPI a2] = Val $ PtListV $ bbox a2 a2

noop :: CompFn
noop [] g = (Val (StrV "TODO"), g)

-- Set the opacity to a given fraction of the value.
setOpacity :: ConstCompFn
setOpacity [Val (ColorV (RGBA r g b a)), Val (FloatV frac)] = Val $ ColorV (RGBA r g b (r2f frac * a))

--------------------------------------------------------------------------------
-- Objective Functions

near :: ObjFn
near [GPI o1, GPI o2] = distsq (getX o1, getY o1) (getX o2, getY o2)
near [GPI img@("Image", _), GPI lab@("Text", _), Val (FloatV xoff), Val (FloatV yoff)] =
    let center = (getNum img "centerX", getNum img "centerY")
        offset = (xoff, yoff)
    in distsq (getX lab, getY lab) (center `plus2` offset)
    where plus2 (a, b) (c, d) = (a + c, b + d)

center :: ObjFn
center [GPI o] = tr "center: " $ distsq (getX o, getY o) (0, 0)

centerX :: ObjFn
centerX [Val (FloatV x)] = tr "centerX" $ x^2

-- | 'sameCenter' encourages two objects to center at the same point
sameCenter :: ObjFn
sameCenter [GPI a, GPI b] = (getX a - getX b)^2 + (getY a - getY b)^2

centerLabel :: ObjFn
centerLabel [a, b, Val (FloatV w)] = w * centerLabel [a, b] -- TODO factor out
-- TODO revert
-- centerLabel [GPI curve, GPI text]
--     | curve `is` "Curve" && text `is` "Text" =
--         let ((lx, ly), (rx, ry)) = bezierBbox curve
--             (xmargin, ymargin) = (-10, 30)
--             midbez = ((lx + rx) / 2 + xmargin, (ly + ry) / 2 + ymargin) in
--         distsq midbez (getX text, getY text)
centerLabel [GPI p, GPI l]
    | p `is` "AnchorPoint" && l `is` "Text" =
        let [px, py, lx, ly] = [getX p, getY p, getX l, getY l] in
        (px + 10 - lx)^2 + (py + 20 - ly)^2 -- Top right from the point
-- -- TODO: depends on orientation of arrow
centerLabel [GPI arr, GPI text]
    | ((arr `is` "Arrow") || (arr `is` "Line")) && text `is` "Text" =
        let (sx, sy, ex, ey) = (getNum arr "startX", getNum arr "startY", getNum arr "endX", getNum arr "endY")
            (mx, my) = midpoint (sx, sy) (ex, ey)
            (lx, ly) = (getX text, getY text) in
        (mx - lx)^2 + (my + 1.1 * getNum text "h" - ly)^2 -- Top right from the point
centerLabel [a, b] = sameCenter [a, b]
-- centerLabel [CB' a, L' l] [mag] = -- use the float input?
--                 let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
--                     (mx, my) = midpoint (sx, sy) (ex, ey)
--                     (lx, ly) = (xl' l, yl' l) in
                -- (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point

-- | `centerArrow` positions an arrow between two objects, with some spacing
centerArrow :: ObjFn
centerArrow [GPI arr@("Arrow", _), GPI sq1@("Square", _), GPI sq2@("Square", _)] =
            _centerArrow arr [getX sq1, getY sq1] [getX sq2, getY sq2]
                [spacing + (halfDiagonal . flip getNum "side") sq1, negate $ spacing + (halfDiagonal . flip getNum "side") sq2]

centerArrow [GPI arr@("Arrow", _), GPI sq@("Square", _), GPI circ@("Circle", _)] =
            _centerArrow arr [getX sq, getY sq] [getX circ, getY circ]
                [spacing + (halfDiagonal . flip getNum "side") sq, negate $ spacing + getNum circ "radius"]

centerArrow [GPI arr@("Arrow", _), GPI circ@("Circle", _), GPI sq@("Square", _)] =
            _centerArrow arr [getX circ, getY circ] [getX sq, getY sq]
                [spacing + getNum circ "radius", negate $ spacing + (halfDiagonal . flip getNum "side") sq]

centerArrow [GPI arr@("Arrow", _), GPI circ1@("Circle", _), GPI circ2@("Circle", _)] =
            _centerArrow arr [getX circ1, getY circ1] [getX circ2, getY circ2]
                [ spacing * getNum circ1 "r", negate $ spacing * getNum circ2 "r"]

centerArrow [GPI arr@("Arrow", _), GPI ell1@("Ellipse", _), GPI ell2@("Ellipse", _)] =
            _centerArrow arr [getX ell1, getY ell1] [getX ell2, getY ell2]
                [ spacing * getNum ell1 "radius1", negate $ spacing * getNum ell2 "radius2"]
                -- FIXME: inaccurate, only works for horizontal cases

centerArrow [GPI arr@("Arrow", _), GPI pt1@("AnchorPoint", _), GPI pt2@("AnchorPoint", _)] =
            _centerArrow arr [getX pt1, getY pt1] [getX pt2, getY pt2]
                [ spacing * 2 * r2f ptRadius, negate $ spacing * 2 * r2f ptRadius]
                -- FIXME: anchor points have no radius

centerArrow [GPI arr@("Arrow", _), GPI text1@("Text", _), GPI text2@("Text", _)] =
            _centerArrow arr [getX text1, getY text1] [getX text2, getY text2]
                [spacing * getNum text1 "h", negate $ 2 * spacing * getNum text2 "h"]

centerArrow [GPI arr@("Arrow", _), GPI text@("Text", _), GPI circ@("Circle", _)] =
            _centerArrow arr [getX text, getY text] [getX circ, getY circ]
                [1.5 * getNum text "w", negate $ spacing * getNum circ "radius"]

spacing :: (Autofloat a) => a
spacing = 1.1 -- TODO: arbitrary

_centerArrow :: Autofloat a => Shape a -> [a] -> [a] -> [a] -> a
_centerArrow arr@("Arrow", _) s1@[x1, y1] s2@[x2, y2] [o1, o2] =
    let vec  = [x2 - x1, y2 - y1] -- direction the arrow should point to
        dir = normalize vec
        [sx, sy, ex, ey] = if norm vec > o1 + abs o2
                then (s1 +. o1 *. dir) ++ (s2 +. o2 *. dir) else s1 ++ s2
        [fromx, fromy, tox, toy] = [getNum arr "startX", getNum arr "startY",
                                    getNum arr "endX",   getNum arr "endY"] in
    (fromx - sx)^2 + (fromy - sy)^2 + (tox - ex)^2 + (toy - ey)^2

-- | 'repel' exert an repelling force between objects
-- TODO: temporarily written in a generic way
-- Note: repel's energies are quite small so the function is scaled by repelWeight before being applied
repel :: ObjFn
repel [GPI a, GPI b] = 1 / (distsq (getX a, getY a) (getX b, getY b) + epsd)
repel [GPI a, GPI b, Val (FloatV weight)] = weight / (distsq (getX a, getY a) (getX b, getY b) + epsd)
    -- trace ("REPEL: " ++ show a ++ "\n" ++ show b ++ "\n" ++ show res) res
-- repel [C' c, S' d] [] = 1 / distsq (xc' c, yc' c) (xs' d, ys' d) - r' c - side' d + epsd
-- repel [S' c, C' d] [] = 1 / distsq (xc' d, yc' d) (xs' c, ys' c) - r' d - side' c + epsd
-- repel [P' c, P' d] [] = if c == d then 0 else 1 / distsq (xp' c, yp' c) (xp' d, yp' d) - 2 * r2f ptRadius + epsd
-- repel [L' c, L' d] [] = if c == d then 0 else 1 / distsq (xl' c, yl' c) (xl' d, yl' d)
-- repel [L' c, C' d] [] = 1 / distsq (xl' c, yl' c) (xc' d, yc' d)
-- repel [C' c, L' d] [] = 1 / distsq (xc' c, yc' c) (xl' d, yl' d)
-- repel [L' c, S' d] [] = 1 / distsq (xl' c, yl' c) (xs' d, ys' d)
-- repel [S' c, L' d] [] = 1 / distsq (xs' c, ys' c) (xl' d, yl' d)
-- repel [A' c, L' d] [] = repel' (startx' c, starty' c) (xl' d, yl' d) +
--         repel' (endx' c, endy' c) (xl' d, yl' d)
-- repel [A' c, C' d] [] = repel' (startx' c, starty' c) (xc' d, yc' d) +
--         repel' (endx' c, endy' c) (xc' d, yc' d)
-- repel [IM' c, IM' d] [] = 1 / (distsq (xim' c, yim' c) (xim' d, yim' d) + epsd) - sizeXim' c - sizeXim' d --TODO Lily check this math is correct
-- repel [a, b] [] = if a == b then 0 else 1 / (distsq (getX a, getY a) (getX b, getY b) )

topRightOf :: ObjFn
topRightOf [GPI l@("Text", _), GPI s@("Square", _)] = dist (getX l, getY l) (getX s + 0.5 * getNum s "side", getY s + 0.5 * getNum s "side")
topRightOf [GPI l@("Text", _), GPI s@("Rectangle", _)] = dist (getX l, getY l) (getX s + 0.5 * getNum s "sizeX", getY s + 0.5 * getNum s "sizeY")

topLeftOf :: ObjFn
topLeftOf [GPI l@("Text", _), GPI s@("Square", _)] = dist (getX l, getY l) (getX s - 0.5 * getNum s "side", getY s - 0.5 * getNum s "side")
topLeftOf [GPI l@("Text", _), GPI s@("Rectangle", _)] = dist (getX l, getY l) (getX s - 0.5 * getNum s "sizeX", getY s - 0.5 * getNum s "sizeY")

nearHead :: ObjFn
nearHead [GPI l, GPI lab@("Text", _), Val (FloatV xoff), Val (FloatV yoff)] =
    if linelike l
    then let end = (getNum l "endX", getNum l "endY")
             offset = (xoff, yoff)
         in distsq (getX lab, getY lab) (end `plus2` offset)
    else error "GPI type for nearHead must be line-like"
      where plus2 (a, b) (c, d) = (a + c, b + d)

nearEndVert :: ObjFn
-- expects a vertical line
nearEndVert [GPI line@("Line", _), GPI lab@("Text", _)] =
            let (sx, sy, ex, ey) = linePts line in
            let bottompt = if sy < ey then (sx, sy) else (ex, ey) in
            let yoffset = -25 in
            let res = distsq (getX lab, getY lab) (fst bottompt, snd bottompt + yoffset) in res

nearEndHoriz :: ObjFn
-- expects a horiz line
nearEndHoriz [GPI line@("Line", _), GPI lab@("Text", _)] =
            let (sx, sy, ex, ey) = linePts line in
            let leftpt = if sx < ex then (sx, sy) else (ex, ey) in
            let xoffset = -25 in
            distsq (getX lab, getY lab) (fst leftpt + xoffset, snd leftpt)

-- | 'above' makes sure the first argument is on top of the second.
above :: ObjFn
above [GPI top, GPI bottom, Val (FloatV offset)] = (getY top - getY bottom - offset)^2
above [GPI top, GPI bottom] = (getY top - getY bottom - 100)^2

-- | 'sameHeight' forces two objects to stay at the same height (have the same Y value)
sameHeight :: ObjFn
sameHeight [GPI a, GPI b] = (getY a - getY b)^2

equal :: ObjFn
equal [Val (FloatV a), Val (FloatV b)] = (a - b)^2

distBetween :: ObjFn
distBetween [GPI c1, GPI c2, Val (FloatV padding)] =
    let (r1, r2, x1, y1, x2, y2) = (getNum c1 "r", getNum c2 "r", getX c1, getY c1, getX c2, getY c2) in
    -- If one's a subset of another or has same radius as other
    -- If they only intersect
    if dist (x1, y1) (x2, y2) < (r1 + r2)
    then repel [GPI c1, GPI c2, Val (FloatV repelWeight)] -- 1 / distsq (x1, y1) (x2, y2)
    -- If they don't intersect
    else -- trace ("padding: " ++ show padding)
         (dist (x1, y1) (x2, y2) - r1 - r2 - padding)^2

--------------------------------------------------------------------------------
-- Constraint Functions

at :: ConstrFn
at [GPI o, Val (FloatV x), Val (FloatV y)] =
    (getX o - x)^2 + (getY o - y)^2

lessThan :: ConstrFn
lessThan [Val (FloatV x), Val (FloatV y)] =
         x - y

contains :: ConstrFn
contains [GPI o1@("Circle", _), GPI o2@("Circle", _)] =
    dist (getX o1, getY o1) (getX o2, getY o2) - (getNum o1 "r" - getNum o2 "r")
contains [GPI outc@("Circle", _), GPI inc@("Circle", _), Val (FloatV padding)] =
    dist (getX outc, getY outc) (getX inc, getY inc) - (getNum outc "r" - padding - getNum inc "r")
contains [GPI c@("Circle", _), GPI rect@("Rectangle", _)] =
    let (x, y, w, h)     =
            (getX rect, getY rect, getNum rect "sizeX", getNum rect "sizeY")
        [x0, x1, y0, y1] = [x - w/2, x + w/2, y - h/2, y + h/2]
        pts              = [(x0, y0), (x0, y1), (x1, y0), (x1, y1)]
        (cx, cy, radius) = (getX c, getY c, getNum c "r")
    in sum $ map (\(a, b) -> max 0 $ dist (cx, cy) (a, b) - radius) pts
contains [GPI c@("Circle", _), GPI t@("Text", _)] =
    let res = dist (getX t, getY t) (getX c, getY c) - getNum c "r" + max (getNum t "w") (getNum t "h")
    in if res < 0 then 0 else res
    -- TODO: factor out the vertex access code to a high-level getter
    -- NOTE: seems that the following version doesn't perform as well as the hackier old version. Maybe it's the shape of the obj that is doing it, but we do observe that the labels tend to get really close to the edges
    -- let (x, y, w, h)     = (getX t, getY t, getNum t "w", getNum t "h")
    --     [x0, x1, y0, y1] = [x - w/2, x + w/2, y - h/2, y + h/2]
    --     pts              = [(x0, y0), (x0, y1), (x1, y0), (x1, y1)]
    --     (cx, cy, radius) = (getX c, getY c, getNum c "r")
    -- in sum $ map (\(a, b) -> (max 0 $ dist (cx, cy) (a, b) - radius)^2) pts
contains [GPI s@("Square", _), GPI l@("Text", _)] =
    dist (getX l, getY l) (getX s, getY s) - getNum s "side" / 2 + getNum l "w"
contains [GPI s@("Rectangle", _), GPI l@("Text", _)] =
    -- TODO: implement precisely, max (w, h)? How about diagonal case?
    dist (getX l, getY l) (getX s, getY s) - getNum s "sizeX" / 2 + getNum l "sizeX"
contains [GPI outc@("Square", _), GPI inc@("Square", _)] =
    dist (getX outc, getY outc) (getX inc, getY inc) - (0.5 * getNum outc "side" - 0.5 * getNum inc "side")
contains [GPI outc@("Square", _), GPI inc@("Circle", _)] =
    dist (getX outc, getY outc) (getX inc, getY inc) - (0.5 * getNum outc "side" - getNum inc "r")
contains [GPI outc@("Square", _), GPI inc@("Circle", _), Val (FloatV padding)] =
    dist (getX outc, getY outc) (getX inc, getY inc) - (0.5 * getNum outc "side" - padding - getNum inc "r")
contains [GPI outc@("Circle", _), GPI inc@("Square", _)] =
    dist (getX outc, getY outc) (getX inc, getY inc) - (getNum outc "r" - 0.5 * getNum inc "side")
contains [GPI set@("Ellipse", _), GPI label@("Text", _)] =
    dist (getX label, getY label) (getX set, getY set) - max (getNum set "r") (getNum set "r") + getNum label "w"
contains [GPI sq@("Square", _), GPI ar@("Arrow", _)] =
     let (startX, startY, endX, endY) = arrowPts ar
         (x, y) = (getX sq, getY sq)
         side = getNum sq "side"
         (lx, ly) = ((x - side / 2) * 0.75, (y - side / 2) * 0.75)
         (rx, ry) = ((x + side / 2) * 0.75, (y + side / 2) * 0.75)
     in inRange startX lx rx
         + inRange startY ly ry
         + inRange endX lx rx
         + inRange endY ly ry
contains [GPI rt@("Rectangle", _), GPI ar@("Arrow", _)] =
    let (startX, startY, endX, endY) = arrowPts ar
        (x, y) = (getX rt, getY rt)
        (w, h) = (getNum rt "sizeX", getNum rt "sizeY")
        (lx, ly) = (x - w / 2, y - h / 2)
        (rx, ry) = (x + w / 2, y + h / 2)
    in inRange startX lx rx
        + inRange startY ly ry
        + inRange endX lx rx
        + inRange endY ly ry

inRange a l r
    | a < l  = (a-l)^2
    | a > r  = (a-r)^2
    | otherwise = 0

inRange' :: ConstrFn
inRange' [Val (FloatV v), Val (FloatV left), Val (FloatV right)]
    | v < left = left - v
    | v > right = v - right
    | otherwise = 0
-- = inRange v left right

-- contains [GPI set@("Circle", _), P' GPI pt@("", _)] = dist (getX pt, getX pt) (getX set, getY set) - 0.5 * r' set
-- TODO: only approx
-- contains [S' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (getX set, getX set) - 0.4 * side' set
-- FIXME: doesn't work
-- contains [E' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (xe' set, getX set) - max (rx' set) (ry' set) * 0.9

-- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented
maxSize :: ConstrFn
-- TODO: why do we need `r2f` now? Didn't have to before
limit = max canvasWidth canvasHeight
maxSize [GPI c@("Circle", _)] = getNum c "r" - r2f (limit / 6)
maxSize [GPI s@("Square", _)] = getNum s "side" - r2f (limit  / 3)
maxSize [GPI r@("Rectangle", _)] =
    let max_side = max (getNum r "sizeX") (getNum r "sizeY")
    in max_side - r2f (limit  / 3)
maxSize [GPI im@("Image", _)] =
    let max_side = max (getNum im "lengthX") (getNum im "lengthY")
    in max_side - r2f (limit / 3)
maxSize [GPI e@("Ellipse", _)] = max (getNum e "r") (getNum e "r") - r2f (limit  / 3)
maxSize _ = 0

-- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented
minSize :: ConstrFn
minSize [GPI c@("Circle", _)] = 20 - getNum c "r"
minSize [GPI s@("Square", _)] = 20 - getNum s "side"
minSize [GPI r@("Rectangle", _)] =
    let min_side = min (getNum r "sizeX") (getNum r "sizeY")
    in 20 - min_side
minSize [GPI e@("Ellipse", _)] = 20 - min (getNum e "r") (getNum e "r")
minSize [GPI g] =
        if fst g == "Line" || fst g == "Arrow" then
        let vec = [ getNum g "endX" - getNum g "startX",
                    getNum g "endY" - getNum g "startY"] in
        50 - norm vec
        else 0
minSize [GPI g, Val (FloatV len)] =
        if fst g == "Line" || fst g == "Arrow" then
        let vec = [ getNum g "endX" - getNum g "startX",
                    getNum g "endY" - getNum g "startY"] in
        len - norm vec
        else 0

smallerThan  :: ConstrFn
smallerThan [GPI inc@("Circle", _), GPI outc@("Circle", _)] =
            getNum inc "r" - getNum outc "r" - 0.4 * getNum outc "r" -- TODO: taking this as a parameter?
smallerThan [GPI inc@("Circle", _), GPI outs@("Square", _)] =
            0.5 * getNum outs "side" - getNum inc "r"
smallerThan [GPI ins@("Square", _), GPI outc@("Circle", _)] =
            halfDiagonal $ getNum ins "side" - getNum outc "r"
smallerThan [GPI ins@("Square", _), GPI outs@("Square", _)] =
            getNum ins "side" - getNum outs "side" - subsetSizeDiff

outsideOf :: ConstrFn
outsideOf [GPI l@("Text", _), GPI c@("Circle", _)] =
    let padding = 10.0 in
    let labelR = max (getNum l "w") (getNum l "h") in
    -dist (getX l, getY l) (getX c, getY c) + getNum c "r" + labelR + padding
-- TODO: factor out runtime weights
outsideOf [GPI l@("Text", _), GPI c@("Circle", _), Val (FloatV weight)] =
    weight * outsideOf [GPI l, GPI c]

overlapping :: ConstrFn
overlapping [GPI xset@("Circle", _), GPI yset@("Circle", _)] =
    looseIntersect [[getX xset, getY xset, getNum xset "r"], [getX yset, getY yset, getNum yset "r"]]
overlapping [GPI xset@("Square", _), GPI yset@("Circle", _)] =
    looseIntersect [[getX xset, getY xset, 0.5 * getNum xset "side"], [getX yset, getY yset, getNum yset "r"]]
overlapping [GPI xset@("Circle", _), GPI yset@("Square", _)] =
    looseIntersect [[getX xset, getY xset, getNum xset "r"], [getX yset, getY yset, 0.5 * getNum yset "side"]]
overlapping [GPI xset@("Square", _), GPI yset@("Square", _)] =
    looseIntersect [[getX xset, getY xset, 0.5 * getNum xset "side"], [getX yset, getY yset, 0.5 * getNum yset "side"]]

looseIntersect :: (Autofloat a) => [[a]] -> a
looseIntersect [[x1, y1, s1], [x2, y2, s2]] = dist (x1, y1) (x2, y2) - (s1 + s2 - 10)

disjoint :: ConstrFn
disjoint [GPI xset@("Circle", _), GPI yset@("Circle", _)] =
    noIntersect [[getX xset, getY xset, getNum xset "r"], [getX yset, getY yset, getNum yset "r"]]

disjoint [GPI xset@("Square", _), GPI yset@("Square", _)] =
    noIntersect [[getX xset, getY xset, 0.5 * getNum xset "side"], [getX yset, getY yset, 0.5 * getNum yset "side"]]

-- For horizontally collinear line segments only
-- with endpoints (si, ei), assuming si < ei (e.g. enforced by some other constraint)
-- Make sure the closest endpoints are separated by some padding
disjoint [GPI o1, GPI o2] =
    if linelike o1 && linelike o2 then
        let (start1, end1, start2, end2) =
                (fst $ getPoint "start" o1, fst $ getPoint "end" o1,
                 fst $ getPoint "start" o2, fst $ getPoint "end" o2) -- Throw away y coords
            padding = 30 -- should be > 0
            -- Six cases for two intervals: disjoint [-] (-), overlap (-[-)-], contained [-(-)-], and swapping the intervals
            -- Assuming si < ei, we can just push away the closest start and end of the two intervals
            distA = unsignedDist end1 start2
            distB = unsignedDist end2 start1
        in if distA <= distB
           then end1 + padding - start2 -- Intervals separated by padding (original condition: e1 + c < s2)
           else end2 + padding - start1 -- e2 + c < s1
    else error "expected two linelike GPIs in `disjoint`"
    where unsignedDist :: (Autofloat a) => a -> a -> a
          unsignedDist x y = abs $ x - y

-- exterior point method constraint: no intersection (meaning also no subset)
noIntersect :: (Autofloat a) => [[a]] -> a
noIntersect [[x1, y1, s1], [x2, y2, s2]] = -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset where offset = 10

--------------------------------------------------------------------------------
-- Default functions for every shape

defaultConstrsOf :: ShapeTypeStr -> [FuncName]
defaultConstrsOf "Text"  = []
defaultConstrsOf "Curve" = []
-- defaultConstrsOf "Line" = []
defaultConstrsOf _ = [] -- [ "minSize", "maxSize" ]
                 -- TODO: remove? these fns make the optimization too hard to solve sometimes
defaultObjFnsOf :: ShapeTypeStr -> [FuncName]
defaultObjFnsOf _ = [] -- NOTE: not used yet

--------------------------------------------------------------------------------
-- Errors
noFunctionError n = error ("Cannot find function \"" ++ n ++ "\"")
noSignatureError n = error ("Cannot find signatures defined for function \"" ++ n ++ "\"")
sigMismatchError n sig argTypes =
    error ("Invalid arguments for function \"" ++ n
        ++ "\". Passed in:\n" ++ show argTypes
        ++ "\nPredefined signature is: " ++ show sig)
noMatchedSigError n sigs argTypes =
    error ("Cannot find matching signatures defined for function \"" ++ n
        ++ "\". Passed in:\n" ++ show argTypes
        ++ "\nPossible signatures are: " ++ sigStrs)
    where sigStrs = concatMap ((++ "\n") . show) sigs

--------------------------------------------------------------------------------
-- DEBUG: main function
--
-- main :: IO ()
-- main = do
--     -- let c = Circle
--     -- let c = Arrow :: ShapeT
--     print $ toJSON (ColorV black :: Value Color)
--     print $ defaultShapeOf circType
--     print $ invokeComp "rgba" [Val (FloatV 1), Val (FloatV 0.0), Val (FloatV 0.0), Val (FloatV 0.0)] compSignatures
--     print $ invokeComp "rgba" [Val (StrV "Wrong arg"), Val (FloatV 0.0), Val (FloatV 0.0), Val (FloatV 0.0)] compSignatures
--     print $ invokeConstr "at" [GPI exampleCirc, Val (IntV 1), Val (FloatV 0.0) ] constrSignatures
