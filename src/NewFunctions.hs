-- {-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module NewFunctions where

import Utils
import System.Random
import Debug.Trace
import ShapeDef
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
import           Data.List                          (nub, sort)
import           System.Random.Shuffle
import qualified Data.Map.Strict as M
import qualified Data.MultiMap as MM

-- genShapeType $ shapeTypes shapeDefs
-- deriving instance Show ShapeType

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
type CompFnOn   a = [ArgVal a] -> ArgVal a
type ObjFn    = forall a. (Autofloat a) => [ArgVal a] -> a
type ConstrFn = forall a. (Autofloat a) => [ArgVal a] -> a
type CompFn   = forall a. (Autofloat a) => [ArgVal a] -> ArgVal a

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


--------------------------------------------------------------------------------
-- Computations
compDict :: forall a. (Autofloat a) => M.Map String (CompFnOn a)
compDict = M.fromList
    [
        ("rgba", rgba),
        ("bboxWidth", bboxWidth),
        ("bboxHeight", bboxHeight),
        ("intersectionX", intersectionX),
        ("intersectionY", intersectionY),
        ("midpointX", midpointX),
        ("midpointY", midpointY),
        ("len", len),
        ("computeSurjectionLines", computeSurjectionLines),
        ("lineLeft", lineLeft),
        ("lineRight", lineRight),
        ("norm_", norm_), -- type: any two GPIs with centers (getX, getY)
        ("bbox", noop), -- TODO
        ("sampleMatrix", noop), -- TODO
        ("sampleReal", noop), -- TODO
        ("sampleVectorIn", noop), -- TODO
        ("intersection", noop), -- TODO
        ("midpoint", noop), -- TODO
        ("determinant", noop), -- TODO
        ("apply", noop) -- TODO
    ] -- TODO: port existing comps

compSignatures :: CompSignatures
compSignatures = M.fromList
    [
        ("rgba",
            ([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],
              ValueT ColorT)),
        ("intersectionX", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("intersectionY", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("bboxHeight", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("bboxWidth", ([GPIType "Arrow", GPIType "Arrow"], ValueT FloatT)),
        ("len", ([GPIType "Arrow"], ValueT FloatT)),
        ("computeSurjectionLines", ([ValueT IntT, GPIType "Line", GPIType "Line", GPIType "Line", GPIType "Line"], ValueT PathT)),
        ("lineLeft", ([ValueT FloatT, GPIType "Arrow", GPIType "Arrow"], ValueT PathT))
        -- ("len", ([GPIType "Arrow"], ValueT FloatT))
        -- ("bbox", ([GPIType "Arrow", GPIType "Arrow"], ValueT StrT)), -- TODO
        -- ("sampleMatrix", ([], ValueT StrT)), -- TODO
        -- ("sampleVectorIn", ([], ValueT StrT)), -- TODO
        -- ("intersection", ([], ValueT StrT)), -- TODO
        -- ("midpoint", ([], ValueT StrT)), -- TODO
        -- ("determinant", ([], ValueT StrT)), -- TODO
        -- ("apply", ([], ValueT StrT)) -- TODO
    ]

invokeComp :: (Autofloat a) =>
    FuncName -> [ArgVal a] -> CompSignatures -> ArgVal a
invokeComp n args sigs =
    let (argTypes, retType) =
            fromMaybe (noSignatureError n) (M.lookup n compSignatures)
        args'  = checkArgs args argTypes n
        f      = fromMaybe (noFunctionError n) (M.lookup n compDict)
        ret    = f args'
    in if checkReturn ret retType then ret else
        error ("invalid return value \"" ++ show ret ++ "\" of computation \"" ++ show n ++ "\". expected type is \"" ++ show retType ++ "\"")

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
        ("sameHeight", sameHeight),
        ("equal", equal)

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
                ("smallerThan", smallerThan),
                ("minSize", minSize),
                ("maxSize", maxSize),
                ("outsideOf", outsideOf),
                ("nonOverlapping", nonOverlapping)
                -- ("lessThan", lessThan)
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
        ("nonOverlapping", [GPIType "Circle", GPIType "Circle"])
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

-- TODO: use the rng in state
compRng :: StdGen
compRng = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

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

-- Computes the surjection to lie inside a bounding box defined by the corners of a box
-- defined by four straight lines, assuming their lower/left coordinates come first.
-- Their intersections give the corners.
computeSurjectionLines :: CompFn
computeSurjectionLines args = Val $ PathV $ computeSurjectionLines' compRng args

computeSurjectionLines' :: (Autofloat a) => StdGen -> [ArgVal a] -> [Pt2 a]
computeSurjectionLines' g args@[Val (IntV n), GPI left@("Line", _), GPI right@("Line", _), GPI bottom@("Line", _), GPI top@("Line", _)] =
    let lower_left = (getNum left "startX", getNum bottom "startY") in
    let top_right = (getNum right "startX", getNum top "startY") in
    computeSurjection g n lower_left top_right
-- Assuming left and bottom are perpendicular and share one point
computeSurjectionLines' g [Val (IntV n), GPI left@("Arrow", _), GPI bottom@("Arrow", _)] = 
    let lower_left = (getNum left "startX", getNum left "startY") in
    let top_right = (getNum bottom "endX", getNum left "endY") in
    computeSurjection g n lower_left top_right

computeSurjection :: Autofloat a => StdGen -> Integer -> Pt2 a -> Pt2 a -> [Pt2 a]
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
        in zip xs_increasing ys_perm -- len xs == len ys

-- calculates a line (of two points) intersecting the first axis, stopping before it leaves bbox of second axis
-- TODO rename lineLeft and lineRight
-- assuming a1 horizontal and a2 vertical, respectively
lineLeft :: CompFn
lineLeft [Val (FloatV lineFrac), GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let a1_start = getNum a1 "startX" in
    let a1_len = abs (getNum a1 "endX" - a1_start) in
    let xpos = a1_start + lineFrac * a1_len in
    Val $ PathV [(xpos, getNum a1 "startY"), (xpos, getNum a2 "endY")]

-- assuming a1 vert and a2 horiz, respectively
-- can this be written in terms of lineLeft?
lineRight :: CompFn
lineRight [Val (FloatV lineFrac), GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let a1_start = getNum a1 "startY" in
    let a1_len = abs (getNum a1 "endY" - a1_start) in
    let ypos = a1_start + lineFrac * a1_len in
    Val $ PathV [(getNum a2 "startX", ypos), (getNum a2 "endX", ypos)]

rgba :: CompFn
rgba [Val (FloatV r), Val (FloatV g), Val (FloatV b), Val (FloatV a)] =
    Val (ColorV $ makeColor' r g b a)

linePts, arrowPts :: (Autofloat a) => Shape a -> (a, a, a, a)
linePts = arrowPts
arrowPts a = (getNum a "startX", getNum a "startY", getNum a "endX", getNum a "endY")

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)

intersectionX :: CompFn
intersectionX [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a1
        (x2, y2, x3, y3) = arrowPts a2
        det = (x0 - x1) * (y2 - y3) - (y0 - y1) * (x2 - x3)
    in Val $ FloatV $
       if det == 0 then infinity
       else (x0*y1 - y0*x1)*(x2 - x3) - (x0 - x1)*(x2*x3 - y2*y3) / det
intersectionY :: CompFn
intersectionY [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a1
        (x2, y2, x3, y3) = arrowPts a2
        det = (x0 - x1) * (y2 - y3) - (y0 - y1) * (x2 - x3)
    in Val $ FloatV $
       if det == 0 then infinity
       else (x0*y1 - y0*x1)*(x2 - x3) - (y0 - y1)*(x2*x3 - y2*y3) / det

bboxHeight :: CompFn
bboxHeight [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let ys@[y0, y1, y2, y3] = getYs a1 ++ getYs a2
        (ymin, ymax) = (minimum ys, maximum ys)
    in Val $ FloatV $ abs $ ymax - ymin
    where getYs a = [getNum a "startY", getNum a "endY"]

bboxWidth :: CompFn
bboxWidth [GPI a1@("Arrow", _), GPI a2@("Arrow", _)] =
    let xs@[x0, x1, x2, x3] = getXs a1 ++ getXs a2
        (xmin, xmax) = (minimum xs, maximum xs)
    in Val $ FloatV $ abs $ xmax - xmin
    where getXs a = [getNum a "startX", getNum a "endX"]

len :: CompFn
len [GPI a@("Arrow", _)] =
    let (x0, y0, x1, y1) = arrowPts a
    in Val $ FloatV $ dist (x0, y0) (x1, y1)

midpointX :: CompFn
midpointX [GPI a@("Arrow", _)] =
    let (x0, x1) = (getNum a "startX", getNum a "endX")
    in Val $ FloatV $ x1 - x0 / 2

midpointY :: CompFn
midpointY [GPI a@("Arrow", _)] =
    let (y0, y1) = (getNum a "startY", getNum a "endY")
    in Val $ FloatV $ y1 - y0 / 2

norm_ :: CompFn
norm_ [Val (FloatV x), Val (FloatV y)] = Val $ FloatV $ norm [x, y]

noop :: CompFn
noop [] = Val (StrV "TODO")

--------------------------------------------------------------------------------
-- Objective Functions

near :: ObjFn
near [GPI o1, GPI o2] = distsq (getX o1, getY o1) (getX o2, getY o2)

center :: ObjFn
center [GPI o] = tr "center: " $ distsq (getX o, getY o) (0, 0)

centerX :: ObjFn
centerX [Val (FloatV x)] = tr "centerX" $ x^2

-- TODO move this elsewhere? (also applies to polyline)
bezierBbox :: (Autofloat a) => Shape a -> ((a, a), (a, a)) -- poly Point type?
bezierBbox cb = let path = getPath cb
                    (xs, ys) = (map fst path, map snd path)
                    lower_left = (minimum xs, minimum ys)
                    top_right = (maximum xs, maximum ys) in
                (lower_left, top_right)

-- | 'sameCenter' encourages two objects to center at the same point
sameCenter :: ObjFn
sameCenter [GPI a, GPI b] = (getX a - getX b)^2 + (getY a - getY b)^2

centerLabel :: ObjFn
centerLabel [GPI curve, GPI text]
    | curve `is` "Curve" && text `is` "Text" =
        let ((lx, ly), (rx, ry)) = bezierBbox curve
            (xmargin, ymargin) = (-10, 30)
            midbez = ((lx + rx) / 2 + xmargin, (ly + ry) / 2 + ymargin) in
        distsq midbez (getX text, getY text)
centerLabel [GPI p, GPI l]
    | p `is` "AnchorPoint" && l `is` "Text" =
        let [px, py, lx, ly] = [getX p, getY p, getX l, getY l] in
        (px + 10 - lx)^2 + (py + 20 - ly)^2 -- Top right from the point
-- -- TODO: depends on orientation of arrow
centerLabel [GPI arr, GPI text]
    | arr `is` "Arrow" && text `is` "Text" =
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
                [spacing * getNum text1 "h", negate $ spacing * getNum text2 "h"]

centerArrow [GPI arr@("Arrow", _), GPI text@("Text", _), GPI circ@("Circle", _)] =
            _centerArrow arr [getX text, getY text] [getX circ, getY circ]
                [1.5 * getNum text "w", negate $ spacing * getNum circ "radius"]

spacing :: (Autofloat a) => a
spacing = 1.1 -- TODO: arbitrary

_centerArrow :: Autofloat a => Shape a -> [a] -> [a] -> [a] -> a
_centerArrow arr@("Arrow", _) s1@[x1, y1] s2@[x2, y2] [o1, o2] =
    let vec  = [x2 - x1, y2 - y1] -- direction the arrow should point to
        dir = normalize vec -- direction the arrow should point to
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
topRightOf [GPI l@("Text", _), GPI s@("Rectangle", _)] = dist (getX l, getY l) (getX s + 0.5 * getNum s "w", getY s + 0.5 * getNum s "h")

topLeftOf :: ObjFn
topLeftOf [GPI l@("Text", _), GPI s@("Square", _)] = dist (getX l, getY l) (getX s - 0.5 * getNum s "side", getY s - 0.5 * getNum s "side")
topLeftOf [GPI l@("Text", _), GPI s@("Rectangle", _)] = dist (getX l, getY l) (getX s - 0.5 * getNum s "w", getY s - 0.5 * getNum s "h")

nearHead :: ObjFn
nearHead [GPI arr@("Arrow", _), GPI lab@("Text", _), Val (FloatV xoff), Val (FloatV yoff)] =
    let end = (getNum arr "endX", getNum arr "endY") in -- arrowhead
    let offset = (xoff, yoff) in
    distsq (getX lab, getY lab) (end `plus2` offset)
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

--------------------------------------------------------------------------------
-- Constraint Functions

at :: ConstrFn
at [GPI o, Val (FloatV x), Val (FloatV y)] =
    (getX o - x)^2 + (getY o - y)^2

lessThan :: ConstrFn
lessThan [] = 0.0 -- TODO

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
    dist (getX l, getY l) (getX s, getY s) - getNum s "w" / 2 + getNum l "w"
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
        (lx, ly) = (x - side / 2, y - side / 2)
        (rx, ry) = (x + side / 2, y + side / 2)
    in inRange startX lx rx
        + inRange startY ly ry
        + inRange endX lx rx
        + inRange endY ly ry
contains [GPI rt@("Rectangle", _), GPI ar@("Arrow", _)] =
    let (startX, startY, endX, endY) = arrowPts ar
        (x, y) = (getX rt, getY rt)
        (w, h) = (getNum rt "w", getNum rt "h")
        (lx, ly) = (x - w / 2, y - h / 2)
        (rx, ry) = (x + w / 2, y + h / 2)
    in inRange startX lx rx
        + inRange startY ly ry
        + inRange endX lx rx
        + inRange endY ly ry

inRange a l r
    | a < l  = (a-l)^2
    | a > r  = (a-r)^2
    | a == r = 0


-- TODO: is the "Point type still there?"
-- contains [GPI set@("Circle", _), P' GPI pt@("", _)] = dist (getX pt, getX pt) (getX set, getY set) - 0.5 * r' set
-- TODO: only approx
-- contains [S' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (getX set, getX set) - 0.4 * side' set
-- FIXME: doesn't work
-- contains [E' GPI set@("", _), P' GPI pt@("", _)] =
--     dist (getX pt, getX pt) (xe' set, getX set) - max (rx' set) (ry' set) * 0.9

maxSize :: ConstrFn
-- TODO: why do we need `r2f` now? Didn't have to before
limit = max canvasWidth canvasHeight
maxSize [GPI c@("Circle", _)] = getNum c "r" - r2f (limit / 6)
maxSize [GPI s@("Square", _)] = getNum s "side" - r2f (limit  / 3)
maxSize [GPI r@("Rectangle", _)] =
    let max_side = max (getNum r "w") (getNum r "h")
    in max_side - r2f (limit  / 3)
maxSize [GPI im@("Image", _)] =
    let max_side = max (getNum im "lengthX") (getNum im "lengthY")
    in max_side - r2f (limit / 3)
maxSize [GPI e@("Ellipse", _)] = max (getNum e "r") (getNum e "r") - r2f (limit  / 3)
maxSize _ = 0 -- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented
-- maxSize [GPI ar@("Arc", _)] = sizear' ar - limit  / 3
-- maxSize [GPI pa@("Parallelogram", _)] [] =
--     let max_side = max (sizeXpa' pa) (sizeYpa' pa) in
--     max_side - limit  / 3

minSize :: ConstrFn
minSize [GPI c@("Circle", _)] = 20 - getNum c "r"
minSize [GPI s@("Square", _)] = 20 - getNum s "side"
minSize [GPI r@("Rectangle", _)] =
    let min_side = min (getNum r "w") (getNum r "h")
    in 20 - min_side
minSize [GPI e@("Ellipse", _)] = 20 - min (getNum e "r") (getNum e "r")
minSize _ = 0 -- NOTE/HACK: all objects will have min/max size attached, but not all of them are implemented

-- minSize [AR' ar] _ = 2.5 - sizear' ar
-- minSize [IM' im] [] = let min_side = min (sizeXim' im) (sizeYim' im) in 20 - min_side
-- minSize [PA' pa] [] = let min_side = min (sizeXpa' pa) (sizeYpa' pa) in 20 - min_side

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
    let labelR = max (getNum l "w") (getNum l "h")
    in -dist (getX l, getX l) (getX c, getY c) + getNum c "r" + labelR
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

nonOverlapping :: ConstrFn
nonOverlapping [GPI xset@("Circle", _), GPI yset@("Circle", _)] =
    noIntersect [[getX xset, getY xset, getNum xset "r"], [getX yset, getY yset, getNum yset "r"]]

-- exterior point method constraint: no intersection (meaning also no subset)
noIntersect :: (Autofloat a) => [[a]] -> a
noIntersect [[x1, y1, s1], [x2, y2, s2]] = -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset where offset = 10

--------------------------------------------------------------------------------
-- Default functions for every shape

defaultConstrsOf :: ShapeTypeStr -> [FuncName]
defaultConstrsOf "Text"  = []
defaultConstrsOf _ = [ "minSize", "maxSize" ]
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
