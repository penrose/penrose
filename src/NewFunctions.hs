-- {-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module NewFunctions where

import Utils
import Debug.Trace
import ShapeDef
import Data.Aeson (toJSON)
import Data.Maybe (fromMaybe)
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
    -- in f args
    in f args'


--------------------------------------------------------------------------------
-- Computations
compDict :: forall a. (Autofloat a) => M.Map String (CompFnOn a)
compDict = M.fromList
    [
        ("rgba", rgba),
        ("bbox", noop), -- TODO
        ("len", noop), -- TODO
        ("sampleMatrix", noop), -- TODO
        ("sampleVectorIn", noop), -- TODO
        ("intersection", noop), -- TODO
        ("midpoint", noop), -- TODO
        ("mulV", noop), -- TODO
        ("determinant", noop), -- TODO
        ("addV", noop), -- TODO
        ("apply", noop) -- TODO
    ] -- TODO: port existing comps

compSignatures :: CompSignatures
compSignatures = M.fromList
    [
        ("rgba",
            ([ValueT FloatT, ValueT FloatT, ValueT FloatT, ValueT FloatT],
              ValueT ColorT)),
        ("bbox", ([], ValueT StrT)), -- TODO
        ("len", ([], ValueT StrT)), -- TODO
        ("sampleMatrix", ([], ValueT StrT)), -- TODO
        ("sampleVectorIn", ([], ValueT StrT)), -- TODO
        ("intersection", ([], ValueT StrT)), -- TODO
        ("midpoint", ([], ValueT StrT)), -- TODO
        ("mulV", ([], ValueT StrT)), -- TODO
        ("determinant", ([], ValueT StrT)), -- TODO
        ("addV", ([], ValueT StrT)), -- TODO
        ("apply", ([], ValueT StrT)) -- TODO
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

-- | 'objFuncDict' stores a mapping from the name of objective functions to the actual implementation
objFuncDict :: forall a. (Autofloat a) => M.Map String (ObjFnOn a)
objFuncDict = M.fromList
    [
        ("near", near),
        ("center", center),
        ("centerX", centerX),
        ("centerLabel", centerLabel),
        ("centerArrow", centerArrow)
{-      ("centerLine", centerLine),
        ("increasingX", increasingX),
        ("increasingY", increasingY),
        ("horizontal", horizontal),
        ("upright", upright),
        ("xInRange", xInRange),
        ("yInRange", yInRange),
        ("orthogonal", orthogonal),
        ("toLeft", toLeft),
        ("above", above),
        ("between", between),
        ("sameHeight", sameHeight),
        ("sameX", sameX),
        ("equal", equal),
        ("ratioOf", ratioOf),
        ("topRightOf", topRightOf),
        ("sameY", sameY),
        -- ("sameX", (*) 0.6 `compose2` sameX),
        -- ("sameX", (*) 0.2 `compose2` sameX),
        ("sameCenter", sameCenter),
        ("repel", (*)  900000  `compose2` repel),
        -- ("repel", (*)  1000000  `compose2` repel),
        -- ("repel", (*)  10000  `compose2` repel),
        -- ("repel", repel),
        ("outside", outside),
        ("nearEndVert", nearEndVert),
        ("nearEndHoriz", nearEndHoriz),
        ("nearHead", nearHead) -}
    ]

objSignatures :: OptSignatures
objSignatures = MM.fromList
    [
        ("near", [GPIType "Circle", GPIType "Circle"]),
        ("center", [AnyGPI]),
        ("centerX", [ValueT FloatT])
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
                ("lessThan", lessThan)
            ]

constrSignatures :: OptSignatures
constrSignatures = MM.fromList
    [
        ("at", [AnyGPI, ValueT FloatT, ValueT FloatT]),
        ("contains", [GPIType "Circle", GPIType "Circle"]),
        ("contains", [GPIType "Circle", GPIType "Text"]),
        ("contains", [GPIType "Square", GPIType "Text"]),
        ("contains", [GPIType "Circle", GPIType "Rectangle"]),
        ("minSize", [AnyGPI]),
        ("maxSize", [AnyGPI]),
        ("smallerThan", [GPIType "Circle", GPIType "Circle"]),
        ("outsideOf", [GPIType "Text", GPIType "Circle"]),
        ("lessThan", []) --TODO
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

matchWith args sig = and $ zipWith checkArg args sig

checkArgs :: (Autofloat a) => [ArgVal a] -> [ArgType] -> String -> [ArgVal a]
checkArgs arguments signature n = if arguments `matchWith` signature
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

rgba :: CompFn
rgba [Val (FloatV r), Val (FloatV g), Val (FloatV b), Val (FloatV a)] =
    Val (ColorV $ makeColor' r g b a)
-- bbox :: CompFn
-- bbox =
-- len :: CompFn
-- len =
-- sampleMatrix :: CompFn
-- sampleMatrix =
-- sampleVectorIn :: CompFn
-- sampleVectorIn =
-- intersection :: CompFn
-- intersection =
-- midpoint :: CompFn
-- midpoint =
-- mulV :: CompFn
-- mulV =
-- determinant :: CompFn
-- determinant =
-- addV :: CompFn
-- addV =
-- apply :: CompFn
-- apply =
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

-- centerLabel [CB' a, L' l] [mag] = -- use the float input?
--                 let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
--                     (mx, my) = midpoint (sx, sy) (ex, ey)
--                     (lx, ly) = (xl' l, yl' l) in
                -- (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point

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

-- | `centerArrow` positions an arrow between two objects, with some spacing
centerArrow :: ObjFn

centerArrow [GPI arr@("Arrow", _), GPI sq1@("Square", _), GPI sq2@("Square", _)] =
            _centerArrow arr [getX sq1, getY sq1] [getX sq2, getY sq2]
                [spacing + (halfDiagonal . flip getNum "sideLength") sq1, negate $ spacing + (halfDiagonal . flip getNum "sideLength") sq2]

centerArrow [GPI arr@("Arrow", _), GPI sq@("Square", _), GPI circ@("Circle", _)] =
            _centerArrow arr [getX sq, getY sq] [getX circ, getY circ]
                [spacing + (halfDiagonal . flip getNum "sideLength") sq, negate $ spacing + getNum circ "radius"]

centerArrow [GPI arr@("Arrow", _), GPI circ@("Circle", _), GPI sq@("Square", _)] =
            _centerArrow arr [getX circ, getY circ] [getX sq, getY sq]
                [spacing + getNum circ "radius", negate $ spacing + (halfDiagonal . flip getNum "sideLength") sq]

centerArrow [GPI arr@("Arrow", _), GPI circ1@("Circle", _), GPI circ2@("Circle", _)] =
            _centerArrow arr [getX circ1, getY circ1] [getX circ2, getY circ2]
                [ spacing * getNum circ1 "radius", negate $ spacing * getNum circ2 "radius"]

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

centerArrow o = error ("CenterMap: unsupported arguments: " ++ show o)

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

--------------------------------------------------------------------------------
-- Constraint Functions

at :: ConstrFn
at [GPI o, Val (FloatV x), Val (FloatV y)] =
    (getX o - x)^2 + (getY o - y)^2

lessThan :: ConstrFn
lessThan [] = 0.0 -- TODO

contains :: ConstrFn
contains [GPI o1@("Circle", _), GPI o2@("Circle", _)] =
    dist (getX o1, getY o1) (getX o2, getY o2) - (getNum o2 "r" - getNum o1 "r")
contains [GPI c@("Circle", _), GPI rect@("Rectangle", _)] =
    let (x, y, w, h)     =
            (getX rect, getY rect, getNum rect "sizeX", getNum rect "sizeY")
        [x0, x1, y0, y1] = [x - w/2, x + w/2, y - h/2, y + h/2]
        pts              = [(x0, y0), (x0, y1), (x1, y0), (x1, y1)]
        (cx, cy, radius) = (getX c, getY c, getNum c "r")
    in sum $ map (\(a, b) -> max 0 $ dist (cx, cy) (a, b) - radius) pts
        -- dist (cx, cy) (x, y)
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

-- contains [C' outc, C' inc] [FloatV padding] = strictSubset [[xc' inc, yc' inc, r' inc + padding], [xc' outc, yc' outc, r' outc]]
-- contains [S' outc, S' inc] [] = strictSubset
--     [[xs' inc, ys' inc, 0.5 * side' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
-- contains [S' outc, C' inc] [] = strictSubset [[xc' inc, yc' inc, r' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
-- contains [S' outc, C' inc] [FloatV padding] = strictSubset [[xc' inc, yc' inc, r' inc + padding], [xs' outc, ys' outc, 0.5 * side' outc]]
-- contains [C' outc, S' inc] [] = strictSubset
--     [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
-- contains [C' set, P' pt] [] =
--         dist (xp' pt, yp' pt) (xc' set, yc' set) - 0.5 * r' set
-- contains [S' set, P' pt] [] =
--     dist (xp' pt, yp' pt) (xs' set, ys' set) - 0.4 * side' set
-- -- TODO: only approx
-- contains [E' set, P' pt] [] =
--     dist (xp' pt, yp' pt) (xe' set, ye' set) - max (rx' set) (ry' set) * 0.9
-- contains [C' set, L' label] _ =
--     let res = dist (xl' label, yl' label) (xc' set, yc' set) - r' set + max (wl' label) (hl' label) in
--     if res < 0 then 0 else res
-- -- FIXME: doesn't work
-- contains [E' set, L' label] [] =
--     dist (xl' label, yl' label) (xe' set, ye' set) -  max (rx' set) (ry' set) + wl' label
-- contains [S' sq, A' ar] [] = let
--                              lx = (xs' sq) - (side' sq)/2
--                              rx = (xs' sq) + (side' sq)/2
--                              ly = (ys' sq) - (side' sq)/2
--                              ry = (ys' sq) + (side' sq)/2
--                              ret = (isInRange (startx' ar) lx rx) + (isInRange (endx' ar) lx rx) + (isInRange (starty' ar) ly ry) + (isInRange (endy' ar) ly ry)
--                             in ret

maxSize :: ConstrFn
-- TODO: why do we need `r2f` now? Didn't have to before
limit = max canvasWidth canvasHeight
maxSize [GPI c@("Circle", _)] = getNum c "r" - r2f (limit / 6)
maxSize [GPI s@("Square", _)] = getNum s "side" - r2f (limit  / 3)
maxSize [GPI r@("Rectangle", _)] =
    let max_side = max (getNum r "sizeX") (getNum r "sizeY")
    in max_side - r2f (limit  / 3)
-- maxSize [GPI ar@("Arrow", _)] = sizear' ar - limit  / 3
-- maxSize [GPI im@("Image", _)] =
--     let max_side = max (sizeXim' im) (sizeYim' im)
--     in max_side - limit  / 3
-- maxSize [GPI pa@("Parallelogram", _)] [] =
--     let max_side = max (sizeXpa' pa) (sizeYpa' pa) in
--     max_side - limit  / 3
-- maxSize [GPI e@("Ellipse", _)] [] = max (ry' e) (rx' e) - limit  / 3

minSize :: ConstrFn
minSize [GPI c@("Circle", _)] = 20 - getNum c "r"
minSize [GPI s@("Square", _)] = 20 - getNum s "side"
minSize [GPI r@("Rectangle", _)] =
    let min_side = min (getNum r "sizeX") (getNum r "sizeY")
    in 20 - min_side
-- minSize [AR' ar] _ = 2.5 - sizear' ar
-- minSize [IM' im] [] = let min_side = min (sizeXim' im) (sizeYim' im) in
--                    20 - min_side
-- minSize [PA' pa] [] = let min_side = min (sizeXpa' pa) (sizeYpa' pa) in
--                    20 - min_side
-- minSize [E' e] [] = 20 - min (ry' e) (rx' e)

smallerThan  :: ConstrFn
smallerThan [GPI inc@("Circle", _), GPI outc@("Circle", _)] =  getNum inc "r" - getNum outc "r" - 0.4 * getNum outc "r" -- TODO: taking this as a parameter?

outsideOf :: ConstrFn
outsideOf [GPI l@("Text", _), GPI c@("Circle", _)] =
    let labelR = max (getNum l "w") (getNum l "h")
    in -dist (getX l, getX l) (getX c, getY c) + getNum c "r" + labelR

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
