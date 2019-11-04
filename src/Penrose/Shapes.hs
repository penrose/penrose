{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# OPTIONS_HADDOCK prune #-}

module Penrose.Shapes where

import           Data.Aeson         (FromJSON, ToJSON, toJSON)
import           Data.List          (foldl')
import qualified Data.Map.Strict    as M
import           Data.Maybe         (fromMaybe)
import           Debug.Trace
import           GHC.Generics
import           Penrose.Transforms
import           Penrose.Util
import           System.Random

default (Int, Float)

-- import Language.Haskell.TH
--
-- type Autofloat a = (RealFloat a, Floating a, Real a, Show a, Ord a)
-- type Pt2 a = (a, a)
-- genShapeType :: [ShapeTypeStr] -> Q [Dec]
-- genShapeType shapeTypes = do
--     let mkconstructor n = NormalC (mkName n) []
--         constructors    = map mkconstructor shapeTypes
--     return [DataD [] (mkName "ShapeT") [] Nothing constructors []]
--------------------------------------------------------------------------------
-- Types
-- | shape can have multiple pieces, e.g. multiple "M"s
type PathData a = [Path' a]

-- | TODO
-- NOTE: the order of the elements is important
data Path' a
  = Closed [Elem a] -- ^  "Z"
  | Open [Elem a] -- ^  no "Z"
  deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Path' a)

instance (ToJSON a) => ToJSON (Path' a)

-- | TODO
-- | Arc { x, y, sweep1, … }  -- "A" TODO
data Elem a
  = Pt (Pt2 a) -- ^ Replace "M," "L", "H", "V"
  | CubicBez (Pt2 a, Pt2 a, Pt2 a) -- ^ "C": two control pts, 1 endpt
  | CubicBezJoin (Pt2 a, Pt2 a) -- ^ "S": 1 control pt, 1 endpt
  | QuadBez (Pt2 a, Pt2 a) -- ^ "Q": 1 control pt, 1 endpt
  | QuadBezJoin (Pt2 a) -- ^ "T": 1 endpt
  deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Elem a)

instance (ToJSON a) => ToJSON (Elem a)

-------------- Types
-- | possible values in the argument of computation, constraint, or objectives
-- Used for type checking functions
data ArgVal a
  = GPI (Shape a)
  | Val (Value a)
  deriving (Eq, Show, Generic)

data ArgType
  = GPIType ShapeTypeStr
  | ValueT ValueType
  | OneOf [ShapeTypeStr]
  | AnyGPI
  deriving (Eq, Show)

-- | types of fully evaluated values in Style
data ValueType
  = FloatT
  | IntT
  | BoolT
  | StrT
  | PtT
  | PtListT
  | ListT
  | TupT
  | LListT
  | PathDataT
  | ColorT
  | FileT
  | StyleT
  | TransformT
  | HMatrixT
  | MapT
  | PolygonT
  deriving (Eq, Show)

-- | fully evaluated values in Style
data Value a
  = FloatV a -- ^ floating point number
  | IntV Int -- ^ integer
  | BoolV Bool -- ^ boolean value
  | StrV String -- ^ string literal
  | PtV (Pt2 a) -- ^ point in R^2
  | PathDataV (PathData a) -- ^ path commands
  | PtListV [Pt2 a] -- ^ a list of points
  | ColorV Color -- ^ an RGBA color value
  | FileV String -- ^ path for image
  | StyleV String -- ^ dotted, etc.
  | ListV [a] -- ^ a list of floats
  | TupV (a, a) -- ^ a tuple of floats
  | LListV [[a]] -- ^ a 2D list of floats
  | HMatrixV (HMatrix a) -- ^ single transformation (homogeneous transformation)
  | PolygonV (Polygon a) -- ^ multiple shapes with holes
  deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Value a)

instance (ToJSON a) => ToJSON (Value a)

instance (FromJSON a) => FromJSON (ArgVal a)

instance (ToJSON a) => ToJSON (ArgVal a)

-- | returns the type of a 'Value'
typeOf :: (Autofloat a) => Value a -> ValueType
typeOf v =
  case v of
    FloatV _    -> FloatT
    IntV _      -> IntT
    BoolV _     -> BoolT
    StrV _      -> StrT
    PtV _       -> PtT
    PtListV _   -> PtListT
    TupV _      -> TupT
    ListV _     -> ListT
    LListV _    -> LListT
    PathDataV _ -> PathDataT
    ColorV _    -> ColorT
    FileV _     -> FileT
    StyleV _    -> StyleT
    HMatrixV _  -> HMatrixT
    PolygonV _  -> PolygonT

-----------------
toPolymorphics ::
     [Shape Double]
  -> (forall a. (Autofloat a) =>
                  [Shape a])
toPolymorphics = map toPolymorphic

toPolymorphic ::
     Shape Double
  -> (forall a. (Autofloat a) =>
                  Shape a)
toPolymorphic (ctor, properties) = (ctor, M.map toPolyProperty properties)

toPolyArgVal ::
     ArgVal Double
  -> (forall a. (Autofloat a) =>
                  ArgVal a)
toPolyArgVal (GPI x) = GPI $ toPolymorphic x
toPolyArgVal (Val x) = Val $ toPolyProperty x

toPolyProperty ::
     Value Double
  -> (forall a. (Autofloat a) =>
                  Value a)
toPolyProperty v =
  case v
    -- Not sure why these have to be rewritten from scratch...
        of
    FloatV n -> FloatV $ r2f n
    BoolV x -> BoolV x
    StrV x -> StrV x
    IntV x -> IntV x
    PtV p -> PtV $ r2 p
    PtListV xs -> PtListV $ map r2 xs
    ListV xs -> ListV $ map r2f xs
    TupV x -> TupV $ r2 x
    LListV xs -> LListV $ map (map r2f) xs
    ColorV x -> ColorV x
    FileV x -> FileV x
    StyleV x -> StyleV x
    PathDataV es -> PathDataV $ map toPolyPath es
    HMatrixV m ->
      HMatrixV $
      HMatrix
      { xScale = r2f $ xScale m
      , xSkew = r2f $ xSkew m
      , ySkew = r2f $ ySkew m
      , yScale = r2f $ yScale m
      , dx = r2f $ dx m
      , dy = r2f $ dy m
      }
    PolygonV (b, h, bbox, samples) ->
      PolygonV $ (map (map r2) b, map (map r2) h, map2 r2 bbox, map r2 samples)
  where
    r2 (x, y) = (r2f x, r2f y)

toPolyPath ::
     Path' Double
  -> (forall a. (Autofloat a) =>
                  Path' a)
toPolyPath (Closed es) = Closed $ map toPolyElem es
toPolyPath (Open es)   = Open $ map toPolyElem es

toPolyElem ::
     Elem Double
  -> (forall a. (Autofloat a) =>
                  Elem a)
toPolyElem (Pt p)                  = Pt $ r2ft p
toPolyElem (CubicBez (p0, p1, p2)) = CubicBez (r2ft p0, r2ft p1, r2ft p2)
toPolyElem (CubicBezJoin (p0, p1)) = CubicBezJoin (r2ft p0, r2ft p1)
toPolyElem (QuadBez (p0, p1))      = QuadBez (r2ft p0, r2ft p1)
toPolyElem (QuadBezJoin p)         = QuadBezJoin $ r2ft p

r2ft (x, y) = (r2f x, r2f y)

-- | the type string of a shape
type ShapeTypeStr = String

-- | the string identifier of a property
type PropID = String

--------- new
type PropertiesDef2 a = M.Map PropID (ValueType, SampledValue a)

-- | Mutually recursive types because we might want to nest a definition of properties (e.g. A.shape.center.x)
type PropertyValue a = Either (SampledValue a) (PropertiesDef2 a)

-- | A dict storing names, types, and default values of properties
type PropertiesDef1 a = M.Map PropID (ValueType, PropertyValue a)

type ShapeDef' a = (ShapeTypeStr, PropertiesDef1 a)

----------
-- | A dict storing names, types, and default values of properties
type PropertiesDef a = M.Map PropID (ValueType, SampledValue a)

-- | definition of a new shape/graphical primitive
-- TODO: rewrite as a record?
type ShapeDef a = (ShapeTypeStr, PropertiesDef a)

type ShapeDefs a = M.Map ShapeTypeStr (ShapeDef a)

-- | A dictionary storing properties of a Style object, e.g. "startx" for 'Arrow'
-- COMBAK: serializer to JSON
type Properties a = M.Map PropID (Value a)

-- | definition of a new shape/graphical primitive
-- TODO: rewrite as a record? Probably better for serialization
type Shape a = (ShapeTypeStr, Properties a)

--------------------------------------------------------------------------------
-- Shape introspection functions
-- | all of the shape defs supported in the system
shapeDefs :: (Autofloat a) => ShapeDefs a
shapeDefs = M.fromList $ zipWithKey shapeDefList
  where
    zipWithKey = map (\x -> (fst x, x))

shapeDefList :: (Autofloat a) => [ShapeDef a]
shapeDefList =
  [ anchorPointType
  , circType
  , ellipseType
  , arrowType
  , braceType
  , curveType
  , lineType
  , rectType
  , squareType
  , parallelogramType
  , imageType
  , textType
  , arcType
  , rectTransformType
  , polygonType
  , circTransformType
  , curveTransformType
  , lineTransformType
  , squareTransformType
  , imageTransformType
  , ellipseTransformType
  , parallelogramTransformType
  , textTransformType
  ]

-- | retrieve type strings of all shapes
shapeTypes :: (Autofloat a) => ShapeDefs a -> [ShapeTypeStr]
shapeTypes defs = map fst $ M.toList defs

-- | given a type string, find the corresponding shape def
findDef :: (Autofloat a) => ShapeTypeStr -> ShapeDef a
findDef typ = fromMaybe (noShapeError "findDef" typ) (M.lookup typ shapeDefs)

-- | given a shape def, construct a default shape
defaultShapeOf :: (Autofloat a) => StdGen -> ShapeDef a -> (Shape a, StdGen)
defaultShapeOf g (t, propDict) =
  let (properties, g') = sampleProperties g propDict
  in ((t, properties), g')

defaultValueOf ::
     (Autofloat a) => StdGen -> PropID -> ShapeDef a -> (Value a, StdGen)
defaultValueOf g prop (t, propDict) =
  let sampleF =
        snd $
        fromMaybe (noPropError "defaultValueOf" prop t) (M.lookup prop propDict)
  in sampleF g

--------------------------------------------------------------------------------
-- Computing derived properties
-- Initially, a computed property starts with the initial value specified in the shape initialized (e.g. idH)
-- To say that a property is computed: add it to the list in `computedProperties`
-- to say how it is computed: specify the input properties of the shape and write a function that expects them and returns a value
-- Note: DOFs are preserved through this
-- How this feature is implemented: see `computeProperty` in GenOptProblem, which is called by `evalExpr` (as well as anything that calls `evalExpr`, like `evalShape` and `initShape`
-- evalExpr, for a PropertyPath X.f.p, checks if p is in computedProperties for the type of X.f.
-- If so, it evaluates the GPI properties requested by the function, passes the values to the function, and returns the value the function returns
-- If not, then it evaluates the path as usual (by looking up its expr and evaluating it)
-- TODO: move implementation documentation to the wiki and PR
type ComputedValue a = ([Property], [Value a] -> Value a)

computedProperties ::
     (Autofloat a) => M.Map (ShapeTypeStr, Property) (ComputedValue a)
computedProperties =
  M.fromList [] -- TODO: makes things really alow
    -- [ (("RectangleTransform", "transformation"), rectTransformFn)
    -- , (("CircleTransform", "transformation"), circTransformFn)
    -- , (("Polygon", "transformation"), polygonTransformFn)
    -- , (("CurveTransform", "transformation"), polygonTransformFn)
    --                                                  -- Same parameters as polygon
    -- , (("LineTransform", "transformation"), polygonTransformFn)
    --                                                 -- Same parameters as polygon
    -- , (("SquareTransform", "transformation"), squareTransformFn)
    -- , (("ImageTransform", "transformation"), imageTransformFn)
    -- , (("EllipseTransform", "transformation"), ellipseTransformFn)
    -- , (("ParallelogramTransform", "transformation"), parallelogramTransformFn)
    -- , (("TextTransform", "transformation"), textTransformFn)
    -- , (("RectangleTransform", "polygon"), rectPolygonFn)
    -- , (("CircleTransform", "polygon"), circPolygonFn)
    -- , (("CurveTransform", "polygon"), curvePolygonFn)
    -- , (("Polygon", "polygon"), polygonPolygonFn)
    -- , (("LineTransform", "polygon"), linePolygonFn)
    -- , (("SquareTransform", "polygon"), squarePolygonFn)
    -- , (("ImageTransform", "polygon"), imagePolygonFn)
    -- , (("EllipseTransform", "polygon"), ellipsePolygonFn)
    -- , (("ParallelogramTransform", "polygon"), parallelogramPolygonFn)
    -- , (("TextTransform", "polygon"), textPolygonFn)
    -- ]

rectTransformFn :: (Autofloat a) => ComputedValue a
rectTransformFn = (props, fn)
  where
    props = ["sizeX", "sizeY", "rotation", "x", "y", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV sizeX, FloatV sizeY, FloatV rotation, FloatV x, FloatV y, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (sizeX, sizeY, rotation, x, y)
      in HMatrixV $ customTransform # defaultTransform

polygonTransformFn :: (Autofloat a) => ComputedValue a
polygonTransformFn = (props, fn)
  where
    props = ["scaleX", "scaleY", "rotation", "dx", "dy", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV scaleX, FloatV scaleY, FloatV rotation, FloatV dx, FloatV dy, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (scaleX, scaleY, rotation, dx, dy)
      in HMatrixV $ customTransform # defaultTransform

circTransformFn :: (Autofloat a) => ComputedValue a
circTransformFn = (props, fn)
  where
    props = ["x", "y", "r", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV r, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (r, r, 0.0, x, y)
      in HMatrixV $ customTransform # defaultTransform

squareTransformFn :: (Autofloat a) => ComputedValue a
squareTransformFn = (props, fn)
  where
    props = ["x", "y", "side", "rotation", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV side, FloatV rotation, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (side, side, rotation, x, y)
      in HMatrixV $ customTransform # defaultTransform

imageTransformFn :: Autofloat a => ComputedValue a
imageTransformFn = (props, fn)
  where
    props = ["centerX", "centerY", "scaleX", "scaleY", "rotation", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV centerX, FloatV centerY, FloatV scaleX, FloatV scaleY, FloatV rotation, HMatrixV customTransform] =
      let defaultTransform =
            paramsToMatrix (scaleX, scaleY, rotation, centerX, centerY)
      in HMatrixV $ customTransform # defaultTransform

ellipseTransformFn :: Autofloat a => ComputedValue a
ellipseTransformFn = (props, fn)
  where
    props = ["x", "y", "rx", "ry", "rotation", "transform"]
    fn :: Autofloat a => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV rx, FloatV ry, FloatV rotation, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (rx, ry, rotation, x, y)
      in HMatrixV $ customTransform # defaultTransform

textTransformFn :: Autofloat a => ComputedValue a
textTransformFn = (props, fn)
  where
    props = ["x", "y", "scaleX", "scaleY", "rotation", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV scaleX, FloatV scaleY, FloatV rotation, HMatrixV customTransform]
             -- Note that this overall transformation does NOT use the "w" and "h" parameters set by the frontend
     =
      let defaultTransform = paramsToMatrix (scaleX, scaleY, rotation, x, y)
      in HMatrixV $ customTransform # defaultTransform

parallelogramTransformFn :: (Autofloat a) => ComputedValue a
parallelogramTransformFn = (props, fn)
  where
    props = ["width", "height", "rotation", "x", "y", "innerAngle", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV width, FloatV height, FloatV rotation, FloatV x, FloatV y, FloatV innerAngle, HMatrixV customTransform] =
      let defaultTransform =
            toParallelogram (width, height, rotation, x, y, innerAngle)
      in HMatrixV $ customTransform # defaultTransform

-- TODO: there's a bit of redundant computation with recomputing the full transformation
rectPolygonFn :: (Autofloat a) => ComputedValue a
rectPolygonFn = (props, fn)
  where
    props = ["sizeX", "sizeY", "rotation", "x", "y", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV sizeX, FloatV sizeY, FloatV rotation, FloatV x, FloatV y, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (sizeX, sizeY, rotation, x, y)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $ transformPoly fullTransform $ toPoly unitSq

polygonPolygonFn :: (Autofloat a) => ComputedValue a
polygonPolygonFn = (props, fn)
  where
    props = ["scaleX", "scaleY", "rotation", "dx", "dy", "transform", "points"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV scaleX, FloatV scaleY, FloatV rotation, FloatV dx, FloatV dy, HMatrixV customTransform, PolygonV points] =
      let defaultTransform = paramsToMatrix (scaleX, scaleY, rotation, dx, dy)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $ transformPoly fullTransform points

circPolygonFn :: (Autofloat a) => ComputedValue a
circPolygonFn = (props, fn)
  where
    props = ["x", "y", "r", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV r, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (r, r, 0.0, x, y)
      in let fullTransform = customTransform # defaultTransform
         in let res =
                  PolygonV $ transformPoly fullTransform $ toPoly $ circlePoly r
            in res {-trace ("getting circle polygon output: " ++ show res)-}

-- | Polygonize a Bezier curve, even if the curve was originally made using a list of points.
-- TODO: distinguish between filled curves (polygons) and unfilled ones (polylines)
curvePolygonFn :: (Autofloat a) => ComputedValue a
curvePolygonFn = (props, fn)
  where
    props =
      [ "scaleX"
      , "scaleY"
      , "rotation"
      , "dx"
      , "dy"
      , "transform"
      , "pathData"
      , "strokeWidth"
      , "leftArrowhead"
      , "rightArrowhead"
      ]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV scaleX, FloatV scaleY, FloatV rotation, FloatV dx, FloatV dy, HMatrixV customTransform, PathDataV path, FloatV strokeWidth, BoolV leftArrow, BoolV rightArrow] =
      let defaultTransform = paramsToMatrix (scaleX, scaleY, rotation, dx, dy)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $
            transformPoly fullTransform $
            toPoly $
            polygonizePathPolygon maxIter strokeWidth leftArrow rightArrow path
      where
        maxIter = 1 -- TODO: what should this be?

-- | Polygonize a line segment, accounting for its thickness.
-- TODO: would it usually be more efficient to just use a polyline?
linePolygonFn :: (Autofloat a) => ComputedValue a
linePolygonFn = (props, fn)
  where
    props =
      [ "scaleX"
      , "scaleY"
      , "rotation"
      , "dx"
      , "dy"
      , "transform"
      , "thickness"
      , "startX"
      , "startY"
      , "endX"
      , "endY"
      , "leftArrowhead"
      , "rightArrowhead"
      ]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV scaleX, FloatV scaleY, FloatV rotation, FloatV dx, FloatV dy, HMatrixV customTransform, FloatV thickness, FloatV startX, FloatV startY, FloatV endX, FloatV endY, BoolV leftArrow, BoolV rightArrow] =
      let defaultTransform = paramsToMatrix (scaleX, scaleY, rotation, dx, dy)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $
            transformPoly fullTransform $
            toPoly $
            extrude thickness (startX, startY) (endX, endY) leftArrow rightArrow

-- TODO: add ones for final properties; also refactor so it's more generic across shapes
squarePolygonFn :: (Autofloat a) => ComputedValue a
squarePolygonFn = (props, fn)
  where
    props = ["x", "y", "side", "rotation", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV side, FloatV rotation, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (side, side, rotation, x, y)
          fullTransform = customTransform # defaultTransform
      in PolygonV $ transformPoly fullTransform $ toPoly unitSq

imagePolygonFn :: Autofloat a => ComputedValue a
imagePolygonFn = (props, fn)
  where
    props =
      [ "centerX"
      , "centerY"
      , "scaleX"
      , "scaleY"
      , "rotation"
      , "transform"
      , "initWidth"
      , "initHeight"
      ]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV centerX, FloatV centerY, FloatV scaleX, FloatV scaleY, FloatV rotation, HMatrixV customTransform, FloatV initWidth, FloatV initHeight]
             -- Note that the unit square is implicitly scaled to (w, h)
             -- (from the frontend) before having the default transform applied
     =
      let defaultTransform =
            paramsToMatrix
              ( scaleX * initWidth
              , scaleY * initHeight
              , rotation
              , centerX
              , centerY)
          fullTransform = customTransform # defaultTransform
      in PolygonV $ transformPoly fullTransform $ toPoly unitSq

ellipsePolygonFn :: Autofloat a => ComputedValue a
ellipsePolygonFn = (props, fn)
  where
    props = ["x", "y", "rx", "ry", "rotation", "transform"]
    fn :: Autofloat a => [Value a] -> Value a
    fn [FloatV x, FloatV y, FloatV rx, FloatV ry, FloatV rotation, HMatrixV customTransform] =
      let defaultTransform = paramsToMatrix (rx, ry, rotation, x, y)
          fullTransform = customTransform # defaultTransform
      in PolygonV $ transformPoly fullTransform $ toPoly $ circlePoly 1

parallelogramPolygonFn :: (Autofloat a) => ComputedValue a
parallelogramPolygonFn = (props, fn)
  where
    props = ["width", "height", "rotation", "x", "y", "innerAngle", "transform"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV width, FloatV height, FloatV rotation, FloatV x, FloatV y, FloatV innerAngle, HMatrixV customTransform] =
      let defaultTransform =
            toParallelogram (width, height, rotation, x, y, innerAngle)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $ transformPoly fullTransform $ toPoly unitSq

textPolygonFn :: (Autofloat a) => ComputedValue a
textPolygonFn = (props, fn)
  where
    props = ["scaleX", "scaleY", "rotation", "x", "y", "transform", "w", "h"]
    fn :: (Autofloat a) => [Value a] -> Value a
    fn [FloatV scaleX, FloatV scaleY, FloatV rotation, FloatV x, FloatV y, HMatrixV customTransform, FloatV w, FloatV h]
             -- Note that the unit square is implicitly scaled to (w, h)
             -- (from the frontend) before having the default transform applied
     =
      let defaultTransform =
            paramsToMatrix (scaleX * w, scaleY * h, rotation, x, y)
      in let fullTransform = customTransform # defaultTransform
         in PolygonV $ transformPoly fullTransform $ toPoly unitSq

--------------------------------------------------------------------------------
-- Property samplers
type SampledValue a = StdGen -> (Value a, StdGen)

type FloatInterval = (Float, Float)

rndInterval :: (Float, Float)
rndInterval = (0, canvasWidth / 6)

-- COMBAK: SHAME. Parametrize the random generators properly!
canvasHeight, canvasWidth :: Float
canvasHeight = 700.0

canvasWidth = 800.0

debugRng :: StdGen
debugRng = mkStdGen seed
  where
    seed = 16 -- deterministic RNG with seed

constValue :: (Autofloat a) => Value a -> SampledValue a
constValue v g = (v, g)

-- NOTE: this function does not enforce that all values have the same type
sampleDiscrete :: (Autofloat a) => [Value a] -> SampledValue a
sampleDiscrete list g =
  let (idx, g') = randomR (0, length list - 1) g
  in (list !! idx, g')

sampleFloatIn :: (Autofloat a) => FloatInterval -> SampledValue a
sampleFloatIn interval g =
  let (n, g') = randomR interval g
  in (FloatV $ r2f n, g')

samplePointIn ::
     (Autofloat a) => (FloatInterval, FloatInterval) -> SampledValue a
samplePointIn (interval1, interval2) g =
  let (n1, g1) = randomR interval1 g
      (n2, g2) = randomR interval2 g1
  in (PtV (r2f n1, r2f n2), g2)

sampleColor :: (Autofloat a) => SampledValue a
sampleColor rng =
  let interval = (0.1, 0.9)
      (r, rng1) = randomR interval rng
      (g, rng2) = randomR interval rng1
      (b, rng3) = randomR interval rng2
      a = 0.5
  in (ColorV $ makeColor r g b a, rng3)

-- | Samples all properties of input shapes
sampleShapes :: (Autofloat a) => StdGen -> [Shape a] -> ([Shape a], StdGen)
sampleShapes g shapes =
  let (shapes', g') = foldl' sampleShape ([], g) shapes
  in (reverse shapes', g')

sampleShape (shapes, g) oldShape@(typ, oldProperties) =
  let (_, propDefs) = findDef typ
      (properties, g') = sampleProperties g propDefs
      shape = (typ, properties)
      namedShape = setName (getName oldShape) shape
  in (namedShape : shapes, g')

sampleProperties ::
     (Autofloat a) => StdGen -> PropertiesDef a -> (Properties a, StdGen)
sampleProperties g propDefs =
  M.foldlWithKey sampleProperty (M.empty, g) propDefs

sampleProperty ::
     (Autofloat a)
  => (Properties a, StdGen)
  -> PropID
  -> (ValueType, SampledValue a)
  -> (Properties a, StdGen)
sampleProperty (properties, g) propID (typ, sampleF) =
  let (val, g') = sampleF g
  in (M.insert propID val properties, g')

--------------------------------------------------------------------------------
-- Example shape defs
-- | TODO: derived properties
-- | TODO: instantiation of objs with (1) default values; (2) random sampling w.r.t. constraints
-- constructShape :: ShapeDef a -> [SampleRule] -> Shape a
canvasDims :: (Float, Float)
canvasDims = (-canvasHeight / 2, canvasHeight / 2)

-- TODO: change everything to camel case
x_sampler, y_sampler, pointSampler, width_sampler, height_sampler, angle_sampler, stroke_sampler, stroke_style_sampler, bool_sampler ::
     (Autofloat a) => SampledValue a
x_sampler = sampleFloatIn canvasDims

y_sampler = sampleFloatIn canvasDims

pointSampler = samplePointIn (canvasDims, canvasDims)

width_sampler = sampleFloatIn (3, canvasWidth / 6)

height_sampler = sampleFloatIn (3, canvasHeight / 6)

angle_sampler = sampleFloatIn (0, 360) -- TODO: check that frontend uses degrees, not radians

stroke_sampler = sampleFloatIn (0.5, 3)

stroke_style_sampler = sampleDiscrete [StrV "dashed", StrV "solid"]

bool_sampler = sampleDiscrete [BoolV True, BoolV False]

anchorPointType, circType, ellipseType, arrowType, braceType, curveType, lineType, rectType, squareType, parallelogramType, imageType, textType, arcType, rectTransformType, polygonType, circTransformType, curveTransformType, lineTransformType, squareTransformType, imageTransformType, ellipseTransformType, parallelogramTransformType, textTransformType ::
     (Autofloat a) => ShapeDef a
anchorPointType =
  ( "AnchorPoint"
  , M.fromList
        -- ("x", (FloatT, x_sampler)),
        -- ("y", (FloatT, y_sampler)),
      [ ("location", (PtT, pointSampler))
      , ("name", (StrT, constValue $ StrV "defaultAnchorPoint"))
      ])

circType =
  ( "Circle"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("r", (FloatT, width_sampler))
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "filled"))
      , ("strokeStyle", (StrT, constValue $ StrV "solid"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultCircle"))
      ])

ellipseType =
  ( "Ellipse"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("rx", (FloatT, width_sampler))
      , ("ry", (FloatT, height_sampler))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, sampleDiscrete [StrV "filled"]))
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeStyle", (StrT, stroke_style_sampler))
      , ("color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultEllipse"))
      ])

-- When explicitly declared or computed in Style programs, w and h take precedence over fontSize.
-- Therefore, custom fontSize in Style will only work when w and h are not specified or computed.
textType =
  ( "Text"
  , M.fromList
      [ ("x", (FloatT, sampleFloatIn (-canvasWidth / 2, canvasWidth / 2)))
      , ("y", (FloatT, sampleFloatIn (-canvasHeight / 2, canvasHeight / 2)))
      , ("w", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
      , ("h", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
      , ("fontSize", (StrT, constValue $ StrV "12pt")) 
      , ("string", (StrT, constValue $ StrV "defaultLabelText"))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("style", (StrT, constValue $ StrV "none"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("color", (ColorT, constValue $ ColorV black))
      , ("name", (StrT, constValue $ StrV "defaultCircle"))
      ])

arrowType =
  ( "Arrow"
  , M.fromList
      [ ("startX", (FloatT, x_sampler))
      , ("startY", (FloatT, y_sampler))
      , ("endX", (FloatT, x_sampler))
      , ("endY", (FloatT, y_sampler))
      , ("thickness", (FloatT, sampleFloatIn (5, 15)))
      , ("style", (StrT, constValue $ StrV "straight"))
      , ("color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultArrow"))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
      ])

braceType =
  ( "Brace"
  , M.fromList
      [ ("startX", (FloatT, x_sampler))
      , ("startY", (FloatT, y_sampler))
      , ("endX", (FloatT, x_sampler))
      , ("endY", (FloatT, y_sampler))
      , ("color", (ColorT, sampleColor))
      , ("thickness", (FloatT, sampleFloatIn (1, 6)))
      , ("name", (StrT, constValue $ StrV "defaultBrace"))
      ])

curveType =
  ( "Curve"
  , M.fromList
        -- These two fields are for storage.
      [ ("path", (PtListT, constValue $ PtListV [])) -- TODO: sample path
      , ("polyline", (PtListT, constValue $ PtListV [])) -- TODO: sample path
        -- The frontend only uses pathData to draw the curve.
      , ("pathData", (PathDataT, constValue $ PathDataV [])) -- TODO: sample path
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "solid"))
      , ("fill", (ColorT, sampleColor)) -- for no fill, set opacity to 0
      , ("color", (ColorT, sampleColor))
      , ("leftArrowhead", (BoolT, constValue $ BoolV False))
      , ("rightArrowhead", (BoolT, constValue $ BoolV False))
      , ("arrowheadStyle", (StrT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (FloatT, constValue $ FloatV 1.0))
      , ("name", (StrT, constValue $ StrV "defaultCurve"))
      ])

lineType =
  ( "Line"
  , M.fromList
      [ ("startX", (FloatT, x_sampler))
      , ("startY", (FloatT, y_sampler))
      , ("endX", (FloatT, x_sampler))
      , ("endY", (FloatT, y_sampler))
      , ("thickness", (FloatT, sampleFloatIn (5, 15)))
      , ("leftArrowhead", (BoolT, constValue $ BoolV False))
      , ("rightArrowhead", (BoolT, constValue $ BoolV False))
      , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
      , ("color", (ColorT, sampleColor))
      , ("style", (StrT, constValue $ StrV "solid"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("name", (StrT, constValue $ StrV "defaultLine"))
      ])

rectType =
  ( "Rectangle"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("sizeX", (FloatT, width_sampler))
      , ("sizeY", (FloatT, height_sampler))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("color", (ColorT, sampleColor))
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "filled"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeStyle", (StrT, constValue $ StrV "none"))
      , ("name", (StrT, constValue $ StrV "defaultRect"))
      ])

squareType =
  ( "Square"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("side", (FloatT, width_sampler))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
        -- TODO: distinguish between stroke color and fill color everywhere
      , ("color", (ColorT, sampleColor))
      , ("style", (StrT, constValue $ StrV "none")) -- TODO: what is this?
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeWidth", (FloatT, constValue $ FloatV 0.0))
      , ("name", (StrT, constValue $ StrV "defaultSquare"))
      ])

parallelogramType =
  ( "Parallelogram"
  , M.fromList
      [ ("x", (FloatT, x_sampler)) -- (x, y) is the bottom-left corner of the parallelogram
      , ("y", (FloatT, y_sampler))
      , ("lengthX", (FloatT, width_sampler))
      , ("lengthY", (FloatT, height_sampler))
      , ("angle", (FloatT, constValue $ FloatV 0.0))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("color", (ColorT, sampleColor))
      , ("stroke-style", (StrT, stroke_style_sampler))
      , ("stroke-color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultParallelogram"))
      ])

imageType =
  ( "Image"
  , M.fromList
      [ ("centerX", (FloatT, x_sampler))
      , ("centerY", (FloatT, y_sampler))
      , ("lengthX", (FloatT, width_sampler))
      , ("lengthY", (FloatT, height_sampler))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("style", (StrT, constValue $ StrV "none"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("path", (StrT, constValue $ StrV "missing image path")) -- Absolute path (URL)
      , ("name", (StrT, constValue $ StrV "defaultImage"))
      ])

arcType =
  ( "Arc"
  , M.fromList
      [ ("x", (FloatT, x_sampler)) -- x,y are the cordinates for the bottom left position
      , ("y", (FloatT, y_sampler)) -- of the right angle or the midlle pos of a regular angle
      , ("r", (FloatT, width_sampler))
      , ("size", (FloatT, width_sampler))
      , ("lengthX", (FloatT, width_sampler))
      , ("lengthY", (FloatT, height_sampler))
      , ("angle", (FloatT, angle_sampler))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("isRight", (BoolT, bool_sampler)) -- This property overrides the angle property
      , ("strokeWidth", (FloatT, constValue $ FloatV 1.0))
      , ("strokeColor", (ColorT, sampleColor))
      , ("fillColor", (ColorT, sampleColor))
      , ("leftArrowhead", (BoolT, constValue $ BoolV False))
      , ("rightArrowhead", (BoolT, constValue $ BoolV False))
      , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
      , ("name", (StrT, constValue $ StrV "defaultArc"))
      ])

rectTransformType =
  ( "RectangleTransform"
  , M.fromList
        -- These attributes serve as DOF in the default transformation
        -- They are NOT the final x, etc.
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("sizeX", (FloatT, width_sampler))
      , ("sizeY", (FloatT, height_sampler))
      , ("rotation", (FloatT, angle_sampler))
      , ("transform", (FloatT, constValue $ HMatrixV idH)) -- Set in Style
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
        -- TODO compute these
        -- ("finalX", (FloatT, constValue $ FloatV 0.0)),
        -- ("finalY", (FloatT, constValue $ FloatV 0.0)),
        -- ("finalSizeX", (FloatT, constValue $ FloatV 1.0)),
        -- ("finalSizeY", (FloatT, constValue $ FloatV 1.0)),
        -- ("finalRotation", (FloatT, constValue $ FloatV 0.0)),
      , ("color", (ColorT, sampleColor))
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "filled"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeStyle", (StrT, constValue $ StrV "none"))
      , ("name", (StrT, constValue $ StrV "defaultRect"))
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      ])

-- Also using the new transforms
polygonType =
  ( "Polygon"
  , M.fromList
      [ ("points", (PtListT, constValue $ PtListV testTriangle))
      , ("polygon", (PtListT, constValue $ PtListV []))
        -- These attributes serve as DOF in the default transformation
        -- They are NOT the final x, etc.
        -- TODO: should these be sampled?
      , ("dx", (FloatT, x_sampler)) -- Polygon doesn't have a natural "center"
      , ("dy", (FloatT, y_sampler))
      , ("scaleX", (FloatT, width_sampler))
      , ("scaleY", (FloatT, height_sampler))
      , ("rotation", (FloatT, angle_sampler))
        -- TODO: currently rotates about the center of the Penrose canvas, (0, 0)
      , ("centerX", (FloatT, constValue $ FloatV 0.0))
      , ("centerY", (FloatT, constValue $ FloatV 0.0))
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("strokeStyle", (StrT, constValue $ StrV "solid"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("fillColor", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultPolygon"))
      ])

circTransformType =
  ( "CircleTransform"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("r", (FloatT, width_sampler))
        -- TODO: circle currently has rotations hardcoded to 0.0 (since we aren't yet rotating about a point)
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "filled"))
      , ("strokeStyle", (StrT, constValue $ StrV "solid"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultCircle"))
      ])

curveTransformType =
  ( "CurveTransform"
  , M.fromList
      [ ("dx", (FloatT, constValue $ FloatV 0.0)) -- Curve doesn't have a natural "center"
      , ("dy", (FloatT, constValue $ FloatV 0.0))
      , ("scaleX", (FloatT, constValue $ FloatV 1.0))
      , ("scaleY", (FloatT, constValue $ FloatV 1.0))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
        -- TODO: currently rotates about the center of the Penrose canvas, (0, 0)
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      , ("path", (PtListT, constValue $ PtListV [])) -- TODO: sample path
      , ("polyline", (PtListT, constValue $ PtListV [])) -- TODO: sample path
      , ("pathData", (PathDataT, constValue $ PathDataV [])) -- TODO: sample path
      , ("strokeWidth", (FloatT, stroke_sampler))
      , ("style", (StrT, constValue $ StrV "solid"))
      , ("fill", (ColorT, sampleColor)) -- for no fill, set opacity to 0
      , ("color", (ColorT, sampleColor))
      , ("leftArrowhead", (BoolT, constValue $ BoolV False))
      , ("rightArrowhead", (BoolT, constValue $ BoolV False))
      , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
      , ("name", (StrT, constValue $ StrV "defaultCurve"))
      ])

-- If the start and end are not set, by default, it's a unit line segment aligned w/ x-axis, centered at origin.
-- If the start and end are set in Style, then that will be the line segment to which all subsequent transforms apply.
lineTransformType =
  ( "LineTransform"
  , M.fromList
      [ ("startX", (FloatT, constValue $ FloatV $ -0.5))
      , ("startY", (FloatT, constValue $ FloatV 0))
      , ("endX", (FloatT, constValue $ FloatV 0.5))
      , ("endY", (FloatT, constValue $ FloatV 0.0))
        -- By default, this is NOT set. Should it be?
      , ("dx", (FloatT, constValue $ FloatV 0.0)) -- Curve doesn't have a natural "center"
      , ("dy", (FloatT, constValue $ FloatV 0.0))
      , ("scaleX", (FloatT, constValue $ FloatV 1.0))
      , ("scaleY", (FloatT, constValue $ FloatV 1.0))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
        -- TODO: currently rotates about the center of the Penrose canvas, (0, 0)
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      , ("leftArrowhead", (BoolT, constValue $ BoolV False))
      , ("rightArrowhead", (BoolT, constValue $ BoolV False))
      , ("arrowheadStyle", (BoolT, constValue $ StrV "arrowhead-2"))
      , ("arrowheadSize", (BoolT, constValue $ FloatV 1.0))
      , ("thickness", (FloatT, sampleFloatIn (5, 15)))
      , ("color", (ColorT, sampleColor))
      , ("style", (StrT, constValue $ StrV "solid"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("name", (StrT, constValue $ StrV "defaultLine"))
      ])

squareTransformType =
  ( "SquareTransform"
  , M.fromList
      [ ("x", (FloatT, x_sampler)) -- x and y as dx and dy in transform?
      , ("y", (FloatT, y_sampler))
      , ("side", (FloatT, width_sampler)) -- as both scaleX and scaleY ?
      , ("rotation", (FloatT, constValue $ FloatV 0.0)) -- initialize as 0?
        -- matrices
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH))
        -- ones that remain the same
      , ("color", (ColorT, sampleColor))
      , ("style", (StrT, constValue $ StrV "none"))
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeWidth", (FloatT, constValue $ FloatV 0.0))
      , ("name", (StrT, constValue $ StrV "defaultSquare"))
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      ])

imageTransformType =
  ( "ImageTransform"
  , M.fromList
      [ ("centerX", (FloatT, x_sampler))
      , ("centerY", (FloatT, y_sampler))
      , ("initWidth", (FloatT, constValue $ FloatV 0.0)) -- Set by frontend
      , ("initHeight", (FloatT, constValue $ FloatV 0.0)) -- (same)
      , ("scaleX", (FloatT, constValue $ FloatV 1.0)) -- set by image file?
      , ("scaleY", (FloatT, constValue $ FloatV 1.0))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH))
      , ("style", (StrT, constValue $ StrV "none"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("path", (StrT, constValue $ StrV "missing image path")) -- Absolute path (URL)
      , ("name", (StrT, constValue $ StrV "defaultImage"))
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      ])

ellipseTransformType =
  ( "EllipseTransform"
  , M.fromList
      [ ("x", (FloatT, x_sampler)) --dx
      , ("y", (FloatT, y_sampler)) --dy
      , ("rx", (FloatT, width_sampler)) --sx
      , ("ry", (FloatT, height_sampler)) --sy
      , ("rotation", (FloatT, constValue $ FloatV 0.0)) --rot
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH))
      , ("stroke-width", (FloatT, stroke_sampler))
      , ("style", (StrT, sampleDiscrete [StrV "filled"]))
      , ("stroke-style", (StrT, stroke_style_sampler))
      , ("color", (ColorT, sampleColor))
      , ("name", (StrT, constValue $ StrV "defaultEllipse"))
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      ])

-- Starting with a square centered at origin, we apply this transformation:
--  (in T1 then T2 format)
-- scale by (lengthX, lengthY)
-- shear X by lambda = f(angle in radians) = 1 / tan(angle)
-- rotate by angle
-- translate by (x, y)
parallelogramTransformType =
  ( "ParallelogramTransform"
  , M.fromList
      [ ("x", (FloatT, x_sampler)) -- (x, y) is the CENTER of the parallelogram
      , ("y", (FloatT, y_sampler))
      , ("width", (FloatT, width_sampler)) -- width of the rectangle pre-shear
      , ("height", (FloatT, height_sampler)) -- height of the rectangle pre-shear
      , ("innerAngle", (FloatT, constValue $ FloatV (pi / 3))) -- shear angle of the rectangle
      , ("rotation", (FloatT, constValue $ FloatV 0.0)) -- about the origin
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      , ("color", (ColorT, sampleColor))
      , ("strokeStyle", (StrT, stroke_style_sampler))
      , ("strokeColor", (ColorT, sampleColor))
      , ("strokeWidth", (FloatT, constValue $ FloatV 0.0))
      , ("name", (StrT, constValue $ StrV "defaultParallelogram"))
      ])

textTransformType =
  ( "TextTransform"
  , M.fromList
      [ ("x", (FloatT, x_sampler))
      , ("y", (FloatT, y_sampler))
      , ("scaleX", (FloatT, constValue $ FloatV 1.0)) -- TODO: set text size in points
      , ("scaleY", (FloatT, constValue $ FloatV 1.0))
      , ("rotation", (FloatT, constValue $ FloatV 0.0))
      , ("w", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
      , ("h", (FloatT, constValue $ FloatV 0)) -- NOTE: updated by front-end
      , ("path", (PathDataT, constValue $ PathDataV [])) -- NOTE: updated by front-end
        -- NOTE: the polygon will only show up after a few steps, since we have to wait for the server to set the width/height/path
      , ("transform", (FloatT, constValue $ HMatrixV idH))
      , ("transformation", (FloatT, constValue $ HMatrixV idH)) -- Computed
      , ("polygon", (PolygonT, constValue $ PolygonV emptyPoly))
      , ("string", (StrT, constValue $ StrV "defaultLabelTextTransform"))
      , ("style", (StrT, constValue $ StrV "none"))
      , ("stroke", (StrT, constValue $ StrV "none"))
      , ("color", (ColorT, constValue $ ColorV black))
      , ("name", (StrT, constValue $ StrV "defaultCircle"))
      ])

-----
exampleCirc :: (Autofloat a) => Shape a
exampleCirc =
  ( "Circle"
  , M.fromList
      [ ("x", FloatV 5.5)
      , ("y", FloatV 100.2)
      , ("r", FloatV 5)
      , ("name", StrV "exampleCirc")
      , ("style", StyleV "filled")
      , ("color", ColorV black)
      ])

--------------------------------------------------------------------------------
-- Parser for shape def DSL (TODO)
--------------------------------------------------------------------------------
-- Type checker for a particular shape instance against its def (TODO)
--
-- checkShape :: (Autofloat a) => Shape a -> ShapeDef a -> Shape a
-- checkShape shape def =
--------------------------------------------------------------------------------
-- Utility functions for Runtime
-- | given a translation generated by the Style compiler, generate all GPIs
-- NOTE: equilavant to genAllObjs
-- generateShapes :: (Autofloat a) => Translation a -> [Shape a]
-- COMBAK:
-- - where to define default objs such as sizeFuncs?
-- - do we allow extended properties? If so, where do we resolve them?
-- generateShapes trans = []
findShapeSafe :: (Autofloat a) => String -> [Shape a] -> Maybe (Shape a)
findShapeSafe shapeName shapes =
  case filter (\s -> getName s == shapeName) shapes of
    [x] -> Just x
    [] -> Nothing
    _ ->
      error
        ("findShape: expected one shape for \"" ++
         shapeName ++ "\", but did not find just one (returned 1+ shapes).")

findShape :: (Autofloat a) => String -> [Shape a] -> Shape a
findShape shapeName shapes =
  case filter (\s -> getName s == shapeName) shapes of
    [x] -> x
    _ ->
      error
        ("findShape: expected one shape for \"" ++
         shapeName ++ "\", but did not find just one (returned one or many).")

-- TODO: can use alter, update, adjust here. Come back if performance matters
-- | Setting the value of a property
set :: (Autofloat a) => Shape a -> PropID -> Value a -> Shape a
set (t, propDict) prop val =
  case M.lookup prop propDict of
    Nothing -> noPropError "set" prop t
    _       -> (t, M.update (const $ Just val) prop propDict)

-- | Getting the value of a property
get :: (Autofloat a) => Shape a -> PropID -> Value a
get (t, propDict) prop =
  fromMaybe (noPropError "get" prop t) (M.lookup prop propDict)

{-# INLINE get #-}
-- | batch get
getAll :: (Autofloat a) => Shape a -> [PropID] -> [Value a]
getAll shape = map (get shape)

-- | batch set
setAll :: (Autofloat a) => Shape a -> [(PropID, Value a)] -> Shape a
setAll = foldl' (\s (k, v) -> set s k v)

-- | reset a property to its default value
-- TODO: now needs to take in a random generator
-- reset :: (Autofloat a) => Shape a -> PropID -> Shape a
-- reset (t, propDict) prop =
--     let val = defaultValueOf prop $ findDef t shapeDefs
--     in (t, M.update (const $ Just val) prop propDict)
-- | whether a shape has a prop
hasProperty :: (Autofloat a) => Shape a -> PropID -> Bool
hasProperty (t, propDict) prop = M.member prop propDict

-- | given name of prop, return type
typeOfProperty :: (Autofloat a) => PropID -> Shape a -> ValueType
typeOfProperty prop (t, propDict) =
  case M.lookup prop propDict of
    Nothing -> noPropError "typeOfProperty" prop t
    Just v  -> typeOf v

-- | property IDs in alphabetical order
propertyIDs :: (Autofloat a) => Shape a -> [PropID]
propertyIDs (_, propDict) = map fst $ M.toAscList propDict

-- | vals in alphabetical order of their keys
propertyVals :: (Autofloat a) => Shape a -> [Value a]
propertyVals (_, propDict) = map snd $ M.toAscList propDict

--------------------------------------------------------------------------------
-- Utility functions for objective/constraint function writers

-- | 'is' checks whether a shape is of a certain type
is :: (Autofloat a) => Shape a -> ShapeTypeStr -> Bool
is (t1, _) t2 = t1 == t2

-- | short-hand for 'get'
(.:) :: (Autofloat a) => Shape a -> PropID -> Value a
(.:) = get

-- {-# INLINE (.:) #-} TODO: causes parse error in linter. Revert if there's any performance issue.
getX, getY :: (Autofloat a) => Shape a -> a
getX shape =
  case shape .: "x" of
    FloatV x -> x
    _        -> error "getX: expected float but got something else"

getY shape =
  case shape .: "y" of
    FloatV y -> y
    _        -> error "getY: expected float but got something else"

getPoint :: (Autofloat a) => String -> Shape a -> (a, a)
getPoint "start" shape =
  case (shape .: "startX", shape .: "startY") of
    (FloatV x, FloatV y) -> (x, y)
    _ -> error "getPoint expected two floats but got something else"
getPoint "end" shape =
  case (shape .: "endX", shape .: "endY") of
    (FloatV x, FloatV y) -> (x, y)
    _ -> error "getPoint expected two floats but got something else"
getPoint _ shape = error "getPoint did not receive existing property name"

-- To use with vector operations
getPointV :: (Autofloat a) => String -> Shape a -> [a]
getPointV "start" shape =
  case (shape .: "startX", shape .: "startY") of
    (FloatV x, FloatV y) -> [x, y]
    _ -> error "getPointV expected two floats but got something else"
getPointV "end" shape =
  case (shape .: "endX", shape .: "endY") of
    (FloatV x, FloatV y) -> [x, y]
    _ -> error "getPointV expected two floats but got something else"
getPointV _ shape = error "getPointV did not receive existing property name"

getName :: (Autofloat a) => Shape a -> String
getName shape =
  case shape .: "name" of
    StrV s -> s
    _      -> error "getName: expected string but got something else"

setName :: (Autofloat a) => String -> Shape a -> Shape a
setName v shape = set shape "name" (StrV v)

setX, setY :: (Autofloat a) => Value a -> Shape a -> Shape a
setX v shape@("Arrow", _) = set shape "startX" v
setX v shape              = set shape "x" v

setY v shape@("Arrow", _) = set shape "startY" v
setY v shape              = set shape "y" v

getNum :: (Autofloat a) => Shape a -> PropID -> a
getNum shape prop =
  case shape .: prop of
    FloatV x -> x
    res -> error ("getNum: expected float but got something else: " ++ show res)

getPoints :: (Autofloat a) => Shape a -> [Pt2 a]
getPoints shape =
  case shape .: "points" of
    PtListV x -> x
    _ -> error "getPoints: expected [(Float, Float)] but got something else"

getPolygon :: (Autofloat a) => Shape a -> Polygon a
getPolygon shape =
  case shape .: "polygon" of
    PolygonV x -> x
    _ -> error "getPolygon: expected [(Float, Float)] but got something else"

getPath :: (Autofloat a) => Shape a -> [Pt2 a]
getPath shape =
  case shape .: "path" of
    PtListV x -> x
    _ -> error "getPath: expected [(Float, Float)] but got something else"

getPathData :: (Autofloat a) => Shape a -> PathData a
getPathData shape =
  case shape .: "pathData" of
    PathDataV x -> x
    _ -> error "getPathData: expected [PathData a] but got something else"

-- | Apply a function to each point in a path
mapPathData :: (Autofloat a) => (Pt2 a -> Pt2 a) -> PathData a -> PathData a
mapPathData f pd = map (mapPath f) pd

mapPath :: (Autofloat a) => (Pt2 a -> Pt2 a) -> Path' a -> Path' a
mapPath f (Closed es) = Closed $ map (mapElem f) es
mapPath f (Open es)   = Open $ map (mapElem f) es

mapElem :: (Autofloat a) => (Pt2 a -> Pt2 a) -> Elem a -> Elem a
mapElem f (Pt p)                  = Pt $ f p
mapElem f (CubicBez (p0, p1, p2)) = CubicBez (f p0, f p1, f p2)
mapElem f (CubicBezJoin (p0, p1)) = CubicBezJoin (f p0, f p1)
mapElem f (QuadBez (p0, p1))      = QuadBez (f p0, f p1)
mapElem f (QuadBezJoin p)         = QuadBezJoin $ f p

-- | Add an offset to each point in a path
translatePath :: (Autofloat a) => Pt2 a -> PathData a -> PathData a
translatePath offset pd = mapPathData (+: offset) pd

-- | Figure out whether to translate pathManual or pathData
movePath :: (Autofloat a) => (a, a) -> Shape a -> Shape a
movePath offset shape =
  let pathManual = getPath shape
      pathData = getPathData shape
  in case (pathManual, pathData) of
       (_:_, []) ->
         let res = (map (+: offset) pathManual)
         in set shape "path" (PtListV res)
       ([], _:_) ->
         let res = translatePath offset pathData
         in set shape "pathData" (PathDataV res)
       (_:_, _:_) ->
         error "shape can't have both path manually set and path data"
       ([], []) -> error "shape has no path or data"

-- | Transform utils
getTransform :: (Autofloat a) => Shape a -> HMatrix a
getTransform shape =
  case shape .: "transform" of
    HMatrixV x -> x
    _ -> error "getTransform: expected HMatrix but got something else"

getTransformation :: (Autofloat a) => Shape a -> HMatrix a
getTransformation shape =
  case shape .: "transformation" of
    HMatrixV x -> x
    _ -> error "getTransformation: expected HMatrix but got something else"

-- | ternary op for set (TODO: maybe later)
-- https://wiki.haskell.org/Ternary_operator
-- | HACK: returns true of a property of a shape is not supposed to be
-- | changed by the optimizer
isPending :: ShapeTypeStr -> PropID -> Bool
isPending typ propId = propId `elem` pendingProperties typ

-- | HACK: returns all "pending" properties that are undeterminded until
-- | rendered by the frontend
pendingProperties :: ShapeTypeStr -> [PropID]
pendingProperties "Text"           = ["w", "h"]
pendingProperties "TextTransform"  = ["w", "h"]
pendingProperties "ImageTransform" = ["initWidth", "initHeight"]
pendingProperties _                = []

-- | Given 'ValueType' and 'ShapeTypeStr', return all props of that ValueType
propertiesOf :: ValueType -> ShapeTypeStr -> [PropID]
propertiesOf propType shapeType =
  M.keys $ M.filter (\(t, _) -> t == propType) $ snd $ findDef shapeType

-- | Given 'ValueType' and 'ShapeTypeStr', return all props NOT of that ValueType
propertiesNotOf :: ValueType -> ShapeTypeStr -> [PropID]
propertiesNotOf propType shapeType =
  M.keys $ M.filter (\(t, _) -> t /= propType) $ snd $ findDef shapeType

-- filterProperties :: (Autofloat a) => ((ValueType, SampledValue a) -> Bool) -> [PropID]
-- filterProperties filterF =
-- | Map over all properties of a shape
-- TODO: withKey?
mapProperties :: (Autofloat a) => (Value a -> Value a) -> Shape a -> Shape a
mapProperties f (t, propDict) = (t, M.map f propDict)

-- | fold over all properties of a shape
-- TODO: withKey?
foldlProperties :: (Autofloat a) => (b -> Value a -> b) -> b -> Shape a -> b
foldlProperties f accum (_, propDict) = M.foldl' f accum propDict

foldlPropertyDefs ::
     (Autofloat a)
  => (b -> (ValueType, SampledValue a) -> b)
  -> b
  -> ShapeDef a
  -> b
foldlPropertyDefs f accum (_, propDict) = M.foldl' f accum propDict

foldlPropertyMappings ::
     (Autofloat a)
  => (b -> PropID -> (ValueType, SampledValue a) -> b)
  -> b
  -> ShapeDef a
  -> b
foldlPropertyMappings f accum (_, propDict) = M.foldlWithKey f accum propDict

--------------------------------------------------------------------------------
-- Error Msgs
noShapeError functionName shapeType =
  error (functionName ++ ": Shape \"" ++ shapeType ++ "\" does not exist")

noPropError functionName prop shapeType =
  error
    (functionName ++
     ": Property \"" ++
     prop ++ "\" does not exist in shape \"" ++ shapeType ++ "\"")

--------------------------------------------------------------------------------
-- Color definition
-- Adopted from gloss: https://github.com/benl23x5/gloss/blob/c63daedfe3b60085f8a9e810e1389cbc29110eea/gloss-rendering/Graphics/Gloss/Internals/Data/Color.hs
data Color
    -- | Holds the color components. All components lie in the range [0..1.
      =
  RGBA !Float
       !Float
       !Float
       !Float
  deriving (Show, Eq, Generic)

instance ToJSON Color

instance FromJSON Color

-- | Make a custom color. All components are clamped to the range  [0..1].
makeColor ::
     Float -- ^ Red component.
  -> Float -- ^ Green component.
  -> Float -- ^ Blue component.
  -> Float -- ^ Alpha component.
  -> Color
makeColor r g b a = clampColor $ RGBA r g b a

{-# INLINE makeColor #-}
-- | Take the RGBA components of a color.
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a) = (r, g, b, a)

{-# INLINE rgbaOfColor #-}
-- | Clamp components of a raw color into the required range.
clampColor :: Color -> Color
clampColor cc =
  let (r, g, b, a) = rgbaOfColor cc
  in RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)

black, white :: Color
black = makeColor 0.0 0.0 0.0 1.0

white = makeColor 1.0 1.0 1.0 1.0

makeColor' :: (Autofloat a) => a -> a -> a -> a -> Color
makeColor' r g b a = makeColor (r2f r) (r2f g) (r2f b) (r2f a)

colToList :: Autofloat a => Color -> [a]
colToList (RGBA r g b a) = [r2f r, r2f g, r2f b, r2f a]

listToCol :: Autofloat a => [a] -> Color
listToCol [r, g, b, a] = RGBA (r2f r) (r2f g) (r2f b) (r2f a)

dsqEuclideanColor :: Autofloat a => Color -> Color -> a
dsqEuclideanColor c1 c2 = normsq $ (255 *. colToList c1) -. (255 *. colToList c2)

-- https://en.wikipedia.org/wiki/Color_difference
-- http://zschuessler.github.io/DeltaE/learn/
dsqApproxColor :: Autofloat a => Color -> Color -> a
dsqApproxColor c1 c2 =
   let ([r1, g1, b1, a1], [r2, g2, b2, a2]) = (255 *. colToList c1, 255 *. colToList c2) -- Ignoring opacity
       r' = (r1 + r2) / 2
       rf = 2 + r' / 256
       gf = 4
       bf = 2 + (255 - r') / 256
       inner = rf * (r2 - r1)^2 + gf * (g2 - g1)^2 + bf * (b2 - b1)^2 -- sqrt $ inner + epsd
   in inner -- TODO: do we need the sqrt? Note that if the values are not in color range, this will go negative with sqrt and NaN

--------------------------------------------------------------------------------
-- Approximating a Bezier curve via polygon or bbox
-- | Polygonize path by extruding the polyline to account for thickness and adding any arrowheads, if present.
-- For each point, find the corresponding inner and outer points on the extruded line
-- by taking the tangent and then taking the inner and outer normal
-- Note: not good for steep angles!
-- Then make the polygon by joining the points in order, accounting for arrowheads
-- TODO: (performance optimization) remove the ++, the accesses to last of list, and the reverses
polygonizePathPolygon ::
     Autofloat a => Int -> a -> Bool -> Bool -> PathData a -> [Pt2 a]
polygonizePathPolygon n thickness leftArr rightArr p =
  let polyline = removeClosePoints $ polygonizePath n p
      tangents = map calcTangent $ zip polyline (tail polyline)
      tangentsWithLast = tangents ++ [last tangents] -- use the same tangent for the last point
        -- outer and inner are two "copies" of the polyline, offset in the +/- normal directions
      (outer, inner) =
        unzip $ map (extrude thickness) $ zip polyline tangentsWithLast
      snipSize = 4 * thickness
       -- snipSize is the amount to remove from the polygonized curve to attach the arrows at either end
      (ltArrow, outer', inner') =
        if not leftArr
          then ([], outer, inner)
          else let ltCenter = head polyline
                   ltAngle = atan2' $ head tangents
                   ltTransform = translationM ltCenter # rotationM ltAngle
                   ltArrow =
                     transformSimplePoly ltTransform $ ltArrowheadPoly thickness
                   (outerFirst, innerFirst) = (head outer, head inner)
                   outer' =
                     dropWhile (\x -> mag (x -: outerFirst) < snipSize) outer
                   inner' =
                     dropWhile (\x -> mag (x -: innerFirst) < snipSize) inner
               in (ltArrow, outer', inner')
      (rtArrow, outer'', inner'') =
        if not rightArr
          then ([], outer', inner')
          else let rtCenter = last polyline
                   rtAngle = atan2' $ last tangents
                   rtTransform = translationM rtCenter # rotationM rtAngle
                   rtArrow =
                     transformSimplePoly rtTransform $ rtArrowheadPoly thickness
                   (outerLast, innerLast) = (last outer, last inner)
                   outer'' =
                     reverse $
                     dropWhile (\x -> mag (x -: outerLast) < snipSize) $
                     reverse outer'
                   inner'' =
                     reverse $
                     dropWhile (\x -> mag (x -: innerLast) < snipSize) $
                     reverse inner'
               in (rtArrow, outer'', inner'')
      polygonWithArrows = outer'' ++ rtArrow ++ reverse inner'' ++ ltArrow
  in polygonWithArrows
  where
    atan2' (x, y) = atan2 y x
    removeClosePoints :: (Autofloat a) => [Pt2 a] -> [Pt2 a]
          -- Heuristic: if any two points are too close together, remove the first one
          -- Not super principled but just to make sure that the tangents don't NaN if two points are too close
    removeClosePoints ps =
      let ptsWithNext = zip ps (tail ps)
      in (map fst $ filter (\(p1, p2) -> mag (p2 -: p1) >= epsp) ptsWithNext) ++
         [last ps]
      where
        epsp = 10 ** (-8)
    calcTangent :: (Autofloat a) => (Pt2 a, Pt2 a) -> Pt2 a
    calcTangent (start, end) = normalize' $ end -: start
    extrude :: (Autofloat a) => a -> (Pt2 a, Pt2 a) -> (Pt2 a, Pt2 a)
    extrude thickness (base, tangent) =
      let normal = thickness / 2.0 *: rot90 tangent
      in (base +: normal, base -: normal)

-- | Polygonize path as polyline
polygonizePath :: Autofloat a => Int -> PathData a -> [Pt2 a]
polygonizePath n p =
  case polygonize n p of
    []   -> error "empty curve: did you set the pathdata or path?"
    x:xs -> x -- What are the other parts of this list?

polygonize :: Autofloat a => Int -> PathData a -> [[Pt2 a]]
polygonize maxIter = map go
  where
    go (Closed path) = error "TODO"
    go (Open path)   = concatMap (polyCubicBez 0 maxIter) $ expandCurves path

type CubicBezCoeffs a = (Pt2 a, Pt2 a, Pt2 a, Pt2 a)

expandCurves :: Autofloat a => [Elem a] -> [CubicBezCoeffs a]
expandCurves elems = zipWith attach elems $ tail elems
  where
    attach (Pt a) (CubicBez (b, c, d))               = (a, b, c, d)
    attach (CubicBez (_, _, a)) (CubicBez (b, c, d)) = (a, b, c, d)

-- | implements http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.86.162&rep=rep1&type=pdf
polyCubicBez :: Autofloat a => Int -> Int -> CubicBezCoeffs a -> [Pt2 a]
polyCubicBez count maxCount curve@(a, b, c, d) =
  if count >= maxCount
    then [a, b, c, d]
    else concatMapTuple (polyCubicBez (count + 1) maxCount) $
         divideCubicBezier curve
  where
    concatMapTuple f (a1, a2) = f a1 ++ f a2

isFlat :: Autofloat a => CubicBezCoeffs a -> Bool
isFlat (a, b, c, d) = True

divideCubicBezier ::
     Autofloat a => CubicBezCoeffs a -> (CubicBezCoeffs a, CubicBezCoeffs a)
divideCubicBezier bezier@(a, _, _, d) = (left, right)
  where
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
splitCubicBezier ::
     Autofloat a
  => CubicBezCoeffs a
  -> (Pt2 a, Pt2 a, Pt2 a, Pt2 a, Pt2 a, Pt2 a)
splitCubicBezier (a, b, c, d) = (ab, bc, cd, abbc, bccd, abbcbccd)
  where
    ab = a `midpoint` b
    bc = b `midpoint` c
    cd = c `midpoint` d
    abbc = ab `midpoint` bc
    bccd = bc `midpoint` cd
    abbcbccd = abbc `midpoint` bccd

bezierBbox :: (Autofloat a) => Shape a -> ((a, a), (a, a)) -- poly Point type?
bezierBbox cb =
  let path = getPath cb
      (xs, ys) = (map fst path, map snd path)
      lower_left = (minimum xs, minimum ys)
      top_right = (maximum xs, maximum ys)
  in (lower_left, top_right)
--------------------------------------------------------------------------------
-- DEBUG: main function to test out the module
--
-- main :: IO ()
-- main = do
--     let c = exampleCirc
--     print $ c .: "r"
--     let c' = set c "r" (FloatV 20)
--     print c'
--     let c'' = reset c "r"
--     print c''
--     print $ propertiesOf FloatT "Circle" shapeDefs
