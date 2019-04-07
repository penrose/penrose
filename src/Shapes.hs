{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Shapes where

import Utils
import Transforms
import System.Random
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M

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
    | Open [Elem a]   -- ^  no "Z"
    deriving (Generic, Eq, Show)
instance (FromJSON a) => FromJSON (Path' a)
instance (ToJSON a)   => ToJSON (Path' a)

-- | TODO
data Elem a
    = Pt (Pt2 a)                   -- ^ Replace "M," "L", "H", "V"
    | CubicBez (Pt2 a, Pt2 a, Pt2 a) -- ^ "C": two control pts, 1 endpt
    | CubicBezJoin (Pt2 a, Pt2 a)  -- ^ "S": 1 control pt, 1 endpt
    | QuadBez (Pt2 a, Pt2 a)       -- ^ "Q": 1 control pt, 1 endpt
    | QuadBezJoin (Pt2 a)          -- ^ "T": 1 endpt
    deriving (Generic, Eq, Show)
    -- | Arc { x, y, sweep1, … }  -- "A" TODO
instance (FromJSON a) => FromJSON (Elem a)
instance (ToJSON a)   => ToJSON (Elem a)

--------- Transforms

data Transform a = RotateAbout a a a
                    | Scale a a
                    | Translate a a
     deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Transform a)
instance (ToJSON a)   => ToJSON (Transform a)

-- Homogeneous matrix / flat transform type
-- TODO: make it from a list of transforms
-- TODO: specify order of application
data Transformation a = Transformation { -- Should this be quantified individually?
     rotation :: a,
     center_r :: Pt2 a,
     scaleXY :: Pt2 a,
     dxy :: Pt2 a
} deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Transformation a)
instance (ToJSON a)   => ToJSON (Transformation a)

idT :: (Autofloat a) => Transformation a -- Identity transformation
idT = Transformation {
    rotation = 0.0,
    center_r = (0.0, 0.0), -- Is this right?
    scaleXY = (1.0, 1.0),
    dxy = (0.0, 0.0) 
}

-- TODO: Should this be a map instead of a record?
-- TODO: Should we extend the path type to access these fields? So it would really be a nested map
-- How do we start with an idH matrix?
-- How should the "x" "y" etc properties be treated? Also, where should the init/default values be stored?
-- Perhaps as "x0" "size0"?
-- It also depends on how you want to actually apply the matrix to a shape
-- Given a shape's parameters, need to generate a unit axis-aligned rect at origin, then do 

hmatrixSchema :: (Autofloat a) => PropertiesDef2 a
hmatrixSchema = M.fromList [
              ("xScale", (FloatT, constValue $ FloatV 1.0)),
              ("xSkew", (FloatT, constValue $ FloatV 0.0)),
              ("ySkew", (FloatT, constValue $ FloatV 0.0)),
              ("yScale", (FloatT, constValue $ FloatV 1.0)),
              ("dx", (FloatT, constValue $ FloatV 0.0)),
              ("dy", (FloatT, constValue $ FloatV 0.0))
              ]

type HMatrix' a = Properties a

idH' :: (Autofloat a) => HMatrix' a
idH' = M.fromList [
     ("xScale", FloatV 1.0),
     ("xSkew", FloatV 0.0),
     ("ySkew", FloatV 0.0),
     ("yScale", FloatV 1.0),
     ("dx", FloatV 0.0),
     ("dy", FloatV 0.0)
     ]

---------

-- | types of fully evaluated values in Style
data ValueType
    = FloatT
    | IntT
    | BoolT
    | StrT
    | PtT
    | PtListT
    | PathDataT
    | ColorT
    | FileT
    | StyleT
    | TransformT
    | HMatrixT
    | MapT
    deriving (Eq, Show)

-- | fully evaluated values in Style
data Value a
    -- | Floating point number
    = FloatV a
    -- | integer
    | IntV Integer
    -- | boolean value
    | BoolV Bool
    -- | string literal
    | StrV String
    -- | point in R^2
    | PtV (Pt2 a)
    -- | path commands
    | PathDataV (PathData a)
    -- | a list of points
    | PtListV [Pt2 a]
    -- | an RGBA color value
    | ColorV Color
    -- | path for image
    | FileV String
    -- | dotted, etc.
    | StyleV String

    -- | single transformation
         -- TODO: which to use?
    | TransformV (Transformation a)
    | HMatrixV (HMatrix a)
    | MapV (Properties a)

    deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Value a)
instance (ToJSON a)   => ToJSON (Value a)

-- | returns the type of a 'Value'
typeOf :: (Autofloat a) => Value a -> ValueType
typeOf v = case v of
     FloatV _     -> FloatT
     IntV   _     -> IntT
     BoolV  _     -> BoolT
     StrV   _     -> StrT
     PtV    _     -> PtT
     PtListV    _ -> PtListT
     PathDataV  _ -> PathDataT
     ColorV _     -> ColorT
     FileV  _     -> FileT
     StyleV _     -> StyleT
     TransformV _ -> TransformT
     HMatrixV _ -> HMatrixT
     MapV _ -> MapT

toPolymorphics :: [Shape Double] -> (forall a . (Autofloat a) => [Shape a])
toPolymorphics = map toPolymorphic

toPolymorphic :: Shape Double -> (forall a . (Autofloat a) => Shape a)
toPolymorphic (ctor, properties) = (ctor, M.map toPolyProperty properties)

toPolyProperty :: Value Double -> (forall a . (Autofloat a) => Value a)
toPolyProperty v = case v of
    -- Not sure why these have to be rewritten from scratch...
    FloatV n  -> FloatV $ r2f n
    BoolV x   -> BoolV x
    StrV x    -> StrV x
    IntV x    -> IntV x
    PtV p -> PtV $ r2 p
    PtListV xs  -> PtListV $ map r2 xs
    ColorV x  -> ColorV x
    FileV x   -> FileV x
    StyleV x  -> StyleV x
    PathDataV es -> PathDataV $ map toPolyPath es
    TransformV t -> TransformV $ Transformation { rotation = r2f $ rotation t,
                                                  center_r = r2 $ center_r t,
                                                  scaleXY = r2 $ scaleXY t,
                                                  dxy = r2 $ dxy t }
    HMatrixV m -> HMatrixV $ HMatrix { xScale = r2f $ xScale m,
                                       xSkew = r2f $ xSkew m,
                                       ySkew = r2f $ ySkew m,
                                       yScale = r2f $ yScale m,
                                       dx = r2f $ dx m,
                                       dy = r2f $ dy m
                                     }
    where r2 (x, y) = (r2f x, r2f y)

toPolyPath :: Path' Double -> (forall a . (Autofloat a) => Path' a)
toPolyPath (Closed es) = Closed $ map toPolyElem es
toPolyPath (Open es) = Open $ map toPolyElem es

toPolyElem :: Elem Double -> (forall a . (Autofloat a) => Elem a)
toPolyElem (Pt p) = Pt $ r2ft p
toPolyElem (CubicBez (p0, p1, p2)) = CubicBez (r2ft p0, r2ft p1, r2ft p2)
toPolyElem (CubicBezJoin (p0, p1)) = CubicBezJoin (r2ft p0, r2ft p1)
toPolyElem (QuadBez (p0, p1)) = QuadBez (r2ft p0, r2ft p1)
toPolyElem (QuadBezJoin p) = QuadBezJoin $ r2ft p
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
    where zipWithKey = map (\x -> (fst x, x))

shapeDefList :: (Autofloat a) => [ShapeDef a]
shapeDefList = [ anchorPointType, circType, ellipseType, arrowType, braceType, curveType, lineType, rectType, squareType, parallelogramType, imageType, textType, arcType, rectTransformType ]

-- | retrieve type strings of all shapes
shapeTypes :: (Autofloat a) => ShapeDefs a -> [ShapeTypeStr]
shapeTypes defs = map fst $ M.toList defs

-- | given a type string, find the corresponding shape def
findDef :: (Autofloat a) => ShapeTypeStr -> ShapeDef a
findDef typ = fromMaybe
    (noShapeError "findDef" typ)
    (M.lookup typ shapeDefs)

-- | given a shape def, construct a default shape
defaultShapeOf :: (Autofloat a) => StdGen -> ShapeDef a -> (Shape a, StdGen)
defaultShapeOf g (t, propDict) =
    let (properties, g') = sampleProperties g propDict in
    ((t, properties), g')

defaultValueOf :: (Autofloat a) => StdGen -> PropID -> ShapeDef a -> (Value a, StdGen)
defaultValueOf g prop (t, propDict) =
    let sampleF = snd $ fromMaybe
            (noPropError "defaultValueOf" prop t)
            (M.lookup prop propDict)
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

computedProperties :: (Autofloat a) => M.Map (ShapeTypeStr, Property) (ComputedValue a)
computedProperties = M.fromList [computeTransformation]

computeTransformation :: (Autofloat a) => ((ShapeTypeStr, Property), ComputedValue a)
computeTransformation = (("RectangleTransform", "transformation"), (props, fn))
    where props = ["sizeX", "sizeY", "rotation", "x", "y", "transform"]
          fn :: (Autofloat a) => [Value a] -> Value a
          fn [FloatV sizeX, FloatV sizeY, FloatV rotation, 
              FloatV x, FloatV y, HMatrixV customTransform] = 
             let defaultTransform = paramsToMatrix (sizeX, sizeY, rotation, x, y) in
             HMatrixV $ customTransform # defaultTransform

-- TODO: add ones for polygon, final properties; also refactor so it's more generic across shapes

------ Polygonization code

getParams :: (Autofloat a) => Shape a -> (a, a, a, a, a)
getParams s@("RectangleTransform", props) =
          (getNum s "sizeX", getNum s "sizeY", getNum s "rotation", getX s, getY s)
getParams _ = error "TODO: getParams not yet implemented for this shape"

-- Polygonize shape (or get unit polygon) and apply the transform to it to get a final polygon
polygonOf :: (Autofloat a) => Shape a -> [Pt2 a]
polygonOf s@("RectangleTransform", props) =
          -- Apply transform to a unit square centered about the origin
          -- First make default transform for scaling, rotation, then translation
          let params = getParams s in
          let defaultTransform = paramsToMatrix params in
          let customTransform = getTransform s in
          let fullTransform = customTransform # defaultTransform in
          transformPoly fullTransform unitSq
polygonOf _ = error "TODO: polygonOf not yet implemented for this shape"

-- TODO: function to set polygon property?

--------------------------------------------------------------------------------
-- Property samplers

type SampledValue a = StdGen -> (Value a, StdGen)
type FloatInterval = (Float, Float)

rndInterval :: (Float, Float)
rndInterval = (0, canvasWidth / 6)

-- COMBAK: SHAME. Parametrize the random generators properly!
canvasHeight, canvasWidth :: Float
canvasHeight = 700.0
canvasWidth  = 800.0

debugRng :: StdGen
debugRng = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

constValue :: (Autofloat a) => Value a -> SampledValue a
constValue v g = (v, g)

-- NOTE: this function does not enforce that all values have the same type
sampleDiscrete :: (Autofloat a) => [Value a] -> SampledValue a
sampleDiscrete list g =
    let (idx, g') = randomR (0, length list - 1) g
    in (list !! idx, g')

sampleFloatIn :: (Autofloat a) => FloatInterval -> SampledValue a
sampleFloatIn interval g =
    let (n, g') = randomR interval g in (FloatV $ r2f n, g')

samplePointIn :: (Autofloat a) =>
    (FloatInterval, FloatInterval)  -> SampledValue a
samplePointIn (interval1, interval2) g =
    let (n1, g1) = randomR interval1 g
        (n2, g2) = randomR interval2 g1
    in (PtV (r2f n1, r2f n2), g2)

sampleColor :: (Autofloat a) => SampledValue a
sampleColor rng =
    let interval = (0.1, 0.9)
        (r, rng1)  = randomR interval rng
        (g, rng2)  = randomR interval rng1
        (b, rng3)  = randomR interval rng2
    in (ColorV $ makeColor r g b 0.5, rng3)
        -- (a, rng4)  = randomR (0.3, 0.7) rng3
    -- in (ColorV $ makeColor r g b a, rng4)

-- | Samples all properties of input shapes
sampleShapes :: (Autofloat a) => StdGen -> [Shape a] -> ([Shape a], StdGen)
sampleShapes g shapes =
    let (shapes', g') = foldl sampleShape ([], g) shapes
    in (reverse shapes', g')
sampleShape (shapes, g) oldShape@(typ, oldProperties) =
    let (_, propDefs)    = findDef typ
        (properties, g') = sampleProperties g propDefs
        shape            = (typ, properties)
        namedShape       = setName (getName oldShape) shape
    in (namedShape : shapes, g')

sampleProperties :: (Autofloat a) => StdGen -> PropertiesDef a -> (Properties a, StdGen)
sampleProperties g propDefs = M.foldlWithKey sampleProperty (M.empty, g) propDefs

sampleProperty :: (Autofloat a) => (Properties a, StdGen) -> PropID -> (ValueType, SampledValue a) -> (Properties a, StdGen)
sampleProperty (properties, g) propID (typ, sampleF) =
    let (val, g') = sampleF g in (M.insert propID val properties, g')

--------------------------------------------------------------------------------
-- Example shape defs

-- | TODO: derived properties
-- | TODO: instantiation of objs with (1) default values; (2) random sampling w.r.t. constraints
-- constructShape :: ShapeDef a -> [SampleRule] -> Shape a

canvasDims :: (Float, Float)
canvasDims = (-canvasHeight / 2, canvasHeight / 2)

-- TODO: change everything to camel case
x_sampler, y_sampler, pointSampler, width_sampler, height_sampler, angle_sampler, stroke_sampler, stroke_style_sampler, bool_sampler :: (Autofloat a) => SampledValue a
x_sampler = sampleFloatIn canvasDims
y_sampler = sampleFloatIn canvasDims
pointSampler = samplePointIn (canvasDims, canvasDims)
width_sampler = sampleFloatIn (3, canvasWidth / 6)
height_sampler = sampleFloatIn (3, canvasHeight / 6)
angle_sampler = sampleFloatIn (0, 360) -- TODO: check that frontend uses degrees, not radians
stroke_sampler = sampleFloatIn (0.5, 3)
stroke_style_sampler = sampleDiscrete [StrV "dashed", StrV "solid"]
bool_sampler = sampleDiscrete [BoolV True, BoolV False]

anchorPointType, circType, ellipseType, arrowType, braceType, curveType, lineType, rectType, squareType, parallelogramType, imageType, textType, arcType, rectTransformType :: (Autofloat a) => ShapeDef a

anchorPointType = ("AnchorPoint", M.fromList
    [
        -- ("x", (FloatT, x_sampler)),
        -- ("y", (FloatT, y_sampler)),
        ("location", (PtT, pointSampler)),
        ("name", (StrT, constValue $ StrV "defaultAnchorPoint"))
    ])

circType = ("Circle", M.fromList
    [
        -- ("x", (FloatT, constValue $ FloatV 100)),
        -- ("y", (FloatT, constValue $ FloatV 100)),
        ("x", (FloatT, x_sampler)),
        ("y", (FloatT, y_sampler)),
        ("r", (FloatT, width_sampler)),
        ("strokeWidth", (FloatT, stroke_sampler)),
        ("style", (StrT, constValue $ StrV "filled")),
        ("strokeStyle", (StrT, constValue $ StrV "solid")),
        ("strokeColor", (ColorT, sampleColor)),
        ("color", (ColorT, sampleColor)),
        ("name", (StrT, constValue $ StrV "defaultCircle"))
    ])

ellipseType = ("Ellipse", M.fromList
    [
        ("x", (FloatT, x_sampler)),
        ("y", (FloatT, y_sampler)),
        ("rx", (FloatT, width_sampler)),
        ("ry", (FloatT, height_sampler)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("stroke-width", (FloatT, stroke_sampler)),
        ("style", (StrT, sampleDiscrete [StrV "filled"])),
        ("stroke-style", (StrT, stroke_style_sampler)),
        ("color", (ColorT, sampleColor)),
        ("name", (StrT, constValue $ StrV "defaultEllipse"))
    ])

textType = ("Text", M.fromList
    [
        ("x", (FloatT, sampleFloatIn (-canvasWidth / 2, canvasWidth / 2))),
        ("y", (FloatT, sampleFloatIn (-canvasHeight / 2, canvasHeight / 2))),
        ("w", (FloatT, constValue $ FloatV 0)), -- NOTE: updated by front-end
        ("h", (FloatT, constValue $ FloatV 0)), -- NOTE: updated by front-end
        ("string", (StrT, constValue $ StrV "defaultLabelText")),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("style", (StrT, constValue $ StrV "none")),
        ("stroke", (StrT, constValue $ StrV "none")),
        ("color", (ColorT, constValue $ ColorV black)),
        ("name", (StrT, constValue $ StrV "defaultCircle"))
    ])

arrowType = ("Arrow", M.fromList
    [
        ("startX", (FloatT, x_sampler)),
        ("startY", (FloatT, y_sampler)),
        ("endX", (FloatT, x_sampler)),
        ("endY", (FloatT, y_sampler)),
        ("thickness", (FloatT, sampleFloatIn (5, 15))),
        ("style", (StrT, constValue $ StrV "straight")),
        ("color", (ColorT, sampleColor)),
        ("name", (StrT, constValue $ StrV "defaultArrow")),
        ("rotation", (FloatT, constValue $ FloatV 0.0))
    ])

braceType = ("Brace", M.fromList
    [
        ("startX", (FloatT, x_sampler)),
        ("startY", (FloatT, y_sampler)),
        ("endX", (FloatT, x_sampler)),
        ("endY", (FloatT, y_sampler)),
        ("color", (ColorT, sampleColor)),
        ("thickness", (FloatT, sampleFloatIn (1, 6))),
        ("name", (StrT, constValue $ StrV "defaultBrace"))
      ])

curveType = ("Curve", M.fromList
    [
        ("path", (PtListT, constValue $ PtListV [])), -- TODO: sample path
        ("polyline", (PtListT, constValue $ PtListV [])), -- TODO: sample path
        ("pathData", (PathDataT, constValue $ PathDataV [])), -- TODO: sample path
        ("strokeWidth", (FloatT, stroke_sampler)),
        ("style", (StrT, constValue $ StrV "solid")),
        ("fill", (ColorT, sampleColor)), -- for no fill, set opacity to 0
        ("color", (ColorT, sampleColor)),
        ("left-arrowhead", (BoolT, constValue $ BoolV False)),
        ("right-arrowhead", (BoolT, constValue $ BoolV False)),
        ("name", (StrT, constValue $ StrV "defaultCurve"))
    ])

lineType = ("Line", M.fromList
    [
        ("startX", (FloatT, x_sampler)),
        ("startY", (FloatT, y_sampler)),
        ("endX", (FloatT, x_sampler)),
        ("endY", (FloatT, y_sampler)),
        -- TODO: deal with derived property "path" here or in the frontend
        ("thickness", (FloatT, sampleFloatIn (5, 15))),
        -- TODO: list the possible styles for each attribute of each GPI
        ("color", (ColorT, sampleColor)),
        ("style", (StrT, constValue $ StrV "solid")),
        ("stroke", (StrT, constValue $ StrV "none")),
        ("name", (StrT, constValue $ StrV "defaultLine"))
    ])

rectType = ("Rectangle", M.fromList
    [
        ("x", (FloatT, x_sampler)),
        ("y", (FloatT, y_sampler)),
        ("sizeX", (FloatT, width_sampler)),
        ("sizeY", (FloatT, height_sampler)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("color", (ColorT, sampleColor)),
        ("strokeWidth", (FloatT, stroke_sampler)),
        ("style", (StrT, constValue $ StrV "filled")),
        ("strokeColor", (ColorT, sampleColor)),
        ("strokeStyle", (StrT, constValue $ StrV "none")),
        ("name", (StrT, constValue $ StrV "defaultRect"))
    ])

rectTransformType = ("RectangleTransform", M.fromList
    [
        -- These attributes serve as DOF in the default transformation
        -- They are NOT the final x, etc.
        ("x", (FloatT, constValue $ FloatV 0.0)),
        ("y", (FloatT, constValue $ FloatV 0.0)),
        ("sizeX", (FloatT, constValue $ FloatV 1.0)),
        ("sizeY", (FloatT, constValue $ FloatV 1.0)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),

        ("transform", (FloatT, constValue $ HMatrixV idH)),
        ("transformation", (FloatT, constValue $ HMatrixV idH)), -- Computed

        -- TODO compute these
        -- ("finalX", (FloatT, constValue $ FloatV 0.0)),
        -- ("finalY", (FloatT, constValue $ FloatV 0.0)),
        -- ("finalSizeX", (FloatT, constValue $ FloatV 1.0)),
        -- ("finalSizeY", (FloatT, constValue $ FloatV 1.0)),
        -- ("finalRotation", (FloatT, constValue $ FloatV 0.0)),

        ("color", (ColorT, sampleColor)),
        ("strokeWidth", (FloatT, stroke_sampler)),
        ("style", (StrT, constValue $ StrV "filled")),
        ("strokeColor", (ColorT, sampleColor)),
        ("strokeStyle", (StrT, constValue $ StrV "none")),
        ("name", (StrT, constValue $ StrV "defaultRect")),
        ("polygon", (PtListT, constValue $ PtListV []))
    ])

squareType = ("Square", M.fromList
    [
        ("x", (FloatT, x_sampler)),
        ("y", (FloatT, y_sampler)),
        ("side", (FloatT, width_sampler)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        -- TODO: distinguish between stroke color and fill color everywhere
        ("color", (ColorT, sampleColor)),
        ("style", (StrT, constValue $ StrV "none")), -- TODO: what is this?
        ("strokeColor", (ColorT, sampleColor)),
        ("strokeWidth", (FloatT, constValue $ FloatV 0.0)),
        ("name", (StrT, constValue $ StrV "defaultSquare"))
    ])

parallelogramType = ("Parallelogram", M.fromList
    [
        ("x", (FloatT, x_sampler)), -- (x, y) is the bottom-left corner of the parallelogram
        ("y", (FloatT, y_sampler)),
        ("lengthX", (FloatT, width_sampler)),
        ("lengthY", (FloatT, height_sampler)),
        ("angle", (FloatT, constValue $ FloatV 0.0)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("color", (ColorT, sampleColor)),
        ("stroke-style", (StrT, stroke_style_sampler)),
        ("stroke-color",  (ColorT, sampleColor)),
        ("name", (StrT, constValue $ StrV "defaultParallelogram"))
    ])

imageType = ("Image", M.fromList
    [
        ("centerX", (FloatT, x_sampler)),
        ("centerY", (FloatT, y_sampler)),
        ("lengthX", (FloatT, width_sampler)),
        ("lengthY", (FloatT, height_sampler)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("style", (StrT, constValue $ StrV "none")),
        ("stroke", (StrT, constValue $ StrV "none")),
        ("path", (StrT, constValue $ StrV "missing image path")), -- Absolute path (URL)
        ("name", (StrT, constValue $ StrV "defaultImage"))
    ])

arcType = ("Arc", M.fromList
    [
        ("x", (FloatT, x_sampler)), -- x,y are the cordinates for the bottom left position
        ("y", (FloatT, y_sampler)), -- of the right angle or the midlle pos of a regular angle
        ("r", (FloatT, width_sampler)),
        ("size", (FloatT, width_sampler)),
        ("lengthX", (FloatT, width_sampler)),
        ("lengthY", (FloatT, height_sampler)),
        ("angle", (FloatT, angle_sampler)),
        ("rotation", (FloatT, constValue $ FloatV 0.0)),
        ("isRight", (BoolT, bool_sampler)), -- This property overrides the angle property
        ("color", (ColorT, sampleColor)),
        ("style", (StrT, constValue $ StrV "none")),
        ("stroke", (StrT, constValue $ StrV "none")),
        ("name", (StrT, constValue $ StrV "defaultArc"))
    ])

-----

exampleCirc :: (Autofloat a) => Shape a
exampleCirc = ("Circle", M.fromList
    [
        ("x", FloatV 5.5),
        ("y", FloatV 100.2),
        ("r", FloatV 5),
        ("name", StrV "exampleCirc"),
        ("style", StyleV "filled"),
        ("color", ColorV black)
    ])

------------

rectTransformType' :: (Autofloat a) => ShapeDef' a
rectTransformType' = ("RectangleTransform", M.fromList
    [
        ("x", (FloatT, Left $ constValue $ FloatV 0.0)),
        ("y", (FloatT, Left $ constValue $ FloatV 0.0)),
        ("sizeX", (FloatT, Left $ constValue $ FloatV 100.0)), -- TODO should be unit size
        ("sizeY", (FloatT, Left $ constValue $ FloatV 100.0)),
        ("rotation", (FloatT, Left $ constValue $ FloatV 0.0)),
        ("transform", (MapT, Right $ hmatrixSchema)),
        ("color", (ColorT, Left $ sampleColor)),
        ("strokeWidth", (FloatT, Left $ stroke_sampler)),
        ("style", (StrT, Left $ constValue $ StrV "filled")),
        ("strokeColor", (ColorT, Left $ sampleColor)),
        ("strokeStyle", (StrT, Left $ constValue $ StrV "none")),
        ("name", (StrT, Left $ constValue $ StrV "defaultRect"))
    ]
    )

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
    -- TODO write out full procedure

findShape :: (Autofloat a) => String -> [Shape a] -> Shape a
findShape shapeName shapes =
    case filter (\s -> getName s == shapeName) shapes of
        [x] -> x
        _   -> error ("findShape: expected one shape for \"" ++ shapeName ++ "\", but did not find just one (returned zero or many).")

-- TODO: can use alter, update, adjust here. Come back if performance matters
-- | Setting the value of a property
set :: (Autofloat a) => Shape a -> PropID -> Value a -> Shape a
set (t, propDict) prop val = case M.lookup prop propDict of
    Nothing -> noPropError "set" prop t
    _       -> (t, M.update (const $ Just val) prop propDict)

-- | Getting the value of a property
get :: (Autofloat a) => Shape a -> PropID -> Value a
get (t, propDict) prop = fromMaybe
    (noPropError "get" prop t)
    (M.lookup prop propDict)
{-# INLINE get #-}

-- | batch get
getAll :: (Autofloat a) => Shape a -> [PropID] -> [Value a]
getAll shape = map (get shape)

-- | batch set
setAll :: (Autofloat a) => Shape a -> [(PropID, Value a)] -> Shape a
setAll = foldl (\s (k, v) -> set s k v)


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
typeOfProperty prop (t, propDict) = case M.lookup prop propDict of
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
{-# INLINE (.:) #-}

getX, getY :: (Autofloat a) => Shape a -> a
getX shape = case shape .: "x" of
    FloatV x -> x
    _ -> error "getX: expected float but got something else"
getY shape = case shape .: "y" of
    FloatV y -> y
    _ -> error "getY: expected float but got something else"

getPoint :: (Autofloat a) => String -> Shape a -> (a, a)
getPoint "start" shape = case (shape .: "startX", shape .: "startY") of
   (FloatV x, FloatV y) -> (x, y)
   _ -> error "getPoint expected two floats but got something else"
getPoint "end" shape = case (shape .: "endX", shape .: "endY") of
   (FloatV x, FloatV y) -> (x, y)
   _ -> error "getPoint expected two floats but got something else"
getPoint _ shape = error "getPoint did not receive existing property name"

-- To use with vector operations
getPointV :: (Autofloat a) => String -> Shape a -> [a]
getPointV "start" shape = case (shape .: "startX", shape .: "startY") of
   (FloatV x, FloatV y) -> [x, y]
   _ -> error "getPointV expected two floats but got something else"
getPointV "end" shape = case (shape .: "endX", shape .: "endY") of
   (FloatV x, FloatV y) -> [x, y]
   _ -> error "getPointV expected two floats but got something else"
getPointV _ shape = error "getPointV did not receive existing property name"

getName :: (Autofloat a) => Shape a -> String
getName shape = case shape .: "name" of
    StrV s -> s
    _ -> error "getName: expected string but got something else"

setName :: (Autofloat a) => String -> Shape a -> Shape a
setName v shape = set shape "name" (StrV v)

setX, setY :: (Autofloat a) => Value a -> Shape a -> Shape a
setX v shape@("Arrow", _) = set shape "startX" v
setX v shape = set shape "x" v

setY v shape@("Arrow", _) = set shape "startY" v
setY v shape = set shape "y" v

getNum :: (Autofloat a) => Shape a -> PropID -> a
getNum shape prop = case shape .: prop of
    FloatV x -> x
    res -> error ("getNum: expected float but got something else: " ++ show res)

getPath :: (Autofloat a) => Shape a -> [Pt2 a]
getPath shape = case shape .: "path" of
    PtListV x -> x
    _ -> error "getPath: expected [(Float, Float)] but got something else"

getPathData :: (Autofloat a) => Shape a -> PathData a
getPathData shape = case shape .: "pathData" of
    PathDataV x -> x
    _ -> error "getPathData: expected [PathData a] but got something else"

-- | Apply a function to each point in a path
mapPathData :: (Autofloat a) => (Pt2 a -> Pt2 a) -> PathData a -> PathData a
mapPathData f pd = map (mapPath f) pd

mapPath :: (Autofloat a) => (Pt2 a -> Pt2 a) -> Path' a -> Path' a
mapPath f (Closed es) = Closed $ map (mapElem f) es
mapPath f (Open es)   = Open $ map (mapElem f) es

mapElem :: (Autofloat a) => (Pt2 a -> Pt2 a) -> Elem a -> Elem a
mapElem f (Pt p) = Pt $ f p
mapElem f (CubicBez (p0, p1, p2)) = CubicBez (f p0, f p1, f p2)
mapElem f (CubicBezJoin (p0, p1)) = CubicBezJoin (f p0, f p1)
mapElem f (QuadBez (p0, p1)) = QuadBez (f p0, f p1)
mapElem f (QuadBezJoin p) = QuadBezJoin $ f p

-- | Add an offset to each point in a path
translatePath :: (Autofloat a) => Pt2 a -> PathData a -> PathData a
translatePath offset pd = mapPathData (+: offset) pd

-- | Figure out whether to translate pathManual or pathData
movePath :: (Autofloat a) => (a, a) -> Shape a -> Shape a
movePath offset shape =
      let pathManual = getPath shape
          pathData   = getPathData shape in
      case (pathManual, pathData) of
      (_:_, []) -> let res = (map (+: offset) pathManual) in
                   set shape "path" (PtListV res)
      ([], _:_) -> let res = translatePath offset pathData in
                   set shape "pathData" (PathDataV res)
      (_:_, _:_) -> error "shape can't have both path manually set and path data"
      ([], []) -> error "shape has no path or data"

-- | Transform utils
getTransform :: (Autofloat a) => Shape a -> HMatrix a
getTransform shape = case shape .: "transform" of
    HMatrixV x -> x
    _ -> error "getTransform: expected HMatrix but got something else"

-- | ternary op for set (TODO: maybe later)
-- https://wiki.haskell.org/Ternary_operator

-- | HACK: returns true of a property of a shape is not supposed to be
-- | changed by the optimizer
isPending :: ShapeTypeStr -> PropID -> Bool
isPending typ propId = propId `elem` pendingProperties typ

-- | HACK: returns all "pending" properties that are undeterminded until
-- | rendered by the frontend
pendingProperties :: ShapeTypeStr -> [PropID]
pendingProperties "Text" = ["w", "h"]
pendingProperties _ = []

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
foldlProperties f accum (_, propDict)  =  M.foldl f accum propDict

foldlPropertyDefs :: (Autofloat a) => (b -> (ValueType, SampledValue a) -> b) -> b -> ShapeDef a -> b
foldlPropertyDefs f accum (_, propDict) =  M.foldl f accum propDict

foldlPropertyMappings :: (Autofloat a) =>
    (b -> PropID -> (ValueType, SampledValue a) -> b) -> b -> ShapeDef a -> b
foldlPropertyMappings f accum (_, propDict) =  M.foldlWithKey f accum propDict

--------------------------------------------------------------------------------
-- Error Msgs

noShapeError functionName shapeType =
    error (functionName ++ ": Shape \"" ++ shapeType ++ "\" does not exist")
noPropError functionName prop shapeType =
    error (functionName ++ ": Property \"" ++ prop ++
        "\" does not exist in shape \"" ++ shapeType ++ "\"")


--------------------------------------------------------------------------------
-- Color definition
-- Adopted from gloss: https://github.com/benl23x5/gloss/blob/c63daedfe3b60085f8a9e810e1389cbc29110eea/gloss-rendering/Graphics/Gloss/Internals/Data/Color.hs

data Color
    -- | Holds the color components. All components lie in the range [0..1.
    = RGBA  !Float !Float !Float !Float
    deriving (Show, Eq, Generic)
instance ToJSON   Color
instance FromJSON Color

-- | Make a custom color. All components are clamped to the range  [0..1].
makeColor :: Float        -- ^ Red component.
          -> Float        -- ^ Green component.
          -> Float        -- ^ Blue component.
          -> Float        -- ^ Alpha component.
          -> Color
makeColor r g b a
        = clampColor
        $ RGBA r g b a
{-# INLINE makeColor #-}

-- | Take the RGBA components of a color.
rgbaOfColor :: Color -> (Float, Float, Float, Float)
rgbaOfColor (RGBA r g b a)      = (r, g, b, a)
{-# INLINE rgbaOfColor #-}

-- | Clamp components of a raw color into the required range.
clampColor :: Color -> Color
clampColor cc
   = let  (r, g, b, a)    = rgbaOfColor cc
     in   RGBA (min 1 r) (min 1 g) (min 1 b) (min 1 a)

black, white :: Color
black = makeColor 0.0 0.0 0.0 1.0
white = makeColor 1.0 1.0 1.0 1.0

makeColor' :: (Autofloat a) => a -> a -> a -> a -> Color
makeColor' r g b a = makeColor (r2f r) (r2f g) (r2f b) (r2f a)

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
