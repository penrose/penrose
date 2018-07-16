{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module ShapeDef where

import Utils
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

-- | types of fully evaluated values in Style
data ValueType
    = FloatT
    | IntT
    | BoolT
    | StrT
    | PtT
    | PathT
    | ColorT
    | FileT
    | StyleT
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
    -- | a list of points
    | PathV [Pt2 a]
    -- | an RGBA color value
    | ColorV Color
    -- | path for image
    | FileV String
    -- | dotted, etc.
    | StyleV String
    deriving (Generic, Eq, Show)

instance (FromJSON a) => FromJSON (Value a)
instance (ToJSON a)   => ToJSON (Value a)

-- | returns the type of a 'Value'
typeOf :: (Autofloat a) => Value a -> ValueType
typeOf v = case v of
     FloatV _ -> FloatT
     IntV   _ -> IntT
     BoolV  _ -> BoolT
     StrV   _ -> StrT
     PtV    _ -> PtT
     PathV  _ -> PathT
     ColorV _ -> ColorT
     FileV  _ -> FileT
     StyleV _ -> StyleT


-- | the type string of a shape
type ShapeTypeStr = String
-- | the string identifier of a property
type PropID = String

-- | A dict storing names, types, and default values of properties
type PropertiesDef a = M.Map PropID (ValueType, Value a)
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
shapeDefs = M.fromList $ zipWithKey defList
    where defList = [circType, arrowType, curveType]
          zipWithKey = map (\x -> (fst x, x))

-- | retrieve type strings of all shapes
shapeTypes :: (Autofloat a) => ShapeDefs a -> [ShapeTypeStr]
shapeTypes defs = map fst $ M.toList defs

-- | given a type string, find the corresponding shape def
findDef :: (Autofloat a) => ShapeTypeStr -> ShapeDefs a -> ShapeDef a
findDef typ defs = fromMaybe
    (noShapeError "findDef" typ)
    (M.lookup typ defs)

-- | given a shape def, construct a default shape
defaultShapeOf :: (Autofloat a) => ShapeDef a -> Shape a
defaultShapeOf (t, propDict) = (t, M.map snd propDict)

defaultValueOf :: (Autofloat a) => PropID -> ShapeDef a -> Value a
defaultValueOf prop (t, propDict) = snd $ fromMaybe
    (noPropError "defaultValueOf" prop t)
    (M.lookup prop propDict)

--------------------------------------------------------------------------------
-- Example shape defs


-- | TODO: derived properties
-- | TODO: instantiation of objs with (1) default values; (2) random sampling w.r.t. constraints
-- constructShape :: ShapeDef a -> [SampleRule] -> Shape a

circType, arrowType, curveType :: (Autofloat a) => ShapeDef a
circType = ("Circle", M.fromList
    [
        ("x", (FloatT, FloatV 100.0)),
        ("y", (FloatT, FloatV 50.0)),
        ("r", (FloatT, FloatV 10.0)),
        ("stroke-width", (IntT, IntV 0)),
        ("name", (StrT, StrV "defaultCircle")),
        ("style", (StrT, StrV "filled")),
        ("color", (ColorT, ColorV black))
    ])
arrowType = ("Arrow", M.fromList
    [
        ("startx", (FloatT, FloatV 0.0)),
        ("starty", (FloatT, FloatV 0.0)),
        ("endx", (FloatT, FloatV 0.0)),
        ("endy", (FloatT, FloatV 0.0)),
        ("name", (StrT, StrV "defaultArrow")),
        ("style", (StrT, StrV "straight")),
        ("color", (ColorT, ColorV black))
    ])
curveType = ("Curve", M.fromList
    [
        ("path", (PathT, PathV [])),
        ("name", (StrT, StrV "defaultCurve")),
        ("style", (StrT, StrV "solid")),
        ("color", (ColorT, ColorV black))
    ])

exampleCirc :: (Autofloat a) => Shape a
exampleCirc = ("Circle", M.fromList
    [
        ("x", FloatV 5.5),
        ("y", FloatV 100.2),
        ("r", FloatV 5),
        ("name", StrV "C1"),
        ("style", StyleV "filled"),
        ("color", ColorV black)
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

-- | batch get
getAll :: (Autofloat a) => Shape a -> [PropID] -> [Value a]
getAll shape = map (get shape)

-- | batch set
setAll :: (Autofloat a) => Shape a -> [(PropID, Value a)] -> Shape a
setAll = foldl (\s (k, v) -> set s k v)


-- | reset a property to its default value
reset :: (Autofloat a) => Shape a -> PropID -> Shape a
reset (t, propDict) prop =
    let val = defaultValueOf prop $ findDef t shapeDefs
    in (t, M.update (const $ Just val) prop propDict)

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

getX, getY :: (Autofloat a) => Shape a -> a
getX shape = case shape .: "x" of
    FloatV x -> x
    _ -> error "getX: expected float but got something else"
getY shape = case shape .: "y" of
    FloatV y -> y
    _ -> error "getY: expected float but got something else"

getName :: (Autofloat a) => Shape a -> String
getName shape = case shape .: "name" of
    StrV s -> s
    _ -> error "getName: expected string but got something else"

setX, setY :: (Autofloat a) => Value a -> Shape a -> Shape a
setX v shape = set shape "x" v
setY v shape = set shape "y" v

getNum :: (Autofloat a) => Shape a -> PropID -> a
getNum shape prop = case shape .: prop of
    FloatV x -> x
    _ -> error "getNum: expected float but got something else"

-- | ternary op for set (TODO: maybe later)
-- https://wiki.haskell.org/Ternary_operator

-- | Given 'ValueType' and 'ShapeTypeStr', return all props of that ValueType
propertiesOf :: (Autofloat a) =>
    ValueType -> ShapeTypeStr -> ShapeDefs a -> [PropID]
propertiesOf propType shapeType defs =
    M.keys $ M.filter (\(t, _) -> t == propType) $ snd $ findDef shapeType defs

-- | Map over all properties of a shape
-- TODO: withKey?
mapProperties :: (Autofloat a) => (Value a -> Value a) -> Shape a -> Shape a
mapProperties f (t, propDict) = (t, M.map f propDict)

-- | fold over all properties of a shape
-- TODO: withKey?
foldlProperties :: (Autofloat a) => (b -> Value a -> b) -> b -> Shape a -> b
foldlProperties f accum (_, propDict)  =  M.foldl f accum propDict

foldlPropertyDefs :: (Autofloat a) => (b -> (ValueType, Value a) -> b) -> b -> ShapeDef a -> b
foldlPropertyDefs f accum (_, propDict) =  M.foldl f accum propDict

foldlPropertyMappings :: (Autofloat a) =>
    (b -> PropID -> (ValueType, Value a) -> b) -> b -> ShapeDef a -> b
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
