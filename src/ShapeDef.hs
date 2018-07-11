module ShapeDef where

import Utils
import qualified Data.Map.Strict as M

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
    -- COMBAK: decide whether GPIs or shortcuts to multiple GPIs should be vals
    -- -- | Substance ID
    -- | TAllShapes String
    -- -- | shape ID
    -- | TShape String
    deriving (Eq, Show)

-- | the type string of a shape
type ShapeType = String
-- | the string identifier of a property
type PropID = String

-- | A dict storing names, types, and default values of properties
type PropertiesDef a = M.Map PropID (ValueType, Value a)
-- | definition of a new shape/graphical primitive
type ShapeDef a = (String, PropertiesDef a)
type ShapeDefs a = M.Map String (PropertiesDef a)

-- | A dictionary storing properties of a Style object, e.g. "start" for 'Arrow'
-- COMBAK: serializer to JSON
type Properties a = M.Map PropID (Value a)
-- | definition of a new shape/graphical primitive
type Shape a = (String, Properties a)

--------------------------------------------------------------------------------
-- Example shape defs

shapeDefs :: (Autofloat a) => ShapeDefs a
shapeDefs = M.fromList defList
    where defList = [circType]

circType, arrowType, curveType :: (Autofloat a) => ShapeDef a
circType = ("Circle", M.fromList
    [
        ("x", (FloatT, FloatV 0.0)),
        ("y", (FloatT, FloatV 0.0)),
        ("stroke", (FloatT, FloatV 0.0)),
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
exampleCirc = ("Circ", M.fromList
    [
        ("x", FloatV 5.5),
        ("y", FloatV 100.2),
        ("name", StrV "C1"),
        ("color", ColorV black)
    ])

--------------------------------------------------------------------------------
-- Parser for shape def DSL (TODO)

--------------------------------------------------------------------------------
-- Type checker for a particular shape instance again its def (TODO)

-- checkShape :: (Autofloat a) => Shape a -> ShapeDef a -> Shape a
-- checkShape shape def =

--------------------------------------------------------------------------------
-- Utility functions for Runtime
-- * set
-- * get

-- TODO: can use alter, update, adjust here. Come back if performance matters
-- | Setting the value of a property
set :: (Autofloat a) => Shape a -> PropID -> Value a -> Shape a
set (n, propDict) prop val = case M.lookup prop propDict of
    Nothing -> error ("set: Property \"" ++ prop ++
                    "\" does not exist in shape \"" ++ n)
    _       -> (n, M.update (const $ Just val) prop propDict)

-- | Getting the value of a property
get :: (Autofloat a) => Shape a -> PropID -> Value a
get (n, propDict) prop = case M.lookup prop propDict of
    Nothing -> error ("get: Property \"" ++ prop ++
                    "\" does not exist in shape \"" ++ n)
    Just v  -> v

--------------------------------------------------------------------------------
-- Utility functions for objective/constraint function writers

-- | 'is' checks whether a shape is of a certain type
is :: (Autofloat a) => Shape a -> ShapeType -> Bool
is (t1, _) t2 = t1 == t2

--------------------------------------------------------------------------------
-- Color definition
-- Adopted from gloss: https://github.com/benl23x5/gloss/blob/c63daedfe3b60085f8a9e810e1389cbc29110eea/gloss-rendering/Graphics/Gloss/Internals/Data/Color.hs

data Color
    -- | Holds the color components. All components lie in the range [0..1.
    = RGBA  !Float !Float !Float !Float
    deriving (Show, Eq)

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

--------------------------------------------------------------------------------
-- DEBUG: main function to test out the module

main :: IO ()
main = putStrLn "Hello"
