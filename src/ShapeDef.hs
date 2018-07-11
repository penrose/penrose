module ShapeDef where

import Utils
import qualified Data.Map.Strict as M

--------------------------------------------------------------------------------
-- Types

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

-- | fully evaluated values in Style program
data Value a
    -- | Floating point number
    = VNum a
    -- | integer
    | VInt Integer
    -- | boolean value
    | VBool Bool
    -- | string literal
    | VStr String
    -- | point in R^2
    | VPt (Pt2 a)
    -- | a list of points
    | VPath [Pt2 a]
    -- | an RGBA color value
    | VColor Color
    -- | path for image
    | VFile String
    -- | dotted, etc.
    | VStyle String
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

-- | A dictionary storing properties of a Style object, e.g. "start" for 'Arrow'
type PropertiesDef = M.Map PropID ValueType
-- | definition of a new shape/graphical primitive
type ShapeDef = (String, PropertiesDef)

-- | A dictionary storing properties of a Style object, e.g. "start" for 'Arrow'
type Properties a = M.Map PropID (Value a)
-- | definition of a new shape/graphical primitive
type Shape a = (String, Properties a)

--------------------------------------------------------------------------------
-- Example shape defs

circType :: ShapeDef
circType = ("Circ", M.fromList
    [
        ("x", FloatT),
        ("y", FloatT),
        ("stroke", FloatT),
        ("name", StrT),
        ("style", StrT),
        ("color", ColorT)
    ])

exampleCirc :: (Autofloat a) => Shape a
exampleCirc = ("Circ", M.fromList
    [
        ("x", VNum 5.5),
        ("y", VNum 100.2),
        ("name", VStr "C1"),
        ("color", VColor black)
    ])


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
