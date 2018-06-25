-- | "Shapes" contains all geometric primitives that Penrose supports

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module Shapes where
-- module Shapes (Obj, Obj') where
import Data.Aeson
import Data.Monoid ((<>))
import GHC.Generics

import Utils

type Name = String

class Located a b where
      getX :: a -> b
      getY :: a -> b
      setX :: b -> a -> a
      setY :: b -> a -> a

class Named a where
      getName :: a -> Name
      setName :: Name -> a -> a


-------
data CubicBezier = CubicBezier {
    pathcb           :: [(Float, Float)],
    namecb           :: String,
    stylecb          :: String,
    colorcb          :: Color
} deriving (Eq, Show, Generic)

instance Named CubicBezier where
         getName = namecb
         setName x cb = cb { namecb = x }

instance Located CubicBezier Float where
         getX c   = let xs = map fst $ pathcb c in maximum xs - minimum xs
         getY c   = let ys = map snd $ pathcb c in maximum ys - minimum ys
         setX x c = let xs = map fst $ pathcb c
                        dx = x - (maximum xs - minimum xs) in
                        c { pathcb = map (\(xx, yy) -> (xx + dx, yy)) $ pathcb c }
         setY y c = let ys = map snd $ pathcb c
                        dy = y - (maximum ys - minimum ys) in
                        c { pathcb = map (\(xx, yy) -> (xx, yy - dy)) $ pathcb c }

instance ToJSON CubicBezier
instance FromJSON CubicBezier

-------
data Line = Line {
    startx_l         :: Float,
    starty_l         :: Float,
    thickness_l      :: Float,
    endx_l           :: Float,
    endy_l           :: Float,
    name_l           :: String,
    style_l          :: String,
    color_l          :: Color
} deriving (Eq, Show, Generic)

instance Named Line where
         getName = name_l
         setName x l = l { name_l = x }

instance Located Line Float where
         getX l = (startx_l l + endx_l l) / 2
         getY l = (starty_l l + endy_l l) / 2

         setX x l = l { startx_l = x } -- only sets start
         setY y l = l { starty_l = y }

instance ToJSON Line
instance FromJSON Line

-------
data SolidArrow = SolidArrow { startx :: Float
                             , starty :: Float
                             , endx :: Float
                             , endy :: Float
                             , thickness :: Float -- the maximum thickness, i.e. the thickness of the head
                             , selsa :: Bool -- is the arrow currently selected? (mouse is dragging it)
                             , namesa :: String
                             , stylesa :: String -- curved or straight arrow, the default will be straight
                             , colorsa :: Color
                            --  , bbox :: BBox
                         }
         deriving (Eq, Show, Generic)

instance Located SolidArrow Float where
        --  getX a = endx a - startx a
        --  getY a = endy a - starty a
         getX a   = startx a
         getY a   = starty a
         setX x c = c { startx = x } -- TODO
         setY y c = c { starty = y }

instance Named SolidArrow where
         getName a = namesa a
         setName x a = a { namesa = x }

instance ToJSON SolidArrow
instance FromJSON SolidArrow

-------

data Circ = Circ {
    xc      :: Float,
    yc      :: Float,
    r       :: Float,
    strokec :: Float,
    namec   :: String,
    colorc  :: Color,
    stylec  :: String
} deriving (Eq, Show, Generic)

instance Located Circ Float where
         getX = xc
         getY = yc
         setX x c = c { xc = x }
         setY y c = c { yc = y }

instance Named Circ where
         getName = namec
         setName x c = c { namec = x }

instance ToJSON Circ
instance FromJSON Circ

----------------------

data Square = Square {
    xs      :: Float,
    ys      :: Float,
    side    :: Float,
    ang     :: Float,
    strokes :: Float,
    names   :: String,
    styles  :: String,
    colors  :: Color
}
 deriving (Eq, Show, Generic)

instance Located Square Float where
         getX = xs
         getY = ys
         setX x s = s { xs = x }
         setY y s = s { ys = y }

instance Named Square where
         getName = names
         setName x s = s { names = x }

instance ToJSON Square
instance FromJSON Square

-------------------------- (Angle Mark, added by Dor)
data Arc = Arc { xar:: Float -- Starting point for angle mark
                     , yar:: Float
                     , radiusar:: Float -- The radius of the angle mark
                     , sizear:: Float
                     , namear:: String
                     , colorar:: Color
                     , stylear :: String
                     , selar :: Bool -- is the arc currently selected? (mouse is dragging it)
                     , anglear:: Float -- The angle that the angle mark specifies
                     , rotationar:: Float -- The rotation angle of argle mark
                     , isRightar:: String -- Is this a right angle or not
                 }
     deriving (Eq, Show, Generic)

instance Located Arc Float where
         getX ar= xar ar
         getY ar= yar ar
         setX x ar= ar{ xar= x }
         setY y ar= ar{ yar= y }

instance Named Arc where
         getName ar= namear ar
         setName x ar= ar{ namear= x }

instance ToJSON Arc
instance FromJSON Arc

--------------------------

data Rect = Rect { xr :: Float -- center of rect
                     , yr :: Float
                     , sizeX :: Float -- x
                     , sizeY :: Float -- y
                     , angr :: Float -- angle for which the obj is rotated
                     , selr :: Bool
                     , namer :: String
                     , colorr :: Color }
     deriving (Eq, Show, Generic)

instance Located Rect Float where
         getX s = xr s
         getY s = yr s
         setX x s = s { xr = x }
         setY y s = s { yr = y }

-- NO instance for Sized

instance Named Rect where
         getName r = namer r
         setName x r = r { namer = x }

instance ToJSON Rect
instance FromJSON Rect

--------------------------

data Parallelogram = Parallelogram { xpa :: Float -- center of rect
                     , ypa :: Float
                     , sizeXpa :: Float -- x
                     , sizeYpa :: Float -- y
                     , anglepa :: Float -- the angle of the parallelogram
                     , rotationpa :: Float -- the rotation of the parallelogram
                     , selpa :: Bool
                     , namepa :: String
                     , colorpa :: Color }
     deriving (Eq, Show, Generic)

instance Located Parallelogram Float where
         getX pa = xpa pa
         getY pa = ypa pa
         setX x pa = pa { xpa = x }
         setY y pa = pa { ypa = y }

instance Named Parallelogram where
         getName pa = namepa pa
         setName x pa = pa { namepa = x }

instance ToJSON Parallelogram
instance FromJSON Parallelogram

--------------------------

data Label = Label { xl :: Float
                   , yl :: Float
                   , wl :: Float
                   , hl :: Float
                   , textl :: String
                   -- , scalel :: Float  -- calculate h,w from it
                   , sell :: Bool -- selected label
                   , namel :: String }
     deriving (Eq, Show, Generic)

instance Located Label Float where
         getX l = xl l
         getY l = yl l
         setX x l = l { xl = x }
         setY y l = l { yl = y }

instance Named Label where
         getName l = namel l
         setName x l = l { namel = x }

instance ToJSON Label
instance FromJSON Label
------

data Pt = Pt { xp :: Float
             , yp :: Float
             , selp :: Bool
             , namep :: String }
     deriving (Eq, Show, Generic)

instance Located Pt Float where
         getX p = xp p
         getY p = yp p
         setX x p = p { xp = x }
         setY y p = p { yp = y }

instance Named Pt where
         getName p   = namep p
         setName x p = p { namep = x }

instance ToJSON Pt
instance FromJSON Pt

-------------------

data Img = Img { xim :: Float -- center of Img
                     , yim :: Float
                     , sizeXim :: Float -- x
                     , sizeYim :: Float -- y
                     , angim :: Float -- angle for which the obj is rotated
                     , selim :: Bool
                     , nameim :: String
                     , path :: String}
     deriving (Eq, Show, Generic)

instance Located Img Float where
         getX s = xim s
         getY s = yim s
         setX x s = s { xim = x }
         setY y s = s { yim = y }


-- NO instance for Sized

instance Named Img where
         getName im = nameim im
         setName x im = im { nameim = x }

instance ToJSON Img
instance FromJSON Img

----------------------------

data Obj = S Square
         | A SolidArrow
         | R Rect
         | C Circ
         | E Ellipse
         | L Label
         | P Pt
         | AR Arc
         | CB CubicBezier
         | LN Line
         | IM Img
         | PA Parallelogram
         deriving (Eq, Show, Generic)

instance ToJSON Obj
instance FromJSON Obj

---
data Ellipse = Ellipse { xe :: Float
                 , ye :: Float
                 , rx :: Float
                 , ry :: Float
                 , namee :: String
                 , colore :: Color }
     deriving (Eq, Show, Generic)

instance Located Ellipse Float where
         getX = xe
         getY = ye
         setX x c = c { xe = x }
         setY y c = c { ye = y }

instance Named Ellipse where
         getName = namee
         setName x c = c { namee = x }

instance ToJSON Ellipse
instance FromJSON Ellipse

---

instance FromJSON Color where
    parseJSON = withObject "Color" $ \v -> makeColor
           <$> v .: "r"
           <*> v .: "g"
           <*> v .: "b"
           <*> v .: "a"

instance ToJSON Color where
    -- this generates a Value
    toJSON c =
        let (r, g, b, a) = rgbaOfColor  c in
        object ["r" .= r, "g" .= g, "b" .= b, "a" .= a]
    toEncoding c =
        let (r, g, b, a) = rgbaOfColor  c in
        pairs ("r" .= r <> "g" .= g <> "b" .= b <> "a" .= a)

-- TODO: is there some way to reduce the top-level boilerplate?
instance Located Obj Float where
         getX o = case o of
                 C c -> getX c
                 E e -> getX e
                 L l -> getX l
                 P p -> getX p
                 S s -> getX s
                 R r -> getX r
                 A a -> getX a
                 CB c -> getX c
                 LN l -> getX l
                 IM im -> getX im
                 AR ar -> getX ar
                 PA pa -> getX pa

         getY o = case o of
                 C c -> getY c
                 E e -> getY e
                 L l -> getY l
                 P p -> getY p
                 S s -> getY s
                 R r -> getY r
                 A a -> getY a
                 CB c -> getY c
                 LN l -> getY l
                 IM im -> getY im
                 AR ar -> getY ar
                 PA pa -> getY pa

         setX x o = case o of
                C c -> C $ setX x c
                E e -> E $ setX x e
                L l -> L $ setX x l
                P p -> P $ setX x p
                S s -> S $ setX x s
                R r -> R $ setX x r
                A a -> A $ setX x a
                CB c -> CB $ setX x c
                LN l -> LN $ setX x l
                IM im -> IM $ setX x im
                AR ar -> AR $ setX x ar
                PA pa -> PA $ setX x pa

         setY y o = case o of
                C c -> C $ setY y c
                E e -> E $ setY y e
                L l -> L $ setY y l
                P p -> P $ setY y p
                S s -> S $ setY y s
                R r -> R $ setY y r
                A a -> A $ setY y a
                CB c -> CB $ setY y c
                LN l -> LN $ setY y l
                IM im -> IM $ setY y im
                AR ar -> AR $ setY y ar

instance Named Obj where
         getName o = case o of
                 C c   -> getName c
                 E e   -> getName e
                 L l   -> getName l
                 P p   -> getName p
                 S s   -> getName s
                 R r   -> getName r
                 A a   -> getName a
                 CB cb -> getName cb
                 LN l -> getName l
                 IM im -> getName im
                 AR ar -> getName ar
                 PA pa -> getName pa

         setName x o = case o of
                C c   -> C $ setName x c
                E e   -> E $ setName x e
                L l   -> L $ setName x l
                P p   -> P $ setName x p
                S s   -> S $ setName x s
                R r   -> R $ setName x r
                A a   -> A $ setName x a
                CB cb -> CB $ setName x cb
                LN l -> LN $ setName x l
                IM im -> IM $ setName x im
                AR ar -> AR $ setName x ar
                PA pa -> PA $ setName x pa


--------------------------------------------------------------------------------
-- Polymorphic versions of the primitives

data Obj' a
    = C' (Circ' a)
    | E' (Ellipse' a)
    | L' (Label' a)
    | P' (Pt' a)
    | S' (Square' a)
    | R' (Rect' a)
    | PA' (Parallelogram' a)
    | A' (SolidArrow' a)
    | CB' (CubicBezier' a)
    | LN' (Line' a)
    | IM' (Img' a)
    | AR' (Arc' a)
    deriving (Eq, Show)

data SolidArrow' a = SolidArrow' {
    startx'    :: a,
    starty'    :: a,
    endx'      :: a,
    endy'      :: a,
    thickness' :: a, -- the maximum thickness, i.e. the thickness of the head
    selsa'     :: Bool, -- is the circle currently selected? (mouse is dragging it)
    namesa'    :: String,
    stylesa'    :: String,
    colorsa'   :: Color
} deriving (Eq, Show)

data Circ' a = Circ' {
    xc'     :: a,
    yc'     :: a,
    r'      :: a,
    strokec':: a,
    namec'  :: String,
    stylec' :: String,
    colorc' :: Color
} deriving (Eq, Show)

data Ellipse' a = Ellipse' {
    xe' :: a,
    ye' :: a,
    rx' :: a,
    ry' :: a,
    namee'  :: String,
    colore' :: Color
} deriving (Eq, Show)

data Label' a = Label' { xl' :: a -- middle (x, y) of label
                       , yl' :: a
                       , wl' :: a
                       , hl' :: a
                       , textl' :: String
                       , sell' :: Bool -- selected label
                       , namel' :: String }
                       deriving (Eq, Show)

data Pt' a = Pt' { xp' :: a
                 , yp' :: a
                 , selp' :: Bool
                 , namep' :: String }
                 deriving (Eq, Show)

data Square' a  = Square' {
    xs'               :: a,
    ys'               :: a,
    side'             :: a,
    strokes'          :: a,
    ang'              :: Float,
    names'            :: String,
    colors'           :: Color,
    styles'           :: String
} deriving (Eq, Show)


data Arc' a  = Arc' { xar' :: a
                     , yar' :: a
                     , radiusar' :: a
                     , rotationar' :: a
                     , anglear' :: a
                     , selar' :: Bool
                     , stylear' :: String
                     , isRightar' :: String
                     , sizear' :: a
                     , namear' :: String
                     , colorar' :: Color }
                     deriving (Eq, Show)


data Rect' a = Rect' { xr' :: a -- I assume this is top left?
                     , yr' :: a
                     , sizeX' :: a
                     , sizeY' :: a
                     , angr' :: Float -- angle the obj is rotated, TODO make polymorphic
                     , selr' :: Bool
                     , namer' :: String
                     , colorr' :: Color }
     deriving (Eq, Show, Generic)

data Parallelogram' a = Parallelogram' { xpa' :: a -- I assume this is top left?
                     , ypa' :: a
                     , sizeXpa' :: a
                     , sizeYpa' :: a
                     , anglepa' :: Float
                     , rotationpa' :: Float
                     , selpa' :: Bool
                     , namepa' :: String
                     , colorpa' :: Color }
     deriving (Eq, Show, Generic)


data Img' a = Img' { xim' :: a -- I assume this is top left?
                     , yim' :: a
                     , sizeXim' :: a
                     , sizeYim' :: a
                     , selim' :: Bool
                     , angim' :: Float
                     , nameim' :: String
                     , path' :: String } -- angle the obj is rotated, TODO make polymorphic
     deriving (Eq, Show, Generic)


data CubicBezier' a = CubicBezier' {
    pathcb'           :: [(a, a)],
    namecb'           :: String,
    stylecb'          :: String,
    colorcb'          :: Color
} deriving (Eq, Show)

data Line' a = Line' {
    startx_l'         :: a,
    starty_l'         :: a,
    thickness_l'      :: a,
    endx_l'           :: a,
    endy_l'           :: a,
    name_l'           :: String,
    style_l'          :: String,
    color_l'          :: Color
} deriving (Eq, Show, Generic)

instance Named (SolidArrow' a) where
         getName = namesa'
         setName x sa = sa { namesa' = x }

instance Named (Circ' a) where
         getName = namec'
         setName x c = c { namec' = x }

instance Named (Ellipse' a) where
         getName = namee'
         setName x c = c { namee' = x }

instance Named (Square' a) where
         getName = names'
         setName x s = s { names' = x }

instance Named (Arc' a) where
         getName = namear'
         setName x ar= ar{ namear' = x }

instance Named (Rect' a) where
         getName = namer'
         setName x r = r { namer' = x }


instance Named (Parallelogram' a) where
         getName = namepa'
         setName x pa = pa { namepa' = x }

instance Named (Label' a) where
         getName = namel'
         setName x l = l { namel' = x }

instance Named (Pt' a) where
         getName = namep'
         setName x p = p { namep' = x }

instance Named (CubicBezier' a) where
         getName = namecb'
         setName x cb = cb { namecb' = x }

instance Named (Line' a) where
         getName = name_l'
         setName x l = l { name_l' = x }

instance Named (Img' a) where
         getName = nameim'
         setName x im = im { nameim' = x }

instance Named (Obj' a) where
         getName o = case o of
                 C' c   -> getName c
                 E' c   -> getName c
                 L' l   -> getName l
                 P' p   -> getName p
                 S' s   -> getName s
                 R' r   -> getName r
                 PA' pa -> getName pa
                 A' a   -> getName a
                 CB' cb -> getName cb
                 LN' ln -> getName ln
                 IM' im -> getName im
                 AR' ar-> getName ar

         setName x o = case o of
                C' c   -> C' $ setName x c
                S' s   -> S' $ setName x s
                R' r   -> R' $ setName x r
                PA' pa -> PA' $ setName x pa
                L' l   -> L' $ setName x l
                P' p   -> P' $ setName x p
                A' a   -> A' $ setName x a
                CB' cb -> CB' $ setName x cb
                LN' ln -> LN' $ setName x ln
                IM' im -> IM' $ setName x im
                AR' ar-> AR' $ setName x ar
--
instance Located (Circ' a) a where
         getX = xc'
         getY = yc'
         setX x c = c { xc' = x }
         setY y c = c { yc' = y }

instance Located (Ellipse' a) a where
         getX = xe'
         getY = ye'
         setX x e = e { xe' = x }
         setY y e = e { ye' = y }

instance Located (Square' a) a where
         getX = xs'
         getY = ys'
         setX x s = s { xs' = x }
         setY y s = s { ys' = y }

instance Located (Arc' a) a where
         getX = xar'
         getY = yar'
         setX x ar= ar{ xar' = x }
         setY y ar= ar{ yar' = y }

instance Located (Rect' a) a where
         getX = xr'
         getY = yr'
         setX x r = r { xr' = x }
         setY y r = r { yr' = y }

instance Located (Parallelogram' a) a where
         getX = xpa'
         getY = ypa'
         setX x pa = pa { xpa' = x }
         setY y pa = pa { ypa' = y }

instance Located (SolidArrow' a) a where
         getX  = startx'
         getY  = starty'
         setX x c = c { startx' = x } -- TODO
         setY y c = c { starty' = y }

instance Located (Label' a) a where
         getX = xl'
         getY = yl'
         setX x l = l { xl' = x }
         setY y l = l { yl' = y }

instance Located (Pt' a) a where
         getX = xp'
         getY = yp'
         setX x p = p { xp' = x }
         setY y p = p { yp' = y }

instance Located (Img' a) a where
         getX = xim'
         getY = yim'
         setX x im = im { xim' = x }
         setY y im = im { yim' = y }

-- TODO: Added context for max and min functions. Consider rewriting the whole `Located` interface. For general shapes, simply setX and getX does NOT make sense.
instance (Real a, Floating a, Show a, Ord a) => Located (CubicBezier' a) a where
         getX c   = let xs = map fst $ pathcb' c in maximum xs - minimum xs
         getY c   = let ys = map snd $ pathcb' c in maximum ys - minimum ys
         setX x c = let xs = map fst $ pathcb' c
                        dx = x - (maximum xs - minimum xs) in
                        c { pathcb' = map (\(xx, yy) -> (xx + dx, yy)) $ pathcb' c }
         setY y c = let ys = map snd $ pathcb' c
                        dy = y - (maximum ys - minimum ys) in
                        c { pathcb' = map (\(xx, yy) -> (xx, yy - dy)) $ pathcb' c }

instance (Num a, Fractional a) => Located (Line' a) a where
         getX l = (startx_l' l + endx_l' l) / 2
         getY l = (starty_l' l + endy_l' l) / 2

         setX x l = l { startx_l' = x } -- only sets start
         setY y l = l { starty_l' = y }

instance (Num a, Fractional a) => Located (Obj' a) a where
         getX o = case o of
             C' c -> xc' c
             E' e -> xe' e
             L' l -> xl' l
             P' p -> xp' p
             S' s -> xs' s
             AR' ar-> xar' ar
             R' r -> xr' r
             PA' pa -> xpa' pa
             A' a -> startx' a
             LN' l -> getX l
             IM' im -> xim' im
         getY o = case o of
             C' c -> yc' c
             E' e -> ye' e
             L' l -> yl' l
             P' p -> yp' p
             S' s -> ys' s
             AR' ar-> yar' ar
             R' r -> yr' r
             PA' pa -> ypa' pa
             A' a -> starty' a
             LN' l -> getY l
             IM' im -> yim' im
         setX x o = case o of
             C' c -> C' $ setX x c
             E' e -> E' $ setX x e
             L' l -> L' $ setX x l
             P' p -> P' $ setX x p
             S' s -> S' $ setX x s
             AR' ar-> AR' $ setX x ar
             R' r -> R' $ setX x r
             PA' pa -> PA' $ setX x pa
             A' a -> A' $ setX x a
             LN' l -> LN' $ setX x l
             IM' im -> IM' $ setX x im
         setY y o = case o of
             C' c -> C' $ setY y c
             E' e -> E' $ setY y e
             L' l -> L' $ setY y l
             P' p -> P' $ setY y p
             S' s -> S' $ setY y s
             AR' ar-> AR' $ setY y ar
             R' r -> R' $ setY y r
             PA' pa -> PA' $ setY y pa
             A' a -> A' $ setY y a
             LN' l -> LN' $ setY y l
             IM' im -> IM' $ setY y im

-----------------------------------------------
-- Defining the interface between Style types/operations and internal computation types / object properties

type Property = String

-- | Possible computation input types (internal types)
data TypeIn a = TNum a
              | TBool Bool
              | TStr String
              | TInt Integer
              | TFloat Float
              | TPt (Pt2 a)
              | TPath [Pt2 a]
              | TColor Color
              -- | path for image
              | TFile String
              -- | dotted, etc.
              | TStyle String
              -- | Substance ID
              | TAllShapes String
              -- | shape ID
              | TShape String
              -- | shape ID, property of that shape
              | TProp String Property
              -- | a call to computation function
              | TCall String [TypeIn a]
     deriving (Eq, Show)

-- TODO: should we collect the types that need computation to another single type. For example:
-- data TypeIn a = ...
--               | TPending (TypeIn a)
-- data NeedComp a =

-- | Getters for all shapes
-- TODO using better record fields names + template haskell, could maybe generate these "interpreter"s
get :: (Autofloat a) => Property -> Obj' a -> TypeIn a
-- Circles
get "radius" (C' o)        = TNum $ r' o
get "x" (C' o)             = TNum $ xc' o
get "y" (C' o)             = TNum $ yc' o
get "color" (C' o)         = TColor $ colorc' o
get "stroke-width" (C' o)  = TNum $ strokec' o
get "stroke-style" (C' o)  = TStyle $ stylec' o

-- Ellipses
get "rx" (E' o)            = TNum $ rx' o
get "ry" (E' o)            = TNum $ ry' o
get "x" (E' o)             = TNum $ xe' o
get "y" (E' o)             = TNum $ ye' o
get "color" (E' o)         = TColor $ colore' o

-- Points
get "x" (P' o)             = TNum $ xp' o
get "y" (P' o)             = TNum $ yp' o
get "location" (P' o)      = TPt (xp' o, yp' o)

-- Squares
get "x" (S' o)             = TNum $ xs' o
get "y" (S' o)             = TNum $ ys' o
get "center" (S' o)        = TPt (xs' o, ys' o)
get "side" (S' o)          = TNum $ side' o
get "angle" (S' o)         = TNum $ r2f $ ang' o
get "color" (S' o)         = TColor $ colors' o
get "stroke-width" (S' o)  = TNum $ strokes' o
get "stroke-style" (S' o)  = TStyle $ styles' o

-- Arcs

get "radius" (AR' o)        = TNum $ radiusar' o
get "rotation" (AR' o)      = TNum $ rotationar' o
get "angle" (AR' o)         = TNum $ anglear' o
get "isRight" (AR' o)       = TStyle $ isRightar' o
get "x" (AR' o)             = TNum $ xar' o
get "style" (AR' o)         = TStyle $ stylear' o
get "y" (AR' o)             = TNum $ yar' o
get "size" (AR' o)          = TNum $ sizear' o
get "color" (AR' o)         = TColor $ colorar' o

-- Rectangles
get "x" (R' o)             = TNum $ xr' o
get "y" (R' o)             = TNum $ yr' o
get "center" (R' o)        = TPt (xr' o, yr' o)
get "length" (R' o)        = TNum $ sizeX' o
get "width" (R' o)         = TNum $ sizeY' o
get "angle" (R' o)         = TNum $ r2f $ angr' o
get "color" (R' o)         = TColor $ colorr' o


-- Parallelogram
get "x" (PA' o)             = TNum $ xpa' o
get "y" (PA' o)             = TNum $ ypa' o
get "center" (PA' o)        = TPt (xpa' o, ypa' o)
get "length" (PA' o)        = TNum $ sizeXpa' o
get "width" (PA' o)         = TNum $ sizeYpa' o
get "angle" (PA' o)         = TNum $ r2f $ anglepa' o
get "rotation" (PA' o)      = TNum $ r2f $ anglepa' o
get "color" (PA' o)         = TColor $ colorpa' o

-- Cubic beziers
get "path" (CB' o)         = TPath $ pathcb' o
get "style" (CB' o)        = TStyle $ stylecb' o
get "color" (CB' o)        = TColor $ colorcb' o

-- Solid arrows
get "startx" (A' o)        = TNum $ startx' o
get "starty" (A' o)        = TNum $ starty' o
get "start" (A' o)         = TPt (startx' o, starty' o)
get "endx" (A' o)          = TNum $ endx' o
get "endy" (A' o)          = TNum $ endy' o
get "end" (A' o)           = TPt (endx' o, endy' o)
get "thickness" (A' o)     = TNum $ thickness' o
get "color" (A' o)         = TColor $ colorsa' o
get "style" (A' o)         = TStyle $ stylesa' o


-- Lines
get "startx" (LN' o)        = TNum $ startx_l' o
get "starty" (LN' o)        = TNum $ starty_l' o
get "endx" (LN' o)          = TNum $ endx_l' o
get "endy" (LN' o)          = TNum $ endy_l' o
get "thickness" (LN' o)     = TNum $ thickness_l' o
get "style" (LN' o)         = TStyle $ style_l' o
get "color" (LN' o)         = TColor $ color_l' o
get "path" (LN' o)          = TPath [(startx_l' o, starty_l' o), (endx_l' o, endy_l' o)]

-- Labels
get "x" (L' o)             = TNum $ xl' o
get "y" (L' o)             = TNum $ yl' o
get "location" (L' o)      = TPt (xl' o, yl' o)

-- Images
get "x" (IM' o)             = TNum $ xim' o
get "y" (IM' o)             = TNum $ yim' o
get "center" (IM' o)        = TPt (xim' o, yim' o)
get "length" (IM' o)        = TNum $ sizeXim' o
get "width" (IM' o)         = TNum $ sizeYim' o
get "angle" (IM' o)         = TNum $ r2f $ angim' o
get "path" (IM' o)         = TFile $ path' o


get prop obj = error ("getting property/object combination not supported: \n" ++ prop ++ "\n"
                                   ++ show obj ++ "\n" ++ show obj)


-- | Setters for all shapes' properties (both "base" and "derived") for the computations to use.
set :: (Autofloat a) => Property -> Obj' a -> TypeIn a -> Obj' a
-- Circles
set "radius" (C' o) (TNum n)  = C' $ o { r' = n }
set "x" (C' o) (TNum n)       = C' $ o { xc' = n }
set "y" (C' o) (TNum n)       = C' $ o { yc' = n }
set "color" (C' o) (TColor n) = C' $ o { colorc' = n }
set "stroke-width" (C' o) (TNum w)   = C' $ o { strokec' = w }
set "stroke-style" (C' o) (TStyle s) =  C' $ o { stylec' = s }

-- Ellipses
set "rx" (E' o) (TNum n)      = E' $ o { rx' = n }
set "ry" (E' o) (TNum n)      = E' $ o { ry' = n }
set "x" (E' o) (TNum n)       = E' $ o { xe' = n }
set "y" (E' o) (TNum n)       = E' $ o { ye' = n }
set "color" (E' o) (TColor n) = E' $ o { colore' = n }

-- Points
set "x" (P' o) (TNum n)            = P' $ o { xp' = n }
set "y" (P' o) (TNum n)            = P' $ o { yp' = n }
set "location" (P' o) (TPt (x, y)) = P' $ o { xp' = x, yp' = y }

-- Squares
set "x" (S' o) (TNum n)          = S' $ o { xs' = n }
set "y" (S' o) (TNum n)          = S' $ o { ys' = n }
set "center" (S' o) (TPt (x, y)) = S' $ o { xs' = x, ys' = y }
set "side" (S' o) (TNum n)       = S' $ o { side' = n }
set "angle" (S' o) (TNum n)      = S' $ o { ang' = r2f n }
set "color" (S' o) (TColor n)    = S' $ o { colors' = n }

-- Arcs
set "x" (AR' o) (TNum n)            = AR' $ o { xar' = n }
set "y" (AR' o) (TNum n)            = AR' $ o { yar' = n }
set "radius" (AR' o) (TNum n)       = AR' $ o { radiusar' = n }
set "rotation" (AR' o) (TNum n)     = AR' $ o { rotationar' = r2f n }
set "angle" (AR' o) (TNum n)        = AR' $ o { anglear' = r2f n }
set "isRight" (AR' o) (TStyle n)    = AR' $ o { isRightar' = n }
set "style" (AR' o) (TStyle n)    = AR' $ o { stylear' = n }
set "start" (AR' o) (TPt (x, y))    = AR' $ o { xar' = x, yar' = y }
set "size" (AR' o) (TNum n)         = AR' $ o { sizear' = n }
set "color" (AR' o) (TColor n)      = AR' $ o { colorar' = n }

-- Rectangles
set "x" (R' o) (TNum n)          = R' $ o { xr' = n }
set "y" (R' o) (TNum n)          = R' $ o { yr' = n }
set "center" (R' o) (TPt (x, y)) = R' $ o { xr' = x, yr' = y }
set "length" (R' o) (TNum n)     = R' $ o { sizeX' = n }
set "width" (R' o) (TNum n)      = R' $ o { sizeY' = n }
set "angle" (R' o) (TNum n)      = R' $ o { angr' = r2f n }
set "color" (R' o) (TColor n)    = R' $ o { colorr' = n }


-- Parallelogram
set "x" (PA' o) (TNum n)          = PA' $ o { xpa' = n }
set "y" (PA' o) (TNum n)          = PA' $ o { ypa' = n }
set "center" (PA' o) (TPt (x, y)) = PA' $ o { xpa' = x, ypa' = y }
set "length" (PA' o) (TNum n)     = PA' $ o { sizeXpa' = n }
set "width" (PA' o) (TNum n)      = PA' $ o { sizeYpa' = n }
set "angle" (PA' o) (TNum n)      = PA' $ o { anglepa' = r2f n }
set "rotation" (PA' o) (TNum n)   = PA' $ o { rotationpa' = r2f n }
set "color" (PA' o) (TColor n)    = PA' $ o { colorpa' = n }

-- Cubic beziers
set "path" (CB' o) (TPath n)      = CB' $ o { pathcb' = n }
set "style" (CB' o) (TStyle n)    = CB' $ o { stylecb' = n }
set "color" (CB' o) (TColor n)    = CB' $ o { colorcb' = n }

-- Solid arrows
set "startx" (A' o) (TNum n)     = A' $ o { startx' = n }
set "starty" (A' o) (TNum n)     = A' $ o { starty' = n }
set "endx" (A' o) (TNum n)       = A' $ o { endx' = n }
set "endy" (A' o) (TNum n)       = A' $ o { endy' = n }
set "start" (A' o) (TPt (x, y))  = A' $ o { startx' = x, starty' = y }
set "end" (A' o) (TPt (x, y))    = A' $ o { endx' = x, endy' = y }
set "thickness" (A' o) (TNum n)  = A' $ o { thickness' = n }
set "color" (A' o) (TColor n)    = A' $ o { colorsa' = n }
set "style" (A' o) (TStyle n)    = A' $ o { stylesa' = n }

-- TODO add angle and length properties

-- Imgs
set "x" (IM' o) (TNum n)          = IM' $ o { xim' = n }
set "y" (IM' o) (TNum n)          = IM' $ o { yim' = n }
set "center" (IM' o) (TPt (x, y)) = IM' $ o { xim' = x, yim' = y }
set "length" (IM' o) (TNum n)     = IM' $ o { sizeXim' = n }
set "width" (IM' o) (TNum n)      = IM' $ o { sizeYim' = n }
set "path" (IM' o) (TFile n)      = IM' $ o { path' = n }

-- Lines
set "startx" (LN' o) (TNum n)     = LN' $ o { startx_l' = n }
set "starty" (LN' o) (TNum n)     = LN' $ o { starty_l' = n }
set "endx" (LN' o) (TNum n)       = LN' $ o { endx_l' = n }
set "endy" (LN' o) (TNum n)       = LN' $ o { endy_l' = n }
set "start" (LN' o) (TPt (x, y))  = LN' $ o { startx_l' = x, starty_l' = y }
set "end" (LN' o) (TPt (x, y))    = LN' $ o { endx_l' = x, endy_l' = y }
set "thickness" (LN' o) (TNum n)  = LN' $ o { thickness_l' = n }
set "color" (LN' o) (TColor n)    = LN' $ o { color_l' = n }
set "style" (LN' o) (TStyle n)    = LN' $ o { style_l' = n }
set "path" (LN' o) (TPath [(sx, sy), (ex, ey)])      = LN' $ o { startx_l' = sx, starty_l' = sy,
                                                                 endx_l' = ex, endy_l' = ey }
set "path" (LN' o) (TPath p)      = error ("line expects two points on a path; got: " ++ show p)

-- Labels
set "location" (L' o) (TPt (x, y)) = L' $ o { xl' = x, yl' = y }

set prop obj val = error ("setting property/object/value combination not supported: \n" ++ prop ++ "\n"
                                   ++ show obj ++ "\n" ++ show val)


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
