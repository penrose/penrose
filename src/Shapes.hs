-- | "Shapes" contains all geometric primitives that Penrose supports

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Shapes where
-- module Shapes (Obj, Obj') where
import Data.Aeson
import Data.Monoid ((<>))
import GHC.Generics
import Graphics.Gloss

type Name = String

class Located a b where
      getX :: a -> b
      getY :: a -> b
      setX :: b -> a -> a
      setY :: b -> a -> a

class Selectable a where
      select :: a -> a
      deselect :: a -> a
      selected :: a -> Bool

class Sized a where
      getSize :: a -> Float
      setSize :: Float -> a -> a

class Named a where
      getName :: a -> Name
      setName :: Name -> a -> a

data BBox = BBox {
    cx :: Float,
    cy :: Float,
    h :: Float,
    w :: Float
} deriving (Show, Eq, Generic)
instance ToJSON BBox
instance FromJSON BBox

-------
data SolidArrow = SolidArrow { startx :: Float
                             , starty :: Float
                             , endx :: Float
                             , endy :: Float
                             , thickness :: Float -- the maximum thickness, i.e. the thickness of the head
                             , selsa :: Bool -- is the circle currently selected? (mouse is dragging it)
                             , namesa :: String
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

instance Selectable SolidArrow where
         select x = x { selsa = True }
         deselect x = x { selsa = False }
         selected x = selsa x

instance Named SolidArrow where
         getName a = namesa a
         setName x a = a { namesa = x }

instance ToJSON SolidArrow
instance FromJSON SolidArrow

-------

data Circ = Circ { xc :: Float
                 , yc :: Float
                 , r :: Float
                 , selc :: Bool -- is the circle currently selected? (mouse is dragging it)
                 , namec :: String
                 , colorc :: Color }
     deriving (Eq, Show, Generic)

instance Located Circ Float where
         getX c = xc c
         getY c = yc c
         setX x c = c { xc = x }
         setY y c = c { yc = y }

instance Selectable Circ where
         select x = x { selc = True }
         deselect x = x { selc = False }
         selected x = selc x

instance Sized Circ where
         getSize x = r x
         setSize size x = x { r = size }

instance Named Circ where
         getName c = namec c
         setName x c = c { namec = x }

instance ToJSON Circ
instance FromJSON Circ

----------------------

data Square = Square { xs :: Float
                     , ys :: Float
                     , side :: Float
                     , ang  :: Float -- angle for which the obj is rotated
                     , sels :: Bool -- is the circle currently selected? (mouse is dragging it)
                     , names :: String
                     , colors :: Color }
     deriving (Eq, Show, Generic)

instance Located Square Float where
         getX s = xs s
         getY s = ys s
         setX x s = s { xs = x }
         setY y s = s { ys = y }

instance Selectable Square where
         select x = x { sels = True }
         deselect x = x { sels = False }
         selected x = sels x

instance Sized Square where
         getSize x = side x
         setSize size x = x { side = size }

instance Named Square where
         getName s = names s
         setName x s = s { names = x }

instance ToJSON Square
instance FromJSON Square
-------

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

instance Selectable Label where
         select x = x { sell = True }
         deselect x = x { sell = False }
         selected x = sell x

instance Sized Label where
         getSize x = xl x -- TODO generalize label size, distance to corner? ignores scale
         setSize size x = x { xl = size, yl = size } -- TODO currently sets both of them, ignores scale
                 -- changing a label's size doesn't actually do anything right now, but should use the scale
                 -- and the base font size

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

instance Selectable Pt where
         select   x = x { selp = True }
         deselect x = x { selp = False }
         selected x = selp x

instance Named Pt where
         getName p   = namep p
         setName x p = p { namep = x }

instance ToJSON Pt
instance FromJSON Pt

data Obj = S Square
         | C Circ
         | E Ellipse
         | L Label
         | P Pt
         | A SolidArrow
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
                 A a -> getX a
         getY o = case o of
                 C c -> getY c
                 E e -> getY e
                 L l -> getY l
                 P p -> getY p
                 S s -> getY s
                 A a -> getY a
         setX x o = case o of
                C c -> C $ setX x c
                E e -> E $ setX x e
                L l -> L $ setX x l
                P p -> P $ setX x p
                S s -> S $ setX x s
                A a -> A $ setX x a
         setY y o = case o of
                C c -> C $ setY y c
                E e -> E $ setY y e
                L l -> L $ setY y l
                P p -> P $ setY y p
                S s -> S $ setY y s
                A a -> A $ setY y a

instance Selectable Obj where
         select x = case x of
                C c -> C $ select c
                L l -> L $ select l
                P p -> P $ select p
                S s -> S $ select s
                A a -> A $ select a
         deselect x = case x of
                C c -> C $ deselect c
                L l -> L $ deselect l
                P p -> P $ deselect p
                S s -> S $ deselect s
                A a -> A $ deselect a
         selected x = case x of
                C c -> selected c
                L l -> selected l
                P p -> selected p
                S s -> selected s
                A a -> selected a

instance Sized Obj where
         getSize o = case o of
                 C c -> getSize c
                 S s -> getSize s
                 L l -> getSize l
         setSize x o = case o of
                C c -> C $ setSize x c
                L l -> L $ setSize x l
                S s -> S $ setSize x s

instance Named Obj where
         getName o = case o of
                 C c -> getName c
                 E e -> getName e
                 L l -> getName l
                 P p -> getName p
                 S s -> getName s
                 A a -> getName a
         setName x o = case o of
                C c -> C $ setName x c
                E e -> E $ setName x e
                L l -> L $ setName x l
                P p -> P $ setName x p
                S s -> S $ setName x s
                A a -> A $ setName x a

--------------------------------------------------------------------------------
-- Polymorphic versions of the primitives

-- data Obj' a =
--     SolidArrow'
--         { startx :: a
--         , starty :: a
--         , endx :: a
--         , endy :: a
--         , thickness :: a -- the maximum thickness, i.e. the thickness of the head
--         , sel :: Bool -- is the circle currently selected? (mouse is dragging it)
--         , name :: String
--         , color :: Color }
--     | Circ'
--         { x :: a
--         , y :: a
--         , r :: a
--         , sel :: Bool -- is the circle currently selected? (mouse is dragging it)
--         , name :: String
--         , color :: Color }
--     | Label'
--         { x :: a
--         , y :: a
--         , w :: a
--         , h :: a
--         , text :: String
--         , sel :: Bool -- selected label
--         , name :: String }
--     | Pt'
--         { x :: a
--         , y :: a
--         , sel :: Bool
--         , name :: String }
--     | Square'
--         { x :: a
--         , y :: a
--         , side :: a
--         , ang  :: Float -- angle for which the obj is rotated
--         , sel :: Bool
--         , name :: String
--         , color :: Color }
--         deriving (Eq, Show)

data Obj' a
    = C' (Circ' a)
    | E' (Ellipse' a)
    | L' (Label' a)
    | P' (Pt' a)
    | S' (Square' a)
    | A' (SolidArrow' a)
    deriving (Eq, Show)

-- FIXME: fix the formatting
data SolidArrow' a = SolidArrow' { startx' :: a
                               , starty' :: a
                               , endx' :: a
                               , endy' :: a
                               , thickness' :: a -- the maximum thickness, i.e. the thickness of the head
                               , selsa' :: Bool -- is the circle currently selected? (mouse is dragging it)
                               , namesa' :: String
                               , colorsa' :: Color }
                               deriving (Eq, Show)

data Circ' a = Circ' { xc' :: a
                     , yc' :: a
                     , r' :: a
                     , selc' :: Bool -- is the circle currently selected? (mouse is dragging it)
                     , namec' :: String
                     , colorc' :: Color }
                     deriving (Eq, Show)

data Ellipse' a = Ellipse' { xe' :: a
                     , ye' :: a
                     , rx' :: a
                     , ry' :: a
                     , namee' :: String
                     , colore' :: Color }
                     deriving (Eq, Show)

data Label' a = Label' { xl' :: a
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

data Square' a  = Square' { xs' :: a
                     , ys' :: a
                     , side' :: a
                     , ang'  :: Float -- angle for which the obj is rotated
                     , sels' :: Bool
                     , names' :: String
                     , colors' :: Color }
                     deriving (Eq, Show)

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

instance Named (Label' a) where
         getName = namel'
         setName x l = l { namel' = x }

instance Named (Pt' a) where
         getName = namep'
         setName x p = p { namep' = x }

instance Named (Obj' a) where
         getName o = case o of
                 C' c -> getName c
                 E' c -> getName c
                 L' l -> getName l
                 P' p -> getName p
                 S' s -> getName s
                 A' a -> getName a
         setName x o = case o of
                C' c -> C' $ setName x c
                S' s -> S' $ setName x s
                L' l -> L' $ setName x l
                P' p -> P' $ setName x p
                A' a -> A' $ setName x a
--
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

instance Located (Obj' a) a  where
         getX o = case o of
             C' c -> xc' c
             E' e -> xe' e
             L' l -> xl' l
             P' p -> xp' p
             S' s -> xs' s
             A' a -> startx' a
         getY o = case o of
             C' c -> yc' c
             E' e -> ye' e
             L' l -> yl' l
             P' p -> yp' p
             S' s -> ys' s
             A' a -> starty' a
         setX x o = case o of
             C' c -> C' $ setX x c
             E' e -> E' $ setX x e
             L' l -> L' $ setX x l
             P' p -> P' $ setX x p
             S' s -> S' $ setX x s
             A' a -> A' $ setX x a
         setY y o = case o of
             C' c -> C' $ setY y c
             E' e -> E' $ setY y e
             L' l -> L' $ setY y l
             P' p -> P' $ setY y p
             S' s -> S' $ setY y s
             A' a -> A' $ setY y a
