{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

{- TODO:
   - Wolfram Alpha competitor
   - first, just draw hardcoded expressions on 1-2 sets (A / B)
   - do some arc math for the shading
   - then deal with 3
   - then deal with arbitrary expressions
   - then deal with name equivalence
   - simple parser and pretty-printer
   - simple expression generator
   
   - port to Elm
   
   - visualize more complex expressions, e.g. keenanSpec
   - more complex parser and pretty-printer
   - more complex program generator
   - expression simplifier
   - visualize relationships between objects
-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox        

data Set =
     St String
     | Empty
     | Universe
     | Intersection Set Set
     | Union Set Set
     | Complement Set
     | Minus Set Set
     deriving (Show, Eq)

data Pt = MkPt String
     deriving (Show, Eq)

data Spec =
     LetSet String Set
     | BindSets [String]
     | Intersect Set Set Bool   -- note difference b/t this and Intersection
     | Subset Set Set Bool
     | PointIn Pt Set
     deriving (Show, Eq)

type Prog = [Spec]

--------------------------------------

keenanSpec :: Prog
keenanSpec = [
           BindSets ["A", "B", "C", "D"], -- A, B, C, D := Set
           Intersect (St "A") (Intersection (St "B") (St "C")) True,
                                -- Intersect (A, B, C) = TRUE
           Subset (St "D") (Intersection (St "A") (Intersection (St "B") (St "C")))
              True,            -- Subset( D, Intersection(A,B,C) ) = TRUE 
           PointIn (MkPt "p") (Minus (Intersection (St "A") (Intersection (St "B")
              (St "C"))) (St "D")), -- p := Point in (Intersection( A, B, C ) \ D)
           PointIn (MkPt "q") (Intersection (St "B") (St "C")),
           BindSets ["E"],      -- E := Set
           Intersect (St "E") (Union (Union (St "A") (St "B")) (St "C")) False]
                                -- Intersect( E, Union(A,B,C) ) = FALSE

a :: Set
a = St "A"

b :: Set
b = St "B"

c :: Set
c = Intersection a b

--------------------

vennRed :: Diagram B
vennRed = (cir <> cir # translateX 1) # opacity 0.3
       where cir = circle 1 # lw none # fc red

-- assuming the picture is 500x500
-- some issues with scaling--the pictures all look the same if the rectangle isn't there
randCir :: Diagram B
randCir = cir # translateX randX # translateY randY
        where cir = circle randR
              randX = 100
              randY = 100
              randR = 100

box :: Diagram B
box = rect 500 500

main = mainWith (randCir <> box)
