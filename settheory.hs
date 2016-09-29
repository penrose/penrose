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

-- some unrelated / helpful examples from the diagrams site
        
andThen t1 t2 = t1 <> t2 # rotate (angleBetween d1 d2)
  where
    d1 = tangentAtEnd t1
    d2 = tangentAtStart t2

str = fromOffsets [unitX]
cap = arc (negated xDir) (1/2 @@ turn)
arm = str `andThen` cap `andThen` str

armUnit = arm `andThen` (arc xDir (3/10 @@ turn) # reflectX)

example :: Diagram B
example = foldr andThen mempty (replicate 5 armUnit)
  # glueLine # strokeLoop # fc blue
  # rotateBy (1/20)
  # centerXY # pad 1.1 # sized (mkWidth 2)

----

p = pentagon 1 # onLineSegments init

example_pentagon :: Diagram B
example_pentagon = iterateN 5 (rotateBy (1/5)) p
   # mconcat # glueLine # strokeLoop
   # fc green # centerXY # pad 1.1 # sized (mkWidth 2)
   
--------------------

-- figure out how to fill 3-arc
-- sets, parser, expression, etc.
-- generalize to 3 sets   
a_intersect_b :: Diagram B
a_intersect_b = (arc (d angle) a <> arc (d (-(1/4 - 1/12))) a)
        -- # lc red
        # closeLine
        -- # fromSegments
        # strokeLoop # fc red # lw none # centerXY
        where angle = 1/4 + 1/12 -- 1/4 of a circle + 1/12
              d :: Double -> Direction V2 Double
              d a1 = rotateBy a1 xDir
              a :: Angle Double
              a = tau / 3 @@ rad

minusDiag =
          (arc (d angle) (tau - (tau / 3) @@ rad) # translateX (-0.5)
          <>
          arc (d (1/4 + 1/12)) a # translateX 0.5)
          -- fromLocSegments $ 
          -- [(arc (d angle) (tau - (tau / 3) @@ rad) `at` p2 (-0.5, 0)),
          -- (arc (d (1/4 + 1/12)) a `at` p2 (0.5, 0))]
        -- # glueLine # strokeLoop
        -- # fc blue
        # lw 5
        where angle = 1/6
              d :: Double -> Direction V2 Double
              d a1 = rotateBy a1 xDir
              a :: Angle Double
              a = tau / 3 @@ rad
        
a_minus_b :: Diagram B
a_minus_b = minusDiag # lc blue

b_minus_a :: Diagram B
b_minus_a = minusDiag # reflectX # lc green

venn :: Diagram B
venn = cir # translateX (-0.5) <> cir # translateX 0.5
       where cir = circle 1

-- leftmost on top. TODO put venn on top
main = mainWith (b_minus_a <> a_minus_b <> a_intersect_b <> venn)
-- main = putStrLn (show keenanSpec)
