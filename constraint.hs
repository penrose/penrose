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
import System.Random
import Control.Arrow
import Data.Function
import Data.Colour (withOpacity)
import Debug.Trace
import Diagrams.TwoD.Text

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

vennRed :: Diagram B
vennRed = (cir <> cir # translateX 1) # opacity 0.3
       where cir = circle 1 # lw none # fc red

--------------------

basicSpec :: Prog
basicSpec = [BindSets ["A", "B"], -- A, B := Set
          -- TODO: sets are nonempty
           Intersect (St "A") (St "B") True]
                                -- Intersect (A, B) = TRUE
                     -- TODO: bind a set to intersection, and have a pt in it

rng :: StdGen
rng = mkStdGen seed
    where seed = 12 -- deterministic RNG with seed

dim :: Double
dim = 500 -- dim x dim

rRange :: (Double, Double)
-- rRange = (10, 90)
rRange = (50, 200)
 
imgRange = (-dim/2, dim/2)

data Circle = Circle { x :: Double, y :: Double, r :: Double } deriving (Show)

-- TODO figure out how to draw a bunch of random circles / global diagrams
-- just use || and ---?
-- assuming the picture is 500x500
-- some issues with scaling--the pictures all look the same if the rectangle isn't there
-- the circle starts at (0,0) and it's actually hard to ensure that it doesn't leave the bounding box...   
-- i wish i could have an interactive loop that would show drawUntil... maybe draw in a faded color?

-- i should really pass the generator around in a monad
-- also output the circle stats

-- draw a single circle once until it's in a region (generalized condition check)
-- then draw another circle in a fixed place, then in a random place (using the same rng)
-- then, draw another circle until they intersect (in faded color)
-- then generalize to "point in intersection"
-- then actually parse basicSpec
-- then write up some of my questions and post on slack

-- & is reverse apply
circPic :: RandomGen g => Circle -> g -> Diagram B
circPic prevCoords gen =
        let (bads, good) = circs gen & crop (cIntersect prevCoords) in
        let badsPic = map drawBad bads & mconcat in
        let goodPic = drawGood good in
        goodPic <> badsPic

-- from stackoverflow. truncate to 2 points
trunc num = (fromInteger $ round $ num * (10^2)) / (10.0^^2)
drawBad c = draw c # fc red # opacity 0.05 <> circText c
drawGood c = draw c # fc green # opacity 0.5 <> circText c
circText c = alignedText cx cy textC # scale 20
         where cx = x c
               cy = y c
               textC = "x = " ++ (show $ trunc cx) ++ ", y = " ++ (show $ trunc cy) ++ ", r = " ++ (show $ trunc $ r c)

draw :: Circle -> Diagram B
draw randC = circle (r randC) # translateX (x randC) # translateY (y randC)

-- not the most efficient impl. also assumes infinite list s.t. head always exists
crop :: (a -> Bool) -> [a] -> ([a], a)
crop cond xs = (takeWhile (not . cond) xs, head $ dropWhile (not . cond) xs)

-- eventually conditions will have to refer to prev results
-- note: circle values are doubles. so, choose a condition that will eventually be true, and not cause an infinite list
-- circles intersect iff the distance b/t their centers < the sum of their radii
cIntersect :: Circle -> Circle -> Bool
cIntersect c1 c2 = traceShowId $ distance (p2 (x c1, y c1)) (p2 (x c2, y c2)) < r c1 + r c2

inBox :: Circle -> Bool
inBox c = x c >= -len && x c <= len && y c >= -len && y c <= len
          where len = 10

-- TODO keep the last generator for the "good" element?
circs :: RandomGen g => g -> [Circle]
circs gen = map fst circgens -- throw the intermediate generators away
        where circgens = iterate (\(c, g) -> cirCoords g) (cirCoords gen)

cirCoords :: RandomGen g => g -> (Circle, g)
cirCoords gen = (Circle { x = randX, y = randY, r = randR }, gen3)
        where (randX, gen1) = randomR imgRange gen
              (randY, gen2) = randomR imgRange gen1
              (randR, gen3) = randomR rRange gen2
        
box :: Diagram B
box = rect 500 500

-- TODO figure out how to interpret basicSpec, keenanSpec
main = mainWith (box # opacity 0.5 <>
     (circ1coords & drawGood) <> circPic circ1coords rng')
     where (circ1coords, rng') = cirCoords rng
