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
import Control.Arrow ((>>>))
import Data.Function
import Data.Colour (withOpacity)
import Debug.Trace
import Diagrams.TwoD.Text
import Diagrams.TwoD.Layout.Grid
import Data.Colour.Palette.BrewerSet
import Graphics.SVGFonts

data Set =
     St String
     | Empty
     | Universe
     | Intersection Set Set
     | Union Set Set
     | Complement Set
     | Minus Set Set
     deriving (Show, Eq)

data Pt = MkPt String -- TODO location?
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
    where seed = 17 -- deterministic RNG with seed

dim :: Double
dim = 500 -- dim x dim

rRange :: (Double, Double)
-- rRange = (10, 90)
rRange = (dim/10, dim/2)
-- implicitly nonzero? unless i add that in manually
 
imgRange = (-dim/2, dim/2)

-- some issues with scaling--the pictures all look the same if the rectangle isn't there
-- the circle starts at (0,0) and it's actually hard to ensure that it doesn't leave the bounding box...   
-- i should pass the generator around in a monad

-- TODO
-- deal with subset (and any other constraints) individually
-- then figure out how to bind/label/have constraints with regions (e.g. point in region, region intersection, and region subset region) 
-- also distinguish between "base sets" and regions in types?
-- label points and sets globally

-- then deal with two constraints
-- then deal with multiple sets
-- then actually parse basicSpec to deal with multiple, possibly unsatisfiable-in-some-orders constraints
-- also set up my monitor   

-- list of features: points, binding set names to arbitrary regions, labeling, multiple (possibly unsatisfiable-in-some-orders) constraints, and underconstrained diagrams

-- & is reverse apply
-- need to make a list of prevCoords and deal w each
-- also, to deal w 3 -> arbitrary sets, i should be able to loop genMany and crop
-- horrible hack to deal w lack of dynamic typing or typeclasses
badsAndGoodCirs :: RandomGen g => [Circle] -> g -> (([Circle], Circle), g)
badsAndGoodCirs prevCirs gen =
                let (bads, (good, gen')) = genMany gen cirCoords & crop (cIntersectAll prevCirs) in ((bads, good), gen')

-- hardcoded intersection
badsAndGoodPts :: RandomGen g => [Circle] -> g -> (([LocPt], LocPt), g)
badsAndGoodPts prevCirs gen =
               let (bads, (good, gen')) = genMany gen ptCoords & crop (ptInAll prevCirs) in ((bads, good), gen')

drawBadsCir bads = map drawBadCir bads & mconcat
drawGoodsCir :: [Circle] -> Diagram B
drawGoodsCir goods = zipWith (\l c -> c { nmc = l }) ['A', 'B'..] goods & map (\c -> cName c [nmc c] <> (draw c # opacity 0.4 # lw none)) & zipWith fc (brewerSet Set2 8) & mconcat
-- note palette above from http://hackage.haskell.org/package/palette-0.1.0.4/docs/Data-Colour-Palette-BrewerSet.html
-- also: factor the zipWith out into calling drawGoodCir!

drawBadsPt bads = map drawBadPt bads & mconcat
drawGoodsPt goods = map drawGoodPt goods & mconcat

-- typeclass tests
-- tctest :: Num a => (a -> a) -> a -> a
-- tctest f x = f x
-- test2 :: Int -> Int
-- test2 x = -x
-- test3 = tctest test2

-- from stackoverflow. truncate to 2 points
trunc num = (fromInteger $ round $ num * (10^2)) / (10.0^^2)
-- TODO add debug flag
drawBadCir c = draw c # fc red # opacity 0.0 # lw none -- <> circText c -- TODO don't display failed attempts
drawGoodCir c = cName c [nmc c] <> draw c # fc green # opacity 0.4 # lw none

badPointR = 4
goodPointR = 10
drawBadPt (px, py) = drawBadCir $ Circle {x = px, y = py, r = badPointR, nmc = 'X'}
drawGoodPt (px, py) = pName (px, py) "p" <> circle goodPointR # translateX px # translateY py # fc black # lw none # opacity 0.7 

fontSize = 40
padC = 0
cName c textC = alignedText 0.5 0.5 textC # scale 60 # translateX (cx + padC) # translateY (cy + padC)
      -- # translateX (cx + padP) # translateY (cy + padP)
      where cx = x c
            cy = y c

padP = 40
-- TODO actually give points names / take as input. the types should have a name param
pName :: LocPt -> String -> Diagram B
pName (x,y) textC =
      alignedText 0.5 0.5 textC # scale 60
      -- stroke (textSVG textC 1) # fc purple # fillRule EvenOdd -- doesn't work
            # translateX (x + padP) # translateY (y + padP)

circText c = alignedText cx cy textC # scale 20
         where cx = x c
               cy = y c
               textC = "x = " ++ (show $ trunc cx) ++ ", y = " ++ (show $ trunc cy) ++ ", r = " ++ (show $ trunc $ r c)

draw :: Circle -> Diagram B
draw randC = circle (r randC) # translateX (x randC) # translateY (y randC)

-- not the most efficient impl. also assumes infinite list s.t. head always exists
crop :: RandomGen g => (a -> Bool) -> [(a, g)] -> ([a], (a, g))
crop cond xs = (takeWhile (not . cond) (map fst xs), -- drop gens
                head $ dropWhile (\(x, _) -> not $ cond x) xs) -- keep good's gen

-- eventually conditions will have to refer to *an arbitrary number* of previous results of various types (points, sets, regions, and their locations) -- and possible prev constraints and constraints on labeling
-- the type might look like [Point] -> [Set] -> [Region] -> Bool 
-- note: circle values are doubles. so, choose a condition that will eventually be true, and not cause an infinite list
-- circles intersect iff the distance b/t their centers < the sum of their radii
cIntersect :: Circle -> Circle -> Bool
cIntersect c1 c2 = {- traceShowId $ -} distance (p2 (x c1, y c1)) (p2 (x c2, y c2)) < r c1 + r c2

-- TODO factor out common code
cIntersectAll :: [Circle] -> Circle -> Bool
cIntersectAll cs c = and $ map (cIntersect c) cs

ptIn :: LocPt -> Circle -> Bool
ptIn p c = distance (p2 p) (p2 (x c, y c)) < r c

ptInAll :: [Circle] -> LocPt -> Bool
ptInAll cs p = and $ map (ptIn p) cs

inBox :: LocPt -> Bool
inBox (x, y) = x >= -len && x <= len && y >= -len && y <= len
          where len = 50

-- keep the last generator for the "good" element
genMany :: RandomGen g => g -> (g -> (a, g)) -> [(a, g)]
genMany gen genOne = iterate (\(c, g) -> genOne g) (genOne gen)

cirCoords :: RandomGen g => g -> (Circle, g)
cirCoords gen = (Circle { x = randX, y = randY, r = randR, nmc = 'X' }, gen3) -- hacky name
        where (randX, gen1) = randomR imgRange gen
              (randY, gen2) = randomR imgRange gen1
              (randR, gen3) = randomR rRange gen2

ptCoords :: RandomGen g => g -> (LocPt, g)
ptCoords gen = ((randX, randY), gen2)
        where (randX, gen1) = randomR imgRange gen
              (randY, gen2) = randomR imgRange gen1

box :: Diagram B
box = rect dim dim

numSets = 3
rowSize = 3
numRows = 3
horizSep = 50
vertSep = horizSep -- TODO put all params together


-- slowly building up to handle the entirety of Substance??
-- TODO also handle maps between objects
data DiagramType = PtIn | NSetIntersect Int deriving (Eq, Show) -- | Subset, 

-- this name might be a hack?
data Circle = Circle { x :: Double, y :: Double, r :: Double, nmc :: Char } deriving (Eq, Show)
-- cx, cy = center
data Rect = Rect { cx :: Double, cy :: Double, w :: Double, h :: Double, nmr :: Char } deriving (Eq, Show) 
type LocPt = (Double, Double) -- TODO point name

-- another type specifying set rendering: Style??
-- also one for point rendering?
data SetStyle = CircleStyle | RectStyle -- | Graph

-- TODO same pattern as circs, factor out? 
diags :: RandomGen g => g -> DiagramType -> [(Diagram B, g)]
diags gen dtype = iterate (\(c, g) -> boxedDiagram g dtype) (boxedDiagram gen dtype)

-- assume infinite list. also why isn't this in the std lib? also n > 0
breakInto :: Int -> [a] -> [[a]]
breakInto n l = (take n l) : (breakInto n (drop n l))

boxedDiagram :: RandomGen g => g -> DiagramType -> (Diagram B, g)
boxedDiagram gen dtype = (pic {-<> box # lw 0.8 # opacity 0.2-}, gen') -- remove box
             where pic = drawGoodsPt (goodPts r) <> drawGoodsCir (goodCirs r) <> drawBadsCir (badCirs r) <> drawBadsPt (badPts r)
                   (r, gen') = genUnstyledShapes gen dtype

-- test :: DiagramType -> String
-- test dtype = show dtype ++ depending
--      where depending = case dtype of
--                        TwoSetIntersect -> "tsi"
--                        PtIn -> "pi"

-- draw one picture of multiple attempts to satisfy the constraint until one succeeds. include all preceding failures and one success
-- bit confusing bc some functions are polymorphic and some aren't

-- TODO organize all types together
data UnstyledShapes = Shapes { goodPts :: [LocPt], goodCirs :: [Circle],
                               badPts :: [LocPt], badCirs :: [Circle] }
                      deriving (Show)

-- TODO factor out common code?
-- TODO deal with n = 0, 1. also this could be made more efficient by sampling all circles at once?? vs getting 2 to intersect then sampling the third?
genUnstyledShapes :: RandomGen g => g -> DiagramType -> (UnstyledShapes, g)
genUnstyledShapes gen (NSetIntersect 2) = (res, gen'') -- base case
         where (circ1, gen') = cirCoords gen
               ((badCirsRes, goodCirc2), gen'') = badsAndGoodCirs [circ1] gen'
               res = Shapes { goodPts = [], goodCirs = [circ1, goodCirc2],
                              badPts = [], badCirs = badCirsRes }
genUnstyledShapes gen (NSetIntersect n) = (res, gen'') -- recursive case
         where (nmoIntersect, gen') = genUnstyledShapes gen (NSetIntersect (n-1))
               circsSoFar = goodCirs nmoIntersect
               ((badCirsRes, goodCircN), gen'') = badsAndGoodCirs circsSoFar gen'
               res = Shapes { goodPts = [], goodCirs = goodCircN:circsSoFar,
                              badPts = [], badCirs = badCirsRes }
genUnstyledShapes gen PtIn = (res, gen'')
         where (intersecting, gen') = genUnstyledShapes gen (NSetIntersect numSets)
               ((badPtsRes, goodPt), gen'') = badsAndGoodPts (goodCirs intersecting) gen'
               res = Shapes { goodPts = [goodPt], goodCirs = goodCirs intersecting,
                              badPts = badPtsRes, badCirs = [] }

-- TODO figure out how to interpret basicSpec, keenanSpec
-- TODO pass in size as command line args
-- note diags is infinite list
-- TODO use monad / diagram queries as here? http://projects.haskell.org/diagrams/doc/manual.html#using-queries
main = mainWith (diags rng PtIn
                 & map fst & take (rowSize * numRows) & gridCat)
     -- breakInto rowSize
     --             & map (hsep horizSep) & take numRows & vsep vertSep)
