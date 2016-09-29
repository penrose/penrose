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
import qualified Data.Set as Set
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox        
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

--------------------

-- TODO:
-- parser: read command-line argument, deal with 2 arbitrary set names
-- batch visualizer: draw all examples   
-- figure out how to fill 3-arc
-- support complements and universe
-- generalize to 3 sets   
data Region = ASubB | AIntersectB | BSubA deriving (Eq, Ord)

-- using Haskell sets
a_regions :: Set.Set Region
a_regions = Set.fromList [ASubB, AIntersectB]

b_regions :: Set.Set Region
b_regions = Set.fromList [BSubA, AIntersectB]

-- examples: empty, Universe, A, B, A n B, A u B, A \ B, B \ A, A \ B u B \ A (A del B), A^c, A^c U B^c, (A n B)^c, (A U B)^c, A^c n B^c, U n A, U n <expr>
ex_empty :: Set
ex_empty = Empty

ex_intersect = Intersection a b
ex_union = Union a b
ex_AsubB = Minus a b
ex_BsubA = Minus b a
ex_AuA = Union a a
ex_sym_diff = Union ex_AsubB ex_BsubA

-- doesn't deal with these yet b/c fill issues
ex_Ac = Complement a
ex_Bc = Complement b
ex_c_identity_1a = Union ex_Ac ex_Bc
ex_c_identity_1b = Complement ex_intersect
ex_c_identity_2a = Intersection ex_Ac ex_Bc
ex_c_identity_2b = Complement ex_union

ex_universe = Universe
ex_universe_id = Intersect ex_universe a
ex_universe_id2 = Intersect ex_universe ex_c_identity_2a

----

-- does not currently support custom set names
regions :: String -> Set.Set Region
regions "A" = a_regions
regions "B" = b_regions
regions _ = Set.empty

ast_to_regions :: Set -> Set.Set Region
ast_to_regions set =
               case set of
                  St st              -> regions st
                  Empty              -> Set.empty
                  Union s1 s2        -> ast_to_regions s1 `Set.union`
                                        ast_to_regions s2
                  Intersection s1 s2 -> ast_to_regions s1 `Set.intersection`
                                        ast_to_regions s2
                  Minus s1 s2        -> ast_to_regions s1 `Set.difference`
                                        ast_to_regions s2
                  Universe           -> Set.empty -- TODO
                  Complement s1      -> Set.empty -- TODO

-- note: leftmost on top?
regions_to_diagram :: [Region] -> [Diagram B]
regions_to_diagram [] = [venn]
regions_to_diagram (region:regions) = result : (regions_to_diagram regions)
                   where result = case region of
                                  ASubB       -> a_minus_b 
                                  AIntersectB -> a_intersect_b
                                  BSubA       -> b_minus_a

-- TODO abstract out walking over AST
prettyPrint :: Set -> String
prettyPrint set =
               case set of
                  St st              -> st
                  Empty              -> "∅"
                  Union s1 s2        -> prettyPrint s1 ++ " ∪ " ++
                                        prettyPrint s2
                  Intersection s1 s2 -> prettyPrint s1 ++ " ∩ " ++
                                        prettyPrint s2
                  Minus s1 s2        -> prettyPrint s1 ++ " ∖ " ++
                                        prettyPrint s2
                  Universe           -> "Ω"
                  Complement s1      -> "ᶜ"

-- change this to change what it draws
ex_to_use = ex_sym_diff
        
----

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
main = mainWith
     ((alignedText 0 0 $ prettyPrint ex_to_use) # scale 0.2
     <> (mconcat $ regions_to_diagram
     $ Set.toList $ ast_to_regions
     ex_to_use))
-- main = mainWith (b_minus_a <> a_minus_b <> a_intersect_b <> venn)
-- main = putStrLn (show keenanSpec)
