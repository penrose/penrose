{- TODO:
   - parser, pretty-printer, program generator, program visualizer
   - convert to Elm, fix GHC -}

data Set =
     St String
     | Empty
     | Universe
     | Intersection Set Set
     | Union Set Set
     | Complement Set
     | Minus Set Set
     deriving (Show, Eq)

data Point = Pt String
     deriving (Show, Eq)

data Spec =
     LetSet String Set
     | BindSets [String]
     | In Point Set
     | Intersect Set Set Bool   -- note difference b/t this and Intersection
     | Subset Set Set Bool
     | PointIn Point Set
     deriving (Show, Eq)

type Prog = [Spec]

--------------------------------------

keenanSpec :: Prog
keenanSpec = [
           BindSets ["A", "B", "C", "D"], -- A, B, C, D := Set
           Intersect (St "A") (Intersection (St "B") (St "C")) True,
                                -- Intersect (A, B, C) = TRUE
           Subset (St "D") (Intersection (St "A") (Intersection (St "B") (St "C")))
              True,             -- Subset( D, Intersection(A,B,C) ) = TRUE 
           PointIn (Pt "p") (Minus (Intersection (St "A") (Intersection (St "B")
              (St "C"))) (St "D")), -- p := Point in (Intersection( A, B, C ) \ D)
           PointIn (Pt "q") (Intersection (St "B") (St "C")),
           BindSets ["E"],      -- E := Set
           Intersect (St "E") (Union (Union (St "A") (St "B")) (St "C")) False]
                                -- Intersect( E, Union(A,B,C) ) = FALSE

a :: Set
a = St "A"

b :: Set
b = St "B"

c :: Set
c = Intersection a b

main = putStrLn (show keenanSpec)
