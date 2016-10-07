{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

import Data.List
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.BoundingBox        

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

----
   
main = mainWith example
