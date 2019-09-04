module Penrose.Main where

import           System.Directory
import           System.IO
import           System.Random

{- import qualified Data.Map.Strict as M
import Data.Set

type Name = String
type Constraint = (String, [String])
data State = State { sets :: M.Map Name (Set Int),
                     points :: M.Map Name Int,
                     constraints :: Constraint, -- "PointIn(C, p)"
                     rng :: StdGen }

gen :: StdGen
gen = mkStdGen 10

initState = State { }

makeSet :: State -> Name -> State
makeSet state name =
        let newSet = fromList [1, 2, 3] -- make a list of rand ints in some range
        in state { sets = M.insert name newSet $ sets state }

makePoint :: State -> Name -> State
makePoint state name = state -- TODO

ensureSubset :: State -> Name -> Name -> State
ensureSubset state s1 s2 = state -- pick s1 and s2 so s1 \subset s2

dealWithSurjection :: State -> Name -> State
dealWithSurjection s n = s

caller :: JSON -> State
caller j =
       case objects !! 0 of
       "Set", name -> let newState = makeSet s name

outputter :: State -> String -- Substance program
outputter s = "" -}
-- TODO: actually instantiate
instantiate :: String -> String
instantiate x = "Point a"

main :: IO ()
main = do
  path <- getCurrentDirectory
  inFile <- makeAbsolute "Sub_enduser.json"
  outFile <- makeAbsolute "Sub_instantiated.sub"
  putStrLn "-------"
  putStrLn $ "plugin dir path: " ++ path
  putStrLn $ "plugin reading from file: " ++ inFile
  subProg <- readFile inFile
  putStrLn "plugin received Sub prog: "
  putStrLn subProg
     -- Plugin receives JSON and parses it
     -- For each object: make it
     -- For each function: execute it
     -- For each predicate: make sure it's true
  let result = instantiate subProg
  putStrLn $ "plugin writing to file: " ++ outFile
  writeFile outFile result
  putStrLn "done"
  putStrLn "-------"
