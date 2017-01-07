import System.IO -- read/write to file
import System.Environment
import Control.Arrow ((>>>))
import System.Random
import Debug.Trace
import Data.List

-- Substance grammar

data SetType = Open | Closed | Unspecified 
     deriving (Show, Eq)

data Set = Set' String SetType
     deriving (Show, Eq)

data Pt = Pt' String
     deriving (Show, Eq)

data Map = Map' String String String -- TODO needs validation vs Set Set
     deriving (Show, Eq)

data SubObj = OS Set | OP Pt | OM Map
     deriving (Show, Eq)
-- TODO open sets?? they should inherit style characteristics from sets

data SubDecl = Decl SubObj
     deriving (Show, Eq)

-- TODO vs. Set Set. user only specifies names
data SubConstraint = Subset String String | PointIn String String
     deriving (Show, Eq)

data SubLine = LD SubDecl | LC SubConstraint
     deriving (Show, Eq)
-- TODO do some program analysis to split the decls and constraints, which refer to things in the decls
-- and validate that the things they refer to are the right type (inline at some point??)

type SubSpec = [SubLine]
type SubSpecDiv = ([SubDecl], [SubConstraint])

-- Sample Substance programs (more in file)
sub0 = "Set A"
sub1 = "OpenSet A"     
sub2 = "Set A\nOpenSet B"
sub3 = "Set A\nOpenSet B\nSubset A B"

-- test
subSpecs = [sub0, sub1, sub2, sub3]
subTest = subValidateAll subSpecs
  
-- specs for 3 continuous map diagrams can be found in continuousmap1.sub, etc.

-- Substance parser
-- TODO divide into decls and constraints for typechecking and reference checking
-- parsing human-written programs: extra spaces ok (words removes them). 
-- extra newlines ok (filter empty lines out). HOWEVER, this is not ok for validation, as
-- the pretty-printer does not add additional whitespace.
subParse :: String -> SubSpec
subParse = map (subToLine . words) . filter nonempty . lines
         where nonempty x = (x /= "")

-- parses based on line length. should really tokenize and behave like DFA
-- TODO fix this or use lexer / parser generator
subToLine :: [String] -> SubLine
subToLine s@[x, y] = LD $ Decl $
                  if x == "Set" then OS (Set' y Unspecified)
                  else if x == "OpenSet" then OS (Set' y Open)
                  else if x == "ClosedSet" then OS (Set' y Closed)
                  else if x == "Point" then OP (Pt' y)
                  else error $ "Substance spec line: 2-token line '"
                       ++ show s ++ "' does not begin with Set/OpenSet/ClosedSet"

subToLine s@[x, y, z] = LC $ 
                   if x == "Subset" then Subset y z -- TODO validate names exist in decls, are of type set
                   else if x == "In" then PointIn y z -- TODO ^
                   else error $ "Substance spec line: 3-token line '"
                       ++ show s ++ "' does not begin with Subset/In"

subToLine s@[w, x, y, z] = LD $ Decl $
                   if w == "Map" then OM (Map' x y z)
                   else error $ "Substance spec line: 4-token line '"
                       ++ show s ++ "' does not begin with Map"
                   
subToLine s = error $ "Substance spec line '" ++ show s ++ "' is not 2, 3, or 4 tokens"

-- Pretty-printer for Substance AST
subToProg :: SubLine -> String
subToProg (LD (Decl decl)) = case decl of
                   OS (Set' name stype) -> case stype of
                                          Open -> "OpenSet " ++ name
                                          Closed -> "ClosedSet " ++ name
                                          Unspecified -> "Set " ++ name
                   OP (Pt' name) -> "Point " ++ name
                   OM (Map' x y z) -> "Map " ++ x ++ " " ++ y ++ " " ++ z
subToProg (LC constr) = case constr of
                     Subset s1 s2 -> "Subset " ++ s1 ++ " " ++ s2
                     PointIn p s -> "In " ++ p ++ " " ++ s

subPrettyPrint :: SubSpec -> String
subPrettyPrint s = concat $ intersperse "\n" $ map subToProg s

-- if a well-formed program is parsed, its output should equal the original
subValidate :: String -> Bool
subValidate s = (s == (subPrettyPrint $ subParse s))

subValidateAll :: [String] -> Bool
subValidateAll = all subValidate

-- Substance typechecker TODO

-- Substance reference checker

---------------------------------------

-- Style grammar (relies on the Substance grammar...)

-- Style parser

-- Sample Style programs and tests
sty1 = "Line Set Solid 2\nShape Set Circle"

-- Pretty-printer for Style AST

-- Style validater

-- Style typechecker

-- Style reference checker

---------------------------------------

-- Take a Substance and Style program, and produce the abstract layout representation
-- TODO try doing this w/o Style first?

-- Also produce the constraints and objective function

-- Add rendering and interaction info to produce a world state for gloss

---------------------------------------

-- Runtime: layout algorithm picks a smart initial state
-- then tries to satisfy constraints and minimize objective function, live and interactively
-- TODO make module for this and optimization code

---------------------------------------

-- ghc compiler.hs; ./compiler <filename>
main = do
       args <- getArgs
       let fileIn = head args
       program <- readFile fileIn
       putStrLn $ show $ subParse program
       putStrLn $ show $ subValidate program
