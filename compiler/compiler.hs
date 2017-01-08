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

data SubObjType = Set SetType | Pt | Map -- how to tie this to the types above
     deriving (Show, Eq)

data SubDecl = Decl SubObj
     deriving (Show, Eq)

-- TODO vs. Set Set. user only specifies names
-- TODO we assume that non-subset sets must not overlap (implicit constraint)
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

-- Substance reference checker TODO
-- returns lines in same order as in program
subSeparate :: SubSpec -> SubSpecDiv
subSeparate = foldr separate ([], [])
            where separate line (decls, constrs) =
                           case line of
                           (LD x) -> (x : decls, constrs)
                           (LC x) -> (decls, x : constrs)

-- Substance typechecker TODO

---------------------------------------

-- Style grammar (relies on the Substance grammar, specifically SubObj)
-- no styling for constraints

data SetShape = SetCircle | Box
     deriving (Show, Eq)

data PtShape = SolidCirc
     deriving (Show, Eq)

data MapShape = LeftArr | RightArr | DoubleArr
     deriving (Show, Eq)

data Direction = Horiz | Vert | Angle
     deriving (Show, Eq)

data SubShape = SS SetShape | SP PtShape | SM MapShape
     deriving (Show, Eq)

data LineType = Solid | Dotted 
     deriving (Show, Eq)

data Color = Red | Blue | Black | Yellow -- idk
     deriving (Show, Eq)

data M a = Auto | Override a -- short for Maybe (option type)
     deriving (Show, Eq)

data StyLevel = SubVal String | LabelOfSubVal String | SubType SubObjType | Global
     deriving (Show, Eq)
-- LabelOfSubObj is meant to allow styling of whatever label object A (originally named A) now has
-- e.g. it could be named "hello" with Label (SubValue "A") (Override "Hello"). (don't label labels)
-- then do (Position (LabelOfSubObj "A") 100 200)

type Opacity = Float -- 0 to 100%
type Priority' = Float -- higher = higher priority
type Angle = Float

-- There are three different layers of Style: global setting (over all types), 
-- type setting (over all values of that type), and value setting. 
-- The more specific ones implicitly override the more general ones.
-- If there are two conflicting ones at the same level, the more recent one will be used for "everything"
-- TODO more sophisticated system with scope
-- TODO if some aspect of style unspecified, then supplement with default
-- any setting is implicitly an override of the global default style
-- can choose to leave out an aspect (= implicitly auto), or explicitly specify auto; same thing
-- TODO need to validate that the shape specified matches that of the type
-- e.g. Shape Map Diamond is invalid
-- also, order does not matter between lines
data StyLine = Shape StyLevel (M SubShape) -- implicitly solid unless line is specified
               | Line StyLevel (M LineType) (M Float) -- hollow shape; non-negative thickness
               | Color StyLevel (M Color) (M Opacity)
               | Priority StyLevel (M Priority') -- for line breaking
               | Dir StyLevel (M Direction)
               | Label StyLevel (M String)
               | Size StyLevel (M Float) -- size and position in pixels
               | AbsPos StyLevel (M Float) (M Float) -- TODO relative positions
     deriving (Show, Eq)

type StySpec = [StyLine]

-- Sample Style programs and tests
-- TODO style is more complicated than substance; this doesn't test it fully

nl = "\n"
together = intercalate nl
sty0 = "Shape Global Circle"
sty1 = "Shape Set Box"
sty2 = "Shape A Circle"
sty3 = together [sty0, sty1, sty2] -- test overrides
sty4 = "Shape Set Circle\nShape Map RightArr\nLine Map Solid"
-- test all
sty_all = "Shape Set Circle\nShape Map RightArr\nLine Map Solid\nColor Global Blue 100\nPriority Map 100\nPriority Set 50\nDir Map Horiz\nDir A Vert\nLabel A NewA\nSize A 100\nPosition A -100 501\nColor Label_A Blue 100"
-- TODO deal with OpenSet, ClosedSet
-- TODO write tests of substance working *with* style

-- TODO add tests that it should fail
styf1 = "Shape"
styf2 = "Shape Label_A"

-- TODO syntactically valid but semantically invalid programs
styfs1 = "Shape Map Circle"

-- Style parser
styParse :: String -> StySpec -- same as subParse
styParse = map (styToLine . words) . filter nonempty . lines
         where nonempty x = (x /= "")

getLevel :: String -> StyLevel
getLevel s = if s == "All" then Global
             -- TODO will need to update parsers whenever I add a new type...
             else if s == "Set" then SubType (Set Unspecified)
             else if s == "OpenSet" then SubType (Set Open)
             else if s == "ClosedSet" then SubType (Set Closed)
             else if s == "Point" then SubType Pt
             else if s == "Map" then SubType Map
             else if (take 6 s == "Label_")
                  then let res = drop 6 s in
                       if length res > 0 then LabelOfSubVal res
                       else error "Empty object name ('Label_') in style level"
             else SubVal s -- sets could be named anything; later we validate that this ref exists

getShape :: [String] -> M SubShape
getShape [] = error "No Style shape param"
getShape [x] = if x == "Auto" then Auto
               else if x == "Circle" then Override (SS SetCircle)
               else if x == "Box" then Override (SS Box)
               else if x == "SolidCircle" then Override (SP SolidCirc)
               else if x == "LeftArrow" then Override (SM LeftArr)
               else if x == "RightArrow" then Override (SM RightArr)
               else if x == "DoubleArrow" then Override (SM DoubleArr)
               else error $ "Invalid shape param '" ++ show x ++ "'"
getShape s = error $ "Too many style shape params in '" ++ show s ++ "'"

styToLine :: [String] -> StyLine
styToLine [] = error "Style spec line is empty"
styToLine s@[x] = error $ "Style spec line '" ++ show s ++ "' is only 1 token"
styToLine s@(x : y : xs) = let level = getLevel y in -- throws its own errors
                           if x == "Shape" then
                              let shp = getShape xs in
                              Shape level shp
                           -- TODO write out the rest of this
                           else error $ "Style spec line: '" ++ show s
                                ++ "' does not begin with Shape/Line/Color/Priority/Direction/Label/Size/Position"

-- Pretty-printer for Style AST

-- Style validater

-- Style typechecker

-- Style reference checker

---------------------------------------

-- Take a Substance and Style program, and produce the abstract layout representation

-- TODO try doing this w/o Style first? everything is compiled to a default style with default labels
-- write out applying Style on top: applying global overrides, then by type, then by name
-- going to need some kind of abstract intermediate type
-- figure out the intermediate subsets of the language I can support 
  -- e.g. map requires me to draw arrows, don't support direction and priority for now
-- how many objects I can support, e.g. A -> B -> C -> D requires scaling the size of each obj to fit on canvas
-- how the optimization code needs to scale up to meet the needs of multiple objects (labels only?)
-- how to lay things out w/ constraints only (maybe)
-- how to apply optimization to the labels & what their obj functions should be

-- TODO finish parser for both, put into Slack tonight w/ description of override, continuousmap1.sub/sty

-- Substance + Style typechecker

-- Substance + Style reference checker

-- Produce the constraints and objective function

-- Add rendering and interaction info to produce a world state for gloss

---------------------------------------

-- Runtime: layout algorithm picks a smart initial state
-- then tries to satisfy constraints and minimize objective function, live and interactively
-- TODO make module for this and optimization code

---------------------------------------

-- ghc compiler.hs; ./compiler <filename>
subContMapTest = do
       args <- getArgs
       let fileIn = head args
       program <- readFile fileIn
       putStrLn $ show $ subParse program
       putStrLn $ show $ subValidate program

main = subContMapTest
