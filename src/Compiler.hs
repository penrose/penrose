 module Compiler where 
-- TODO split this up + do selective export

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
data SubConstr = Intersect String String
               | NoIntersect String String
               | Subset String String
               | NoSubset String String     
               | PointIn String String 
     deriving (Show, Eq)

data SubLine = LD SubDecl | LC SubConstr
     deriving (Show, Eq)
-- TODO do some program analysis to split the decls and constraints, which refer to things in the decls
-- and validate that the things they refer to are the right type (inline at some point??)

type SubSpec = [SubLine]
type SubSpecDiv = ([SubDecl], [SubConstr])

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

-- TODO validate names exist in decls, are of type set
-- TODO auto-gen parser from grammar
subToLine s@[x, y, z] = LC $ 
                   if x == "Intersect" then Intersect y z
                   else if x == "NoIntersect" then NoIntersect y z
                   else if x == "Subset" then Subset y z
                   else if x == "NoSubset" then NoSubset y z
                   else if x == "In" then PointIn y z -- TODO ^
                   else error $ "Substance spec line: 3-token line '"
                     ++ show s ++ "' does not begin with (No)Intersect/Subset/In"

subToLine s@[w, x, y, z] = LD $ Decl $
                   if w == "Map" then OM (Map' x y z)
                   else error $ "Substance spec line: 4-token line '"
                       ++ show s ++ "' does not begin with Map"
                   
subToLine s = error $ "Substance spec line '" ++ show s ++ "' is not 2, 3, or 4 tokens"

-- Pretty-printer for Substance AST
subPrettyPrintLine :: SubLine -> String
subPrettyPrintLine (LD (Decl decl)) = case decl of
                   OS (Set' name stype) -> case stype of
                                          Open -> "OpenSet " ++ name
                                          Closed -> "ClosedSet " ++ name
                                          Unspecified -> "Set " ++ name
                   OP (Pt' name) -> "Point " ++ name
                   OM (Map' x y z) -> "Map " ++ x ++ " " ++ y ++ " " ++ z
subPrettyPrintLine (LC constr) = case constr of
                     Subset s1 s2 -> "Subset " ++ s1 ++ " " ++ s2
                     PointIn p s -> "In " ++ p ++ " " ++ s

subPrettyPrint :: SubSpec -> String
subPrettyPrint s = concat $ intersperse nl $ map subPrettyPrintLine s

-- Ugly pretty-printer for Substance
subPrettyPrintLine' :: SubLine -> String
subPrettyPrintLine' = show

subPrettyPrint' :: SubSpec -> String
subPrettyPrint' s = concat $ intersperse nl $ map subPrettyPrintLine' s

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

data Direction = Horiz | Vert | Angle Float
     deriving (Show, Eq)

data SubShape = SS SetShape | SP PtShape | SM MapShape
     deriving (Show, Eq)

data LineType = Solid | Dotted 
     deriving (Show, Eq, Read)

data Color = Red | Blue | Black | Yellow -- idk
     deriving (Show, Eq, Read)

data M a = Auto | Override a -- short for Maybe (option type)
     deriving (Show, Eq)

data StyLevel = SubVal String | LabelOfSubVal String | SubType SubObjType | Global
     deriving (Show, Eq)
-- LabelOfSubObj is meant to allow styling of whatever label object A (originally named A) now has
-- e.g. it could be named "hello" with Label (SubValue "A") (Override "Hello"). (don't label labels)
-- then do (Position (LabelOfSubObj "A") 100 200)

type Opacity = Float -- 0 to 100%, TODO validate
type Priority' = Float -- higher = higher priority

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
               | Label StyLevel (M String) -- TODO add ability to turn off labeling
               | Scale StyLevel (M Float) -- scale factor
               | AbsPos StyLevel (M (Float, Float)) -- in pixels; TODO relative positions
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
sty5 = "Line Map Dotted 5.01"
sty6 = "Color All Red 66.7"
sty7 = "Priority Label_AB 10.1"
sty8 = "Label All hithere"
sty9 = "Label Label_AB oh_no" -- TODO don't label labels. also allow spaces in labels (strings)
sty10 = "Direction Map Horizontal"
-- test all
sty_all = "Shape Set Circle\nShape Map RightArrow\nLine Map Solid Auto\nColor Global Blue 100\nPriority Map 100\nPriority Set 50\nDirection Map Horizontal\nDirection A Vertical\nLabel A NewA\nScale A 100\nPosition A -100 501\nColor Label_A Blue 100"
-- TODO deal with OpenSet, ClosedSet
-- TODO write tests of substance working *with* style

-- TODO add tests that should fail
styf1 = "Shape"
styf2 = "Shape Label_A"
styf3 = "Line Dotted 5.01"

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

handleAuto :: String -> (String -> a) -> M a
handleAuto v f = if v == "Auto" then Auto else Override (f v)

readFloat :: String -> Float
readFloat x = read x :: Float

styToLine :: [String] -> StyLine
styToLine [] = error "Style spec line is empty"
styToLine s@[x] = error $ "Style spec line '" ++ show s ++ "' is only 1 token"
styToLine s@(x : y : xs) =
          let level = getLevel y in -- throws its own errors
          if x == "Shape" then
             let shp = getShape xs in -- TODO handleAuto
             Shape level shp
          else if x == "Line" then
             case xs of
             [a, b] -> let ltype = handleAuto a (\x -> read x :: LineType) in -- TODO read is hacky, bad errorsn
                       let lthick = handleAuto b readFloat in
                       Line level ltype lthick
             _ -> error $ "Incorrect number of params (not 3) for Line: '" ++ show xs ++ "'"
          else if x == "Color" then
               case xs of
               [a, b] -> let ctype = handleAuto a (\x -> read x :: Color) in
                         let opacity = handleAuto b readFloat in
                         Color level ctype opacity
               _ -> error $ "Incorrect # of params (not 3) for Color: '" ++ show xs ++ "'"
          else if x == "Priority" then
               case xs of
               [a] -> let priority = handleAuto a readFloat in
                      Priority level priority
               _  -> error $ "Incorrect number of params (not 2) for Priority: '" ++ show xs ++ "'"
          else if x == "Direction" then
               case xs of
               [a] -> let dir = handleAuto a (\x -> if x == "Horizontal" then Horiz
                                                    else if x == "Vertical" then Vert
                                                    else Angle (read x :: Float)) in
                      Dir level dir
               _  -> error $ "Incorrect number of params (not 2) for Direction: '" ++ show xs ++ "'"
          else if x == "Label" then
               case xs of
               [a] -> Label level (handleAuto a id) -- label name is a string
               _  -> error $ "Incorrect number of params (not 2) for Label: '" ++ show xs ++ "'"
          else if x == "Scale" then
               case xs of
               [a] -> let scale = handleAuto a readFloat in
                      Scale level scale
               _  -> error $ "Incorrect number of params (not 2) for Scale: '" ++ show xs ++ "'"
          else if x == "Position" then
               case xs of
               [a] -> if a == "Auto" then AbsPos level Auto
                      else error $ "Only 1 param, but it is not Auto: '" ++ show a ++ "'"
               [a, b] -> let x' = readFloat a in -- no Auto allowed if two params
                         let y' = readFloat b in
                         AbsPos level (Override (x', y'))
               _  -> error $ "Incorrect number of params (not 2) for Position: '" ++ show xs ++ "'"
          else error $ "Style spec line: '" ++ show s
               ++ "' does not begin with Shape/Line/Color/Priority/Direction/Label/Scale/Position"

-- Pretty-printer for Style AST
-- TODO write the full pretty-printer
styPrettyPrintLine :: StyLine -> String
styPrettyPrintLine = show

styPrettyPrint :: StySpec -> String
styPrettyPrint s = concat $ intersperse nl $ map styPrettyPrintLine s

-- Style validater

-- Style typechecker TODO

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
-- also, optimization on multiple layouts
-- how to lay things out w/ constraints only (maybe)
-- how to apply optimization to the labels & what their obj functions should be

-- TODO finish parser for both, put into Slack tonight w/ description of override, continuousmap1.sub/sty

-- Substance only, circular sets only -> world state
-- then scale up optimization code to handle any number of sets
-- then support the Subset constraint only
-- then add labels and set styles
subt1 = "Set A"
subt2 = "Set A\nSet B"
subt2a = "Set A\nSet B\nSubset A B"
subt3 = "Set A\nSet B\nSet C"
subt4 = "Set A\nSet B\nSet C"
subt4a = "Set A\nSet B\nOpenSet C"
subt5 = "Set A\nSet B\nOpenSet C\nSubset B C"
styt1 = "Color Set Blue 50"
styt2 = "Color Set Blue 50\nLine OpenSet Dotted 1"

-- New type: inner join (?) of Decl with relevant constraint lines and relevant style lines
-- (only the ones that apply; not the ones that are overridden)
-- Type: Map Var (Decl, [SubConstr], [StyLine]) <-- the Var is the object's name (string) in Substance
-- does this include labels?? it includes overridden labels in StyLine but not labels like Set A -> "A"
-- for now, assume what about labels? -- are they separate? should they be linked? they all get default style
  -- make a separate Map Label ObjName ? no, assuming no renaming for now
-- then we need to write a renderer type: Decl -> (Position, Size) -> [StyLine] -> Picture)
  -- also (Label -> (Position, Size) -> [StyLine] -> Picture)

-- if we do a demo entirely w/o optimization... is that easier? and how would I do it?
  -- layout algo (for initial state, at least): randomly place objs (sets, points) w/ no constraints
  -- or place aligned horizontally so they don't intersect, choose radius as fn of # unconstrained objs?
  -- for constraints: for any set that's a subset of another, pick a smaller radius & place it inside that set. 
  -- constraints: same for points (in fact, easier for points)--place it (at r/2, 45 degrees) inside that set
  -- there's no validation... a point/set could be in multiple sets?? assume not
  -- at this point we're almost hardcoding the diagram? not necessarily
  -- TODO actually hardcode the diagram in gloss; seeing the final representation will help
  -- add'l constraint: all other objects should not intersect. use optimization to maintain exclusion? 
-- it seems likely that i'll get rid of the opt part for diagrams--might rewrite code instead of using opt code

-- BUT can I use opt for labels, given a fixed diagram state? 
  -- put all labels in center. set label is attracted to center or just-outside-or-inside border of set,
  -- point label is attracted to just-outside-point, map label attracted to center
  -- all labels repulsed from other objects and labels.
  -- would hardcoding label locations be a bad thing to do?
  -- creating unconstrained objective fn: f :: DiagramInfo -> LabelInfo -> Label Positions -> Label Positions
  -- where f info pos = f_attract info pos + f_repel info pos, and each includes the pairwise interactions
  -- can autodiff deal with this? does this preserve the (forall a. Floating a => [a] -> a) type?
  -- is the function too complicated for autodiff?

-- TODO simple optimizer type, using state (Position and Size): ??
  -- optimization fn: put all sets at the center, then use centerAndRepel. how to maintain subset?
-- the things the optimizer needs to know are Name, Position, Size, SubObjType (which includes names of other objects that this one is linked to, e.g. Map)... (later: needs to know Direction Label Scale AbsPos)
-- and it updates the Position and possibly the Size

-- TODO how to synthesize the objective functions and constraint functions and implicit constraints?
-- pairwise interactions? see above

-- Since the compiler and the runtime share the layout representation,
-- I'm going to re-type it here since the rep may change.
-- Runtime imports Compiler as qualified anyway, so I can just convert the types again there.
-- Here: removing selc / sell (selected). Don't forget they should satisfy Located typeclass
data Circ = Circ { namec :: String
                 , xc :: Float
                 , yc :: Float
                 , r :: Float } 
     deriving (Eq, Show)

data Label' = Label' { xl :: Float
                   , yl :: Float
                   , textl :: String
                   , scalel :: Float }  -- calculate h,w from it
     deriving (Eq, Show)

data Obj = C Circ | L Label' deriving (Eq, Show)

defaultRad = 100

-- TODO these functions are now unused
-- declToShape :: SubDecl -> [Obj]
-- declToShape (Decl (OS (Set' name setType))) =
--             case setType of
--             Open -> [C $ Circ { namec = name, xc = 0, yc = 0, r = defaultRad }, L $ Label' { xl = 0, yl = 0, textl = name, scalel = 1 }]
--             Closed -> [C $ Circ { namec = name, xc = 0, yc = 0, r = defaultRad }, L $ Label' { xl = 0, yl = 0, textl = name, scalel = 1 }]
--             Unspecified -> [C $ Circ { namec = name, xc = 0, yc = 0, r = defaultRad }, L $ Label' { xl = 0, yl = 0, textl = name, scalel = 1 }]
-- declToShape (Decl (OP (Pt' name))) = error "Substance -> Layout doesn't support points yet"
-- declToShape (Decl (OM (Map' mapName fromSet toSet))) = error "Substance -> Layout doesn't support maps yet"

-- toStateWithDefaultStyle :: [SubDecl] -> [Obj]
-- toStateWithDefaultStyle decls = concatMap declToShape decls -- should use style

-- subToLayoutRep :: SubSpec -> [Obj] -- this needs to know about the Obj type??
-- subToLayoutRep spec = let (decls, constrs) = subSeparate spec in
--                    toStateWithDefaultStyle decls

-- Substance + Style typechecker

-- Substance + Style reference checker

-- Produce the constraints and objective function

-- Add rendering and interaction info to produce a world state for gloss

---------------------------------------

-- Runtime: layout algorithm picks a smart initial state
-- then tries to satisfy constraints and minimize objective function, live and interactively
-- TODO make module for this and optimization code

---------------------------------------

-- ghc compiler.hs; ./compiler <filename>.sub
parseSub = do
       args <- getArgs
       let fileIn = head args
       program <- readFile fileIn
       putStrLn $ show $ subParse program
       putStrLn $ show $ subValidate program

-- ghc compiler.hs; ./compiler <filename>.sty
parseSty = do
       args <- getArgs
       let fileIn = head args
       program <- readFile fileIn
       putStrLn $ styPrettyPrint $ styParse program

-- ghc compiler.hs; ./compiler <filename>.sub <filename>.sty
subAndSty = do
       args <- getArgs
       let (subFile, styFile) = (head args, args !! 1) -- TODO usage
       subIn <- readFile subFile
       styIn <- readFile styFile
       putStrLn $ subPrettyPrint $ subParse subIn
       putStrLn "--------"
       putStrLn $ styPrettyPrint $ styParse styIn

main = subAndSty
