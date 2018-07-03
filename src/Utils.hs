-- | "Utils" contains frequently used utility function, some parameters to "Runtime", and debugging functions.
-- | Utils should not import other modules of Penrose.

-- This is for the "typeclass synonym"
{-# LANGUAGE ConstraintKinds #-}

module Utils where
import Control.Monad (void)
import Data.Void
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Typeable
import Control.Arrow

-- | A more concise typeclass for polymorphism for autodiff
-- | NiceFloating :: * -> Constraint
-- https://stackoverflow.com/questions/48631939/a-concise-way-to-factor-out-multiple-typeclasses-in-haskell/48631986#48631986
type Autofloat a = (RealFloat a, Floating a, Real a, Show a, Ord a)
type Autofloat' a = (RealFloat a, Floating a, Real a, Show a, Ord a, Typeable a)

type Pt2 a = (a, a)

--------------------------------------------------------------------------------
-- Parameters of the system

stepsPerSecond :: Int
stepsPerSecond = 100000

picWidth, picHeight :: Int
picWidth = 800
picHeight = 700

ptRadius :: Float
ptRadius = 4 -- The size of a point on canvas

defaultWeight :: Floating a => a
defaultWeight = 1

-- Debug flags
debug = False
debugStyle = False
debugLineSearch = False
debugObj = False -- turn on/off output in obj fn or constraint

-- used when sampling the inital state, make sure sizes satisfy subset constraints
subsetSizeDiff :: Floating a => a
subsetSizeDiff = 10.0

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

--------------------------------------------------------------------------------
-- General helper functions

divLine :: IO ()
divLine = putStr "\n--------\n\n"

-- don't use r2f outside of zeroGrad or addGrad, since it doesn't interact well w/ autodiff
r2f :: (Fractional b, Real a) => a -> b
r2f = realToFrac

-- | Wrap a list around anything
toList :: a -> [a]
toList x = [x]

-- | similar to fst and snd, get the third element in a tuple
trd :: (a, b, c) -> c
trd (_, _, x) = x

-- | transform from a 2-element list to a 2-tuple
tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

-- | generic cartesian product of elements in a list
cartesianProduct :: [[a]] -> [[a]]
cartesianProduct = foldr f [[]] where f l a = [ x:xs | x <- l, xs <- a ]

-- | given a side of a rectangle, compute the length of the half half diagonal
halfDiagonal :: (Floating a) => a -> a
halfDiagonal side = 0.5 * dist (0, 0) (side, side)

-- | `compose2` is used to compose with a function that takes in
-- two arguments. As if now, it is used to compose `penalty` with
-- constraint functions
compose2 :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
compose2 = (.) . (.)

concat4 :: [([a], [b], [c], [d])] -> ([a], [b], [c], [d])
concat4 x = (concatMap fst4 x, concatMap snd4 x, concatMap thd4 x, concatMap frth4 x)
fst4 (a, _, _, _) = a
snd4 (_, a, _, _) = a
thd4 (_, _, a, _) = a
frth4 (_, _, _, a) = a

-- | Define ternary expressions in Haskell
data Cond a = a :? a

infixl 0 ?
infixl 1 :?

(?) :: Bool -> Cond a -> a
True  ? (x :? _) = x
False ? (_ :? y) = y

--------------------------------------------------------------------------------
-- Internal naming conventions

nameSep, labelWord :: String
nameSep = " "
labelWord = "label"

labelName :: String -> String
labelName name = name ++ nameSep ++ labelWord

-- | Given a Substance ID and a Style ID for one of its associated graphical primitive, generate a globally unique identifier for this primitive
uniqueShapeName :: String -> String -> String
uniqueShapeName subObjName styShapeName = subObjName ++ nameSep ++ styShapeName
 -- e.g. "B yaxis" (the concatenation should be unique), TODO add the two names as separate obj fields


--------------------------------------------------------------------------------
---- Lexer helper functions
-- TODO: separate reserved words and keywords for each of the DSLs

type Parser = Parsec Void String

rws, attribs, attribVs, shapes :: [String] -- list of reserved words
rws =     ["avoid", "global", "as"] ++ shapes ++ dsll
-- ++ types ++ attribs ++ shapes ++ colors
attribs = ["shape", "color", "label", "scale", "position"]
attribVs = shapes
shapes =  ["Auto", "None", "Circle", "Box", "SolidArrow", "SolidDot", "HollowDot", "Cross"]
labelrws = ["Label", "AutoLabel", "NoLabel"]
dsll = ["tconstructor","vconstructor","operator","forvars","fortypes","predicate", "Prop", "type", "<:", "->", "<->"]
-- colors =  ["Random", "Black", "Red", "Blue", "Yellow"]

upperId, lowerId, identifier :: Parser String
identifier = (lexeme . try) (p >>= checkId)
  where p = (:) <$> letterChar <*> many validChar
upperId = (lexeme . try) (p >>= checkId)
  where p = (:) <$> upperChar <*> many validChar
lowerId = (lexeme . try) (p >>= checkId)
  where p = (:) <$> lowerChar <*> many validChar
validChar = alphaNumChar <|> char '_'

checkId :: String -> Parser String
checkId x = if x `elem` rws
          then fail $ "keyword " ++ show x ++ " cannot be an identifier"
          else return x

texExpr :: Parser String
texExpr = dollar >> manyTill asciiChar dollar

-- | 'lineComment' and 'blockComment' are the two styles of commenting in Penrose. Line comments start with @--@. Block comments are wrapped by @/*@ and @*/@.
lineComment, blockComment :: Parser ()
lineComment  = L.skipLineComment "--"
blockComment = L.skipBlockComment "/*" "*/"

-- | A strict space consumer. 'sc' only eats space and tab characters. It does __not__ eat newlines.
sc :: Parser ()
sc = L.space (void $ takeWhile1P Nothing f) lineComment empty
  where
    f x = x == ' ' || x == '\t'

-- | A normal space consumer. 'scn' consumes all whitespaces __including__ newlines.
scn :: Parser ()
scn = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

newline' :: Parser ()
newline' = newline >> scn

backticks :: Parser a -> Parser a
backticks = between (symbol "`") (symbol "`")

lparen, rparen, lbrac, rbrac, colon, arrow, comma :: Parser ()
aps = void (symbol "'")
lbrac = void (symbol "{")
rbrac = void (symbol "}")
lparen = void (symbol "(")
rparen = void (symbol ")")
slparen = void (symbol "[")
srparen = void (symbol "]")
colon = void (symbol ":")
arrow = void (symbol "->")
comma = void (symbol ",")
dot = void (symbol ".")
eq = void (symbol "=")
dollar = void (symbol "$")


dollars :: Parser a -> Parser a
dollars = between (symbol "$") (symbol "$")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | 'integer' parses an integer.
integer :: Parser Integer
unsignedInteger       = lexeme L.decimal
integer = L.signed sc unsignedInteger

-- | 'float' parses a floating point number.
float :: Parser Float
unsignedFloat = lexeme L.float
float = L.signed sc unsignedFloat

-- Reserved words
rword :: String -> Parser ()
rword w = lexeme (string w *> notFollowedBy alphaNumChar)


--------------------------------------------------------------------------------
-- Debug Functions

-- Some debugging functions.
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

-- To send output to a file, do ./EXECUTABLE 2> FILE.txt

-- For Runtime use only
tr :: Show a => String -> a -> a
tr s x = if debug then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

-- For Style use only
trs :: Show a => String -> a -> a
trs s x = if debugStyle then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

trRaw :: Show a => String -> a -> a
trRaw s x = trace "---" $ trace s $ trace (show x ++ "\n") x -- prints in left to right order
-- trRaw s x = if debug then  trace "---" $ trace s $ trace (show x ++ "\n") x else x-- prints in left to right order

trStr :: String -> a -> a
trStr s x = if debug then trace "---" $ trace s x else x -- prints in left to right order

tr' :: Show a => String -> a -> a
tr' s x = if debugLineSearch then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

tro :: Show a => String -> a -> a
tro s x = if debugObj then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

--------------------------------------------------------------------------------
-- Lists-as-vectors utility functions

-- define operator precedence: higher precedence = evaluated earlier
infixl 6 +., -.
infixl 7 *. -- .*, /.

-- assumes lists are of the same length
dotL :: (RealFloat a, Floating a) => [a] -> [a] -> a
dotL u v = if not $ length u == length v
           then error $ "can't dot-prod different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else sum $ zipWith (*) u v

(+.) :: Floating a => [a] -> [a] -> [a] -- add two vectors
(+.) u v = if not $ length u == length v
           then error $ "can't add different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else zipWith (+) u v

(-.) :: Floating a => [a] -> [a] -> [a] -- subtract two vectors
(-.) u v = if not $ length u == length v
           then error $ "can't subtract different-len lists: " ++ (show $ length u) ++ " " ++ (show $ length v)
           else zipWith (-) u v

negL :: Floating a => [a] -> [a]
negL = map negate

(*.) :: Floating a => a -> [a] -> [a] -- multiply by a constant
(*.) c v = map ((*) c) v

-- Again (see below), we add epsd to avoid NaNs. This is a general problem with using `sqrt`.
norm :: Floating a => [a] -> a
norm v = sqrt ((sum $ map (^ 2) v) + epsd)

normsq :: Floating a => [a] -> a
normsq = sum . map (^ 2)

normalize :: Floating a => [a] -> [a]
normalize v = (1 / norm v) *. v

-- Find the angle between x-axis and a line passing points, reporting in radians
findAngle :: Floating a => (a, a) -> (a, a) -> a
findAngle (x1, y1) (x2, y2) = atan $ (y2 - y1) / (x2 - x1)

midpoint :: Floating a => (a, a) -> (a, a) -> (a, a) -- mid point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

-- We add epsd to avoid NaNs in the denominator of the gradient of dist.
-- Now, grad dist (0, 0) (0, 0) is 0 instead of NaN.
dist :: Floating a => (a, a) -> (a, a) -> a -- distance
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2 + epsd)

distsq :: Floating a => (a, a) -> (a, a) -> a -- distance
distsq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

--------------------------------------
-- Reflection capabilities to typecheck Computation functions
-- Typeable doesn't works with polymorphism (e.g. `id`) but works with `Floating a` by replacing it with `Double`
-- Adapted https://stackoverflow.com/questions/5144799/reflection-on-inputs-to-a-function-in-haskell
-- TODO unit test for function
-- Unfortunately it's pretty slow...

-- BTW, to check if something has the same type at runtime, can check equality of their typereps (or [Typerep], or [String], or String)
fnTypes :: Typeable a => a -> [TypeRep]
fnTypes x = split (typeOf x)
       where split t = case first tyConName (splitTyConApp t) of
                       (_     ,  []) -> [t] -- not an arrow
                       ("(->)", [x]) -> [x] -- return value type
                       ("(->)",   x) -> let current = init x
                                            next    = last x
                                        in current ++ split next
                       (_     ,   _) -> [t]

fnTypesStr :: Typeable a => a -> [String]
fnTypesStr = map show . fnTypes

-- If not an arrow type, then first list is empty (as expected)
inputsOutput :: Typeable a => a -> ([TypeRep], TypeRep)
inputsOutput x = let res = fnTypes x in
                 (init res, last res)

inputsOutputStr :: Typeable a => a -> ([String], String)
inputsOutputStr x = let (args, val) = inputsOutput x in
                    (map show args, show val)
