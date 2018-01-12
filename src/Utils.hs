-- | "Utils" contains frequently used utility function, some parameters to "Runtime", and debugging functions.

module Utils where
import Control.Monad (void)
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String -- input stream is of the type ‘String’
import qualified Text.Megaparsec.Lexer as L

divLine = putStr "\n--------\n\n"

-- don't use r2f outside of zeroGrad or addGrad, since it doesn't interact well w/ autodiff
r2f :: (Fractional b, Real a) => a -> b
r2f = realToFrac

toList :: a -> [a]
toList x = [x]

trd :: (a, b, c) -> c
trd (_, _, x) = x

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

stepsPerSecond :: Int
stepsPerSecond = 100000

picWidth, picHeight :: Int
picWidth = 800
picHeight = 700

ptRadius :: Float
ptRadius = 4 -- The size of a point on canvas

defaultWeight :: Floating a => a
defaultWeight = 1


debug = True
debugLineSearch = False
debugObj = False -- turn on/off output in obj fn or constraint

-- used when sampling the inital state, make sure sizes satisfy subset constraints
subsetSizeDiff :: Floating a => a
subsetSizeDiff = 10.0

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

halfDiagonal :: (Floating a) => a -> a
halfDiagonal side = 0.5 * dist (0, 0) (side, side)

labelName :: String -> String
labelName name = "Label_" ++ name

-- | `compose2` is used to compose with a function that takes in
-- two arguments. As if now, it is used to compose `penalty` with
-- constraint functions
compose2:: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c
compose2 = (.) . (.)


--------------------------------------------------------------------------------
---- Lexer helper functions
-- TODO: think about if it make sense to have the same set of reserved words
--       in both Substance and Style.

rws, attribs, attribVs, shapes, types :: [String] -- list of reserved words
rws =     ["avoid", "global", "as"] ++ types ++ shapes
-- ++ types ++ attribs ++ shapes ++ colors

types =   ["Definition", "Set", "Map", "Point", "In", "NotIn", "Subset", "NoSubset", "Intersect", "NoIntersect"]
attribs = ["shape", "color", "label", "scale", "position"]
attribVs = shapes
shapes =  ["Auto", "None", "Circle", "Box", "SolidArrow", "SolidDot", "HollowDot", "Cross"]
-- colors =  ["Random", "Black", "Red", "Blue", "Yellow"]

-- TODO: should the rws for Style and Substance be separated at all?
identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

sc :: Parser ()
sc = L.space (void separatorChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "--" >> newline'
        blockCmnt = L.skipBlockComment "/*" "*/" >> newline'

newline' :: Parser ()
newline' = void sc >> void (many newline) >> void sc

backticks :: Parser a -> Parser a
backticks = between (symbol "`") (symbol "`")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

lparen, rparen, lbrac, rbrac, colon, arrow :: Parser ()
lbrac = void (symbol "{")
rbrac = void (symbol "}")
lparen = void (symbol "(")
rparen = void (symbol ")")
colon = void (symbol ":")
arrow = void (symbol "->")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

comma :: Parser String
comma = symbol ","

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

float :: Parser Float
float = realToFrac <$> lexeme L.float -- TODO: parsing without sign?


-- Reserved words
rword :: String -> Parser ()
rword w = string w *> notFollowedBy alphaNumChar *> sc


--------------------------------------------------------------------------------
-- Debug Functions

-- Some debugging functions.
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

-- To send output to a file, do ./EXECUTABLE 2> FILE.txt
tr :: Show a => String -> a -> a
tr s x = if debug then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

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
dotL :: Floating a => [a] -> [a] -> a
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

norm :: Floating a => [a] -> a
norm = sqrt . sum . map (^ 2)

normsq :: Floating a => [a] -> a
normsq = sum . map (^ 2)

normalize :: Floating a => [a] -> [a]
normalize v = (1 / norm v) *. v

-- Find the angle between x-axis and a line passing points, reporting in radians
findAngle :: Floating a => (a, a) -> (a, a) -> a
findAngle (x1, y1) (x2, y2) = atan $ (y2 - y1) / (x2 - x1)

midpoint :: Floating a => (a, a) -> (a, a) -> (a, a) -- mid point
midpoint (x1, y1) (x2, y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

dist :: Floating a => (a, a) -> (a, a) -> a -- distance
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

distsq :: Floating a => (a, a) -> (a, a) -> a -- distance
distsq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2
