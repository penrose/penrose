module Test where
import Debug.Trace
import Control.Arrow
import Data.Maybe (isJust, fromJust)
import Data.List
-- import Data.Tree.Pretty (drawTree)
import Data.Vector ((!), (//), (!?), Vector)
import Text.Show.Pretty (pPrint)
import qualified Data.Vector as V
import Numeric

--------------------------------------------------------------------------------
-- Types

type Coord = (Int, Int)
data Entry = Ent Float (Maybe Coord)
type Grid  = Vector (Vector Entry)
instance Show Entry where
    -- show (Ent e _)
    --     | e < 0     = "-"
    --     | e > 0     = "+"
    --     | otherwise = "o"
     show (Ent e _) = showFFloat (Just 2)  e ""

--------------------------------------------------------------------------------
-- Constants

cellWidth :: Float
cellWidth = 1.0

xc, yc, r :: Float
xc = 0
yc = 0
r  = 15.0

-- | 'res' is the lowest resolution of BVH leaf nodes
res :: Int
res = 10

defaultCell :: Entry
defaultCell = Ent (-1.0) Nothing

closestPt :: Entry -> Maybe Coord
closestPt (Ent _ i) = i


--------------------------------------------------------------------------------
-- 2D Vector utils

-- function to initizlie a bunch of the same value in a 2d grid of user defined size
-- COMBAK: better naming
gridWith :: Int -> Int -> Entry -> Grid
gridWith x y = V.replicate y . V.replicate x

dimensions :: Grid -> (Int, Int)
dimensions g = (V.length g, V.length $ V.head g)

imap2D :: ((Int, Int) -> Entry -> Entry) -> Grid -> Grid
imap2D f = V.imap (\i row -> V.imap (\j e -> f (i,j) e) row)

ifoldl2D :: ( a -> (Int, Int) -> Entry -> a) -> a -> Grid -> a
ifoldl2D f = V.ifoldl (\ a i row -> V.ifoldl (\ a' j e -> f a' (i,j) e ) a row)

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) matrix (x, y) = matrix ! x ! y

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) matrix (x, y) = case matrix !? x of
    Nothing -> Nothing
    Just r  -> r !? y

--------------------------------------------------------------------------------
-- Level set construction

showGrid :: Grid -> IO ()
showGrid = Prelude.mapM_ print

dist :: Floating a => (a, a) -> (a, a) -> a
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

i2f :: Floating a => Coord -> (a, a)
i2f (x, y) = (fromIntegral x, fromIntegral y)

queryCirc :: Coord -> Float -> (Float, Float) -> Float
queryCirc pt r origin = let d = dist pt' origin - r in
    if abs d < cellWidth then 0.0 else d
    where pt' = (fromIntegral *** fromIntegral) pt

update :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
update matrix (x, y) v = matrix // [(x, matrix ! x // [(y, v)])]

initGrid :: Float -> Grid
initGrid radius = imap2D helper g
    where
        g    = gridWith side side defaultCell
        side = round $ 4 * radius
        (width, height) = dimensions g
        o    = (fromIntegral width / 2, fromIntegral height / 2)
        helper coord _  = let a = queryCirc coord r o in
                Ent a $ if a == 0.0 then Just coord else Nothing

neighbors :: Grid -> Coord -> [Entry]
neighbors grid (x,y) = map fromJust $ filter isJust $ map (grid !!?) ls
    where ls = [ (x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1), (x  , y+1),
                  (x  , y-1), (x-1, y), (x+1, y) ]

updateCell :: Grid -> Coord -> Grid
updateCell grid curCoord =
    let o           = i2f curCoord
        Ent d _     = grid !!! curCoord
        (ns, _)     = partition (isJust . closestPt) $ neighbors grid curCoord
        toBoundry   = dist o . i2f . fromJust . closestPt
        cmp a b     = compare (toBoundry a) (toBoundry b)
        signOf d    = if d== 0 then 0 else ( d / abs d )
        newPt (Ent _ (Just pt)) = update grid curCoord $
                                Ent ( signOf d * dist o (i2f pt)) (Just pt)
    in if null ns then grid else newPt $ minimumBy cmp ns

fsm :: Grid -> Grid
fsm grid = fs1 grid
--COMBAK : fix fast sweeping, add in other passes
-- fs1, fs2, fs3, fs4 :: Grid -> Grid
-- ULtoLR
fs1 :: Grid -> Grid
fs1 grid = foldl updateCell grid coords
    where
        (row, col) = dimensions grid
        coords     = [ (x,y) | x <- [1..row-1], y <- [1..col-1] ]

--------------------------------------------------------------------------------
-- BVH
data BBox = BBox Coord Coord deriving Show
data Tree = Leaf BBox | Node Tree Tree BBox | EmptyNode deriving Show
-- instance Show Tree where
--     show e = show e
data Axis = X | Y deriving Show

tr = trace

showTree :: Tree -> Int -> Int -> String
showTree (Node l r _) curDepth maxDepth = ""
    -- if curDepth < maxDepth
    --     then "    " ++ showTree l (curDepth+1) maxDepth ++ "," ++ showTree r (curDepth+1) maxDepth ++ "\n"
    --     else "\n"

maxInt, minInt :: Int
maxInt = maxBound
minInt = minBound


bVHHelper :: Grid -> BBox -> Axis -> Tree
bVHHelper g bbox@(BBox (x0, y0) (x1, y1)) X =
    if x1 - x0 < res then
        case getBBox g (x0,y0) (x1,y1) of
            Nothing   -> EmptyNode
            Just bbox -> Leaf bbox
    else
        let left  = toNode $ getBBox g (x0,y0) (x1, y1 - div (y1 - y0) 2)
            right = toNode $ getBBox g (x0, y1 - div (y1 - y0) 2) (x1, y1)
        in  Node left right bbox
    where toNode maybeB = case maybeB of
            Nothing -> EmptyNode
            Just b  -> bVHHelper g b Y
bVHHelper g bbox@(BBox (x0, y0) (x1, y1)) Y =
    if y1 - y0 < res then
        case getBBox g (x0,y0) (x1,y1) of
            Nothing   -> EmptyNode
            Just bbox -> Leaf bbox
    else
        let left  = toNode $ getBBox g (x0,y0) (x1 - div (x1 - x0) 2, y1)
            right = toNode $ getBBox g (x1 - div (x1 - x0) 2, y0) (x1, y1)
        in  Node left right bbox
    where toNode maybeB = case maybeB of
            Nothing -> EmptyNode
            Just b  -> bVHHelper g b X

constructBVH :: Grid -> Tree
constructBVH grid =
    let (row, col) = dimensions grid
        rootBox    = getBBox grid (0,0) (row-1, col-1)
    in case rootBox of
        Nothing   -> EmptyNode -- TODO: handle empty shapes (?)
        Just bbox -> bVHHelper grid bbox X

-- slice2D:: Grid -> Coord -> Coord -> Grid
-- slice2D g (startx, starty) (rows, cols) =
--      V.map (V.slice starty cols) $ V.slice startx rows g

getBBox :: Grid -> Coord -> Coord -> Maybe BBox
getBBox g orig@(startx, starty) (endx, endy) =
    let
        defaultPts = ((maxInt, maxInt) ,(minInt, minInt))
        coords     = [ (x,y) | x <- [startx..endx], y <- [starty..endy] ]
        pts@((xMin,yMin) , (xMax,yMax)) = foldl (minMaxBBox g) defaultPts coords
            -- ifoldl2D minMaxBBox defaultPts subG
        --b = BBox pMin pMax
    in if pts == defaultPts then Nothing else Just $ uncurry BBox pts

-- find outer BBox, save L and T in Coord a,
-- R and B in Coord b

-- Divide in half, repeat ^, save in tree Node

minMaxBBox :: Grid -> (Coord, Coord) -> Coord -> (Coord, Coord)
minMaxBBox g c@((xMin, yMin), (xMax, yMax) ) (i,j) =
    let
        (Ent val _) = g !!! (i,j)
    in
    if val <= 0 then
         (( min xMin i, min yMin j), (max xMax i, max yMax j))
    else c

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    let grid = initGrid r
    let (row, col) = dimensions grid
    let bvh  = constructBVH grid
    showGrid grid
    putStrLn ""
    -- let smaller = slice2D grid (0,0) (4,4)
    -- print $ dimensions smaller
    -- showGrid smaller
    -- putStrLn $ showTree bvh 0 0
    pPrint bvh
    -- let rootBox = getBBox grid (0,0) (row-1, col-1)
    -- print rootBox
