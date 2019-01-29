{-# LANGUAGE OverloadedStrings #-}
module Graphics where
import Data.Aeson
import Debug.Trace
import Control.Arrow
import Data.Maybe (isJust, fromJust)
import Data.List
import Data.Text               (Text)
import qualified Data.HashMap.Lazy as HML        ( lookup )

-- import Data.Tree.Pretty (drawTree)
import Data.Vector ((!), (//), (!?), Vector)
import Text.Show.Pretty (pPrint)
import qualified Data.Vector as V
import Numeric

--------------------------------------------------------------------------------
-- Types

type Coord = (Int, Int)
type Grid  = Vector (Vector Entry)

data Entry = Ent Float (Maybe Coord) deriving Eq
instance ToJSON Entry where
    toJSON (Ent n e) = object [ "val" .= n, "neighbor" .= e ]
instance FromJSON Entry where
    parseJSON (Object v) = Ent <$> v .: "val" <*> v .: "neighbor"
instance Show Entry where
    -- show (Ent e _)
    --     | e < 0     = "-"
    --     | e > 0     = "+"
    --     | otherwise = "o"
     show (Ent e _) = showFFloat (Just 2)  e ""


data LevelSet = LevelSet {
    grid :: Grid,
    -- | the dimensions of an individual cell
    resolution :: (Float, Float),
    origin :: (Float, Float)
} deriving (Eq, Show)
instance ToJSON LevelSet where
    toJSON (LevelSet g res origin) = object [ "grid" .= g, "resolution" .= res, "origin" .= origin]
instance FromJSON LevelSet where
    parseJSON (Object v) = LevelSet <$> v .: "grid" <*> v .: "resolution" <*> v .: "origin"

--------------------------------------------------------------------------------
-- Constants

cellWidth :: Float
cellWidth = 5.0

toPixelCoord :: Coord -> (Float, Float)
toPixelCoord (x, y) = (fromIntegral x * cellWidth + halfCell, fromIntegral y * cellWidth + halfCell)
    where halfCell = 0.5 * cellWidth

-- | 'minBBoxSize' is the lowest resolution of BVH leaf nodes
minBBoxSize :: Int
minBBoxSize = round $ 30 / cellWidth

defaultCell :: Entry
defaultCell = Ent (-1.0) Nothing

closestPt :: Entry -> Maybe Coord
closestPt (Ent _ i) = i


--------------------------------------------------------------------------------
-- 2D Vector utils

-- function to initizlie a bunch of the same value in a 2d grid of user defined size
-- COMBAK: better naming
createGrid :: Int -> Int -> Entry -> Grid
createGrid x y = V.replicate y . V.replicate x

dimensions :: Grid -> (Int, Int)
dimensions g = (V.length g, V.length $ V.head g)

imap2D :: ((Int, Int) -> Entry -> Entry) -> Grid -> Grid
imap2D f = V.imap (\i row -> V.imap (\j e -> f (i,j) e) row)

ifoldl2D :: ( a -> (Int, Int) -> Entry -> a) -> a -> Grid -> a
ifoldl2D f = V.ifoldl (\ a i row -> V.ifoldl (\ a' j e -> f a' (i,j) e ) a row)

slice2D:: Grid -> Coord -> Coord -> Grid
slice2D g (startx, starty) (rows, cols) =
     V.map (V.slice starty cols) $ V.slice startx rows g

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) matrix (x, y) = matrix ! x ! y

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) matrix (x, y) = case matrix !? x of
    Nothing -> Nothing
    Just r  -> r !? y

update :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
update matrix (x, y) v = matrix // [(x, matrix ! x // [(y, v)])]

--------------------------------------------------------------------------------
-- Level set construction

showGrid :: Grid -> IO ()
showGrid = Prelude.mapM_ print

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

i2f :: Floating a => Coord -> (a, a)
i2f (x, y) = (fromIntegral x, fromIntegral y)

-- COMBAK: refactor
initCircGrid :: Float -> Grid
initCircGrid radius =
    let side = round $ 4 * (radius / cellWidth)
        g    = createGrid side side defaultCell
        (width, height) = dimensions g
        o    = (fromIntegral width * cellWidth / 2, fromIntegral height * cellWidth / 2)
        initCell coord _  =
            let a = distToCirc (toPixelCoord coord) o radius
            in Ent a $ if a == 0.0 then Just coord else Nothing
    in imap2D initCell g
    where distToCirc pt origin r =
            let d = distance pt origin - r in
            if abs d < cellWidth then 0.0 else d

neighbors :: Grid -> Coord -> [Entry]
neighbors grid (x,y) = map fromJust $ filter isJust $ map (grid !!?) ls
    where ls = [ (x-1, y-1), (x+1, y-1), (x-1, y+1), (x+1, y+1), (x  , y+1),
                  (x  , y-1), (x-1, y), (x+1, y) ]

-- COMBAK: refactor
updateCell :: Grid -> Coord -> Grid
updateCell grid curCoord =
    let o           = i2f curCoord
        Ent d _     = grid !!! curCoord
        (ns, _)     = partition (isJust . closestPt) $ neighbors grid curCoord
        toBoundry   = distance o . i2f . fromJust . closestPt
        cmp a b     = compare (toBoundry a) (toBoundry b)
        signOf d    = if d== 0 then 0 else d / abs d
        newPt (Ent _ (Just pt)) = update grid curCoord $
                                Ent ( signOf d * distance o (i2f pt)) (Just pt)
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
data BBox = BBox Coord Coord deriving (Show, Eq)
instance ToJSON BBox where
    toJSON (BBox a b) = object [ "topLeft" .= a, "bottomRight" .= b ]
instance FromJSON BBox where
    parseJSON (Object v) =
        BBox <$> v .: "topLeft" <*> v .: "bottomRight"

data Tree = Leaf BBox | Node Tree Tree BBox | EmptyNode
    deriving (Show, Eq)

instance ToJSON Tree where
    toJSON EmptyNode    = object [ "type" .= String "EmptyNode" ]
    toJSON (Leaf b)     = object [ "type" .= String "Leaf", "bbox" .= b ]
    toJSON (Node l r b) = object [
        "type" .= String "Node", "bbox" .= b, "left" .= toJSON l, "right" .= toJSON r ]
instance FromJSON Tree where
    parseJSON (Object v) = case HML.lookup "type" v of
        Just "Node" -> Node <$> v .: "left" <*> v .: "right" <*> v .: "bbox"
        Just "Leaf" -> case HML.lookup "bbox" v of
                            Just b  -> Leaf <$> parseJSON b
                            Nothing -> error "FromJSON: not supported node type"
        Just "EmptyNode" -> pure EmptyNode
        Nothing       -> error "FromJSON: not supported node type"


data Axis = X | Y deriving (Show, Eq)

-- tr = trace

maxInt, minInt :: Int
maxInt = maxBound
minInt = minBound

bVHHelper :: Grid -> BBox -> Axis -> Tree
bVHHelper g bbox@(BBox (x0, y0) (x1, y1)) X =
    if x1 - x0 < minBBoxSize then
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
    if y1 - y0 < minBBoxSize then
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

constructBVH :: LevelSet -> Tree
constructBVH levelSet =
    let g          = grid levelSet
        (row, col) = dimensions g
        rootBox    = getBBox g (0,0) (row-1, col-1)
    in case rootBox of
        Nothing   -> EmptyNode -- TODO: handle empty shapes (?)
        Just bbox -> bVHHelper g bbox X

getBBox :: Grid -> Coord -> Coord -> Maybe BBox
getBBox g orig@(startx, starty) (endx, endy) =
    let defaultPts = ((maxInt, maxInt) ,(minInt, minInt))
        coords     = [ (x,y) | x <- [startx..endx], y <- [starty..endy] ]
        pts        = foldl (minMaxBBox g) defaultPts coords
    in if pts == defaultPts then Nothing else Just $ uncurry BBox pts

minMaxBBox :: Grid -> (Coord, Coord) -> Coord -> (Coord, Coord)
minMaxBBox g c@((xMin, yMin), (xMax, yMax)) (i, j) =
    let (Ent val _) = g !!! (i, j)
    in if val <= 0 then ((min xMin i, min yMin j), (max xMax i, max yMax j))
       else c

--------------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
    let grid = initCircGrid 15
    let (row, col) = dimensions grid
    let bvh  = constructBVH LevelSet { grid = grid, resolution = (cellWidth, cellWidth), origin = (0,0) }
    showGrid grid
    putStrLn ""
    -- let smaller = slice2D grid (0,0) (4,4)
    -- print $ dimensions smaller
    -- showGrid smaller
    -- putStrLn $ showTree bvh 0 0
    pPrint bvh
    -- let rootBox = getBBox grid (0,0) (row-1, col-1)
    -- print rootBox
