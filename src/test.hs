module Test where
import Control.Arrow
import Data.Maybe (isJust, fromJust)
import Data.List
import Data.Vector ((!), (//), (!?), Vector)
import qualified Data.Vector as V
import Numeric
--import Data.Tuple.Extra

type Coord = (Int, Int)
data Entry = Ent Float (Maybe Coord)

closestPt :: Entry -> Maybe Coord
closestPt (Ent _ i) = i

instance Show Entry where
    -- show (Ent e _)
    --     | e < 0     = "-"
    --     | e > 0     = "+"
    --     | otherwise = "o"
     show (Ent e _) = showFFloat (Just 2)  e ""
type Grid  = Vector (Vector Entry)

-- function to initizlie a bunch of the same value in a 2d grid of user defined size
gridWith :: Int -> Int -> Entry -> Grid
gridWith x y = V.replicate y . V.replicate x

------------ DEFINITIONS -------------------
-- SOON: Will get info from Penrose not me plugging in automatic values

-- col definitions used in every function
cellWidth :: Float
cellWidth = 1.0

xc, yc, r :: Float
xc = 0
yc = 0
r  = 15.0

defaultCell :: Entry
defaultCell = Ent (-1.0) Nothing

dist :: Floating a => (a, a) -> (a, a) -> a -- distance
dist (x1, y1) (x2, y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

dimensions :: Grid -> (Int, Int)
dimensions g = (V.length g, V.length $ V.head g)

queryCirc :: Coord -> Float -> (Float, Float) -> Float
queryCirc pt r origin = let d = dist pt' origin - r in
    if abs d < cellWidth then 0.0 else d
    where pt' = (fromIntegral *** fromIntegral) pt

imap2D :: ((Int, Int) -> Entry -> Entry) -> Grid -> Grid
imap2D f = V.imap (\i row -> V.imap (\j e -> f (i,j) e) row)

(!!!) :: Vector (Vector a) -> (Int, Int) -> a
(!!!) matrix (x, y) = matrix ! x ! y

(!!?) :: Vector (Vector a) -> (Int, Int) -> Maybe a
(!!?) matrix (x, y) = case matrix !? x of
    Nothing -> Nothing
    Just r  -> r !? y

update :: Vector (Vector a) -> (Int, Int) -> a -> Vector (Vector a)
update matrix (x, y) v = matrix // [(x, matrix ! x // [(y, v)])]

i2f :: Floating a => Coord -> (a, a)
i2f (x, y) = (fromIntegral x, fromIntegral y)

initGrid :: Float -> Grid
initGrid radius = imap2D helper g
    where
        g    = gridWith side side defaultCell
        side = round $ 4 * radius
        (width, height) = dimensions g
        o    = (fromIntegral width / 2, fromIntegral height / 2)
        helper coord _  = let a = queryCirc coord r o in
                Ent a $ if a == 0.0 then Just coord else Nothing
--
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

showGrid :: Grid -> IO ()
showGrid = Prelude.mapM_ print

main :: IO ()
main = do
    let image = initGrid r
    let (row, col) = dimensions image
    showGrid $ fsm image
    -- print image
