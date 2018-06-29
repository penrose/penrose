module Test where
import Data.List
import Data.List.Split
enumerate = zip [0..]

-- function to initizlie a bunch of the same value in a 2d grid of user defined size
grid :: Int -> Int -> a -> [[a]]
grid x y = replicate y . replicate x

------------ DEFINITIONS -------------------
-- SOON: Will get info from Penrose not me plugging in automatic values

-- col definitions used in every function
col :: Int
col = 15

-- row definiton used in every function
row :: Int
row = 15


dist :: Float -> Float -> Float -> Int -> Float
dist radius xc yc n =
  sqrt( (( fromIntegral (n / col) - xc) ^2) + (( fromIntegral (n / col) -yc) ^2) )  - radius

initGrid :: Float -> Float -> Float -> [[Float]]
initGrid radius xc yc =
  chunksOf col (map (dist radius xc yc) [0..(row * col - 1)] )


neighbors:: [[Float]] -> (Int,Int) -> Float
neighbors grid (x,y) =
    let
    cells = [grid!!x!!y,
            grid!!(x+1)!!y,
            grid!!(x-1)!!y,
            grid!!x!!(y+1),
            grid!!x!!(y-1),
            grid!!(x+1)!!(y+1),
            grid!!(x-1)!!(y+1),
            grid!!(x+1)!!(y-1),
            grid!!(x-1)!!(y-1)
            ]
    list = zip3 cells (map abs cells) [0..9]
    in
    head (head (sortBy (compare `on` (minimum . snd) list )) )

fsm :: [[Float]] -> [[Float]]
fsm grid = foldl (neighbors) grid [ (x,y) | x <- [1..row-1], y <- [1..col-1]]
-- foldl (neighbors) grid [ (x,y) | x <- [row-1,row-2..1], y <- [1..col-1]]
-- foldl (neighbors) grid [ (x,y) | x <- [1..row-1], y <- [col-1, col-2..1]]
-- foldl (neighbors) grid [ (x,y) | x <- [row-1,row-2..1], y <- [col-1, col-2..1]]



--How to filter out by cell distance (i.e. how to add 1.414 when at a diagonal cell or 1 at a neighboring cell)

-- get nine neighbors
-- zip3 with absolute values, area, sort by snd smallest










--
-- -------------------------------------------
-- -- FindMin calls the 4 "sweep" functions, in accordance with the fast sweeping method pseudocode
-- -------------------------------------------
-- fsm :: [Float] -> [Float]
-- fsm list =  funcLRtoUL ((funcURtoLL (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)) [] 0 (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)))) [] 0 (((funcURtoLL (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)) [] 0 (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)))))
-- -- fsm list = (funcLRtoUL . funcURtoLL . func....) list
--
-- -------------------------------------------
-- -- OPTIMIZE: the fact that we need to account for this later rather than in the iterations is hacky, but I wanted to avoid a million if statements, and i'm doing that here and I hate it
--
-- -- The sweeping functions don't account for grid edges so I need to iterate through at end and find those values
-- -------------------------------------------
-- -- cleanEdges :: Int -> Int -> Int -> [Float] -> [Float] -> [Float]
-- -- cleanEdges n rows cols newList oldList =
-- --     let
-- --       x = (n) `div` (col)
-- --       y = (n) `mod` (col)
-- --       a = []
-- --     in
-- --       if ( x != 0 ) then
-- --         a ++ [oldList!!(n - row)]
-- --       if ( x != row - 1 ) then
-- --         a ++ [oldList!!(n + row)]
-- --       if ( y != 0 ) then
-- --         a ++ [oldList!!(n - 1)]
-- --       if ( y != col - 1 ) then
-- --         a ++ [oldList!!(n + 1)]
-- --       cleanEdges ( n+1 )  (newList ++ [minimum (map abs a)] ) tail(oldList)
--
-- ------------------------------------------
--
-- ------------------------------------------
-- diag :: Int -> Float
-- diag n =
--   if (n >= 4) then
--     1.0
--   else 0.0
-- ------------------------------------------
-- -- return minimum of list multiplied by its sign
-- ------------------------------------------
-- minimumSign :: [Float] -> Float -> Int -> Float
-- minimumSign (0:y:xs) n count = 0
-- minimumSign [a] n count = n
-- minimumSign (x:y:xs) 0.0 0 =
--   let
--    xSign = x/(abs(x)) -- gives -1 or +1
--    ySign = y/(abs(y))
--   in
--    if ( ( abs(x) ) < ( abs(y) ) ) then
--       minimumSign (x:xs) x 1
--    else
--      minimumSign (y:xs) y 1
--
-- minimumSign (x:y:xs) n count =
--   let
--    xNew = sqrt( (x^2) + ( ( 1 + ( 0.4 * diag(count) ) ) )^2 ) * (x/abs(x))  -- gives -1 or +1
--    yNew = sqrt( (x^2) + ( ( 1 + ( 0.4 * diag(count) ) ) )^2 ) * (y/abs(y))
--   in
--     if ( ( abs(xNew) )   < ( abs(yNew) ) ) then
--        minimumSign (x:xs) xNew (count+1)
--     else
--       minimumSign (y:xs) yNew (count+1)
--
--
-- -------------------------------------------
-- -- Sweep upper lef to lower right
-- -- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-- -------------------------------------------
-- funcULtoLR :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- -- TODO: funcULtoLR list = helper...
-- -- base case, empty list
-- funcULtoLR g list _ [] = list
-- -- find all surrounding cells of a cell and see which has the smallest distance
-- funcULtoLR g beginList n endList =
--     let
--       x = (n) `div` (col)
--       y = (n) `mod` (col)
--     in
--       if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
--         funcULtoLR g (beginList ++ [g!!(n)]) (n+1) (tail(endList))      -- if at a boundary, dont check cells outside
--       else
--         funcULtoLR g (beginList ++
--               [ minimumSign [ g!!(n),
--                               g!!(n+1),
--                               g!!(n-1),
--                               g!!(n+col),
--                               g!!(n-col),
--                               g!!(n+col-1), --diagonals
--                               g!!(n+col+1),
--                               g!!(n-col-1),
--                               g!!(n-col+1)
--                         ] 0.0 0
--               ] )   (n+1)  (tail(endList))
--
-- -------------------------------------------
-- -- Sweep upper lef to lower right
-- -- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-- -------------------------------------------
-- funcLLtoUR :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- -- base case, empty list
-- funcLLtoUR g list _ [] = list
-- -- find all surrounding cells of a cell and see which has the smallest distance
-- funcLLtoUR g beginList n endList =
--     let
--       x = (row-1) - (n) `div` (col)
--       y = (n) `mod` (col)
--     in
--       if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
--         funcLLtoUR g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
--       else
--         funcLLtoUR g (beginList ++
--         [ minimumSign [ g!!(n),
--                         g!!(n+1),
--                         g!!(n-1),
--                         g!!(n+col),
--                         g!!(n-col),
--                         g!!(n+col-1), --diagonals
--                         g!!(n+col+1),
--                         g!!(n-col-1),
--                         g!!(n-col+1)
--                   ] 0.0 0
--         ] )   (n+1)  (tail(endList))
-- -------------------------------------------
-- -- Sweep upper right to lower left
-- -- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-- -------------------------------------------
-- funcURtoLL :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- -- base case, empty list
-- funcURtoLL g list _ [] = list
-- -- find all surrounding cells of a cell and see which has the smallest distance
-- funcURtoLL g beginList n endList =
--     let
--       x = (n) `div` (col)
--       y = (col-1)  - (n) `mod` (col)
--     in
--       if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
--         funcURtoLL g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
--       else
--         funcURtoLL g (beginList ++
--         [ minimumSign [ g!!(n),
--                         g!!(n+1),
--                         g!!(n-1),
--                         g!!(n+col),
--                         g!!(n-col),
--                         g!!(n+col-1), --diagonals
--                         g!!(n+col+1),
--                         g!!(n-col-1),
--                         g!!(n-col+1)
--                   ] 0.0 0
--         ] )   (n+1)  (tail(endList))
--
-- -------------------------------------------
-- -- Sweep lower right to upper left
-- -- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-- -------------------------------------------
-- funcLRtoUL :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- -- base case, empty list
-- funcLRtoUL g list _ [] = list
-- -- find all surrounding cells of a cell and see which has the smallest distance
-- funcLRtoUL g beginList n endList =
--     let
--       x = (row-1) - (n) `div` (col)
--       y = (col-1) - (n) `mod` (col)
--     in
--       if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
--         funcLRtoUL g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
--       else
--         funcLRtoUL g (beginList ++
--         [ minimumSign [ g!!(n),
--                         g!!(n+1),
--                         g!!(n-1),
--                         g!!(n+col),
--                         g!!(n-col),
--                         g!!(n+col-1), --diagonals
--                         g!!(n+col+1),
--                         g!!(n-col-1),
--                         g!!(n-col+1)
--                   ] 0.0 0
--         ] )   (n+1)  (tail(endList))
--
-- -------------------------------------------
-- -- this is the outermost "boundary" function, the one with understandable parameters
-- -- recieve the beginnings of a Level Set from inner boudanry function: findFun
-- -- params: row number, column number, radius of circle, x center, y center
-- -------------------------------------------
-- findBoundaries :: Int -> Int -> Float -> Float -> Float -> [Float]
-- findBoundaries row col radius xc yc =
--   let x = (row * col - 1)
--   in
--     findFunc [] [0..x] radius xc yc -- this function calls another function which does the work but has more complicated params
--
-- -------------------------------------------
-- --OPTIMIZE: I'm having trouble with floating point numbers and its makign accuracy of measurement incorrect for distance funciton and where i measure distance between diagonal grids
-- -- need to get floats working for the level set distances
--
-- -- the inner "boundary" function (see findBoundaries) iterates over the array and creates a new array with the
-- -- dist^2 - radius^2 value for each cell.  If 0 == youre on the surface,
-- -- if negative th cell is inside the circle, if positive the cell is outside
-- -- the greater the magnitue, the farther the cell is from the surface
-- -- params: list, nCount list to iterate through, radius, x center, and y center
-- -------------------------------------------
-- findFunc :: [Float] -> [Int] -> Float -> Float -> Float -> [Float]
-- -- base case: done iterating through intitial list
-- findFunc list [] radius xc yc = list
-- -- for every element in the list find whether it is inside circle, add to final list, call again on tail
-- findFunc list nCount radius xc yc =
--     let
--     n = (head(nCount)) -- find which index we are on by getting head of list
--     x1 = (n `div` col)
--     y1 = (n `mod` col)
--     x = (fromIntegral x1)
--     y = (fromIntegral y1)
--     a = ( sqrt( ((x-xc) ^2) + ((y-yc) ^2) )  - radius )-- distance from center function
--     in
--       findFunc ( list++[a] ) (tail(nCount)) radius xc yc -- append distance from center, recurse through rest of list
--
-- -------------------------------------------
-- --This function creates a 2d Grid from the 1d list we have been using to represent the 2d grid (easier to iterate through funcitonally)
-- -- params: list and length of column
-- -------------------------------------------
-- createGrid :: [Float] -> Int -> [[Float]]
-- createGrid list cols = (chunksOf cols list)
--
--
--
-- --------------- TESTING ------------------------
--
-- -- main = do
-- --       putStrLn "----------------------------------------"
-- --       putStrLn "Testing Find Boundaries "
-- --       putStrLn "Array 8 by 8, center in middle, radius 3"
-- testBoundary = (findBoundaries 255 255 50 125 125)
-- grid1 = createGrid (fsm testBoundary) 255
-- -- putStrLn "RESULT:"
-- -- grid!!0
-- -- grid!!1
-- -- grid!!2
-- -- grid!!3
-- -- grid!!4
-- -- grid!!5
-- -- grid!!6
-- -- grid!!7
-- -- putStrLn "----------------------------------------"
--
-- radius = (fromIntegral row) / 3.0
-- testBoundary2 = (findBoundaries (row) (col) radius ((fromIntegral row) / 2.0) ((fromIntegral col) / 2.0))
-- testB2Grid = createGrid testBoundary2 col
-- testGrid = createGrid ( fsm testBoundary2) (col)
--
-- -- putStrLn
-- -- putStrLn "----------------------------------------"
-- -- putStrLn "Testing Minimum Sign: "
-- -- putStrLn "Array [  1.0,  2.0, 3.0"
-- -- putStrLn "        -0.01, 0.01, 3.0"
-- -- putStrLn "         1.0,  2.0,  3.0]"
-- a = minimumSign [ 1.0,  2.0, 3.0, -0.01, 0.01, 3.0, 1.0, 2.0,  3.0] 0.0 0
-- -- putStrLn "RESULT:"
-- -- a
-- -- putStrLn "----------------------------------------"
--



---------------- PSEUDOCODE --------------------

-- containsLevelSet [outc, inc] [] = 0
--   grid (r' outc * 2) (r' outc * 2) 9000
--
--
--
--   -- for x in width that they overlap
--   --  for y in height that they overlap
--   --      if ( b.grid[ x,y ] <= 0 )
--   --         if ( a.grid[ x,y ] > 0 )
--   --             add to outsidePts
--
--
--   -- for x in outsidePts
--   --       sum and return average
--
-- bvhHelper [outc, inc] [] =
--     --iterate through BVH tree
--     --at smallest leaf node where still intersecting
--     containsLevelSet [outc, inc] []
--
-- contains :: ConstrFn
-- contains [C' outc, C' inc] [] =
--     --if contains BBox a BBox b
--     if (strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]] > 0) then
--       strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
--     else
--       bvhHelper [outc, inc] []
