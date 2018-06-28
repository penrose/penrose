-- | The "Function" module contains a library of objective and constraint
-- functions, and helper functions needed to invoke them.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Functions where
import Shapes
import Utils
import Debug.Trace

import Data.List.Split

import qualified Data.Map.Strict as M

type ObjFnOn    a = [Obj' a] -> [TypeIn a] -> a
type ConstrFnOn a = [Obj' a] -> [TypeIn a] -> a
type ObjFn    = forall a. (Autofloat a) => [Obj' a] -> [TypeIn a] -> a
type ConstrFn = forall a. (Autofloat a) => [Obj' a] -> [TypeIn a] -> a

type Weight       a = a
type ObjFnInfo    a = (ObjFnOn    a, Weight a, [TypeIn a])
type ConstrFnInfo a = (ConstrFnOn a, Weight a, [TypeIn a])

type PairConstrV a = forall a . (Autofloat a) => [[a]] -> a -- takes pairs of "packed" objs

data FnInfo a = ObjFnInfo a | ConstrFnInfo a

-- | 'constrFuncDict' stores a mapping from the name of constraint functions to the actual implementation
constrFuncDict :: forall a. (Autofloat a) => M.Map String (ConstrFnOn a)
constrFuncDict = M.fromList flist
    where
        flist :: (Autofloat a) => [(String, ConstrFnOn a)]
        flist = [
                    ("at", at),
                    ("sameSizeAs", penalty `compose2` sameSize),
                    ("contains", penalty `compose2` contains),
                    ("overlapping", penalty `compose2` overlapping),
                    ("nonOverlapping",  penalty `compose2` nonOverlapping),
                    ("outsideOf", penalty `compose2` outsideOf),
                    ("smallerThan", penalty `compose2` smallerThan), -- TODO: should this be an objective?
                    ("nondegenerate", penalty `compose2` nondegenerate)
                 ]

-- | 'objFuncDict' stores a mapping from the name of objective functions to the actual implementation
objFuncDict :: forall a. (Autofloat a) => M.Map String (ObjFnOn a)
objFuncDict = M.fromList flist
    where flist = [
                    ("center", center),
                    ("centerLabel", centerLabel),
                    ("centerMap", centerMap),
                    ("centerLine", centerLine),
                    ("increasingX", increasingX),
                    ("increasingY", increasingY),
                    ("horizontal", horizontal),
                    ("upright", upright),
                    ("xInRange", xInRange),
                    ("yInRange", yInRange),
                    ("orthogonal", orthogonal),
                    ("toLeft", toLeft),
                    ("above", above),
                    ("between", between),
                    ("sameHeight", sameHeight),
                    ("sameX", sameX),
                    ("equal", equal),
                    ("ratioOf", ratioOf),
                    ("topRightOf", topRightOf),
                    ("sameY", sameY),
                    -- ("sameX", (*) 0.6 `compose2` sameX),
                    -- ("sameX", (*) 0.2 `compose2` sameX),
                    ("sameCenter", sameCenter),
                    ("repel", (*)  900000  `compose2` repel),
                    -- ("repel", (*)  1000000  `compose2` repel),
                    -- ("repel", (*)  10000  `compose2` repel),
                    -- ("repel", repel),
                    ("outside", outside),
                    ("nearEndVert", nearEndVert),
                    ("nearEndHoriz", nearEndHoriz),
                    ("nearHead", nearHead)
                  ]

--------------------------------------------------------------------------------
-- Objective functions
-- TODO write about expectations for the objective function writer
-- TODO deal with lists in a more principled way
-- maybe the typechecking should be done elsewhere...

-- TODO: implement all the location function using a generic version
-- distance (x1, y1) (x2, y2) dx dy = (x1 - x2)

xInRange :: ObjFn
xInRange l [TNum xmin, TNum xmax] = (minimum xs - xmin)^2 + (maximum xs - xmax)^2 + sum (map f xs)
    where xs = map getX l
          f x = (max 0 $ xmax - x)^2 + (max 0 $ x - xmin)^2

yInRange :: ObjFn
yInRange l [TNum ymin, TNum ymax] = (minimum ys - ymin)^2 + (maximum ys - ymax)^2 + sum (map f ys)
    where ys = map getY l
          f y = (max 0 $ ymax - y)^2 + (max 0 $ y - ymin)^2

orthogonal :: ObjFn
orthogonal [A' a1, A' a2] [] = (1 - dotL [startx' a1, starty' a1] [startx' a2, starty' a2])^2

horizontal :: ObjFn
horizontal [A' a] [] = (starty' a - endy' a)^2

upright :: ObjFn
upright [A' a] [] = (startx' a - endx' a)^2

topRightOf :: ObjFn
topRightOf [L' l, S' s] [] = dist (getX l, getY l) (getX s + 0.5 * side' s, getY s + 0.5 * side' s)

increasingX :: ObjFn
increasingX l [] = sum $ map f $ zip l' (drop 1 l')
    where l' = map getX l
          f (y0, y1) = (y1 - (y0 + y1) / 2)^2

increasingY :: ObjFn
increasingY l [] = sum $ map f $ zip l' (drop 1 l')
    where l' = map getY l
          f (x0, x1) = (x1 - (x0 + x1) / 2)^2

-- | 'between' attempts to place an object at the midpoint between the cetners
--    between two other objects
between :: ObjFn
between [mid, left, right] [] = (getX mid - getX left - offset)^2 + (getX right - getX mid - offset)^2
    where offset = 100

-- | 'center' puts the object at the center of the canvas
center :: ObjFn
center [o] [] = getX o ^ 2 + getY o ^ 2

-- | 'above' makes sure the first argument is on top of the second.
above :: ObjFn
above [top, bottom] [TNum offset] = (getY top - getY bottom - offset)^2
above [top, bottom] [] = (getY top - getY bottom - 100)^2

-- | 'toLeft' makes sure the first argument is to the left of the second.
toLeft :: ObjFn
toLeft [a, b] [] = (getX a - getX b + 400)^2

-- | 'sameHeight' forces two objects to stay at the same height (have the same Y value)
sameHeight :: ObjFn
sameHeight [a, b] [] = (getY a - getY b)^2

equal :: ObjFn
equal [] [TNum a, TNum b] = (a - b)^2

ratioOf :: ObjFn
ratioOf [] [TNum a, TNum b, TNum ratio] = (a - b * ratio)^2


-- | encourages two objects to have the same X value
sameX :: ObjFn
sameX [A' a, L' l] [] = -- TODO factor middle calculation out? seems like it would be used often
      let arrMidX = (startx' a + endx' a) / 2 in
      let labMidX = xl' l in
      (arrMidX - labMidX) ^ 2

sameX [a, b] [] = (getX a - getX b)^2

-- | encourages two objects to stay at the same height (have the same Y value)
sameY :: ObjFn
sameY [a, b] [] = (getY a - getY b)^2

-- | 'sameCenter' encourages two objects to center at the same point
sameCenter :: ObjFn
sameCenter [a, b] [] = (getY a - getY b)^2 + (getX a - getX b)^2

-- TODO: more reasonable name
-- | `centerMap` positions an arrow between two objects, with some spacing
centerMap :: ObjFn
centerMap [A' a, S' s, S' e] [] = _centerMap a [xs' s, ys' s] [xs' e, ys' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, S' s, C' e] [] = _centerMap a [xs' s, ys' s] [xc' e, yc' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + r' e]
centerMap [A' a, C' s, S' e] [] = _centerMap a [xc' s, yc' s] [xs' e, ys' e]
                [spacing + r' s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, C' s, C' e] [] = _centerMap a [xc' s, yc' s] [xc' e, yc' e]
                [ spacing * r' s, negate $ spacing * r' e]
centerMap [A' a, E' s, E' e] [] = _centerMap a [xe' s, ye' s] [xe' e, ye' e]
                [ spacing * rx' s, negate $ spacing * rx' e]
                -- FIXME: inaccurate, only works for horizontal cases
centerMap [A' a, P' s, P' e] [] = _centerMap a [xp' s, yp' s] [xp' e, yp' e]
                [ spacing * 2 * r2f ptRadius, negate $ spacing * 2 * r2f ptRadius]
centerMap [A' a, L' s, L' e] [] = _centerMap a [xl' s, yl' s] [xl' e, yl' e]
                [spacing * hl' s, negate $ spacing * hl' e]
centerMap [A' a, L' s, C' e] [] = _centerMap a [xl' s, yl' s] [xc' e, yc' e]
                [1.5 * wl' s, negate $ spacing * r' e]
centerMap [A' a, L' s, A' a1, C' e] [] = _centerMap a [xl' s, yl' s] [xc' e, yc' e]
                [1.5 * wl' s, negate $ spacing * r' e]
centerMap o [] = error ("CenterMap: unsupported arguments: " ++ show o)
spacing = 1.1 -- TODO: arbitrary
--
_centerMap :: forall a. (Floating a, Real a, Show a, Ord a) =>
                SolidArrow' a -> [a] -> [a] -> [a] -> a
_centerMap a s1@[x1, y1] s2@[x2, y2] [o1, o2] =
    let vec  = [x2 - x1, y2 - y1] -- direction the arrow should point to
        dir = normalize vec -- direction the arrow should point to
        [sx, sy, ex, ey] = if norm vec > o1 + abs o2
                then (s1 +. o1 *. dir) ++ (s2 +. o2 *. dir) else s1 ++ s2
        [fromx, fromy, tox, toy] = [startx' a, starty' a, endx' a, endy' a] in
    (fromx - sx)^2 + (fromy - sy)^2 + (tox - ex)^2 + (toy - ey)^2

centerLine :: ObjFn

centerLine [LN' l, P' p1, P' p2] [] =
           let p1_distsq = (startx_l' l - xp' p1)^2 + (starty_l' l - yp' p1)^2 in
           let p2_distsq = (endx_l' l - xp' p2)^2 + (endy_l' l - yp' p2)^2 in
           p1_distsq + p2_distsq
centerLine o [] = error ("center line: unsupported args: " ++ show o)

-- | 'repel' exert an repelling force between objects
repel :: ObjFn
repel [C' c, S' d] [] = 1 / distsq (xc' c, yc' c) (xs' d, ys' d) - r' c - side' d + epsd
repel [S' c, C' d] [] = 1 / distsq (xc' d, yc' d) (xs' c, ys' c) - r' d - side' c + epsd
repel [P' c, P' d] [] = if c == d then 0 else 1 / distsq (xp' c, yp' c) (xp' d, yp' d) - 2 * r2f ptRadius + epsd
repel [L' c, L' d] [] = if c == d then 0 else 1 / distsq (xl' c, yl' c) (xl' d, yl' d)
-- TODO: why are there references to labelName in Functions?
repel [L' c, C' d] [] = if labelName (namec' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xc' d, yc' d)
repel [C' c, L' d] [] = 1 / distsq (xc' c, yc' c) (xl' d, yl' d)
repel [L' c, S' d] [] = if labelName (names' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xs' d, ys' d)
repel [S' c, L' d] [] = 1 / distsq (xs' c, ys' c) (xl' d, yl' d)
repel [A' c, L' d] [] = repel' (startx' c, starty' c) (xl' d, yl' d) +
        repel' (endx' c, endy' c) (xl' d, yl' d)
repel [A' c, C' d] [] = repel' (startx' c, starty' c) (xc' d, yc' d) +
        repel' (endx' c, endy' c) (xc' d, yc' d)
repel [IM' c, IM' d] [] = 1 / (distsq (xim' c, yim' c) (xim' d, yim' d) + epsd) - sizeXim' c - sizeXim' d --TODO Lily check this math is correct
repel [a, b] [] = if a == b then 0 else 1 / (distsq (getX a, getY a) (getX b, getY b) )

-- helper for `repel`
repel' x y = 1 / distsq x y + epsd

-- TODO move this elsewhere? (also applies to polyline)
bezierBbox :: (Floating a, Ord a) => CubicBezier' a -> ((a, a), (a, a)) -- poly Point type?
bezierBbox cb = let path = pathcb' cb
                    (xs, ys) = (map fst path, map snd path)
                    lower_left = (minimum xs, minimum ys)
                    top_right = (maximum xs, maximum ys) in
                    (lower_left, top_right)

-- | 'centerLabel' makes labels stay at the centers of objects.
centerLabel :: ObjFn
-- for now, center label in bezier's bbox
-- TODO smarter bezier/polyline label function
-- TODO specify rotation on labels?
centerLabel [CB' bez, L' lab] [] = -- use the float input? just for testing
            let ((lx, ly), (rx, ry)) = bezierBbox bez
                (xmargin, ymargin) = (-10, 30)
                midbez = ((lx + rx) / 2 + xmargin, (ly + ry) / 2 + ymargin) in
            distsq midbez (getX lab, getY lab)

-- centerLabel [CB' a, L' l] [mag] = -- use the float input?
--                 let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
--                     (mx, my) = midpoint (sx, sy) (ex, ey)
--                     (lx, ly) = (xl' l, yl' l) in
                -- (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point
centerLabel [P' p, L' l] [] =
                let [px, py, lx, ly] = [xp' p, yp' p, xl' l, yl' l] in
                (px + 10 - lx)^2 + (py + 20 - ly)^2 -- Top right from the point

-- TODO: depends on orientation of arrow
centerLabel [A' a, L' l] [] =
                let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
                    (mx, my) = midpoint (sx, sy) (ex, ey)
                    (lx, ly) = (xl' l, yl' l) in
                (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point
centerLabel [a, b] [] = sameCenter [a, b] []

outside :: ObjFn
outside [L' o, C' i] [] = (dist (xl' o, yl' o) (xc' i, yc' i) - (1.5 * r' i) - wl' o)^2
outside [L' o, S' i] [] = (dist (xl' o, yl' o) (xs' i, ys' i) - 2 * (halfDiagonal . side') i)^2
-- TODO: generic version using bbox

nearEndVert :: ObjFn
nearEndVert [LN' line, L' lab] [] = -- expects a vertical line
            let (sx, sy, ex, ey) = {-trace ("inputs: " ++ show line ++ "\n" ++ show lab) $-}
                                   (startx_l' line, starty_l' line, endx_l' line, endy_l' line) in
            let bottompt = if sy < ey then (sx, sy) else (ex, ey) in
            let yoffset = -25 in
            let res = distsq (xl' lab, yl' lab) (fst bottompt, snd bottompt + yoffset) in
            trace ("nearEndVert energy for label " ++ namel' lab ++ " : " ++ show res) res

nearEndHoriz :: ObjFn
nearEndHoriz [LN' line, L' lab] [] = -- expects a horiz line
            let (sx, sy, ex, ey) = (startx_l' line, starty_l' line, endx_l' line, endx_l' line) in
            let leftpt = if sx < ex then (sx, sy) else (ex, ey) in
            let xoffset = -25 in
            distsq (xl' lab, yl' lab) (fst leftpt + xoffset, snd leftpt)

nearHead :: ObjFn

nearHead [A' arr, L' lab] [TNum xoff, TNum yoff] =
         let end = (endx' arr, endy' arr) in -- arrowhead
         let offset = (xoff, yoff) in
         distsq (xl' lab, yl' lab) (end `plus2` offset)
         where plus2 (a, b) (c, d) = (a + c, b + d)

defaultCWeight :: Floating a => a
defaultCWeight = 1

-- TODO: should points also have a weight of 1?
defaultPWeight :: Floating a => a
defaultPWeight = 1

--------------------------------------------------------------------------------
-- Constraint functions
-- are written WRT magnitude of violation
-- List: smallerThan, contains, outsideOf, overlapping, nonOverlapping, samesize, maxsize, minsize
-- TODO metaprogramming for boolean constraints

sameSize :: ConstrFn
sameSize [S' s1, S' s2] [] = (side' s1 - side' s2)**2
sameSize [E' s1, E' s2] [] = (rx' s1 - rx' s2)**2 + (ry' s1 - ry' s2)**2
sameSize [C' s1, C' s2] [] = (r' s1 - r' s2)**2

maxSize :: ConstrFn
limit = max (fromIntegral picWidth) (fromIntegral picHeight)

maxSize [C' c] _ = r' c -  limit / 6
maxSize [S' s] _ = side' s - limit  / 3
maxSize [AR' ar] _ = sizear' ar - limit  / 3
maxSize [R' r] _ = let max_side = max (sizeX' r) (sizeY' r) in
                   max_side - limit  / 3

maxSize [IM' im] [] = let max_side = max (sizeXim' im) (sizeYim' im) in
                   max_side - limit  / 3
maxSize [PA' pa] [] = let max_side = max (sizeXpa' pa) (sizeYpa' pa) in
                   max_side - limit  / 3
maxSize [E' e] [] = max (ry' e) (rx' e) - limit  / 3

at :: ConstrFn
at [o] [TNum x, TNum y] = (getX o - x)^2 + (getY o - y)^2

minSize :: ConstrFn
minSize [C' c] _ = 20 - r' c
minSize [S' s] _ = 20 - side' s
minSize [AR' ar] _ = 2.5 - sizear' ar
minSize [R' r] _ = let min_side = min (sizeX' r) (sizeY' r) in
                   20 - min_side

minSize [IM' im] [] = let min_side = min (sizeXim' im) (sizeYim' im) in
                   20 - min_side
minSize [PA' pa] [] = let min_side = min (sizeXpa' pa) (sizeYpa' pa) in
                   20 - min_side
minSize [E' e] [] = 20 - min (ry' e) (rx' e)

smallerThan  :: ConstrFn
smallerThan [C' inc, C' outc] [] =  (r' inc) - (r' outc) - 0.4 * r' outc -- TODO: taking this as a parameter?
smallerThan [S' inc, S' outc] [] = (side' inc) - (side' outc) - subsetSizeDiff
smallerThan [C' c, S' s] [] = 0.5 * side' s - r' c
smallerThan [S' s, C' c] [] = (halfDiagonal . side') s - r' c

ellipseRatio :: ConstrFn
ellipseRatio [E' e] [] = (rx' e / w - ry' e / l) ** 2
    where (w, l) = (9, 16)

----------------------------------------------------------------------------------------------------
-----------------------------      Beginning Graphics API       ------------------------------------
----------------------------------------------------------------------------------------------------
-- function to initizlie a bunch of the same value in a 2d grid of user defined size
grid :: Int -> Int -> a -> [[a]]
grid x y = replicate y . replicate x

------------ DEFINITIONS -------------------
-- SOON: Will get info from Penrose not me plugging in automatic values

-- col definitions used in every function
col :: Int
col = 10

-- row definiton used in every function
row :: Int
row = 10

-------------------------------------------
-- FindMin calls the 4 "sweep" functions, in accordance with the fast sweeping method pseudocode
-------------------------------------------
fsm :: [Float] -> [Float]
fsm list =  funcLRtoUL ((funcURtoLL (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)) [] 0 (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)))) [] 0 (((funcURtoLL (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)) [] 0 (funcLLtoUR (funcULtoLR list [] 0 list) [] 0 (funcULtoLR list [] 0 list)))))


-------------------------------------------
-- OPTIMIZE: the fact that we need to account for this later rather than in the iterations is hacky, but I wanted to avoid a million if statements, and i'm doing that here and I hate it

-- The sweeping functions don't account for grid edges so I need to iterate through at end and find those values
-------------------------------------------
-- cleanEdges :: Int -> Int -> Int -> [Int] -> [Int] -> [Int]
-- cleanEdges n rows cols newList oldList n =
--     let
--       x = (n) `div` (col)
--       y = (n) `mod` (col)
--       a = []
--     in
--       if ( x != 0 ) then
--         a ++ [oldList!!(n - row)]
--       if ( x != row - 1 ) then
--         a ++ [oldList!!(n + row)]
--       if ( y != 0 ) then
--         a ++ [oldList!!(n - 1)]
--       if ( y != col - 1 ) then
--         a ++ [oldList!!(n + 1)]
--       cleanEdges ( n+1 )  (newList ++ [minimum (map abs a)] ) tail(oldList)

------------------------------------------
-- helper function for minimumSign, Returns 1.0 or 0.0 depending on whether we are evaluating diagonla boxes or not
------------------------------------------
diag :: Int -> Float
diag n =
  if (n >= 4) then
    1.0
  else 0.0

------------------------------------------
-- return minimum distances surrounding a box (including sign preservation)
------------------------------------------
minimumSign :: [Float] -> Float -> Int -> Float
minimumSign (0:y:xs) n count = 0
minimumSign [a] n count = n
minimumSign (x:y:xs) 0.0 0 =
  let
   xSign = x/(abs(x)) -- gives -1 or +1
   ySign = y/(abs(y))
  in
   if ( ( abs(x) ) < ( abs(y) ) ) then
      minimumSign (x:xs) x 1
   else
     minimumSign (y:xs) y 1

minimumSign (x:y:xs) n count =
  let
   xNew = x + (x/abs(x)) * (1 + (0.4 * diag(count))) -- gives -1 or +1
   yNew = y + (y/abs(y)) * (1 + (0.4 * diag(count)))
  in
    if ( ( abs(xNew) )   < ( abs(yNew) ) ) then
       minimumSign (x:xs) xNew (count+1)
    else
      minimumSign (y:xs) yNew (count+1)

-------------------------------------------
-- Sweep upper lef to lower right
-- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-------------------------------------------
funcULtoLR :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- base case, empty list
funcULtoLR g list _ [] = list
-- find all surrounding cells of a cell and see which has the smallest distance
funcULtoLR g beginList n endList =
    let
      x = (n) `div` (col)
      y = (n) `mod` (col)
    in
      if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
        funcULtoLR g (beginList ++ [g!!(n)]) (n+1) (tail(endList))      -- if at a boundary, dont check cells outside
      else
        funcULtoLR g (beginList ++
              [ minimumSign [ g!!(n),
                              g!!(n+1),
                              g!!(n-1),
                              g!!(n+col),
                              g!!(n-col),
                              g!!(n+col-1), --diagonals
                              g!!(n+col+1),
                              g!!(n-col-1),
                              g!!(n-col+1)
                        ] 0.0 0
              ] )   (n+1)  (tail(endList))

-------------------------------------------
-- Sweep upper lef to lower right
-- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-------------------------------------------
funcLLtoUR :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- base case, empty list
funcLLtoUR g list _ [] = list
-- find all surrounding cells of a cell and see which has the smallest distance
funcLLtoUR g beginList n endList =
    let
      x = (row-1) - (n) `div` (col)
      y = (n) `mod` (col)
    in
      if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
        funcLLtoUR g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
      else
        funcLLtoUR g (beginList ++
        [ minimumSign [ g!!(n),
                        g!!(n+1),
                        g!!(n-1),
                        g!!(n+col),
                        g!!(n-col),
                        g!!(n+col-1), --diagonals
                        g!!(n+col+1),
                        g!!(n-col-1),
                        g!!(n-col+1)
                  ] 0.0 0
        ] )   (n+1)  (tail(endList))
-------------------------------------------
-- Sweep upper right to lower left
-- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-------------------------------------------
funcURtoLL :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- base case, empty list
funcURtoLL g list _ [] = list
-- find all surrounding cells of a cell and see which has the smallest distance
funcURtoLL g beginList n endList =
    let
      x = (n) `div` (col)
      y = (col-1)  - (n) `mod` (col)
    in
      if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
        funcURtoLL g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
      else
        funcURtoLL g (beginList ++
        [ minimumSign [ g!!(n),
                        g!!(n+1),
                        g!!(n-1),
                        g!!(n+col),
                        g!!(n-col),
                        g!!(n+col-1), --diagonals
                        g!!(n+col+1),
                        g!!(n-col-1),
                        g!!(n-col+1)
                  ] 0.0 0
        ] )   (n+1)  (tail(endList))

-------------------------------------------
-- Sweep lower right to upper left
-- params: original list, list [0..n-1], nth element we are looking at, list [n+1..end]
-------------------------------------------
funcLRtoUL :: [Float] -> [Float] -> Int -> [Float] -> [Float]
-- base case, empty list
funcLRtoUL g list _ [] = list
-- find all surrounding cells of a cell and see which has the smallest distance
funcLRtoUL g beginList n endList =
    let
      x = (row-1) - (n) `div` (col)
      y = (col-1) - (n) `mod` (col)
    in
      if(y == 0 || y == (col-1) || x == 0 || x == (row-1)) then --avoid upper and lower boundaries
        funcLRtoUL g (beginList ++ [g!!(n)]) (n+1) (tail(endList))
      else
        funcLRtoUL g (beginList ++
        [ minimumSign [ g!!(n),
                        g!!(n+1),
                        g!!(n-1),
                        g!!(n+col),
                        g!!(n-col),
                        g!!(n+col-1), --diagonals
                        g!!(n+col+1),
                        g!!(n-col-1),
                        g!!(n-col+1)
                  ] 0.0 0
        ] )   (n+1)  (tail(endList))

-------------------------------------------
-- this is the outermost "boundary" function, the one with understandable parameters
-- recieve the beginnings of a Level Set from inner boudanry function: findFun
-- params: row number, column number, radius of circle, x center, y center
-------------------------------------------
findBoundaries :: Int -> Int -> Float -> Float -> Float -> [Float]
findBoundaries row col radius xc yc =
  let x = (row * col - 1)
  in
    findFunc [] [0..x] radius xc yc -- this function calls another function which does the work but has more complicated params

-------------------------------------------
--OPTIMIZE: I'm having trouble with floating point numbers and its makign accuracy of measurement incorrect for distance funciton and where i measure distance between diagonal grids
-- need to get floats working for the level set distances

-- the inner "boundary" function (see findBoundaries) iterates over the array and creates a new array with the
-- dist^2 - radius^2 value for each cell.  If 0 == youre on the surface,
-- if negative th cell is inside the circle, if positive the cell is outside
-- the greater the magnitue, the farther the cell is from the surface
-- params: list, nCount list to iterate through, radius, x center, and y center
-------------------------------------------
findFunc :: [Float] -> [Int] -> Float -> Float -> Float -> [Float]
-- base case: done iterating through intitial list
findFunc list [] radius xc yc = list
-- for every element in the list find whether it is inside circle, add to final list, call again on tail
findFunc list nCount radius xc yc =
    let
    n = (head(nCount)) -- find which index we are on by getting head of list
    x1 = (n `div` col)
    y1 = (n `mod` col)
    x = (fromIntegral x1)
    y = (fromIntegral y1)
    a = ( ( ((x-xc) ^2) + ((y-yc) ^2) )  - radius^2 )-- distance from center function
    in
      findFunc ( list++[a] ) (tail(nCount)) radius xc yc -- append distance from center, recurse through rest of list

-------------------------------------------
--This function creates a 2d Grid from the 1d list we have been using to represent the 2d grid (easier to iterate through funcitonally)
-- params: list and length of column
-------------------------------------------
createGrid :: [Float] -> Int -> [[Float]]
createGrid list cols = (chunksOf cols list)


contains :: ConstrFn
-- contains [C' outc, C' inc] [] =
--     --if contains BBox a BBox b
--     if (strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]] > 0) then
--       strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]


----------------------------------------------------------------------------------------------------
-----------------------------        End Graphics API           ------------------------------------
----------------------------------------------------------------------------------------------------

    -- let res =  dist (xc' inc, yc' inc) (xc' outc, yc' outc) - (r' outc - r' inc) in
    -- if res > 0 then res else 0
contains [C' outc, C' inc] [TNum padding] = strictSubset [[xc' inc, yc' inc, r' inc + padding], [xc' outc, yc' outc, r' outc]]

contains [S' outc, S' inc] [] = strictSubset
    [[xs' inc, ys' inc, 0.5 * side' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [S' outc, C' inc] [] = strictSubset [[xc' inc, yc' inc, r' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [S' outc, C' inc] [TNum padding] = strictSubset [[xc' inc, yc' inc, r' inc + padding], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [C' outc, S' inc] [] = strictSubset
    [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
contains [C' set, P' pt] [] =
        dist (xp' pt, yp' pt) (xc' set, yc' set) - 0.5 * r' set
contains [S' set, P' pt] [] =
    dist (xp' pt, yp' pt) (xs' set, ys' set) - 0.4 * side' set
-- TODO: only approx
contains [E' set, P' pt] [] =
    dist (xp' pt, yp' pt) (xe' set, ye' set) - max (rx' set) (ry' set) * 0.9
contains [C' set, L' label] _ =
    let res = dist (xl' label, yl' label) (xc' set, yc' set) - r' set + max (wl' label) (hl' label) in
    if res < 0 then 0 else res
contains [S' s, L' l] [] =
    dist (xl' l, yl' l) (xs' s, ys' s) - side' s / 2 + wl' l
-- FIXME: doesn't work
contains [E' set, L' label] [] =
    dist (xl' label, yl' label) (xe' set, ye' set) -  max (rx' set) (ry' set) + wl' label
contains [L' lab1, L' lab2] [] = 0 -- TODO: hack for venn_subset.sty for talk
contains [S' sq, A' ar] [] = let
                             lx = (xs' sq) - (side' sq)/2
                             rx = (xs' sq) + (side' sq)/2
                             ly = (ys' sq) - (side' sq)/2
                             ry = (ys' sq) + (side' sq)/2
                             ret = (isInRange (startx' ar) lx rx) + (isInRange (endx' ar) lx rx) + (isInRange (starty' ar) ly ry) + (isInRange (endy' ar) ly ry)
                            in
                             ret
contains objs consts = error ("subset: maybe not called with 2 args?\n" ++ show objs ++ "\n" ++ show consts)

isInRange a l r = if (a < l) then (a-l)^2 else if (a > r) then (a-r)^2 else 0

outsideOf :: ConstrFn
outsideOf [C' inc, C' outc] [] =
    noSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
outsideOf [S' inc, S' outc] [] =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc],
        [xs' outc, ys' outc, (halfDiagonal . side') outc]]
outsideOf [C' inc, S' outs] [] =
    noSubset [[xc' inc, yc' inc, r' inc], [xs' outs, ys' outs, (halfDiagonal . side') outs]]
outsideOf [S' inc, C' outc] [] =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
outsideOf [P' pt, C' set] [] =
    -dist (xp' pt, yp' pt) (xc' set, yc' set) + r' set
outsideOf [P' pt, S' set] [] =
    -dist (xp' pt, yp' pt) (xs' set, ys' set) + (halfDiagonal . side') set
outsideOf [L' lout, C' inset] [] =
    let labelR = max (wl' lout) (hl' lout)
        res = - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset + labelR in
    if namel' lout == (labelName $ namec' inset) then 0 else res
    -- if res <= 0 then 1 / res else res
    -- - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset
outsideOf [L' lout, S' inset] [] =
    - dist (xl' lout, yl' lout) (xs' inset, ys' inset) + (halfDiagonal . side') inset
outsideOf [L' lout, E' inset] [] =
    - dist (xl' lout, yl' lout) (xe' inset, ye' inset) + spacing * max (rx' inset) (ry' inset)
outsideOf _ _ = error "noSubset not called with 2 args"

overlapping :: ConstrFn
overlapping [C' xset, C' yset] [] =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
overlapping [S' xset, C' yset] [] =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xc' yset, yc' yset, r' yset]]
overlapping [C' xset, S' yset] [] =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping [S' xset, S' yset] [] =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping _ _ = error "intersect not called with 2 args"

nonOverlapping :: ConstrFn
nonOverlapping [C' xset, C' yset] [] =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [S' xset, C' yset] [] =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [C' xset, S' yset] [] =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [S' xset, S' yset] [] =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset],
        [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [A' arr, L' label] [] =
    let (sx, sy, ex, ey, t) = (startx' arr, starty' arr, endx' arr, endy' arr, thickness' arr)
        (x1, y1, x2, y2) = (sx, sy - t, ex, ey + t)
        dx = maximum [x1 - xl' label, 0, xl' label - x2]
        dy = maximum [y1 - yl' label, 0, yl' label - y2] in
        tr "labelvsArr: " $ -sqrt(dx**2 + dy**2) - wl' label
nonOverlapping  _ _ = error "no intersect not called with 2 args"

nondegenerate :: ConstrFn
nondegenerate [C' c] [] = -(r' c)
nondegenerate _ _ = error "nondegenerate not yet defined for this kind of object"


-- noConstraint :: PairConstrV a
-- noConstraint _ _ = 0

-- To convert your inequality constraint into a violation to be penalized:
-- it needs to be in the form "c < 0" and c is the violation penalized if > 0
-- so e.g. if you want "x < -100" then you would convert it to "x + 100 < 0" with c = x + 100
-- if you want "f x > -100" then you would convert it to "-(f x + 100) < 0" with c = -(f x + 100)"

-- all sets must pairwise-strict-intersect
-- plus an offset so they overlap by a visible amount (perhaps this should be an optimization parameter?)
looseIntersect :: PairConstrV a
looseIntersect [[x1, y1, s1], [x2, y2, s2]] = let offset = 10 in
        -- if s1 + s2 < offset then error "radii too small"  --TODO: make it const
        -- else
            dist (x1, y1) (x2, y2) - (s1 + s2 - offset)

-- the energy actually increases so it always settles around the offset
-- that's because i am centering all of them--test w/objective off
-- TODO flatten energy afterward, or get it to be *far* from the other set
-- offset so the sets differ by a visible amount
noSubset :: PairConstrV a
noSubset [[x1, y1, s1], [x2, y2, s2]] = let offset = 10 in -- max/min dealing with s1 > s2 or s2 < s1
         -(dist (x1, y1) (x2, y2)) + max s2 s1 - min s2 s1 + offset

-- the first (circular) set is the subset of the second (circular) set, and thus smaller than the second.
-- The distance between the centers of the sets must be less than the difference between
-- the radius of the outer set and the radius of the inner set.
-- TODO: test for equal sets? (function is minimized if sets have same radii and location)
strictSubset :: PairConstrV a
strictSubset [[x1, y1, s1], [x2, y2, s2]] = dist (x1, y1) (x2, y2) - (s2 - s1)

-- exterior point method constraint: no intersection (meaning also no subset)
noIntersectExt :: PairConstrV a
noIntersectExt [[x1, y1, s1], [x2, y2, s2]] = -(dist (x1, y1) (x2, y2)) + s1 + s2 + offset where offset = 10

pointInExt :: PairConstrV a
pointInExt [[x1, y1], [x2, y2, r]] = dist (x1, y1) (x2, y2) - 0.5 * r

pointNotInExt :: PairConstrV a
pointNotInExt [[x1, y1], [x2, y2, r]] = - dist (x1, y1) (x2, y2) + r

-- exterior point method: penalty function
penalty :: (Ord a, Floating a, Show a) => a -> a
penalty x = tr "penalty" $ (max x 0) ^ q -- weights should get progressively larger in cr_dist
            where q = 2 -- also, may need to sample OUTSIDE feasible set
            -- where q = 3
