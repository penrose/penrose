-- | The "function" module contains a library of objective and constraint
-- functions, and helper functions needed to invoke them.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Functions where
import Shapes
import Utils
import qualified Data.Map.Strict as M

type ObjFnOn a =  [Obj' a] -> [a] -> a
type ConstrFnOn a =  [Obj' a] -> [a] -> a
type ObjFn = forall a. (Floating a, Real a, Show a, Ord a) => [Obj' a] -> [a]-> a
type ConstrFn = forall a. (Floating a, Real a, Show a, Ord a) => [Obj' a] -> [a]-> a
type Weight a = a
type PairConstrV a = forall a . (Floating a, Ord a, Show a) => [[a]] -> a -- takes pairs of "packed" objs

-- | 'constrFuncDict' stores a mapping from the name of constraint functions to the actual implementation
constrFuncDict :: forall a. (Floating a, Real a, Show a, Ord a) =>
    M.Map String (ConstrFnOn a)
constrFuncDict = M.fromList flist
    where
        flist :: (Floating a, Real a, Show a, Ord a) => [(String, ConstrFnOn a)]
        flist = [
                    ("at", at),
                    ("sameSizeAs", penalty `compose2` sameSize),
                    ("contains", penalty `compose2` contains),
                    ("overlapping", penalty `compose2` overlapping),
                    ("nonOverlapping",  penalty `compose2` nonOverlapping),
                    ("outsideOf", penalty `compose2` outsideOf),
                    ("smallerThan", penalty `compose2` smallerThan) -- TODO: should this be an objective?
                 ]

-- | 'objFuncDict' stores a mapping from the name of objective functions to the actual implementation
objFuncDict :: forall a. (Floating a, Real a, Show a, Ord a) => M.Map String (ObjFnOn a)
objFuncDict = M.fromList flist
    where flist = [
                    ("increasingX", increasingX),
                    ("increasingY", increasingY),
                    ("horizontal", horizontal),
                    ("upright", upright),
                    ("xInRange", xInRange),
                    ("yInRange", yInRange),
                    ("onthogonal", onthogonal),
                    ("center", center),
                    ("centerLabel", centerLabel),
                    ("toLeft", toLeft),
                    ("above", above),
                    ("between", between),
                    ("sameHeight", sameHeight),
                    ("sameX", sameX),
                    -- ("sameX", (*) 0.6 `compose2` sameX),
                    -- ("sameX", (*) 0.2 `compose2` sameX),
                    ("sameCenter", sameCenter),
                    ("repel", (*)  900000  `compose2` repel),
                    -- ("repel", (*)  1000000  `compose2` repel),
                    -- ("repel", (*)  10000  `compose2` repel),
                    -- ("repel", repel),
                    ("outside", outside)
                  ]

-- illegal polymorphic or qualified type--can't return a forall?
-- type ObjFnNamed a = forall a. (Floating a, Real a, Show a, Ord a) => M.Map Name (Obj' a) -> a

-- data Param a = O (Obj' a) | I Int | F Float
-- data ObjectiveFunction = ObjectiveFunction {
--     objName    :: String,
--     objWeight  :: forall a. (Floating a, Real a, Show a, Ord a) => Weight a,
--     objParam   :: forall a. (Floating a, Real a, Show a, Ord a) => [Param a],
--     objFunc    :: forall a. (Floating a, Real a, Show a, Ord a) => ObjFnOn a
-- }

--------------------------------------------------------------------------------
-- Objective functions
-- TODO write about expectations for the objective function writer
-- TODO deal with lists in a more principled way
-- maybe the typechecking should be done elsewhere...

-- TODO: implement all the location function using a generic version
-- distance (x1, y1) (x2, y2) dx dy = (x1 - x2)


xInRange :: ObjFn
xInRange l [xmin, xmax] = (minimum xs - xmin)^2 + (maximum xs - xmax)^2 + sum ( map f xs)
    where xs = map getX l
          f x = (max 0 $ xmax - x)^2 + (max 0 $ x - xmin)^2

yInRange :: ObjFn
yInRange l [ymin, ymax] = (minimum ys - ymin)^2 + (maximum ys - ymax)^2 + sum (map f ys)
    where ys = map getY l
          f y = (max 0 $ ymax - y)^2 + (max 0 $ y - ymin)^2

onthogonal :: ObjFn
onthogonal [A' a1, A' a2] _ = (crossL [startx' a1, starty' a1] [startx' a2, starty' a2]) ^2

horizontal :: ObjFn
horizontal [A' a] _ = (starty' a - endy' a)^2

upright :: ObjFn
upright [A' a] _ = (startx' a - endx' a)^2

increasingX :: ObjFn
increasingX l _ = sum $ map f $ zip l' (drop 1 l')
    where l' = map getX l
          f (y0, y1) = (y1 - (y0 + y1) / 2)^2

increasingY :: ObjFn
increasingY l _ = sum $ map f $ zip l' (drop 1 l')
    where l' = map getY l
          f (x0, x1) = (x1 - (x0 + x1) / 2)^2

-- | 'between' attempts to place an object at the midpoint between the cetners
--    between two other objects
between :: ObjFn
between [mid, left, right] _ = (getX mid - getX left - offset)^2 + (getX right - getX mid - offset)^2
    where offset = 100

-- | 'center' puts the object at the center of the canvas
center :: ObjFn
center [o] _ = getX o ^ 2 + getY o ^ 2

-- | 'above' makes sure the first argument is on top of the second.
above :: ObjFn
above [top, bottom] [offset] = (getY top - getY bottom - offset)^2
above [top, bottom] _ = (getY top - getY bottom - 100)^2

-- | 'toLeft' makes sure the first argument is to the left of the second.
toLeft :: ObjFn
toLeft [a, b] _ = (getX a - getX b + 400)^2

-- | 'sameHeight' forces two object to stay at the same height(have the same Y value)
sameHeight :: ObjFn
sameHeight [a, b] _ = (getY a - getY b)^2

-- | 'sameHeight' forces two object to have the same X value
sameX :: ObjFn
sameX [a, b] _ = (getX a - getX b)^2

-- | 'sameCenter' forces two object to center at the same point
sameCenter :: ObjFn
sameCenter [a, b] _ = (getY a - getY b)^2 + (getX a - getX b)^2

-- TODO: more reasonable name
-- | `centerMap` positions an arrow between to objects, with some spacing
centerMap :: ObjFn
centerMap [A' a, S' s, S' e] _ = _centerMap a [xs' s, ys' s] [xs' e, ys' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, S' s, C' e] _ = _centerMap a [xs' s, ys' s] [xc' e, yc' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + r' e]
centerMap [A' a, C' s, S' e] _ = _centerMap a [xc' s, yc' s] [xs' e, ys' e]
                [spacing + r' s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, C' s, C' e] _ = _centerMap a [xc' s, yc' s] [xc' e, yc' e]
                [ spacing * r' s, negate $ spacing * r' e]
centerMap [A' a, E' s, E' e] _ = _centerMap a [xe' s, ye' s] [xe' e, ye' e]
                [ spacing * rx' s, negate $ spacing * rx' e]
                -- FIXME: inaccurate, only works for horizontal cases
centerMap [A' a, P' s, P' e] _ = _centerMap a [xp' s, yp' s] [xp' e, yp' e]
                [ spacing * 2 * r2f ptRadius, negate $ spacing * 2 * r2f ptRadius]
centerMap [A' a, L' s, L' e] _ = _centerMap a [xl' s, yl' s] [xl' e, yl' e]
                [spacing * hl' s, negate $ spacing * hl' e]
centerMap [A' a, L' s, C' e] _ = _centerMap a [xl' s, yl' s] [xc' e, yc' e]
                [1.5 * wl' s, negate $ spacing * r' e]
centerMap o _ = error ("CenterMap: unsupported arguments: " ++ show o)
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

-- | 'repel' exert an repelling force between objects
repel :: ObjFn
repel [C' c, S' d] _ = 1 / distsq (xc' c, yc' c) (xs' d, ys' d) - r' c - side' d + epsd
repel [S' c, C' d] _ = 1 / distsq (xc' d, yc' d) (xs' c, ys' c) - r' d - side' c + epsd
repel [C' c, C' d] _ = 1 / distsq (xc' c, yc' c) (xc' d, yc' d) - r' c - r' d + epsd
repel [P' c, P' d] _ = if c == d then 0 else 1 / distsq (xp' c, yp' c) (xp' d, yp' d) - 2 * r2f ptRadius + epsd
repel [L' c, L' d] _ = if c == d then 0 else 1 / distsq (xl' c, yl' c) (xl' d, yl' d)
repel [L' c, C' d] _ = if labelName (namec' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xc' d, yc' d)
repel [C' c, L' d] _ = 1 / distsq (xc' c, yc' c) (xl' d, yl' d)
repel [L' c, S' d] _ = if labelName (names' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xs' d, ys' d)
repel [S' c, L' d] _ = 1 / distsq (xs' c, ys' c) (xl' d, yl' d)
repel [A' c, L' d] _ = repel' (startx' c, starty' c) (xl' d, yl' d) +
        repel' (endx' c, endy' c) (xl' d, yl' d)
repel [A' c, C' d] _ = repel' (startx' c, starty' c) (xc' d, yc' d) +
        repel' (endx' c, endy' c) (xc' d, yc' d)
repel [a, b] _ = 1 / distsq (getX a, getY a) (getX b, getY b) + epsd
-- helper for `repel`
repel' x y = 1 / distsq x y + epsd

-- | 'centerLabel' make labels to stay at the centers of objects.
centerLabel :: ObjFn
centerLabel [P' p, L' l] _ =
                let [px, py, lx, ly] = [xp' p, yp' p, xl' l, yl' l] in
                (px + 10 - lx)^2 + (py + 20 - ly)^2 -- Top right from the point
centerLabel [A' a, L' l] _ =
                let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
                    (mx, my) = midpoint (sx, sy) (ex, ey)
                    (lx, ly) = (xl' l, yl' l) in
                (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point
centerLabel [a, b] _ = sameCenter [a, b] []

outside :: ObjFn
outside [L' o, C' i] _ = (dist (xl' o, yl' o) (xc' i, yc' i) - (1.5 * r' i) - wl' o)^2
outside [L' o, S' i] _ = (dist (xl' o, yl' o) (xs' i, ys' i) - 2 * (halfDiagonal . side') i)^2
-- TODO: generic version using bbox

------- Ambient objective functions

-- -- no names specified; can apply to any combination of objects in M.Map
-- type AmbientObjFn a = forall a. (Floating a, Real a, Show a, Ord a) => M.Map Name (Obj' a) -> a
--
-- -- if there are no circles, doesn't do anything
-- -- TODO fix in case there's only 1 circle?
-- circParams :: (Floating a, Real a, Show a, Ord a) => M.Map Name (Obj' a) -> ([a], [a])
-- circParams m = unpackSplit $ filter isCirc $ M.elems m
--            where isCirc (C' _) = True
--                  isCirc _ = False
--
-- -- reuse existing objective function
-- circlesCenterAndRepel :: AmbientObjFn a
-- circlesCenterAndRepel objMap = let (fix, vary) = circParams objMap in
--                                centerAndRepel_dist fix vary
--
-- circlesCenter :: AmbientObjFn a
-- circlesCenter objMap = let (fix, vary) = circParams objMap in
--                        centerObjs fix vary

-- pairwiseRepel :: [Obj] -> Float
-- pairwiseRepel objs = sumMap pairRepel $ allPairs objs

-- pairRepel :: Obj -> Obj -> Float
-- pairRepel c d = 1 / (distsq c d)
--           where distsq c d = (getX c - getX d)^2 + (getY c - getY c)^2

-- -- returns a list of ambient constraint fns--4 for each object
-- -- i guess i don’t NEED to weight each individually. just sum them and weight the whole thing
-- allInBbox :: [Obj] -> Float
-- allInBbox objs = sum $ concatMap inBbox objs
--           where inBbox o = [boxleft o, boxright o, boxup o, boxdown o]
--                 boxleft o = getX o - leftline -- magnitude of violation


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
sameSize [S' s1, S' s2] _ = (side' s1 - side' s2)**2
sameSize [E' s1, E' s2] _ = (rx' s1 - rx' s2)**2 + (ry' s1 - ry' s2)**2
sameSize [C' s1, C' s2] _ = (r' s1 - r' s2)**2

maxSize :: ConstrFn
limit = max (fromIntegral picWidth) (fromIntegral picHeight)
maxSize [C' c] _ = r' c -  limit / 6
maxSize [S' s] _ = side' s - limit  / 3
maxSize [E' e] _ = max (ry' e) (rx' e) - limit  / 3

at :: ConstrFn
at [o] [x, y] = (getX o - x)^2 + (getY o - y)^2

minSize :: ConstrFn
minSize [C' c] _ = 20 - r' c
minSize [S' s] _ = 20 - side' s
minSize [E' e] _ = 20 - min (ry' e) (rx' e)

smallerThan  :: ConstrFn
smallerThan [C' inc, C' outc] _ =  (r' inc) - (r' outc) - 0.4 * r' outc -- TODO: taking this as a parameter?
smallerThan [S' inc, S' outc] _ = (side' inc) - (side' outc) - subsetSizeDiff
smallerThan [C' c, S' s] _ = 0.5 * side' s - r' c
smallerThan [S' s, C' c] _ = (halfDiagonal . side') s - r' c

ellipseRatio :: ConstrFn
ellipseRatio [E' e] _ = (rx' e / w - ry' e / l) ** 2
    where (w, l) = (9, 16)

contains :: ConstrFn
contains [C' outc, C' inc] _ =
    -- tr (namec' outc ++  " contains " ++ namec' inc ++ " val: ") $
    strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
    -- let res =  dist (xc' inc, yc' inc) (xc' outc, yc' outc) - (r' outc - r' inc) in
    -- if res > 0 then res else 0
contains [S' outc, S' inc] _ = strictSubset
    [[xs' inc, ys' inc, 0.5 * side' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [S' outc, C' inc] _ = strictSubset
    [[xc' inc, yc' inc, r' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [C' outc, S' inc] _ = strictSubset
    [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
contains [C' set, P' pt] _ =
        dist (xp' pt, yp' pt) (xc' set, yc' set) - 0.5 * r' set
contains [S' set, P' pt] _ =
    dist (xp' pt, yp' pt) (xs' set, ys' set) - 0.4 * side' set
-- TODO: only approx
contains [E' set, P' pt] _ =
    dist (xp' pt, yp' pt) (xe' set, ye' set) - max (rx' set) (ry' set) * 0.9
contains [C' set, L' label] _ =
    let res = dist (xl' label, yl' label) (xc' set, yc' set) - 0.5 * r' set in
    if res < 0 then 0 else res
contains [S' set, L' label] _ =
    dist (xl' label, yl' label) (xs' set, ys' set) - side' set / 2 + wl' label
-- FIXME: doesn't work
contains [E' set, L' label] _ =
    dist (xl' label, yl' label) (xe' set, ye' set) -  max (rx' set) (ry' set) + wl' label
contains _  _ = error "subset not called with 2 args"

outsideOf :: ConstrFn
outsideOf [C' inc, C' outc] _ =
    noSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
outsideOf [S' inc, S' outc] _ =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc],
        [xs' outc, ys' outc, (halfDiagonal . side') outc]]
outsideOf [C' inc, S' outs] _ =
    noSubset [[xc' inc, yc' inc, r' inc], [xs' outs, ys' outs, (halfDiagonal . side') outs]]
outsideOf [S' inc, C' outc] _ =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
outsideOf [P' pt, C' set] _ =
    -dist (xp' pt, yp' pt) (xc' set, yc' set) + r' set
outsideOf [P' pt, S' set] _ =
    -dist (xp' pt, yp' pt) (xs' set, ys' set) + (halfDiagonal . side') set
outsideOf [L' lout, C' inset] _ =
    let labelR = max (wl' lout) (hl' lout)
        res = - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset + labelR in
    if namel' lout == (labelName $ namec' inset) then 0 else res
    -- if res <= 0 then 1 / res else res
    -- - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset
outsideOf [L' lout, S' inset] _ =
    - dist (xl' lout, yl' lout) (xs' inset, ys' inset) + (halfDiagonal . side') inset
outsideOf [L' lout, E' inset] _ =
    - dist (xl' lout, yl' lout) (xe' inset, ye' inset) + spacing * max (rx' inset) (ry' inset)
outsideOf _ _ = error "noSubset not called with 2 args"

overlapping :: ConstrFn
overlapping [C' xset, C' yset] _ =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
overlapping [S' xset, C' yset] _ =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xc' yset, yc' yset, r' yset]]
overlapping [C' xset, S' yset] _ =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping [S' xset, S' yset] _ =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping _ _ = error "intersect not called with 2 args"

nonOverlapping :: ConstrFn
nonOverlapping [C' xset, C' yset] _ =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [S' xset, C' yset] _ =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [C' xset, S' yset] _ =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [S' xset, S' yset] _ =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset],
        [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [A' arr, L' label] _ =
    let (sx, sy, ex, ey, t) = (startx' arr, starty' arr, endx' arr, endy' arr, thickness' arr)
        (x1, y1, x2, y2) = (sx, sy - t, ex, ey + t)
        dx = maximum [x1 - xl' label, 0, xl' label - x2]
        dy = maximum [y1 - yl' label, 0, yl' label - y2] in
        tr "labelvsArr: " $ -sqrt(dx**2 + dy**2) - wl' label
nonOverlapping  _ _ = error "no intersect not called with 2 args"


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

-- the first set is the subset of the second, and thus smaller than the second in size.
-- TODO: test for equal sets
-- TODO: for two primitives we have 4 functions, which is not sustainable. NOT NEEDED, remove them.
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
penalty x = (max x 0) ^ q -- weights should get progressively larger in cr_dist
            where  q = 2 -- also, may need to sample OUTSIDE feasible set
            -- where q = 3 -- also, may need to sample OUTSIDE feasible set
