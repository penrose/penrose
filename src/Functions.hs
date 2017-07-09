{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}
module Functions where
import Shapes
import Utils
import qualified Data.Map.Strict as M

----------------------- Sample objective functions that operate on objects (given names)
-- TODO write about expectations for the objective function writer

-- type ObjFnOn a = forall a. (Floating a, Real a, Show a, Ord a) => [Name] -> M.Map Name (Obj' a) -> a
type ObjFnOn a = forall a. (Floating a, Real a, Show a, Ord a) => [Obj' a] -> a
-- illegal polymorphic or qualified type--can't return a forall?
type ObjFnNamed a = forall a. (Floating a, Real a, Show a, Ord a) => M.Map Name (Obj' a) -> a
type Weight a = a

-- TODO deal with lists in a more principled way
-- maybe the typechecking should be done elsewhere...
-- shouldn't these two be parametric over objects?
centerCirc :: ObjFnOn a
centerCirc [C' c] = (xc' c)^2 + (yc' c)^2
centerCirc [L' _] = error "misnamed label"

-- distanceOf :: ObjFnOn a
-- toLeft [fromname, toname] dict =
--     case (M.lookup fromname dict, M.lookup toname dict) of
--         -- (Just (A' a), Just (S' s), Just (S' e)) ->
--         -- (Just (A' a), Just (S' s), Just (C' e)) ->
--         -- (Just (A' a), Just (C' s), Just (S' e)) ->
--         (Just (C' s), Just (C' e)) ->
--             -- (fromx - sx)^2 + (fromy - sy)^2 + (tox - ex)^2 + (toy - ey)^2
--             (xc' s - xc' e + 400)^2

onTop :: ObjFnOn a
onTop [L' s, L' e] = (yl' s - yl' e - 100)^2

sameHeight :: ObjFnOn a
sameHeight [C' s, S' e] = (yc' s - ys' e)^2
sameHeight [S' s, C' e] = (ys' s - yc' e)^2
sameHeight [C' s, C' e] = (yc' s - yc' e)^2
sameHeight [S' s, S' e] = (ys' s - ys' e)^2
sameHeight [L' s, L' e] = (yl' s - yl' e)^2

sameX :: ObjFnOn a
sameX [C' s, S' e] = (xc' s - xs' e)^2
sameX [S' s, C' e] = (xs' s - xc' e)^2
sameX [C' s, C' e] = (xc' s - xc' e)^2
sameX [S' s, S' e] = (xs' s - xs' e)^2
sameX [L' s, L' e] = (xl' s - xl' e)^2

sameCenter :: ObjFnOn a
sameCenter [C' s, S' e] = (yc' s - ys' e)^2 + (xc' s - xs' e)^2
sameCenter [S' s, C' e] = (ys' s - yc' e)^2 + (xs' s - xc' e)^2
sameCenter [C' s, C' e] = (yc' s - yc' e)^2 + (xc' s - xc' e)^2
sameCenter [S' s, S' e] = (ys' s - ys' e)^2 + (xs' s - xs' e)^2
sameCenter [L' s, L' e] = (yl' s - yl' e)^2 + (xl' s - xl' e)^2

toLeft :: ObjFnOn a
toLeft [C' s, S' e] = (xc' s - xs' e + 400)^2
toLeft [S' s, C' e] = (xs' s - xc' e + 400)^2
-- toLeft [C' s, C' e] = (xc' s - xc' e + r' s + r' e + 200)^2
toLeft [C' s, C' e] = (xc' s - xc' e + 400)^2
toLeft [S' s, S' e] = (xs' s - xs' e + 400)^2
toLeft [L' s, L' e] = (xl' s - xl' e + 100)^2


centerMap :: ObjFnOn a
centerMap [A' a, S' s, S' e] = _centerMap a [xs' s, ys' s] [xs' e, ys' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, S' s, C' e] = _centerMap a [xs' s, ys' s] [xc' e, yc' e]
                [spacing + (halfDiagonal . side') s, negate $ spacing + r' e]
centerMap [A' a, C' s, S' e] = _centerMap a [xc' s, yc' s] [xs' e, ys' e]
                [spacing + r' s, negate $ spacing + (halfDiagonal . side') e]
centerMap [A' a, C' s, C' e] = _centerMap a [xc' s, yc' s] [xc' e, yc' e]
                [ spacing * r' s, negate $ spacing * r' e]
centerMap [A' a, L' s, L' e] = _centerMap a [xl' s, yl' s] [xl' e, yl' e]
                [spacing * hl' s, negate $ spacing * hl' e]
centerMap [A' a, L' s, C' e] = _centerMap a [xl' s, yl' s] [xc' e, yc' e]
                [spacing, negate $ spacing + r' e]
centerMap o = error ("CenterMap: unsupported arguments: " ++ show o)
spacing = 1.1 -- TODO: arbitrary

_centerMap :: forall a. (Floating a, Real a, Show a, Ord a) =>
                SolidArrow' a -> [a] -> [a] -> [a] -> a
_centerMap a s1@[x1, y1] s2@[x2, y2] [o1, o2] =
    let vec  = [x2 - x1, y2 - y1] -- direction the arrow should point to
        dir = normalize vec -- direction the arrow should point to
        [sx, sy, ex, ey] = if norm vec > o1 + abs o2
                then (s1 +. o1 *. dir) ++ (s2 +. o2 *. dir) else s1 ++ s2
        [fromx, fromy, tox, toy] = [startx' a, starty' a, endx' a, endy' a] in
    (fromx - sx)^2 + (fromy - sy)^2 + (tox - ex)^2 + (toy - ey)^2

repel :: ObjFnOn a
repel [C' c, S' d] = 1 / distsq (xc' c, yc' c) (xs' d, ys' d) - r' c - side' d + epsd
repel [S' c, C' d] = 1 / distsq (xc' d, yc' d) (xs' c, ys' c) - r' d - side' c + epsd
repel [C' c, C' d] = 1 / distsq (xc' c, yc' c) (xc' d, yc' d) - r' c - r' d + epsd
repel [L' c, L' d] =
    if c == d then 0 else 1 / distsq (xl' c, yl' c) (xl' d, yl' d)
repel [L' c, C' d] = if labelName (namec' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xc' d, yc' d)
repel [C' c, L' d] = 1 / distsq (xc' c, yc' c) (xl' d, yl' d)
repel [L' c, S' d] = if labelName (names' d) == namel' c then 0 else 1 / distsq (xl' c, yl' c) (xs' d, ys' d)
repel [S' c, L' d] = 1 / distsq (xs' c, ys' c) (xl' d, yl' d)
repel [A' c, L' d] = repel' (startx' c, starty' c) (xl' d, yl' d) +
        repel' (endx' c, endy' c) (xl' d, yl' d)
repel [A' c, C' d] = repel' (startx' c, starty' c) (xc' d, yc' d) +
        repel' (endx' c, endy' c) (xc' d, yc' d)
repel _  = error "invalid selectors in repel"

repel' x y = 1 / distsq x y + epsd

centerLabel :: ObjFnOn a
centerLabel [C' c, L' l] =
                let [cx, cy, lx, ly] = [xc' c, yc' c, xl' l, yl' l] in
                -- if dist (cx, cy) (lx, ly) > r' c then (cx - lx)^2 + (cy - ly)^2 else 0.3 *
                     (cx - lx)^2 + (cy - ly)^2
centerLabel [S' s, L' l] =
                let [cx, cy, lx, ly] = [xs' s, ys' s, xl' l, yl' l] in
                (cx - lx)^2 + (cy - ly)^2
centerLabel [P' p, L' l] =
                let [px, py, lx, ly] = [xp' p, yp' p, xl' l, yl' l] in
                (px + 10 - lx)^2 + (py + 20 - ly)^2 -- Top right from the point
centerLabel [A' a, L' l] =
                let (sx, sy, ex, ey) = (startx' a, starty' a, endx' a, endy' a)
                    (mx, my) = midpoint (sx, sy) (ex, ey)
                    (lx, ly) = (xl' l, yl' l) in
                (mx - lx)^2 + (my + 1.1 * hl' l - ly)^2 -- Top right from the point
centerLabel o  = error ("centerLabel not called with 1 arg" ++ show o)

outside :: ObjFnOn a
outside [L' o, C' i] =
            -- let d = dist (xl' o, yl' o) (xc' i, yc' i) in
            -- if d > r' i  then 0 else
            -- (dist (xl' o, yl' o) (xc' i, yc' i) - (1.2 * r' i))^2
            (dist (xl' o, yl' o) (xc' i, yc' i) - (1.2 * r' i))^2
            -- (dist (xl' o, yl' o) (xc' i, yc' i) - (2 * r' i))^2
outside [L' o, S' i] =
            (dist (xl' o, yl' o) (xs' i, ys' i) - 2 * (halfDiagonal . side') i)^2

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

------- Constraints
-- Constraints are written WRT magnitude of violation
-- TODO metaprogramming for boolean constraints
-- TODO use these types?
-- type ConstraintFn a = forall a. (Floating a, Real a, Show a, Ord a) => [Name] -> M.Map Name (Obj' a) -> a

defaultCWeight :: Floating a => a
defaultCWeight = 1

-- TODO: should points also have a weight of 1?
defaultPWeight :: Floating a => a
defaultPWeight = 1


--------------------------------------------------------------------------------
-- Constraint functions
-- List: smallerThan, contains, outsideOf, overlapping, nonOverlapping, samesize, maxsize, minsize
type ConstraintFn = forall a. (Floating a, Real a, Show a, Ord a) => [Obj' a] -> a

sameSize :: ConstraintFn
sameSize [S' s1, S' s2] = (side' s1 - side' s2)**2
sameSize [C' s1, C' s2] = (r' s1 - r' s2)**2

maxSize :: ConstraintFn
limit = max (fromIntegral 700) (fromIntegral 800)
maxSize [C' c] = r' c -  limit / 3
maxSize [S' s] = side' s - limit  / 3

minSize :: ConstraintFn
minSize [C' c] = 20 - r' c
minSize [S' s] = 20 - side' s

smallerThan  :: ConstraintFn
-- smallerThan [C' inc, C' outc] =  (r' outc) - (r' inc) - 0.4 * r' outc -- TODO: taking this as a parameter?
smallerThan [C' inc, C' outc] = (r' inc) - (r' outc)
smallerThan [S' inc, S' outc] = (side' inc) - (side' outc) - subsetSizeDiff
smallerThan [C' c, S' s] = 0.5 * side' s - r' c
smallerThan [S' s, C' c] = (halfDiagonal . side') s - r' c

contains :: ConstraintFn
contains [C' outc, C' inc] =
    -- tr (namec' outc ++  " contains " ++ namec' inc ++ " val: ") $
    -- strictSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
    let res =  dist (xc' inc, yc' inc) (xc' outc, yc' outc) - (r' outc - r' inc) in
    if res > 0 then res else 0
contains [S' outc, S' inc] = strictSubset
    [[xs' inc, ys' inc, 0.5 * side' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [S' outc, C' inc] = strictSubset
    [[xc' inc, yc' inc, r' inc], [xs' outc, ys' outc, 0.5 * side' outc]]
contains [C' outc, S' inc] = strictSubset
    [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
contains [C' set, P' pt] =
        dist (xp' pt, yp' pt) (xc' set, yc' set) - 0.5 * r' set
contains [S' set, P' pt] =
    dist (xp' pt, yp' pt) (xs' set, ys' set) - 0.4 * side' set
contains [C' set, L' label] =
    let res = dist (xl' label, yl' label) (xc' set, yc' set) - 0.5 * r' set in
    if res < 0 then 0 else res
contains [S' set, L' label] =
    dist (xl' label, yl' label) (xs' set, ys' set) - (side' set) / 2 + wl' label
contains _  = error "subset not called with 2 args"

outsideOf :: ConstraintFn
outsideOf [C' inc, C' outc] =
    noSubset [[xc' inc, yc' inc, r' inc], [xc' outc, yc' outc, r' outc]]
outsideOf [S' inc, S' outc] =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc],
        [xs' outc, ys' outc, (halfDiagonal . side') outc]]
outsideOf [C' inc, S' outs] =
    noSubset [[xc' inc, yc' inc, r' inc], [xs' outs, ys' outs, (halfDiagonal . side') outs]]
outsideOf [S' inc, C' outc] =
    noSubset [[xs' inc, ys' inc, (halfDiagonal . side') inc], [xc' outc, yc' outc, r' outc]]
outsideOf [P' pt, C' set] =
    -dist (xp' pt, yp' pt) (xc' set, yc' set) + r' set
outsideOf [P' pt, S' set] =
    -dist (xp' pt, yp' pt) (xs' set, ys' set) + (halfDiagonal . side') set
outsideOf [L' lout, C' inset] =
    let labelR = max (wl' lout) (hl' lout)
        res = - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset + labelR in
    if namel' lout == (labelName $ namec' inset) then 0 else res
    -- if res <= 0 then 1 / res else res
    -- - dist (xl' lout, yl' lout) (xc' inset, yc' inset) + r' inset
outsideOf [L' lout, S' inset] =
    - dist (xl' lout, yl' lout) (xs' inset, ys' inset) + (halfDiagonal . side') inset
outsideOf _ = error "noSubset not called with 2 args"

overlapping :: ConstraintFn
overlapping [C' xset, C' yset] =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
overlapping [S' xset, C' yset] =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xc' yset, yc' yset, r' yset]]
overlapping [C' xset, S' yset] =
    looseIntersect [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping [S' xset, S' yset] =
    looseIntersect [[xs' xset, ys' xset, 0.5 * side' xset], [xs' yset, ys' yset, 0.5 * side' yset]]
overlapping _ = error "intersect not called with 2 args"

nonOverlapping :: ConstraintFn
nonOverlapping [C' xset, C' yset] =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [S' xset, C' yset] =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset], [xc' yset, yc' yset, r' yset]]
nonOverlapping [C' xset, S' yset] =
    noIntersectExt [[xc' xset, yc' xset, r' xset], [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [S' xset, S' yset] =
    noIntersectExt [[xs' xset, ys' xset, (halfDiagonal . side') xset],
        [xs' yset, ys' yset, (halfDiagonal . side') yset]]
nonOverlapping [A' arr, L' label] =
    let (sx, sy, ex, ey, t) = (startx' arr, starty' arr, endx' arr, endy' arr, thickness' arr)
        (x1, y1, x2, y2) = (sx, sy - t, ex, ey + t)
        dx = maximum [x1 - xl' label, 0, xl' label - x2]
        dy = maximum [y1 - yl' label, 0, yl' label - y2] in
        tr "labelvsArr: " $ -sqrt(dx**2 + dy**2) - wl' label
nonOverlapping  _ = error "no intersect not called with 2 args"

type PairConstrV a = forall a . (Floating a, Ord a, Show a) => [[a]] -> a -- takes pairs of "packed" objs


noConstraint :: PairConstrV a
noConstraint _ = 0

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
