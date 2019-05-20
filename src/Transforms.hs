{-# LANGUAGE TemplateHaskell, StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module Transforms where

import Utils
import Debug.Trace
import           Data.List                          (nub, sort, findIndex, find, maximumBy, foldl')
import qualified Data.Map.Strict as M

import GHC.Generics
import Data.Aeson (FromJSON, ToJSON, toJSON)
--import Par

default (Int, Float)

-- a, b, c, d, e, f as here:
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
data HMatrix a = HMatrix {
     xScale :: a, -- a
     xSkew :: a, -- c -- What are c and b called?
     ySkew :: a, -- b
     yScale :: a, -- d
     dx     :: a, -- e
     dy     :: a  -- f
} deriving (Generic, Eq)

instance Show a => Show (HMatrix a) where
         show m = "[ " ++ show (xScale m) ++ " " ++ show (xSkew m) ++ " " ++ show (dx m) ++ " ]\n" ++
                  "  " ++ show (ySkew m) ++ " " ++ show (yScale m) ++ " " ++ show (dy m) ++ "  \n" ++
                  "  0.0 0.0 1.0 ]"

instance (FromJSON a) => FromJSON (HMatrix a)
instance (ToJSON a)   => ToJSON (HMatrix a)

idH :: (Autofloat a) => HMatrix a
idH = HMatrix {
    xScale = 1,
    xSkew = 0,
    ySkew = 0,
    yScale = 1,
    dx = 0,
    dy = 0
}

-- First row, then second row
hmToList :: (Autofloat a) => HMatrix a -> [a]
hmToList m = [ xScale m, xSkew m, dx m, ySkew m, yScale m, dy m ]

listToHm :: (Autofloat a) => [a] -> HMatrix a
listToHm l = if length l /= 6 then error "wrong length list for hmatrix"
             else HMatrix { xScale = l !! 0, xSkew = l !! 1, dx = l !! 2,
                            ySkew = l !! 3, yScale = l !! 4, dy = l !! 5 }

hmDiff :: (Autofloat a) => HMatrix a -> HMatrix a -> a
hmDiff t1 t2 = let (l1, l2) = (hmToList t1, hmToList t2) in
               norm $ l1 -. l2

applyTransform :: (Autofloat a) => HMatrix a -> Pt2 a -> Pt2 a
applyTransform m (x, y) = (x * xScale m + y * xSkew m + dx m, x * ySkew m + y * yScale m + dy m)

infixl ##
(##) :: (Autofloat a) => HMatrix a -> Pt2 a -> Pt2 a
(##) = applyTransform

-- General functions to work with transformations

-- Do t2, then t1. That is, multiply two homogeneous matrices: t1 * t2
composeTransform :: (Autofloat a) => HMatrix a -> HMatrix a -> HMatrix a
composeTransform t1 t2 = HMatrix { xScale = xScale t1 * xScale t2 + xSkew t1  * ySkew t2,
                                   xSkew  = xScale t1 * xSkew t2  + xSkew t1  * yScale t2,
                                   ySkew  =  ySkew t1 * xScale t2 + yScale t1 * ySkew t2,
                                   yScale =  ySkew t1 * xSkew t2  + yScale t1 * yScale t2,
                                   dx     = xScale t1 * dx t2 + xSkew t1 * dy t2 + dx t1,
                                   dy     = yScale t1 * dy t2 + ySkew t1 * dx t2 + dy t1 }
-- TODO: test that this gives expected results for two scalings, translations, rotations, etc.

infixl 7 #
(#) :: (Autofloat a) => HMatrix a -> HMatrix a -> HMatrix a
(#) = composeTransform

-- Compose all the transforms in RIGHT TO LEFT order:
-- [t1, t2, t3] means "do t3, then do t2, then do t1" or "t1 * t2 * t3"
composeTransforms :: (Autofloat a) => [HMatrix a] -> HMatrix a
composeTransforms ts = foldr composeTransform idH ts

-- Specific transformations

rotationM :: (Autofloat a) => a -> HMatrix a
rotationM radians = idH { xScale = cos radians, 
                          xSkew = -(sin radians),
                          ySkew = sin radians,
                          yScale = cos radians
                       }

translationM :: (Autofloat a) => Pt2 a -> HMatrix a
translationM (x, y) = idH { dx = x, dy = y }

scalingM :: (Autofloat a) => Pt2 a -> HMatrix a
scalingM (cx, cy) = idH { xScale = cx, yScale = cy }

rotationAboutM :: (Autofloat a) => a -> Pt2 a -> HMatrix a
rotationAboutM radians (x, y) = 
    -- Make the new point the new origin, do a rotation, then translate back
    composeTransforms [translationM (x, y), rotationM radians, translationM (-x, -y)]

shearingX :: (Autofloat a) => a -> HMatrix a
shearingX lambda = idH { xSkew = lambda }

shearingY :: (Autofloat a) => a -> HMatrix a
shearingY lambda = idH { ySkew = lambda }

------ Solve for final parameters

-- See PR for documentation
-- Note: returns angle in range [0, 2pi)
paramsOf :: (Autofloat a) => HMatrix a -> (a, a, a, a, a) -- There could be multiple solutions
paramsOf m = let (sx, sy) = (norm [xScale m, ySkew m], norm [xSkew m, yScale m]) in -- Ignore negative scale factors
             let theta = atan (ySkew m / (xScale m + epsd)) in -- Prevent atan(0/0) = NaN
             -- atan returns an angle in [-pi, pi]
             (sx, sy, theta, dx m, dy m)

paramsToMatrix :: (Autofloat a) => (a, a, a, a, a) -> HMatrix a
paramsToMatrix (sx, sy, theta, dx, dy) = -- scale then rotate then translate
               composeTransforms [translationM (dx, dy), rotationM theta, scalingM (sx, sy)]

toParallelogram :: (Autofloat a) => (a, a, a, a, a, a) -> HMatrix a
toParallelogram (w, h, rotation, x, y, shearAngle) =
                if shearAngle < 10**(-5) then error "shearAngle can't be 0, tangent will NaN" else
                composeTransforms [translationM (x, y),
                                   rotationM rotation,

                                   -- translationM (-w/2, -h/2),
                                   shearingX (1 / tan shearAngle),
                                   -- translationM (w/2, h/2),

                                   scalingM (w, h)
                                  ]

unitSq :: (Autofloat a) => [Pt2 a]
unitSq = [(0.5, 0.5), (-0.5, 0.5), (-0.5, -0.5), (0.5, -0.5)]

circlePoly :: (Autofloat a) => a -> [Pt2 a]
circlePoly r = let 
-- currently doesn't use r, so that #sides remains the same throughout opt.
-- TODO: might want to depend on radius of the original circle in some way?
    sides :: Int
    sides = max 8 $ floor (64 / 4.0)
    indices :: [Int]
    indices = [0..sides-1]
    angles = map (\i -> (r2f i) / (r2f sides) * 2.0 * pi) indices
    pts = map (\a -> (cos a, sin a)) angles
    in pts

-- Sample a circle about the origin with some density according to its radius
sampleUnitCirc :: (Autofloat a) => a -> [Pt2 a]
sampleUnitCirc r = if r < 0.01 then [(0, 0)] else []

testTriangle :: (Autofloat a) => [Pt2 a]
testTriangle = [(0, 0), (100, 0), (50, 50)]

testNonconvex :: (Autofloat a) => [Pt2 a]
testNonconvex = [(0, 0), (100, 0), (50, 50), (100, 100), (0, 100)]

-- TODO: the samples be transformed along by transforming here?
transformPoly :: (Autofloat a) => HMatrix a -> Polygon a -> Polygon a
transformPoly m (blobs, holes, _, samples) = let
    transformedBlobs = map (map (applyTransform m)) blobs
    in (
        transformedBlobs, 
        map (map (applyTransform m)) holes,
        getBBox (concat transformedBlobs),
        map (applyTransform m) samples
    )

transformSimplePoly :: (Autofloat a) => HMatrix a -> [Pt2 a] -> [Pt2 a]
transformSimplePoly m = map (applyTransform m)

-- Pointing to the right. Note: doesn't try to approximate the "inset" part of the arrowhead
rtArrowheadPoly :: (Autofloat a) => a -> [Pt2 a]
rtArrowheadPoly c = let x = 2 * c in -- c is the thickness of the line
                    let twox = x*2 in
              [(-x, x/4), -- Stem top
              (-twox, twox), -- Top left
              (twox, 0), -- Point of arrownead
              (-twox, -twox), -- Bottom left
              (-x, -x/4)] -- Stem bottom

ltArrowheadPoly :: (Autofloat a) => a -> [Pt2 a]
ltArrowheadPoly c = reverse $ map flipX $ rtArrowheadPoly c
                where flipX (x, y) = (-x, y)
                -- Reverse so the line polygon can join the stem bottom with the stem bottom

-- | Make a rectangular polygon of a line segment, accounting for its thickness and arrowheads
-- (Is this usable with autodiff?)
extrude :: (Autofloat a) => a -> Pt2 a -> Pt2 a -> Bool -> Bool -> [Pt2 a]
extrude c x y leftArr rightArr = 
     let dir = normalize' (y -: x)
         normal = rot90 dir -- normal vector to the line segment
         offset = (c / 2) *: normal -- find offset for each endpoint

         halfLen = mag (y -: x) / 2.0 -- Calculate arrowhead position
         trLeft = (-halfLen) *: dir
         trRight = halfLen *: dir

         left = if leftArr 
                then translate2 trLeft (ltArrowheadPoly c)
                else [x -: offset, x +: offset] -- Note: order matters for making the polygon
         right = if rightArr
                 then translate2 trRight (rtArrowheadPoly c)
                 else [y +: offset, y -: offset] 
     in left ++ right

------ Energies on polygons ------

---- some helpers below ----

type LineSeg a = (Pt2 a, Pt2 a)
type Blob a = [Pt2 a] -- temporary type for polygon. Connected, no holes
-- TODO: assuming that the positive shapes don't overlap and the negative shapes don't overlap (check this)
-- (positive shapes, negative shapes, bbox, samples)
type Polygon a = ([Blob a], [Blob a], (Pt2 a, Pt2 a), [Pt2 a])

emptyPoly :: Autofloat a => Polygon a
emptyPoly = ([], [], ((posInf, posInf), (negInf, negInf)), [])

toPoly :: Autofloat a => [Pt2 a] -> Polygon a
toPoly pts = ([pts], [], getBBox pts, sampleB numSamples pts)

posInf :: Autofloat a => a
posInf = 1 / 0

negInf :: Autofloat a => a
negInf = -1 / 0

-- input point, query parametric t
gettPS :: Autofloat a => Pt2 a -> LineSeg a -> a
gettPS p (a,b) = let
    v_ab = b -: a
    projl = v_ab `dotv` (p -: a)
    in projl / (magsq v_ab)

closestPointPS :: Autofloat a => Pt2 a -> LineSeg a -> Pt2 a
closestPointPS p (a,b) = let
    t = gettPS p (a,b) in
    if t<0.0 then a else if t>=1 then b else let v_ab = b -: a in
    a +: (t *: v_ab)

closestPointPG :: Autofloat a => Pt2 a -> Polygon a -> Pt2 a
closestPointPG p poly = let
    segments = getSegmentsG poly
    --
    mapf s = closestPointPS p s
    cps = map mapf segments -- parmap?
    --
    redf p1 p2 = let
        d1 = dsqPP p1 p
        d2 = dsqPP p2 p
        in if d1 <= d2 then p1 else p2
    in foldl' redf (cps!!0) cps -- reduce?

normS :: Autofloat a => LineSeg a -> Pt2 a
normS (p1, p2) = normalize' $ rot90 $ p2 -: p1

-- test if two segments intersect. Doesn't calculate for ix point though.
ixSS :: Autofloat a => LineSeg a -> LineSeg a -> Bool
ixSS (a,b) (c,d) = let
    ncd = rot90 $ d -: c
    a_cd = ncd `dotv` (a -: c)
    b_cd = ncd `dotv` (b -: c)
    nab = rot90 $ b -: a
    c_ab = nab `dotv` (c -: a)
    d_ab = nab `dotv` (d -: a)
    in ((a_cd>=0&&b_cd<=0) || (a_cd<=0&&b_cd>=0)) && 
       ((c_ab>=0&&d_ab<=0) || (c_ab<=0&&d_ab>=0))

getSegmentsB :: Autofloat a => Blob a -> [LineSeg a]
getSegmentsB pts = let 
    pts' = rotateList pts
    in zip pts pts'

getSegmentsG :: Autofloat a => Polygon a -> [LineSeg a]
getSegmentsG (bds, hs, _, _) = let
    bdsegments = map (\b->getSegmentsB b) bds
    hsegments = map (\h->getSegmentsB h) hs
    in concat [concat bdsegments, concat hsegments]

-- OLD
-- blob inside/outside test wo testing bbox first
isInB'' :: Autofloat a => Blob a -> Pt2 a -> Bool
isInB'' pts (x0,y0) = let
    diffp = map (\(x,y)->(x-x0,y-y0)) pts
    getAngle (x,y) = atan2 y x 
    angles = map getAngle diffp
    angles' = rotateList angles
    sweeps = map (\(a,b)->b-a) $ zip angles angles'
    adjust sweep = 
        if sweep>pi then sweep-2*pi
        else if sweep<(-pi) then 2*pi+sweep
        else sweep
    sweepAdjusted = map adjust sweeps
    -- if inside pos poly, res would be 2*pi,
    -- if inside neg poly, res would be -2*pi,
    -- else res would be 0
    res = foldl' (+) 0.0 sweepAdjusted
    in res>pi || res<(-pi)

-- A cheaper implementation of inside/outside test, found at
-- http://geomalgorithms.com/a03-_inclusion.html
isInB' :: Autofloat a => Blob a -> Pt2 a -> Bool
isInB' pts p@(x0, y0) = let
    segments = getSegmentsB pts
    wnf (a@(x1,y1), b@(x2,y2)) = 
        if (y1>y0 && y2>y0) || (y1<y0 && y2<y0) then 0
        else if (rot90(b-:a))`dotv`(p-:a) > 0 then 1 
        else (-1)
    wn = foldl' (+) 0 $ map wnf segments
    in if wn==0 then False else True

-- general, direct inside/outside test
isInG' :: Autofloat a => Polygon a -> Pt2 a -> Bool
isInG' (bds, hs, _, _) p = let
    inb = foldl' (||) False $ map (\b->isInB' b p) bds
    inh = foldl' (||) False $ map (\h->isInB' h p) hs
    in (inb) && (not inh)

-- inside/outside test w polygon by first testing against bbox.
isInG :: Autofloat a => Polygon a -> Pt2 a -> Bool
isInG poly@(_,_,bbox,_) p = if inBBox bbox p then isInG' poly p else False

-- isOutG :: Autofloat a => Polygon a -> Pt2 a -> a -> Bool
-- isOutG poly p ofs = (not $ isInG' poly p) && (dsqGP poly p 0 >= ofs)

getBBox :: Autofloat a => Blob a -> (Pt2 a, Pt2 a)
getBBox pts = let
    foldfn :: Autofloat a => (Pt2 a, Pt2 a) -> Pt2 a -> (Pt2 a, Pt2 a)
    foldfn ((xlo,ylo), (xhi,yhi)) (x, y) = 
        ( (min xlo x, min ylo y), (max xhi x, max yhi y) )
    in foldl' foldfn ((posInf, posInf), (negInf, negInf)) pts

-- returns the center of the polygon bbox
getCenter :: Autofloat a => Polygon a -> Pt2 a
getCenter (_,_,((xlo,ylo), (xhi,yhi)),_) = ((xhi+xlo)/2, (yhi+ylo)/2)

getDiameter2' :: Autofloat a => [Pt2 a] -> a
getDiameter2' pts = case pts of
    [] -> 0
    p:pts -> foldl' max 0 $ map (dsqPP p) pts

getDiameter :: Autofloat a => Polygon a -> a
getDiameter (bds,hs,_,_) = let
    vertices = (concat bds) ++ (concat hs)
    in sqrt $ getDiameter2' vertices

-- true if in bbox and far enough from boundary
inBBox :: Autofloat a => (Pt2 a, Pt2 a) -> Pt2 a -> Bool
inBBox ((xlo, ylo), (xhi, yhi)) (x,y) = let
    res = x >= xlo && x <= xhi &&
          y >= ylo && y <= yhi
    in res

scaleB :: Autofloat a => a -> Blob a -> Blob a
scaleB k b = map (scaleP k) b

-- sample points along segment with interval.
sampleS :: Autofloat a => a -> LineSeg a -> [Pt2 a]
sampleS numSamplesf (a, b) = let
    -- l = mag $ b -: a
    numSamples :: Int
    numSamples = (floor numSamplesf) + 1--floor $ l/interval
    inds = map realToFrac [0..numSamples-1]
    ks = map (/(realToFrac numSamples)) $ inds
    in map (lerpP a b) ks

sampleB :: Autofloat a => Int -> Blob a -> [Pt2 a]
sampleB numSamples blob = let
    numSamplesf = r2f numSamples
    segments = getSegmentsB blob
    circumfrence = foldl' (+) 0.0 $ map (\(a,b)->dist a b) segments
    seglengths = map (\(a,b)->dist a b) $ segments
    samplesEach = map (\l->l/circumfrence*numSamplesf) seglengths
    zp = zip segments samplesEach
    in concat $ map (\(seg, num) -> sampleS num seg) zp

-- TODO: be absolutely sure #samples are same as input #
sampleG :: Autofloat a => Int -> Polygon a -> [Pt2 a]
sampleG numSamples poly@(bds, hs, _, _) = let 
    numSamplesf = r2f numSamples
    segments = getSegmentsG poly
    circumfrence = foldl' (+) 0.0 $ map (\(a,b)->dist a b) segments
    seglengths = map (\(a,b)->dist a b) $ segments
    samplesEach = map (\l->l/circumfrence*numSamplesf) seglengths
    zp = zip segments samplesEach
    in concat $ map (\(seg, num) -> sampleS num seg) zp

---- dsq functions ----

dsqPP :: Autofloat a => Pt2 a -> Pt2 a -> a
dsqPP a b = magsq $ b -: a

dsqSP :: Autofloat a => LineSeg a -> Pt2 a -> a
dsqSP (a,b) p = let t = gettPS p (a,b) in
    if t<0 then dsqPP a p
    else if t>1 then dsqPP b p
    else (**2) $ (normS (a,b)) `dotv` (p -: a)

dsqSS :: Autofloat a => LineSeg a -> LineSeg a -> a
dsqSS (a,b) (c,d) = if ixSS (a,b) (c,d) then 0 else let
    da = dsqSP (c,d) a 
    db = dsqSP (c,d) b 
    dc = dsqSP (a,b) c 
    dd = dsqSP (a,b) d 
    in min (min da db) (min dc dd)

dsqBP :: Autofloat a => Blob a -> Pt2 a -> a
dsqBP b p = let
    segments = getSegmentsB b
    -- min dist to vertex
    d2v = foldl' min posInf $ map (\q -> magsq $ p -: q) b
    -- min dist to segment (if closest point falls within segment)
    d2s = foldl' min posInf $ map (\s@(a,b) -> let t = gettPS p s in
        if t>1 || t<0 then posInf else (**2) $ (normS (a,b)) `dotv` (p -: a)) segments
    in min d2v d2s

dsqGP :: Autofloat a => Polygon a -> Pt2 a -> a
dsqGP (bds, hs, _, _) p = let
    dsqBD = foldl' min posInf $ map (\b -> dsqBP b p) bds
    dsqHS = foldl' min posInf $ map (\h -> dsqBP h p) hs
    in min dsqBD dsqHS

-- only the magnitude matches with dsq. Negative when inside A.
signedDsqGP :: Autofloat a => Polygon a -> Pt2 a -> a
signedDsqGP poly p = let 
    dsq = dsqGP poly p
    inside = if dsq < epsd then True else isInG poly p
    in if inside then -dsq else dsq

dsqBS :: Autofloat a => Blob a -> LineSeg a -> a
dsqBS b s = foldl' min posInf $ 
    map (\e -> dsqSS e s) $ getSegmentsB b

dsqBB :: Autofloat a => Blob a -> Blob a -> a
dsqBB b1 b2 = let
    min1 = foldl' min posInf $ map (\e -> dsqBS b2 e) $ getSegmentsB b1
    in min1

dsqGG :: Autofloat a => Polygon a -> Polygon a -> a
dsqGG (bds1, hs1, _, _) (bds2, hs2, _, _) = let
    b1b2 = foldl' min posInf $ map (\b->foldl' min posInf $ map (\b'->dsqBB b b') bds1) bds2
    b1h2 = foldl' min posInf $ map (\b->foldl' min posInf $ map (\b'->dsqBB b b') bds1) hs2
    b2b1 = foldl' min posInf $ map (\b->foldl' min posInf $ map (\b'->dsqBB b b') bds2) bds1
    b2h1 = foldl' min posInf $ map (\b->foldl' min posInf $ map (\b'->dsqBB b b') bds2) hs1
    in min (min b1b2 b1h2) (min b2b1 b2h1)

alignPPA :: Autofloat a => Pt2 a -> Pt2 a -> a -> a
alignPPA a b angle = let
    angle_radians = angle * pi / 180.0
    dirN = rot90 (cos angle_radians, sin angle_radians)
    in (**2) $ (a `dotv` dirN) - (b `dotv` dirN)

orderPPA :: Autofloat a => Pt2 a -> Pt2 a -> a -> a
orderPPA a b angle = let
    angle_radians = angle * pi / 180.0
    dir = (cos angle_radians, sin angle_radians)
    in (**2) $ max 0 $ (a `dotv` dir) - (b `dotv` dir)

-- samples per polygon
numSamples :: Int
numSamples = 200

-- dsq integral along boundary functions ----

dsqBinA :: Autofloat a => Polygon a -> Polygon a -> a
dsqBinA bA bB@(_,_,_,samplesB) = let
    samplesIn = filter (\p -> isInG bA p) $ samplesB
    in foldl' (+) 0.0 $ map (\p -> dsqGP bA p) samplesIn

dsqBoutA :: Autofloat a => Polygon a -> Polygon a -> a
dsqBoutA bA bB@(_,_,_,samplesB) = let
    samplesOut = filter (\p -> not $ isInG bA p) $ samplesB
    in foldl' (+) 0.0 $ map (\p -> dsqGP bA p) samplesOut

-- signed distances between polygons based on sampling
-- TODO: the following two can both be achieved from the same iteration (thus save some runtime)

minSignedDsqGG :: Autofloat a => Polygon a -> Polygon a -> a
minSignedDsqGG polyA polyB@(_,_,_,samplesB) = let
    --samples = sampleG ds polyB
    in foldl' min posInf $ map (signedDsqGP polyA) samplesB--samples

maxSignedDsqGG :: Autofloat a => Polygon a -> Polygon a -> a
maxSignedDsqGG polyA polyB@(_,_,_,samplesB) = let
    --samples = sampleG ds polyB
    in foldl' max negInf $ map (signedDsqGP polyA) samplesB

------------ top-level query energies ------------

---- Energies defined with (sampled) minimum/maximum distances ----

---- 2 below: Energy lowest when minimum/maximum signed distance is at ofs pixels.
-- Both functions have similar runtime compared to other inside/outside energies
-- Both are a bit "unstable" (shapes make unexpected big jumps), likely because of how they're defined
-- Both become even more "unstable" when used with Newton's method, although Newton's method
-- doesn't break them right away. For most of the time still give results that are visually correct
-- (other times stuck at local min, or explode/shrink)

-- ofs = 0: an alternative containment+tangent energy. Can use ofs for containment with padding.
eBinAOffs :: Autofloat a => Polygon a -> Polygon a -> a -> a
eBinAOffs polyA polyB ofs = let
    sdsq = maxSignedDsqGG polyA polyB
    sign = if sdsq >= 0 then 1.0 else -1.0
    sdist = (*sign) $ sqrt $ abs sdsq
    in (sdist + ofs)**2

-- ofs = 0: an alternative disjoint+tangent energy. Can use ofs for disjoint plus margin.
eBoutAOffs :: Autofloat a => Polygon a -> Polygon a -> a -> a
eBoutAOffs polyA polyB ofs = let
    sdsq = minSignedDsqGG polyA polyB
    sign = if sdsq >= 0 then 1.0 else -1.0
    sdist = (*sign) $ sqrt $ abs sdsq
    in (sdist - ofs)**2

-- energy lowest when either of the above two energies are lowest
eOffs :: Autofloat a => Polygon a -> Polygon a -> a -> a
eOffs polyA polyB ofs = let
    eIn = eBinAOffs polyA polyB ofs
    eOut = eBoutAOffs polyA polyB ofs
    in min eIn eOut

-- energy lowest when polygon boundary is ofs px away.
eOffsP :: Autofloat a => Polygon a -> Pt2 a -> a -> a
eOffsP poly pt ofs = let
    d = sqrt $ dsqGP poly pt
    in (d - ofs)**2

-- energy lowest when polygon boundary is ofs px away.
eOffsPP :: Autofloat a => Pt2 a -> Pt2 a -> a -> a
eOffsPP p1 p2 ofs = let
    d = sqrt $ dsqPP p1 p2
    in (d - ofs)**2

---- 2 below: Similar to above, but energy lowest when minimum/maximum signed distance is at least ofs pixels.

-- ofs = 0: an alternative containment energy. Can use ofs for containment with minimum padding.
eBinAPad :: Autofloat a => Polygon a -> Polygon a -> a -> a
eBinAPad polyA polyB ofs = let
    sdsq = maxSignedDsqGG polyA polyB
    sign = if sdsq >= 0 then 1.0 else -1.0
    sdist = (*sign) $ sqrt $ abs sdsq
    in (max 0 $ sdist + ofs)**2

-- ofs = 0: an alternative disjoint energy. Can use positive ofs for disjoint plus minimum margin.
eBoutAPad :: Autofloat a => Polygon a -> Polygon a -> a -> a
eBoutAPad polyA polyB ofs = let
    sdsq = minSignedDsqGG polyA polyB
    sign = if sdsq >= 0 then 1.0 else -1.0
    sdist = (*sign) $ sqrt $ abs sdsq
    in (min 0 $ sdist - ofs)**2

-- energy lowest when either of the above two energies are lowest
ePad :: Autofloat a => Polygon a -> Polygon a -> a -> a
ePad polyA polyB ofs = let
    eIn = eBinAPad polyA polyB ofs
    eOut = eBoutAPad polyA polyB ofs
    in min eIn eOut

---- energies based on integral dsq along boundary ----

-- containment
eAcontainB :: Autofloat a => Polygon a -> Polygon a -> a
eAcontainB bA bB = let
    eAinB = dsqBinA bB bA
    eBoutA = dsqBoutA bA bB
    in eAinB + eBoutA

-- disjoint. might be changed later though, bc it gets stuck at local min too often,
-- resulting in two shapes overlap even more
eABdisj :: Autofloat a => Polygon a -> Polygon a -> a
eABdisj bA bB = let
    eAinB = dsqBinA bB bA
    eBinA = dsqBinA bA bB
    in eAinB + eBinA 

{-
-- A and B tangent, B inside A
eBinAtangent :: Autofloat a => Polygon a -> Polygon a -> a
eBinAtangent bA bB = let
    eContainment = eAcontainB bA bB
    eABbdix = dsqGG bA bB
    in eContainment + eABbdix

-- A and B tangent, B outside A
eBoutAtangent :: Autofloat a => Polygon a -> Polygon a -> a
eBoutAtangent bA bB = let
    eDisjoint = eABdisj bA bB
    eABbdix = dsqGG bA bB
    in eDisjoint + eABbdix
-}

---- Energies defined on polygon size (diameter) ----

eMaxSize :: Autofloat a => Polygon a -> a -> a
eMaxSize poly size = let
    d = getDiameter poly
    in (**2) $ max 0 $ d - size

eMinSize :: Autofloat a => Polygon a -> a -> a
eMinSize poly size = let
    d = getDiameter poly
    in (**2) $ max 0 $ size - d

eSameSize :: Autofloat a => Polygon a -> Polygon a -> a
eSameSize bA bB = let
    d1 = getDiameter bA
    d2 = getDiameter bB
    in (d1 - d2) ** 2

eSmallerThan :: Autofloat a => Polygon a -> Polygon a -> a
eSmallerThan bA bB = let
    d1 = getDiameter bA
    d2 = getDiameter bB
    in (max 0 $ d1 - d2) ** 2 -- no penalty if d1 <= d2

---- Other energies (related to alignment) ----

-- A and B align along some direction (input angle in degrees)
-- currently uses center of bbox as position. Could alternatively have a version that lets user specify.
eAlign :: Autofloat a => Polygon a -> Polygon a -> a -> a
eAlign bA bB angle = alignPPA (getCenter bA) (getCenter bB) angle

eOrder :: Autofloat a => Polygon a -> Polygon a -> a -> a
eOrder bA bB angle = orderPPA (getCenter bA) (getCenter bB) angle

---- analytic gradient for dsqPP ---- (not tested) (not in use)

-- using [a] to encode transformation here.
-- different from HMatrix here bc this one contains DOFs from input (ie. shear term means "amt of shear")
-- but terms in HMatrix contain numbers combined from DOFs (ie. shear term is some weird combo of shear+scale)

-- input: two points at their original transformation, plus all the transformations that 
-- brought them here.
dsqGradPP :: Autofloat a => Pt2 a -> Pt2 a -> [a] -> [a] -> ([a], [a])
dsqGradPP (px1,py1) (px2,py2) transA transB = let
    [cx1, cy1, sx1, sy1, kx1, ky1] = transA
    [cx2, cy2, sx2, sy2, kx2, ky2] = transB
    term1 = cx2 - cx1 - px1*sx1 - kx1*py1*sx1 + px2*sx2 + kx2*py2*sx2
    term2 = cy2 - cy1 - ky1*px1*sy1 - py1*(sy1 + kx1*ky1*sy1) + ky2*px2*sy2 + py2*(sy2 + kx2*ky2*sy2)
    d_kx1 = 2*term1 * (-py1 * sx1) + 2*term2 * (-ky1*py1*sy1)
    d_ky1 = 2*term2 * (-px1*sy1 - kx1*py1*sy1)
    d_sx1 = 2*term1 * (-px1 - kx1*py1)
    d_sy1 = 2*term2 * (-ky1*px1 - (1+kx1*ky1)*py1)
    d_cx1 = -2 * term1
    d_cy1 = -2 * term2
    -- looks like only need to hardcode half of these... anyway,
    d_kx2 = 2*term1 * (py2 * sx2) + 2*term2 * (ky2*py2*sy2)
    d_ky2 = 2*term2 * (px2*sy2 + kx2*py2*sy2)
    d_sx2 = 2*term1 * (px2 + kx2*py2)
    d_sy2 = 2*term2 * (ky2*px2 + (1+kx2*ky2)*py2)
    d_cx2 = 2 * term1
    d_cy2 = 2 * term2
    in ([d_cx1, d_cy1, d_sx1, d_sy1, d_kx1, d_ky1], 
        [d_cx2, d_cy2, d_sx2, d_sy2, d_kx2, d_ky2])
