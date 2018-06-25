-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE UnicodeSyntax             #-}
module Computation where
import           Data.Data
import           Data.Dynamic
import           Data.List                          (nub, sort)
import qualified Data.Map.Strict                    as M
import           Data.Typeable
import           Debug.Trace
import           Functions
import           Shapes
import           System.Random
import           System.Random.Shuffle
import           Utils

type Interval = (Float, Float)

-- Each computation uses this rng (not super high-quality)
compRngSur :: StdGen
compRngSur = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

compRng :: StdGen
compRng = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

--------------- Computations

-- Delays some number of seconds (at least in ghci) and returns 0
-- I think the compiler is optimizing or caching the hard part though
-- If you change the exponent, you need to change the number of 9s
-- Why does it change the color?
delay15 :: (Autofloat a) => a -> a
delay15 x = trace "delay15" ((x^2 + r2f (head $ reverse $ take (10^7) [0, 1..])) - (9999999 + x * x))

addVector :: (Autofloat a) => Pt2 a -> Pt2 a -> Pt2 a
addVector (x, y) (c, d) = {-trace "addVector" $ -}(x + c, y + d)

testPoly :: Autofloat a => Circ' a -> a
testPoly c = 5.5

-- Generate n random values uniformly randomly sampled from interval and return generator.
-- NOTE: I'm not sure how backprop works WRT randomness, so the gradients might be inconsistent here.
-- Interval is not polymorphic because I want to avoid using the Random typeclass (Random a)
   -- which causes type inference problems in Style for some reason.
-- Also apparently using Autofloat here with typeable causes problems for generality of returned StdGen.
-- But it works fine without Typeable.
randomsIn :: (Autofloat a) => StdGen -> Integer -> Interval -> ([a], StdGen)
randomsIn g 0 _        =  ([], g)
randomsIn g n interval = let (x, g') = randomR interval g -- First value
                             (xs, g'') = randomsIn g' (n - 1) interval in -- Rest of values
                         (r2f x : xs, g'')

-- Generate n random values uniformly randomly sampled from interval and DO NOT return generator.
-- (has problems with typeable)
randomsIn' :: (Autofloat a) => StdGen -> Integer -> Interval -> [a]
randomsIn' g 0 _        =  []
randomsIn' g n interval = let (x, g') = randomR interval g -- First value
                              (xs, _) = randomsIn g' (n - 1) interval in -- Rest of values
                          r2f x : xs

-- Given a generator, number of points, and lower left and top right of bbox, return points for a surjection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeSurjection :: Autofloat a => StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen)
computeSurjection g numPoints (lowerx, lowery) (topx, topy) =
                  if numPoints < 2 then error "Surjection needs to have >= 2 points"
                  else let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
                           xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
                           xs_increasing = sort xs
                           (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f lowery, r2f topy)
                           ys = lowery : ys_inner ++ [topy] --clude endpts so function is onto
                           ys_perm = shuffle' ys (length ys) g'' in -- Random permutation. TODO return g3?
                           (zip xs_increasing ys_perm, g'') -- len xs == len ys


-- Given a generator, number of points, and lower left and top right of bbox, return points for a bijection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeBijection :: Autofloat a => StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen)
computeBijection g numPoints (lowerx, lowery) (topx, topy) =
                  if numPoints < 2 then error "Bijection needs to have >= 2 points"
                  else let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
                           xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
                           xs_plot = nub (reverse (sort xs))
                           (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f lowery, r2f topy)
                           ys = lowery : ys_inner ++ [topy] --clude endpts so function is onto
                           ys_plot = (nub (sort ys)) in -- Random permutation. TODO return g3?
                           (zip xs_plot ys_plot, g'') -- len xs == len ys


-- Given a generator, number of points, and lower left and top right of bbox, return points for a injection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeInjection :: Autofloat a => StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen)
computeInjection g numPoints (lowerx, lowery) (topx, topy) =
                  if numPoints < 2 then error "Injection needs to have >= 2 points"
                  else let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
                           xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
                           xs_plot = nub (reverse (sort xs))
                           (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f (lowery + (topy - lowery)/4), r2f (topy - (topy - lowery)/4))
                           ys = (lowery + (topy - lowery)/4) : ys_inner ++ [topy - (topy - lowery)/4] --clude endpts so function is onto
                           ys_plot = (nub (sort ys)) in -- Random permutation. TODO return g3?
                           (zip xs_plot ys_plot, g'') -- len xs == len ys



-- this function could be more general, taking in two objects and computing their bounding box
computeSurjectionBbox :: (Autofloat a) => StdGen -> Integer
                                   -> SolidArrow' a -> SolidArrow' a -> ([Pt2 a], StdGen)
computeSurjectionBbox g n a1 a2 = let xs = [startx' a1, endx' a1, startx' a2, endx' a2]
                                      ys = [starty' a1, endy' a1, starty' a2, endy' a2]
                                      lower_left = (minimum xs, minimum ys)
                                      top_right = (maximum xs, maximum ys) in
                                  -- trace ("surjection bbox " ++ show lower_left ++ " " ++ show top_right) $
                                  computeSurjection g n lower_left top_right

-- Computes the surjection to lie inside a bounding box defined by the corners of a box
-- defined by four straight lines, assuming their lower/left coordinates come first.
-- Their intersections give the corners.
computeSurjectionLines :: (Autofloat a) => StdGen -> Integer
                                   -> Line' a -> Line' a -> Line' a -> Line' a -> ([Pt2 a], StdGen)
computeSurjectionLines g n left right bottom top =
                       let lower_left = (startx_l' left, starty_l' bottom) in
                       let top_right = (startx_l' right, starty_l' top) in
                       computeSurjection g n lower_left top_right


-- Computes the bijection to lie inside a bounding box defined by the corners of a box
-- defined by four straight lines, assuming their lower/left coordinates come first.
-- Their intersections give the corners.
computeBijectionLines :: (Autofloat a) => StdGen -> Integer
                                   -> Line' a -> Line' a -> Line' a -> Line' a -> ([Pt2 a], StdGen)
computeBijectionLines g n left right bottom top =
                       let lower_left = (startx_l' left, starty_l' bottom) in
                       let top_right = (startx_l' right, starty_l' top) in
                       computeBijection g n lower_left top_right

-- Computes the injection to lie inside a bounding box defined by the corners of a box
-- defined by four straight lines, assuming their lower/left coordinates come first.
-- Their intersections give the corners.
computeInjectionLines :: (Autofloat a) => StdGen -> Integer
                                   -> Line' a -> Line' a -> Line' a -> Line' a -> ([Pt2 a], StdGen)
computeInjectionLines g n left right bottom top =
                       let lower_left = (startx_l' left, starty_l' bottom) in
                       let top_right = (startx_l' right, starty_l' top) in
                       computeInjection g n lower_left top_right

-- | No arguments for now, to avoid typechecking
computeColor :: () -> Color
computeColor () = makeColor 0.5 0.1 (0.2 / 3) 0.5

computeColor2 :: () -> Color
computeColor2 () = makeColor (0.1 * 0.5) 0.1 0.5 0.5

makeColor' :: (Autofloat a) => a -> a -> a -> a -> Color
makeColor' r g b a = makeColor (r2f r) (r2f g) (r2f b) (r2f a)

computeColorArgs :: (Autofloat a) => String -> a -> Color
computeColorArgs ref1 mag = trace ("computeColorArgs " ++ ref1) $
                                 makeColor' (scale mag) (scale mag) (scale mag) 0.5
                 where scale c = c * 0.1

-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac :: (Autofloat a) => Circ' a -> a -> a
computeRadiusAsFrac circ mag = {-trace ("computeRadiusAsFrac") $-} mag * r' circ

-- Compute the radius of the circle to lie on a point
computeRadiusToMatch :: (Autofloat a) => Circ' a -> Pt' a -> a
computeRadiusToMatch c p = {-trace ("computeRadiusToMatch") $ -}
                           norm [getX c - getX p, getY c - getY p]

computeColorRGBA :: (Autofloat a) => a -> a -> a -> a -> Color
computeColorRGBA = makeColor'

-- calculates a line (of two points) intersecting the first axis, stopping before it leaves bbox of second axis
-- TODO rename lineLeft and lineRight
-- assuming a1 horizontal and a2 vertical, respectively
lineLeft :: (Autofloat a) => a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a]
lineLeft lineFrac a1 a2 = let a1_start = startx' a1 in
                          let a1_len = abs (endx' a1 - a1_start) in
                          let xpos = a1_start + lineFrac * a1_len in
                          [(xpos, starty' a1), (xpos, endy' a2)]

-- assuming a1 vert and a2 horiz, respectively
-- can this be written in terms of lineLeft?
lineRight :: (Autofloat a) => a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a]
lineRight lineFrac a1 a2 = let a1_start = starty' a1 in
                           let a1_len = abs (endy' a1 - a1_start) in
                           let ypos = a1_start + lineFrac * a1_len in
                           [(startx' a2, ypos), (endx' a2, ypos)]

-- calculates the width (or height) of a region defined by two vert (or horiz) lines
regionX :: (Autofloat a) => Line' a -> Line' a -> a
regionX lineL lineR = -- assuming two vertical lines (left, right)
                    let (xl, xr) = (startx_l' lineL, startx_l' lineR) in
                    abs $ xr - xl

regionY :: (Autofloat a) => Line' a -> Line' a -> a
regionY lineB lineT = -- assuming two horiz lines (bottom, top)
                    let (yb, yt) = (starty_l' lineB, starty_l' lineT) in
                    abs $ yb - yt

-- Calculates the center of a rectangular region defined by the intersection of four lines
-- TODO: a lot of code is duplicated from the above two
regionCenter :: (Autofloat a) => Line' a -> Line' a -> Line' a -> Line' a -> (a, a)
regionCenter left right down up = -- expects input lines to be left, right, etc. in a rectangle
                    -- assuming two vertical / and horizontal lines , calculates x left, x right, etc.
                    let (xl, xr, yl, yr) = (startx_l' left, startx_l' right, starty_l' down, starty_l' up) in
                    ((xl + xr) / 2, (yl + yr) / 2)

------------------------------------- Computation boilerplate
-- Registration, typechecking, error handling

type CompFn a = (Autofloat a) => [TypeIn a] -> [Obj' a] -> TypeIn a
type CompFnOn a = [TypeIn a] -> [Obj' a] -> TypeIn a

-- TODO Generate the typechecking and registration dict with Template Haskell
-- typecheck :: [String] -> [String] -> [TypeIn a] -> [Obj' a]

error' :: (Autofloat a) => Name -> [TypeIn a] -> [Obj' a] -> b
error' nm vals objs = error ("unexpected # or type or argument in `" ++ nm ++ "`'s arguments: \n"
                                         ++ show vals ++ "\n" ++ show objs)

-- | this function models property access and calls the general "get" function
get' :: CompFn a
get' [TStr p] [obj] = get p obj

multiply' :: CompFn a
-- TODO pattern match on multiple TNums
multiply' [TNum a, TNum b] [] = TNum $ a * b

add' :: CompFn a
-- TODO pattern match on multiple TNums
add' [TNum a, TNum b] [] = TNum $ a + b

srgba' :: CompFn a
srgba' [TNum r, TNum g, TNum b, TNum a] [] = TColor $ makeColor' r g b a

computeColor' :: CompFn a
computeColor' _ _ = TColor $ computeColor ()

computeColor2' :: CompFn a
computeColor2' _ _ = TColor $ computeColor2 ()

computeColorArgs' :: CompFn a
computeColorArgs' [TStr s, TNum x] _ = TColor $ computeColorArgs s x
computeColorArgs' v o                = error' "computeColorArgs" v o

computeRadiusAsFrac' :: CompFn a
computeRadiusAsFrac' [TNum mag] [C' circ] = TNum $ computeRadiusAsFrac circ mag
computeRadiusAsFrac' v o = error' "computeRadiusAsFrac" v o

computeRadiusToMatch' :: CompFn a
computeRadiusToMatch' [] [C' c, P' p] = TNum $ computeRadiusToMatch c p
computeRadiusToMatch' v o             = error' "computeRadiusToMatch" v o

computeColorRGBA' :: CompFn a
computeColorRGBA' [TNum x1, TNum x2, TNum x3, TNum x4] [] = TColor $ computeColorRGBA x1 x2 x3 x4
computeColorRGBA' v o = error' "computeColorRGBA" v o

-- TODO: revert the next three "x"s to TInt
computeSurjection' :: CompFn a
computeSurjection' [TNum x, TPt p1, TPt p2] [] = TPath $ fst $ computeSurjection compRngSur (floor x) p1 p2
computeSurjection' v o = error' "computeSurjection" v o

computeSurjectionBbox' :: CompFn a
computeSurjectionBbox' [TNum x] [A' a1, A' a2] = TPath $ fst $ computeSurjectionBbox compRng (floor x) a1 a2
computeSurjectionBbox' [TNum x] [AR' ar1, A' a1, A' a2] = TPath $ fst $ computeSurjectionBbox compRng (floor x) a1 a2
computeSurjectionBbox' v o = error' "computeSurjectionBbox" v o

-- TODO: for multiple objects, inputs might not be in right order (depending on lookupAll)
computeSurjectionLines' :: CompFn a
computeSurjectionLines' [TNum x] [LN' l1, LN' l2, LN' l3, LN' l4] =
                        TPath $ fst $ computeSurjectionLines compRng (floor x) l1 l2 l3 l4
computeSurjectionLines' v o = error' "computeSurjectionLines" v o

computeBijectionLines' :: CompFn a
computeBijectionLines' [TNum x] [LN' l1, LN' l2, LN' l3, LN' l4] =
                        TPath $ fst $ computeBijectionLines compRng (floor x) l1 l2 l3 l4
computeBijectionLines' v o = error' "computeBijectionLines" v o

computeInjectionLines' :: CompFn a
computeInjectionLines' [TNum x] [LN' l1, LN' l2, LN' l3, LN' l4] =
                        TPath $ fst $ computeInjectionLines compRng (floor x) l1 l2 l3 l4
computeInjectionLines' v o = error' "computeInjectionLines" v o

lineLeft' :: CompFn a
lineLeft' [TNum x] [A' a1, A' a2] = TPath $ lineLeft x a1 a2
lineLeft' v o                     = error' "lineLeft" v o

lineRight' :: CompFn a -- pretty much same as above
lineRight' [TNum x] [A' a1, A' a2] = TPath $ lineRight x a1 a2
lineRight' v o                     = error' "lineRight" v o

-- TODO make this more principled?
addVector' :: CompFn a
addVector' [TNum n1, TNum n2] [P' p] = TPt $ addVector (n1, n2) (xp' p, yp' p)
addVector' v o                       = error' "addVector" v o

regionX' :: CompFn a
regionX' [] [LN' lineLeft, LN' lineRight] = TNum $ regionX lineLeft lineRight
regionX' v o                              = error' "regionX" v o

-- returns the middle of the square as a starting position
midSquare' :: CompFn a
midSquare' [] [S' b] = TPt(xs' b, ys' b)

rightSquare' :: CompFn a
rightSquare' [] [S' b] = TPt(xs' b + side' b/2, ys' b + side' b/2)

leftSquare' :: CompFn a
leftSquare' [] [S' b] = TPt(xs' b - side' b/2, ys' b + side' b/2)

toRightSquare' :: CompFn a
toRightSquare' [] [S' b] = TPt(xs' b + 200, ys' b)

toLeftSquare' :: CompFn a
toLeftSquare' [] [S' b] = TPt(xs' b - 200, ys' b)

toAboveSquare' :: CompFn a
toAboveSquare' [] [S' b] = TPt(xs' b, ys' b + 125)

toBelowSquare' :: CompFn a
toBelowSquare' [] [S' b] = TPt(xs' b, ys' b-125)

startArrow' :: CompFn a
startArrow' [] [A' a] = TPt(startx' a, starty' a)

endArrow' :: CompFn a
endArrow' [] [A' a] = TPt(endx' a, endy' a)

neg' :: CompFn a
neg' [] [A' a] = let sx = startx' a
                     sy = starty' a
                     ex = endx' a
                     ey = endy' a
                 in TPt(sx - (ex - sx), sy - (ey - sy))


toBelowStartArrow' :: CompFn a
toBelowStartArrow' [] [A' a] = TPt(startx' a, starty' a - 20)

toBelowEndArrow' :: CompFn a
toBelowEndArrow' [] [A' a] = TPt(endx' a, endy' a - 20)

addVecEnd' :: CompFn a
addVecEnd' [] [A' a, A' b] = TPt(endx' a + endx' b - startx' a, endy' a + endy' b - starty' a)

regionY' :: CompFn a
regionY' [] [LN' down, LN' up] = TNum $ regionY down up
regionY' v o                   = error' "regionY" v o

regionCenter' :: CompFn a
regionCenter' [] [LN' l, LN' r, LN' d, LN' u] = TPt $ regionCenter l r d u
regionCenter' v o                             = error' "regionCenter" v o

-- TODO parse these at runtime
atOrigin' :: CompFn a
atOrigin' _ _ = TPt (-100, 0)

atEnd' :: CompFn a
atEnd' _ _ = TPt (100, 100)

atEnd2' :: CompFn a
atEnd2' _ _ = TPt (-100, -100)

toRight' :: CompFn a
toRight' _ _ = TPt (325, 0)

toAbove' :: CompFn a
toAbove' _ _ = TPt (-100, 200)

lightBlue' :: CompFn a
lightBlue' _ _ = TColor $ makeColor 0.1 0.1 0.9 0.2

darkBlue' :: CompFn a
darkBlue' _ _ = TColor $ makeColor 0.05 0.05 0.6 1

-- | 'computationDict' stores a mapping from the name of computations to the actual implementation
computationDict :: (Autofloat a) => M.Map String (CompFnOn a)
computationDict = M.fromList flist
    where flist = [
                    ("_get", get'),
                    ("multiply", multiply'),
                    ("add", add'),
                    ("computeColor", computeColor'),
                    ("computeColor2", computeColor2'),
                    ("computeColorArgs", computeColorArgs'),
                    ("computeRadiusAsFrac", computeRadiusAsFrac'), -- TODO change the primes
                    ("computeRadiusToMatch", computeRadiusToMatch'),
                    ("computeColorRGBA", computeColorRGBA'),
                    ("computeSurjection", computeSurjection'),
                    ("computeSurjectionBbox", computeSurjectionBbox'),
                    ("lineLeft", lineLeft'),
                    ("lineRight", lineRight'),
                    ("addVector", addVector'),
                    ("computeSurjectionLines", computeSurjectionLines'),
                    ("computeBijectionLines", computeBijectionLines'),
                    ("computeInjectionLines", computeInjectionLines'),
                    ("regionX", regionX'),
                    ("regionY", regionY'),
                    ("midSquare", midSquare'),
                    ("srgba", srgba'),
                    ("toRightSquare", toRightSquare'),
                    ("toLeftSquare", toLeftSquare'),
                    ("toAboveSquare", toAboveSquare'),
                    ("toBelowSquare", toBelowSquare'),
                    ("rightSquare", rightSquare'),
                    ("leftSquare", leftSquare'),
                    ("startArrow", startArrow'),
                    ("toBelowStartArrow", toBelowStartArrow'),
                    ("toBelowEndArrow", toBelowEndArrow'),
                    ("endArrow", endArrow'),
                    ("addVecEnd", addVecEnd'),
                    ("neg", neg'),
                    ("regionCenter", regionCenter'),
                    ("atOrigin", atOrigin'),
                    ("toRight", toRight'),
                    ("toAbove", toAbove'),
                    ("lightBlue", lightBlue'),
                    ("darkBlue", darkBlue')
                  ]
