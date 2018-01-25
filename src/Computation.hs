-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Computation where
import Shapes
import Utils
import Functions
import qualified Data.Map.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import System.Random
import System.Random.Shuffle
import Data.List (sort)

-- Temporary solution: register every single different function type as you write it
-- and pattern-match on it later.
-- Fix typechecking: applyComputation in Runtime is essentially doing ad-hoc typechecking
-- TODO figure out how arguments work
-- Doesn't deal well with polymorphism (all type variables need to go in the datatype)

type Pt2 a = (a, a)
type Interval = (Float, Float)
data Computation a = ComputeColor (() -> Color) 
                   | ComputeColorArgs (String -> a -> Color) 
                   | ComputeRadius (Circ' a -> a -> a) 
                   | ComputeRadiusToMatch (Circ' a -> Pt' a -> a) 
                   | ComputeColorRGBA (a -> a -> a -> a -> Color) 
                   | ComputeSurjection (StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen))
                   | ComputeSurjectionBbox (StdGen -> Integer -> SolidArrow' a -> SolidArrow' a -> ([Pt2 a], StdGen))
                   | ComputeSurjectionLines (StdGen -> Integer 
                        -> CubicBezier' a -> CubicBezier' a 
                        -> CubicBezier' a -> CubicBezier' a -> ([Pt2 a], StdGen))
                   | LineLeft (a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a])
                   | LineRight (a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a])
                   | TestPoly (Circ' a -> a)
                   | AddVector (Pt2 a -> Pt2 a -> Pt2 a)
                   | ReturnInt Int
                   | Delay15 (a -> a)
                   | TestNone 

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
-- | All functions must be registered
computationDict :: (Real a, Floating a, Ord a) => M.Map String (Computation a)
computationDict = M.fromList flist
    where
        flist :: (Real b, Floating b, Ord b) => [(String, Computation b)] 
        flist = [
                        ("computeColor", ComputeColor computeColor), -- pretty verbose 
                        ("computeColor2", ComputeColor computeColor2),
                        ("computeColorArgs", ComputeColorArgs computeColorArgs),
                        ("computeRadiusAsFrac", ComputeRadius computeRadiusAsFrac),
                        ("computeRadiusToMatch", ComputeRadiusToMatch computeRadiusToMatch),
                        ("computeColorRGBA", ComputeColorRGBA computeColorRGBA),
                        ("computeSurjection", ComputeSurjection computeSurjection),
                        ("computeSurjectionBbox", ComputeSurjectionBbox computeSurjectionBbox),
                        ("lineLeft", LineLeft lineLeft),
                        ("lineRight", LineRight lineRight),
                        ("addVector", AddVector addVector),
                        ("testPoly", TestPoly testPoly),
                        ("delay15", Delay15 delay15),
                        ("computeSurjectionLines", ComputeSurjectionLines computeSurjectionLines)
                ]

-- Delays some number of seconds (at least in ghci) and returns 0
-- I think the compiler is optimizing or caching the hard part though
-- If you change the exponent, you need to change the number of 9s
-- Why does it change the color?
delay15 :: Floating a => a -> a
delay15 x = trace "delay15" ((x^2 + r2f (head $ reverse $ take (10^7) [0, 1..])) - (9999999 + x * x))

addVector :: (Floating a) => Pt2 a -> Pt2 a -> Pt2 a
addVector (x, y) (c, d) = trace "addVector" $ (x + c, y + d)

testPoly :: Floating a => Circ' a -> a
testPoly c = 5.5

-- Generate n random values uniformly randomly sampled from interval and return generator.
-- NOTE: I'm not sure how backprop works WRT randomness, so the gradients might be inconsistent here.
-- Interval is not polymorphic because I want to avoid using the Random typeclass (Random a)
   -- which causes type inference problems in Style for some reason.
randomsIn :: (Floating a) => StdGen -> Integer -> Interval -> ([a], StdGen)
randomsIn g 0 _        =  ([], g)
randomsIn g n interval = let (x, g') = randomR interval g -- First value
                             (xs, g'') = randomsIn g' (n - 1) interval in -- Rest of values
                         ((r2f x) : xs, g'')

-- Given a generator, number of points, and lower left and top right of bbox, return points for a surjection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeSurjection :: (Floating a, Real a, Ord a) => StdGen -> Integer -> Pt2 a -> Pt2 a -> ([Pt2 a], StdGen)
computeSurjection g numPoints (lowerx, lowery) (topx, topy) = 
                  if numPoints < 2 then error "Surjection needs to have >= 2 points" 
                  else let (xs_inner, g') = randomsIn g (numPoints - 2) (r2f lowerx, r2f topx)
                           xs = lowerx : xs_inner ++ [topx] -- Include endpts so function covers domain
                           xs_increasing = sort xs

                           (ys_inner, g'') = randomsIn g' (numPoints - 2) (r2f lowery, r2f topy) 
                           ys = lowery : ys_inner ++ [topy] --clude endpts so function is onto
                           ys_perm = shuffle' ys (length ys) g'' in -- Random permutation. TODO return g3?

                           (zip xs_increasing ys_perm, g'') -- len xs == len ys

-- this function could be more general, taking in two objects and computing their bounding box
computeSurjectionBbox :: (Floating a, Real a, Ord a) => StdGen -> Integer 
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
computeSurjectionLines :: (Floating a, Real a, Ord a) => StdGen -> Integer 
                                   -> CubicBezier' a -> CubicBezier' a 
                                   -> CubicBezier' a -> CubicBezier' a -> ([Pt2 a], StdGen)
computeSurjectionLines g n left right bottom top = 
                       if not $ all (== 2) $ map (length . pathcb') [left, right, bottom, top]
                       then error "surjection requires straight lines" -- TODO change from cubic bezier
                       else let lower_left = (fst $ pathcb' left !! 0, snd $ pathcb' bottom !! 0) in
                            let top_right = (fst $ pathcb' right !! 1, snd $ pathcb' top !! 1) in
                            computeSurjection g n lower_left top_right

-- | No arguments for now, to avoid typechecking
-- Does this only work in gloss?
computeColor :: () -> Color
-- computeColor () = Colo { redc = 0, greenc = 0, bluec = 0 * 0, opacityc = 50 }
computeColor () = makeColor 0.5 0.1 (0.2 / 3) 0.5

computeColor2 :: () -> Color
computeColor2 () = makeColor (0.1 * 0.5) 0.1 0.5 0.5

makeColor' :: (Real a, Floating a) => a -> a -> a -> a -> Color
makeColor' r g b a = makeColor (r2f r) (r2f g) (r2f b) (r2f a)

computeColorArgs :: (Real a, Floating a) => String -> a -> Color
computeColorArgs ref1 mag = trace ("computeColorArgs " ++ ref1) $ 
                                 makeColor' (scale mag) (scale mag) (scale mag) 0.5
                 where scale c = c * 0.1

-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac :: (Num a) => Circ' a -> a -> a
computeRadiusAsFrac circ mag = {-trace ("computeRadiusAsFrac") $-} mag * (r' circ)

-- Compute the radius of the circle to lie on a point
computeRadiusToMatch :: (Floating a, Num a) => Circ' a -> Pt' a -> a
computeRadiusToMatch c p = trace ("computeRadiusToMatch") $ 
                           norm [getX c - getX p, getY c - getY p]

computeColorRGBA :: (Real a, Floating a) => a -> a -> a -> a -> Color
computeColorRGBA r g b a = makeColor' r g b a

-- TODO rename lineLeft and lineRight
-- assuming a1 horizontal and a2 vertical, respectively
lineLeft :: (Real a, Floating a) => a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a]
lineLeft lineFrac a1 a2 = let a1_start = startx' a1 in
                          let a1_len = abs (endx' a1 - a1_start) in 
                          let xpos = a1_start + lineFrac * a1_len in
                          [(xpos, starty' a1), (xpos, endy' a2)]

-- assuming a1 vert and a2 horiz, respectively
-- can this be written in terms of lineLeft?
lineRight :: (Real a, Floating a) => a -> SolidArrow' a -> SolidArrow' a -> [Pt2 a]
lineRight lineFrac a1 a2 = let a1_start = starty' a1 in
                           let a1_len = abs (endy' a1 - a1_start) in
                           let ypos = a1_start + lineFrac * a1_len in
                           [(startx' a2, ypos), (endx' a2, ypos)]
