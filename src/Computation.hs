-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts, DeriveDataTypeable #-}
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
import Data.Dynamic
import Data.Data
import Data.Typeable

-- Temporary solution: register every single different function type as you write it
-- and pattern-match on it later.
-- Fix typechecking: applyComputation in Runtime is essentially doing ad-hoc typechecking
-- TODO figure out how arguments work
-- Doesn't deal well with polymorphism (all type variables need to go in the datatype)

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
--                   deriving (Typeable)

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
-- | All functions must be registered
-- These could all be stored as Dynamic
computationDict :: (Autofloat a) => M.Map String (Computation a)
computationDict = M.fromList flist

flist :: (Autofloat a) => [(String, Computation a)] 
flist = [
                ("computeColor", ComputeColor computeColor),
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
                         ((r2f x) : xs, g'')

-- Generate n random values uniformly randomly sampled from interval and DO NOT return generator.
-- (has problems with typeable)
randomsIn' :: (Autofloat a) => StdGen -> Integer -> Interval -> [a]
randomsIn' g 0 _        =  []
randomsIn' g n interval = let (x, g') = randomR interval g -- First value
                              (xs, _) = randomsIn g' (n - 1) interval in -- Rest of values
                          (r2f x) : xs

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

makeColor' :: (Autofloat a) => a -> a -> a -> a -> Color
makeColor' r g b a = makeColor (r2f r) (r2f g) (r2f b) (r2f a)

computeColorArgs :: (Autofloat a) => String -> a -> Color
computeColorArgs ref1 mag = trace ("computeColorArgs " ++ ref1) $ 
                                 makeColor' (scale mag) (scale mag) (scale mag) 0.5
                 where scale c = c * 0.1

-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac :: (Autofloat a) => Circ' a -> a -> a
computeRadiusAsFrac circ mag = {-trace ("computeRadiusAsFrac") $-} mag * (r' circ)

-- Compute the radius of the circle to lie on a point
computeRadiusToMatch :: (Autofloat a) => Circ' a -> Pt' a -> a
computeRadiusToMatch c p = {-trace ("computeRadiusToMatch") $ -}
                           norm [getX c - getX p, getY c - getY p]

computeColorRGBA :: (Autofloat a) => a -> a -> a -> a -> Color
computeColorRGBA r g b a = makeColor' r g b a

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

-------------------------------------
-- Test computation rewrite

-- | Possible computation input types (internal types)
data TypeIn a = TNum a
              | TBool Bool
              | TStr String
              | TInt Integer
              | TPt a
              | TPath [Pt2 a]
              | TColor Color
              | TStyle String -- dotted, etc.
     deriving (Eq, Show, Data, Typeable)

-- Each computation uses this rng (not super high-quality)
compRng :: StdGen
compRng = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

type CompFn a = (Autofloat a) => [TypeIn a] -> [Obj' a] -> TypeIn a
type CompFnOn a = [TypeIn a] -> [Obj' a] -> TypeIn a

-- | 'compFuncDict' stores a mapping from the name of computations to the actual implementation
compFuncDict :: (Autofloat a) => M.Map String (CompFnOn a)
compFuncDict = M.fromList flist
    where flist = [
                    ("computeRadiusAsFrac", computeRadiusAsFrac'), -- TODO change the primes
                    ("computeRadiusToMatch", computeRadiusToMatch')
                  ]

-- typecheck :: [String] -> [String] -> [TypeIn a] -> [Obj' a]

-- TODO Generate the typechecking with Template Haskell
-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac' :: CompFn a
computeRadiusAsFrac' [TNum mag] [C' circ] = TNum $ computeRadiusAsFrac circ mag
computeRadiusAsFrac' _ _ = error "unexpected # or type or argument in computeRadiusAsFrac"

-- Compute the radius of the circle to lie on a point
computeRadiusToMatch' :: CompFn a
computeRadiusToMatch' [] [C' c, P' p] = TNum $ computeRadiusToMatch c p
computeRadiusToMatch' _ _ = error "unexpected # or type or argument in computeRadiusToMatch"
