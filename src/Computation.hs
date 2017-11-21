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

data Computation a = ComputeColor (() -> Color) 
                   | ComputeColorArgs (String -> Float -> Color) 
                   | ComputeRadius (Circ -> Float -> Float) 
                   | ComputeColorRGBA (Float -> Float -> Float -> Float -> Color) 
                   | ComputeSurjection (StdGen -> Integer -> Point -> Point -> ([Point], StdGen))
                   | ComputeSurjectionBbox (StdGen -> Integer -> SolidArrow -> SolidArrow -> ([Point], StdGen))
                   | TestPoly (Circ' a -> a)
                   | TestNone 

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
-- | All functions must be registered
computationDict :: Floating a => M.Map String (Computation a)
computationDict = M.fromList flist
    where
        flist :: Floating a => [(String, Computation a)] 
        flist = [
                        ("computeColor", ComputeColor computeColor), -- pretty verbose 
                        ("computeColor2", ComputeColor computeColor2),
                        ("computeColorArgs", ComputeColorArgs computeColorArgs),
                        ("computeRadiusAsFrac", ComputeRadius computeRadiusAsFrac),
                        ("computeColorRGBA", ComputeColorRGBA computeColorRGBA),
                        ("computeSurjection", ComputeSurjection computeSurjection),
                        ("computeSurjectionBbox", ComputeSurjectionBbox computeSurjectionBbox),
                        ("testPoly", TestPoly testPoly)
                ]

testPoly :: Floating a => Circ' a -> a
testPoly c = 5.5

-- Generate n random values uniformly randomly sampled from interval and return generator.
randomsIn :: StdGen -> Integer -> (Float, Float) -> ([Float], StdGen)
randomsIn g 0 _        =  ([], g)
randomsIn g n interval = let (x, g') = randomR interval g in -- First value
                         let (xs, g'') = randomsIn g' (n - 1) interval in -- Rest of values
                         (x : xs, g'')

-- Given a generator, number of points, and lower left and top right of bbox, return points for a surjection.
-- Points generated lie in the bbox given, whether in math space or screen space
-- TODO pass randomness around in Runtime
computeSurjection :: StdGen -> Integer -> Point -> Point -> ([Point], StdGen)
computeSurjection g numPoints (lowerx, lowery) (topx, topy) = 
                  if numPoints < 2 then error "Surjection needs to have >= 2 points" 
                  else let (xs_inner, g') = randomsIn g (numPoints - 2) (lowerx, topx) in
                       let xs = lowerx : xs_inner ++ [topx] in -- Include endpts so function covers domain
                       let xs_increasing = sort xs in

                       let (ys_inner, g'') = randomsIn g' (numPoints - 2) (lowery, topy) in 
                       let ys = lowery : ys_inner ++ [topy] in -- Include endpts so function is onto
                       let ys_perm = shuffle' ys (length ys) g'' in -- Random permutation. TODO return g3?

                       (zip xs_increasing ys_perm, g'') -- len xs == len ys

-- this function could be more general, taking in two objects and computing their bounding box
computeSurjectionBbox :: StdGen -> Integer -> SolidArrow -> SolidArrow -> ([Point], StdGen)
computeSurjectionBbox g n a1 a2 = let xs = [startx a1, endx a1, startx a2, endx a2] in
                                  let ys = [starty a1, endy a1, starty a2, endy a2] in
                                  let lower_left = (minimum xs, minimum ys) in
                                  let top_right = (maximum xs, maximum ys) in
                                  -- trace ("surjection bbox " ++ show lower_left ++ " " ++ show top_right) $
                                  computeSurjection g n lower_left top_right
                      
-- | No arguments for now, to avoid typechecking
-- Does this only work in gloss?
computeColor :: () -> Color
-- computeColor () = Colo { redc = 0, greenc = 0, bluec = 0 * 0, opacityc = 50 }
computeColor () = makeColor 0.5 0.1 (0.2 / 3) 0.5

computeColor2 :: () -> Color
computeColor2 () = makeColor (0.1 * 0.5) 0.1 0.5 0.5

computeColorArgs :: String -> Float -> Color
computeColorArgs ref1 mag = trace ("computeColorArgs " ++ ref1) $ 
                                 makeColor (scale mag) (scale mag) (scale mag) 0.5
                 where scale c = c * 0.1

-- Compute the radius of the inner set to always be half the radius of the outer set, overriding optimization.
computeRadiusAsFrac :: Circ -> Float -> Float
computeRadiusAsFrac circ mag = trace ("computeRadiusAsFrac") $ mag * (r circ)

computeColorRGBA :: Float -> Float -> Float -> Float -> Color
computeColorRGBA = makeColor
