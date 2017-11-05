-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Computation where
import Shapes
import Utils
import Functions
import qualified Data.Map.Strict as M
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace

-- Temporary solution: register every single different function type as you write it
-- and pattern-match on it later.
-- Fix typechecking: applyComputation in Runtime is essentially doing ad-hoc typechecking
-- TODO figure out how arguments work
-- Doesn't deal well with polymorphism (all type variables need to go in the datatype)

data Computation = ComputeColor (() -> Color) 
                   | ComputeColorArgs (String -> Float -> Color) 
                   | ComputeRadius (Obj -> Float -> Float) 
                   | ComputeColorRGBA (Float -> Float -> Float -> Float -> Color) 
                   | TestNone 

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
-- | All functions must be registered
computationDict :: M.Map String Computation
computationDict = M.fromList flist
    where
        flist :: [(String, Computation)] 
        flist = [
                        ("computeColor", ComputeColor computeColor), -- pretty verbose 
                        ("computeColor2", ComputeColor computeColor2),
                        ("computeColorArgs", ComputeColorArgs computeColorArgs),
                        ("computeRadius", ComputeRadius computeRadius),
                        ("computeColorRGBA", ComputeColorRGBA computeColorRGBA)
                ]

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
computeRadius :: Obj -> Float -> Float
computeRadius (C circ) mag = trace ("computeRadius") $ 0.5 * (r circ)
computeRadius _ _ = error "computeRadius expected circle, got other obj (TODO handle cases)"

computeColorRGBA :: Float -> Float -> Float -> Float -> Color
computeColorRGBA = makeColor
