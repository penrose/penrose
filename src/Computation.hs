-- | The "computation" module contains a library of computations to be used in Style files.
{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, UnicodeSyntax, NoMonomorphismRestriction, FlexibleContexts #-}
module Computation where
import Shapes
import Utils
import Functions
import qualified Data.Map.Strict as M

-- I guess one solution is to register every single different function type as you write it
-- and pattern-match on it later? Doesn't deal well with polymorphism (all type variables need to go in the datatype

data Computation = ComputeColor (() -> Color) | TestNone 

-- | 'computationDict' stores a mapping from the name of computation to the actual implementation
computationDict :: M.Map String Computation
computationDict = M.fromList flist
    where
        flist :: [(String, Computation)] 
        flist = [
                        ("computeColor", ComputeColor computeColor) -- pretty verbose
                ]

-- TODO this is duplicated from Style to avoid a circular import
-- need to split up the Style grammar and parser 
data Color = RndColor | Colo
          { redc :: Float
          , greenc :: Float
          , bluec :: Float
          , opacityc :: Float }
          deriving (Show)

-- | No arguments for now, to avoid typechecking
computeColor :: () -> Color
computeColor () = Colo { redc = 0, greenc = 0, bluec = 0 * 0, opacityc = 50 }
