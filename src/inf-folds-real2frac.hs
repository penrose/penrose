{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
module Main where

import qualified Data.List as List

main :: IO ()
main = do

     ---- here's our control; it produces a Float.
     ---- It spins forever, using constant memory.
     --let !a = List.foldl' (+)
     --                     (0 :: Float)
     --                     (realToFrac <$> [(0 :: Float)..])

     -- here's our test; it produces a value of type (Floating a => a)
     -- It evaluates immediately!
     let !a = List.foldl' (+)
                          0
                          (realToFrac <$> [(0 :: Float)..])

     print ()
