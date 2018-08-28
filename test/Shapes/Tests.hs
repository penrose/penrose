module Shapes.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Shapes
import System.Random

tests :: TestTree
tests = testGroup "Shapes tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

gen :: StdGen
gen = mkStdGen seed
    where seed = 16 -- deterministic RNG with seed

circ_get_set_id :: Assertion
circ_get_set_id = let (cir, g) = defaultShapeOf gen circType in
                  let x = 10.0 in
                  let res = getX $ setX (FloatV x) cir in
                  (res == x) @?= True
        
scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ 
          ]

-- Module: topic: function: property
unitTests = testGroup "Unit tests" 
          [ testCase "one kind of object, get/set identity" circ_get_set_id
          ]
