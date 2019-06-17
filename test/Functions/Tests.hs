module Functions.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Debug.Trace
import Data.Fixed

import Functions
import Shapes
import Utils

tests :: TestTree
tests = testGroup "Functions tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]



group1 :: TestTree
group1 = testGroup "Group"
                [ 
                ]

--------

scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ 
          ]

-- Module: topic: function: property
unitTests :: TestTree
unitTests = testGroup "Unit tests" 
          [ group1
          ]
