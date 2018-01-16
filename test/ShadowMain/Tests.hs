module ShadowMain.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import ShadowMain

tests :: TestTree
tests = testGroup "ShadowMain tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ 
          ]

-- TODO: write a pure version of ShadowMain that parses the files and runs the optimization until convergence
-- Module: topic: function: property
unitTests = testGroup "Unit tests" 
          [ 
          ]
