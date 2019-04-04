module Functions.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Functions
import Shapes

tests :: TestTree
tests = testGroup "Functions tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

--------

id_compose_id :: Assertion
id_compose_id = composeTransform idH idH @?= idH

-- TODO: sample random matrix
-- then test A * id = id * A = A

transformsGroup :: TestTree
transformsGroup = testGroup "Transforms"
                [ testCase "idH * idH = idH" id_compose_id
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
          [ transformsGroup
          ]
