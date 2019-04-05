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
id_compose_id = idH # idH @?= idH

rotate_2pi_id :: Assertion
rotate_2pi_id = rotationM (2 * pi) @?= idH

rotate_rev :: Float -> Bool
rotate_rev radians = rotationM radians # rotationM (-radians) == idH

-- TODO: 
-- Change record to map
-- Implement generic map function
-- Check WRT tolerance

-- forall A, A * id = id * A = A
-- rotate by angle, then rotate by angle = rotate by 2 * angle
-- translate by x, then translate by -x = id
-- scale by c, then scale by -c = id
-- do we need any checks for numerical stability?

-- test the canonical order: scale(w, h) then rotate(theta) then translate(x, y) = ???
-- test skewing

-- Should the matrix be a map, not a record??

transformsGroup :: TestTree
transformsGroup = testGroup "Transforms"
                [ testCase "idH * idH = idH" id_compose_id,
                  testCase "rotate by 2pi = idH" rotate_2pi_id
                ]

--------

scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ QC.testProperty "forall x, rotate(x) . rotate (-x) = idH" rotate_rev
          ]

-- Module: topic: function: property
unitTests :: TestTree
unitTests = testGroup "Unit tests" 
          [ transformsGroup
          ]
