module Functions.Tests
  ( tests
  ) where

import           Data.Fixed
import           Debug.Trace
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Penrose.Functions
import           Penrose.Shapes
import           Penrose.Util

tests :: TestTree
tests = testGroup "Functions tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

group1 :: TestTree
group1 = testGroup "Group" []

--------
scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" []

-- Module: topic: function: property
unitTests :: TestTree
unitTests = testGroup "Unit tests" [group1]
