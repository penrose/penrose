module Server.Tests
  ( tests
  ) where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

import           Penrose.Server

tests :: TestTree
tests = testGroup "Server tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" []

-- Module: topic: function: property
unitTests = testGroup "Unit tests" []
