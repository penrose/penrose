module ShadowMain.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import System.IO.Unsafe (unsafePerformIO)
import ShadowMain
import Shapes
import Runtime
import Debug.Trace

tests :: TestTree
tests = testGroup "ShadowMain tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

-- Utils

failed :: Assertion
failed = False @?= True

type Point = (Float, Float)

-- TODO: used to check equality because label sizes depend on choice of renderer (e.g. snap)
locs :: [Obj] -> [Point]
locs = map (\o -> (getX o, getY o))

-----------------------------
-- Regression tests based on examples

---------------
twopoints_init_truth, twopoints_final_truth :: [Obj]

twopoints_init_truth = [L (Label {xl = -175.52109, yl = 324.19635, wl = 0.0, hl = 0.0, textl = "a", sell = False, namel = "Label_a"}),L (Label {xl = 256.43604, yl = 219.0672, wl = 0.0, hl = 0.0, textl = "b", sell = False, namel = "Label_b"}),P (Pt {xp = 9.113647, yp = -13.938477, selp = False, namep = "a"}),P (Pt {xp = 109.11365, yp = 186.06152, selp = False, namep = "b"})]

twopoints_final_truth = [L (Label {xl = 9.966248, yl = 20.311262, wl = 6.65625, hl = 16.359375, textl = "a", sell = False, namel = "Label_a"}),L (Label {xl = 109.96625, yl = 220.31126, wl = 6.9375, hl = 16.359375, textl = "b", sell = False, namel = "Label_b"}),P (Pt {xp = -3.0389898e-2, yp = 0.25816754, selp = False, namep = "a"}),P (Pt {xp = 99.96961, yp = 200.25816, selp = False, namep = "b"})]

twopoints_init :: Maybe State
twopoints_init = let (subName, styName) = ("src/sub/twopoints.sub", "src/sty/twopoints.sty") in
                 unsafePerformIO $ mainRetInit subName styName

twopoints_init_test :: Assertion
twopoints_init_test = case twopoints_init of
                      Nothing -> failed
                      Just init -> {-traceShow (objs init) $ -}(objs init == twopoints_init_truth) @?= True

twopoints_final_test :: Assertion
twopoints_final_test = case twopoints_init of
                       Nothing -> failed
                       Just initState -> let final = mainRetFinal initState in
                                         trace ("final objects\n" ++ show (objs final)) $ 
                                         ((locs $ objs final) == locs twopoints_final_truth) @?= True
---------------

scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ 
          ]

-- TODO: write a pure version of ShadowMain that parses the files and runs the optimization until convergence
-- Module: topic: function: property
unitTests = testGroup "Unit tests" 
          [ testCase "regression test: two points, one computed: init state" twopoints_init_test,
            testCase "regression test: two points, one computed: final state" twopoints_final_test
          ]
