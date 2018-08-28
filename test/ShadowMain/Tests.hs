module ShadowMain.Tests (tests) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import System.IO.Unsafe (unsafePerformIO)
import ShadowMain
import Shapes
import Debug.Trace

tests :: TestTree
tests = testGroup "ShadowMain tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

------------------------------- Utils

failed :: Assertion
failed = False @?= True

-- type V2 = (Float, Float)
-- type TestInfo = (String, String, String, String, [Obj], [Obj])

-- TODO: For final state, only check location equality because label sizes depend on renderer (e.g. snap)
-- locs :: [Obj] -> [V2]
-- locs = map (\o -> (getX o, getY o))

-- Given a test name, Substance file, Style file, and ground truth initial and final states,
-- generates two unit tests: one for the initial states and one for the final states.
-- TODO: tests depend on hardcoded parameters in Runtime: initRng (random seed) and choice of initial state
-- TODO: port mainRetInit and mainRetFinal
-- genTests :: TestInfo -> [TestTree]
-- genTests (testName, subName, styName, dsllName, initTruth, finalTruth) =
        -- []

        -- let nameInit = testName ++ ": init state" in
        -- let nameFinal = testName ++ ": final state" in
        -- let initState = unsafePerformIO $ mainRetInit subName styName dsllName in
        -- let (aInit, aFinal) = case initState of
        --                       Nothing -> (failed, failed)
        --                       Just init -> let testInit = (objs init == initTruth) @?= True in
        --                                    let final = mainRetFinal init in
        --                                    let testFinal = trace ("final objs: " ++ (show $ objs final)) $
        --                                                    ((locs $ objs final) == locs finalTruth) @?= True in
        --                                    (testInit, testFinal) in
        -- [testCase nameInit aInit, testCase nameFinal aFinal]

unitTests = testGroup "Unit tests" []

scProps = testGroup "(checked by SmallCheck)" []

qcProps = testGroup "(checked by QuickCheck)" []

-----------------------------
-- Regression tests based on examples
-- To add a test, add a new t :: TestInfo, and add t to the all_test_info list.
-- TODO: add ability to read from file

{-
twopoints_computed :: TestInfo
twopoints_computed = ("two points, one computed",
                   "src/sub/twopoints.sub", "src/sty/twopoints.sty",
                   "src/dsll/setTheory.dsl",
               [L (Label {xl = -175.52109, yl = 324.19635, wl = 0.0, hl = 0.0, textl = "a", sell = False, namel = "Label_a"}),L (Label {xl = 256.43604, yl = 219.0672, wl = 0.0, hl = 0.0, textl = "b", sell = False, namel = "Label_b"}),P (Pt {xp = 9.113647, yp = -13.938477, selp = False, namep = "a"}),P (Pt {xp = 109.11365, yp = 186.06152, selp = False, namep = "b"})],
                [L (Label {xl = 9.966248, yl = 20.311262, wl = 6.65625, hl = 16.359375, textl = "a", sell = False, namel = "Label_a"}),L (Label {xl = 109.96625, yl = 220.31126, wl = 6.9375, hl = 16.359375, textl = "b", sell = False, namel = "Label_b"}),P (Pt {xp = -3.0389898e-2, yp = 0.25816754, selp = False, namep = "a"}),P (Pt {xp = 99.96961, yp = 200.25816, selp = False, namep = "b"})])
-}

-- all_test_info :: [TestInfo]
-- all_test_info = [
                -- FIXME: commented out because the tests no longer pass
                -- twopoints_computed, venn_subset, tree_subset
                -- ]
