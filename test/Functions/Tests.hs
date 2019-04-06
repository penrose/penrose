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


eps :: Float
eps = 10 ** (-3) -- Kind of arbitrarily picked so the tests pass. Not sure about the right precision

hmEq :: HMatrix Float -> HMatrix Float -> Bool
hmEq t1 t2 = hmDiff t1 t2 <= eps

ptsEq :: Pt2 Float -> Pt2 Float -> Bool
ptsEq (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2) < eps -- Should use a different eps

id_compose_id :: Assertion
id_compose_id = idH # idH @?= idH

rotate_2pi_id :: Assertion
rotate_2pi_id = hmEq idH (rotationM (2 * pi)) @?= True

rotate_rev :: Float -> Bool
rotate_rev radians = hmEq idH (rotationM radians # rotationM (-radians)) == True

rotate_twice :: Float -> Bool
rotate_twice radians = hmEq (rotationM (2 * radians)) (rotationM radians # rotationM (radians)) 
                       == True

translate_id :: Float -> Float -> Bool
translate_id x y = hmEq idH (translationM (x, y) # translationM (-x, -y)) == True

scale_id :: Float -> Float -> Bool
scale_id cx cy = let res = (scalingM (1/cx, 1/cy) # scalingM (cx, cy)) in
                 if cx /= 0.0 && cy /= 0.0 then
                 hmEq idH res == True
                 else True -- Ignore scaling by 0

id_mul :: (Float, Float, Float, Float, Float, Float) -> Bool
id_mul (x1, x2, x3, x4, x5, x6) = let m = listToHm [x1, x2, x3, x4, x5, x6] in
                                  hmEq (idH # m) (m # idH) && hmEq (m # idH) m

-- Not true in general
rot_tr :: Float -> Bool
rot_tr x = if x == 0.0 then True 
           else ptsEq ((rotationM (pi/2) # translationM (x, 0)) ## (0, 0)) 
                      (translationM (0, x) ## (0, 0)) == True

trs1 :: Assertion
trs1 = ptsEq ((translationM (-1, 1) # rotationM (pi/2) # scalingM (2, 1)) ## (1, 0))
             (-1,3.0) @?= True

-- Starting with some parameters, we show we can extract the same (?) parameters from the composed matrix
-- It's lossy though, so some tests might fail when the angle can't be reconstructed
params_matrix_id' :: (Float, Float, Float, Float, Float) -> Bool
params_matrix_id' p@(sx, sy, theta, dx, dy) =
       let p0 = [sx, sy, theta `mod'` (2.0 * pi), dx, dy]
           -- might not work with theta out of range [-2pi, 2pi]
           m = paramsToMatrix p
           (sx', sy', theta', dx', dy') = paramsOf m 
           p' = [sx', sy', theta', dx', dy']
       in if sx < eps || sy < eps then True -- don't scale by 0
          else norm (p0 -. p') < eps

-- This also can't test it fully though, for example if the matrix didn't result from 
-- the canonical scale-then-rotate-then-translate
-- e.g. if it consists only of a shear: m = listToHm [1.0,0.0,0.0,1.0,1.0,0.0]
params_matrix_eq :: (Float, Float, Float, Float, Float, Float) -> Bool
params_matrix_eq (x1, x2, x3, x4, x5, x6) = let m = listToHm [x1, x2, x3, x4, x5, x6] in
                                  if x1 < eps || x5 < eps then True -- don't scale by 0
                                  else hmEq m (paramsToMatrix $ paramsOf m)

-- TODO
-- do we need any checks for numerical stability?
-- test the canonical order: scale(w, h) then rotate(theta) then translate(x, y) = ???
-- test skewing
-- test things that commute
-- test things that don't commute

transformsGroup :: TestTree
transformsGroup = testGroup "Transforms"
                [ testCase "idH * idH = idH" id_compose_id,
                  testCase "rotate by 2pi = idH" rotate_2pi_id,
                  testCase "translate, rotate, scale example" trs1
                ]

--------

scProps = testGroup "(checked by SmallCheck)" 
          [ 
          ]

qcProps = testGroup "(checked by QuickCheck)" 
          [ QC.testProperty "forall x, rotate(x) . rotate (-x) = idH" rotate_rev,
            QC.testProperty "rotate t * rotate -t = id" rotate_rev,
            QC.testProperty "rotate 2t = rotate t * rotate t" rotate_twice,
            QC.testProperty "translate v * translate -v = idH" translate_id,
            QC.testProperty "scale (cx,cy) * scale (-cx, -cy) = idH" scale_id,
            QC.testProperty "id * A = A * id = A" id_mul,
            QC.testProperty "translate then rotate CCW can equal a translation up" rot_tr,
            QC.testProperty "(params -> matrix -> solve for params) ~ params" params_matrix_id',
            QC.testProperty "matrix ~ (matrix -> params -> matrix)" params_matrix_eq
          ]

-- Module: topic: function: property
unitTests :: TestTree
unitTests = testGroup "Unit tests" 
          [ transformsGroup
          ]
