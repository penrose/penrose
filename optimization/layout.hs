{-# LANGUAGE Rank2Types, UnicodeSyntax #-}
-- for autodiff, requires passing in a polymorphic fn

import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Linear.V2 -- vectors
import Linear.V4
import Linear.Metric
import Linear.Vector
import Numeric.AD
import GHC.Float -- float <-> double conversions

main = play
       (InWindow "optimization-based layout" -- display mode, window name
                  (picWidth, picHeight)   -- size
                  (10, 10))    -- position
       white                   -- background color
       50                     -- number of simulation steps to take for each second of real time
       initState                   -- the initial world, defined as a type below
       picOf                   -- fn to convert world to a pic
       handler                 -- fn to handle input events
       step                    -- step the world one iteration; passed period of time (in secs) to be advanced

picWidth :: Int
picWidth = 800

picHeight :: Int
picHeight = 700

----------- Defining types for the state of the world.
-- Objects can be Circs (circles) or Labels. Each has a size and position.
-- The state of the world is simply a list of Objects.

-- Circs and Labels satisfy the Located and Selectable typeclasses. So does Obj.
-- So you can do something like `getX o` on any o (an object) without having to worry
-- unpacking it as a circle or label.
class Located a where
      getX :: a -> Float
      getY :: a -> Float
      setX :: Float -> a -> a
      setY :: Float -> a -> a

class Selectable a where
      select :: a -> a
      deselect :: a -> a
      selected :: a -> Bool

data Circ = Circ { xc :: Float
                 , yc :: Float
                 , r :: Float
                 , selc :: Bool } -- is the circle currently selected? (mouse is dragging it)

instance Located Circ where
         getX c = xc c
         getY c = yc c
         setX x c = c { xc = x }
         setY y c = c { yc = y }

instance Selectable Circ where
         select x = x { selc = True }
         deselect x = x { selc = False }
         selected x = selc x

data Label = Label { xl :: Float
                   , yl :: Float
                   , textl :: String
                   , scalel :: Float  -- calculate h,w from it
                   , sell :: Bool } -- selected label

instance Located Label where
         getX l = xl l
         getY l = yl l
         setX x l = l { xl = x }
         setY y l = l { yl = y }

instance Selectable Label where
         select x = x { sell = True }
         deselect x = x { sell = False }
         selected x = sell x

data Obj = C Circ | L Label -- | Label | Point | Line // is there a better way to do this?

-- is there some way to reduce the top-level boilerplate?
instance Located Obj where
         getX o = case o of
                 C c -> getX c
                 L l -> getX l
         getY o = case o of
                 C c -> getY c
                 L l -> getY l
         setX x o = case o of
                C c -> C $ setX x c
                L l -> L $ setX x l
         setY y o = case o of
                C c -> C $ setY y c
                L l -> L $ setY y l

instance Selectable Obj where
         select x = case x of
                C c -> C $ select c
                L l -> L $ select l
         deselect x = case x of
                C c -> C $ deselect c
                L l -> L $ deselect l
         selected x = case x of
                C c -> selected c
                L l -> selected l

-- State of the world
data State = State { objs :: [Obj]
                   , down :: Bool -- left mouse button is down (dragging)
                   , rng :: StdGen } -- random number benerator

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 11 -- deterministic RNG with seed

rad :: Floating a => a
rad = 100 -- TODO don't hardcode into constant
clamp1D y = if clampflag then 0 else y

-- Initial state of the world.
-- TODO randomly sample s0
initState :: State
initState = State { objs = objsInit, down = False, rng = initRng }
          where objsInit = [c1, c2] -- only handles two objects, with a non-working case for three
                -- TODO handle one obj...
                c1 = C $ Circ { xc = -100, yc = clamp1D 200, r = rad, selc = False }
                c2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad-50, selc = False }
                c3 = C $ Circ { xc = 300, yc = clamp1D 200, r = rad+50, selc = False }
                l1 = L $ Label { xl = -100, yl = clamp1D 200, textl = "B1", scalel = 0.2, sell = False }

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 :: Float
pw2 = picWidth `divf` 2

ph2 :: Float
ph2 = picHeight `divf` 2

widthRange = (-pw2, pw2)
heightRange = (-ph2, ph2)

------------- The "Style" layer: render the state of the world.
renderCirc :: Circ -> Picture
renderCirc c = color scolor $ translate (xc c) (yc c) $ circle (r c)
           where scolor = if selected c then green else light violet

renderLabel :: Label -> Picture
renderLabel l = color scolor $ translate (xl l) (yl l) $ scale 0.2 0.2 $ text (textl l)
            where scolor = if selected l then green else light violet

renderObj :: Obj -> Picture
renderObj (C circ) = renderCirc circ
renderObj (L label) = renderLabel label

picOfState :: State -> Picture
picOfState s = Pictures $ map renderObj (objs s)

picOf :: State -> Picture
picOf s = Pictures [picOfState s, objectiveTxt]
    where lineX = Line [(-pw2, 0), (pw2, 0)] -- unused
          lineY = Line [(0, -ph2), (0, ph2)]
          objectiveTxt = translate (-pw2+50) (ph2-50) $ scale 0.1 0.1
                         $ text "objective: get close to the center"

------- Sampling the state subject to a constraint. Currently not used since we are doing unconstrained optimization.

-- generate an infinite list of sampled elements
-- keep the last generator for the "good" element
genMany :: RandomGen g => g -> (g -> (a, g)) -> [(a, g)]
genMany gen genOne = iterate (\(c, g) -> genOne g) (genOne gen)

-- take the first element that satisfies the condition
-- not the most efficient impl. also assumes infinite list s.t. head always exists
crop :: RandomGen g => (a -> Bool) -> [(a, g)] -> (a, g)
crop cond xs = --(takeWhile (not . cond) (map fst xs), -- drop gens
                head $ dropWhile (\(x, _) -> not $ cond x) xs -- keep good's gen

-- randomly sample location
-- TODO deal with circle and label separately, and take into account bbox
sampleCoord :: Located a => RandomGen g => g -> a -> (a, g)
sampleCoord gen o = (setX x' $ setY (clamp1D y') o, gen2)
        where (x', gen1) = randomR widthRange gen
              (y', gen2) = randomR heightRange gen1

-- sample each object independently, threading thru gen
stateMap :: RandomGen g => g -> (g -> a -> (b, g)) -> [a] -> ([b], g)
stateMap gen f [] = ([], gen)
stateMap gen f (x:xs) = let (x', gen') = f gen x in
                        let (xs', gen'') = stateMap gen' f xs in
                        (x' : xs', gen'')

-- sample a state
genState :: RandomGen g => [Obj] -> g -> ([Obj], g)
genState shapes gen = stateMap gen sampleCoord shapes

-- sample entire state at once until constraint is satisfied
-- TODO doesn't take into account pairwise constraints or results from objects sampled first, sequentially
sampleConstrainedState :: RandomGen g => g -> [Obj] -> ([Obj], g)
sampleConstrainedState gen shapes = (state', gen')
       where (state', gen') = crop constraint states
             states = genMany gen (genState shapes)
             -- init state params are ignored; we just need to know what kinds of objects are in it
--------------- Handle user input. "handler" is the main function here.
-- Whenever the library receives an input event, it calls "handler" with that event
-- and the current state of the world to handle it.

bbox = 60 -- TODO put all flags and consts together
-- hacky bounding box of label

dist :: Point -> Point -> Float -- distance
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- hardcode bbox of label at the center
-- TODO properly get bbox; rn text is centered at bottom left
inObj :: (Float, Float) -> Obj -> Bool
inObj (xm, ym) (L o) = abs (xm - getX o) <= bbox && abs (ym - getY o) <= bbox -- is label
inObj (xm, ym) (C o) = dist (xm, ym) (xc o, yc o) <= r o -- is circle

-- TODO "in object" tests
-- TODO press key to gradient descent step
-- for more on these constructors, see docs: https://hackage.haskell.org/package/gloss-1.10.2.3/docs/Graphics-Gloss-Interface-Pure-Game.html
handler :: Event -> State -> State
handler (EventKey (MouseButton LeftButton) Down _ (xm, ym)) s =
        s { objs = objsFirstSelected, down = True }
        -- so that clicking doesn't select all overlapping objects in bbox
        -- foldl will reverse the list each time, so a diff obj can be selected
        -- foldr will preserve the list order, so objects are stepped consistently
        where (objsFirstSelected, _) = foldr (flip $ selectFirstIfContains (xm, ym)) ([], False) (objs s)
              selectFirstIfContains (x, y) (xs, alreadySelected) o =
                                    if alreadySelected || (not $ inObj (x, y) o) then (o : xs, alreadySelected)
                                    else (select (setX xm $ setY ym o) : xs, True)
-- dragging mouse when down
-- if an object is selected, then if the collection of objects with the object moved satisfies the constraint,
-- then move the object to mouse position
-- TODO there's probably a better way to implement that
handler (EventMotion (xm, ym)) s =
        if down s then s { objs = map (ifSelectedMoveToConstrained (xm, ym)) (objs s), down = down s }
        else s
        where ifSelectedMoveToConstrained (xm, ym) o = if selected o && constraint objsWithSelectedMoved
                                               -- the constraint would be satisfied on the new mouse position
                                                      then setX xm $ setY (clamp1D ym) o else o
              objsWithSelectedMoved = map (ifSelectedMoveTo (xm, ym)) (objs s)
              ifSelectedMoveTo (xm, ym) o = if selected o then setX xm $ setY (clamp1D ym) o else o

-- button released, so deselect all objects
handler (EventKey (MouseButton LeftButton) Up _ _) s =
        s { objs = map deselect $ objs s, down = False }

-- if you press a key while down, then the handler resets the entire state (then Up will just reset again)
handler (EventKey (Char 'r') Up _ _) s =
        State { objs = objs', down = False, rng = rng' }
        where (objs', rng') = sampleConstrainedState (rng s) (objs s)
handler _ s = s

----------- Stepping the state the world via gradient descent.
 -- First, miscellaneous helper functions.

-- Clamp objects' positions so they don't go offscreen.
-- TODO clamp needs to take into account bbox of object
clampX :: Double -> Double
clampX x = if x < -pw2' then -pw2' else if x > pw2' then pw2' else x
           where pw2' = float2Double pw2

clampY :: Double -> Double
clampY y = if y < -ph2' then -ph2' else if y > ph2' then ph2' else y
           where ph2' = float2Double ph2

-- TODO hack so I don't have to deal with pairwise derivatives of an arbitrary-length list.
firstTwo :: [a] -> (a, a)
firstTwo (x1 : x2 : _) = (x1, x2) -- Unsafe pattern-match.

-- Some debugging functions.
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

-- To send output to a file, do ./EXECUTABLE 2> FILE.txt
tr :: Show a => String -> a -> a
tr s x = trace "---" $ trace s $ traceShowId x -- prints in left to right order

-- These functions are now obsolete, since we aren't doing constrained optimization.
-- Still, they might be useful later.
constraint = if constraintFlag then noOverlap else \x -> True
noOverlapPair :: Circ -> Circ -> Bool
noOverlapPair c1 c2 = dist (xc c1, yc c1) (xc c2, yc c2) > r c1 + r c2

-- return true iff satisfied
-- TODO deal with labels and more than two objects
noOverlap :: [Obj] -> Bool
noOverlap ((C c1) : (C c2) : (C c3) : _) = noOverlapPair c1 c2 && noOverlapPair c2 c3 && noOverlapPair c1 c3
-- noOverlap _ _ = True

-- Type aliases for shorter type signatures.
type GradFn' a = Time -> a -> a -> a -> a -> (Time, a, a, a, a) -- old type
type TimeInit = Float
type Time = Double
type Vec4 a = (a, a, a, a) -- TODO use V4
type ObjFn a = forall a . Floating a => Vec4 a -> a
type GradFn a = forall a . Floating a => Vec4 a -> Vec4 a -- TODO clean up input types

toV :: Vec4 a -> V4 a
toV (x1, x2, y1, y2) = V4 x1 x2 y1 y2

fromV :: V4 a -> Vec4 a
fromV (V4 x1 x2 y1 y2) = (x1, x2, y1, y2)

listOf (a, b, c, d) = [a, b, c, d]
vecOf [a, b, c, d] = (a, b, c, d) -- incomplete pattern match

tupMap f (a, b, c, d) = (f a, f b, f c, f d)
vMap f (V4 a b c d) = V4 (f a) (f b) (f c) (f d)

-------- Step the world by one timestep (provided by the library).
-- gloss operates on floats, but the optimization code should be done with doubles, so we
-- convert float to double for the input and convert double to float for the output.
step :: Floating a => TimeInit -> State -> State
step t s = -- if down s then s -- don't step when dragging
            if stepFlag then s { objs = stepObjs (float2Double t) (objs s), down = down s} else s

-- Given the time, position, and evaluated directional derivative at the point, return the new position.
stepT :: Time -> Double -> Double -> Double
stepT dt x dfdx = x - dt * dfdx

-- This function is here to remind me to generalize the objects (currently hard-coded to 2 objects)
-- to a list of them. Otherwise, this function doesn't do anything.
-- TODO assuming all objs are the same and have same obj function, apply obj pairwise step function
-- TODO generalize
-- TODO step one object. problem is that pairwise stepping is hardcoded everywhere
stepObjs :: Time -> [Obj] -> [Obj]
stepObjs t objs@(o1 : o2 : _) = if constraint objs' then objs' else objs
         where (o1', o2') = stepObjsPairwise t (o1, o2)
               objs' = [o1', o2']
-- Old code for three objects.
-- stepObjs t objs@(o1 : o2 : o3 : _) = if constraint objs' then objs' else objs
--          where (o1', _) = stepObjsPairwise t (o1, o2) -- uses already stepped objs, not original state
--                (o2', _) = stepObjsPairwise t (o2, o3) -- TODO ^ hack bc the gradients aren't fns of the others
--                (o3', _) = stepObjsPairwise t (o3, o1) -- TODO order matters!
--                objs' = [o1', o2', o3']

-- Layer of stepping relative to actual objects (their sizes, properties, bbox) and top-level bbox
-- step only if the constraint on the state is satisfied
-- the state will be stuck if the constraint starts out unsatisfied.
-- TODO let GD attempt to satisfy constraint
-- note the float <-> double conversions
stepObjsPairwise :: Time -> (Obj, Obj) -> (Obj, Obj)
stepObjsPairwise t (o1, o2) = objs'
        where (x1, y1, x2, y2) = tupMap float2Double (getX o1, getY o1, getX o2, getY o2)
              -- extract the locations of the two objs, ignoring size
              (x1', x2', y1', y2') = stepWithObjective t x1 x2 y1 y2
              -- get new positions. objective function is a global param
              (x1'c, x2'c, y1'c, y2'c) = tupMap double2Float (clampX x1', clampX x2', clampY y1', clampY y2')
              -- keep objects on canvas
              objs' = (setX x1'c $ setY y1'c o1, setX x2'c $ setY y2'c o2)
              -- update object positions

-- Flags for debugging the surrounding functions.
stepFlag = True
clampflag = False
debug = True
constraintFlag = False

objFn2 = doNothing -- TODO repelInverse
-- gradFn1 = gradCenterObjs

stopEps :: Floating a => a
stopEps = 10 ** (-10)
--10 ** (-10) -- TODO magnitude of gradient is ~0.13 when x,y ~ 0.08... still large

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
-- TODO why isn't the magnitude of the gradient changing with btls?
stepWithObjective :: Time -> Double -> Double -> Double -> Double -> Vec4 Double
stepWithObjective t x1 x2 y1 y2 = if stoppingCriterion (V4 dfdx1 dfdx2 dfdy1 dfdy2) then
                                     trace "STOP" (x1, x2, y1, y2)
                                  else (stepT t' x1 dfdx1, stepT t' x2 dfdx2,
                                        stepT t' y1 dfdy1, stepT t' y2 dfdy2)
                  where (t', (dfdx1, dfdx2, dfdy1, dfdy2)) =
                                     timeAndGrad objFn1 t (x1, x2, y1, y2)
                        -- choose the timestep via backtracking (for now) line search
                        stoppingCriterion gradEval = (tr "gradient: " $ norm $ tr "grad comp:" $ gradEval) <= stopEps

-- Generalized version of above that adds the gradient fns. (Currently unused.) Need to generalize to arbirary #s of obj fns.
-- TODO generalize to add line search to this later
-- could two objective functions be working on different timesteps?
-- should each take into account the dx, dy from the previous?
{- stepWithGradFns :: Floating a => GradFn' a -> GradFn' a -> Time -> a -> a -> a -> a -> Vec4 a
stepWithGradFns f1 f2 t x1 x2 y1 y2 = (x1'', x2'', y1'', y2'')
             where (t1, dfdx1_1, dfdx2_1, dfdy1_1, dfdy2_1) = f1 t x1 x2 y1 y2
                   (t2, dfdx1_2, dfdx2_2, dfdy1_2, dfdy2_2) = f2 t x1 x2 y1 y2
                   (x1', x2', y1', y2') = (stepT t1 x1 dfdx1_1, stepT t1 x2 dfdx2_1,
                                           stepT t1 y1 dfdy1_1, stepT t1 y2 dfdy2_1)
                   (x1'', x2'', y1'', y2'') = (stepT t2 x1' dfdx1_2, stepT t2 x2' dfdx2_2,
                                               stepT t2 y1' dfdy1_2, stepT t2 y2' dfdy2_2)-}

-- a version of grad with a clearer type signature
appGrad :: Floating a => (forall a . Floating a => [a] -> a) -> [a] -> [a]
appGrad f l = grad f l

-- a version of appGrad with lists converted to 4-vectors
-- TODO generalize functions from 4-vectors
appGradV :: Floating a => (forall a . Floating a => Vec4 a -> a) -> Vec4 a -> Vec4 a
appGradV f v = vecOf $ appGrad (listVersionOf f) (listOf v)
         where listVersionOf f = f . vecOf

removeNaN' x = if isNaN x then 0 else x

removeNaN = tupMap removeNaN'

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- TODO change stepWithGradFn(s) to use this fn and its type
-- note: continue to use floats throughout the code, since gloss uses floats
-- the autodiff library requires that objective functions be polymorphic with Floating a
-- M-^ = delete indentation
timeAndGrad :: ObjFn a -> Time -> Vec4 Double -> (Time, Vec4 Double)
timeAndGrad f t (x1, x2, y1, y2) = (timestep, gradEval)
            where gradF :: Floating a => Vec4 a -> Vec4 a
                  gradF = appGradV f
                  gradEval = removeNaN $ gradF (x1, x2, y1, y2)
                  gradEvalV = toV gradEval
                  -- Use line search to find a good timestep.
                  -- Redo if it's NaN, defaulting to 0 if all NaNs. TODO
                  timestep = if not linesearch then t / 100 else -- use a fixed timestep for debugging
                             let resT = awLineSearch f duf (negated gradEvalV) (V4 x1 x2 y1 y2) in
                             if isNaN resT then tr "t returned NaN" 0 else resT
                  -- directional derivative at u, where u is the gradient (see desc. of awLineSearch)
                  duf :: Floating a => V4 a -> a
                  duf (V4 a b c d) = - ((toV $ gradF (a, b, c, d)) `dot` (toV $ gradF (a, b, c, d)))
                  -- Hardcodes descent direction to be the negated evaluated gradient vector
                  -- but we could use a different descent direction if desired
                         -- note: descent direction need not have unit norm
                         -- TODO are we using the timestep?

---------- Debugging A-W line search
{- Results
- for obj fns minx1, centerObjnoSqrt, and centerObjsNoSqrt, gradient descent seems to work instantly WITH dragging. it even works for repelInverse WITH 2 objs and dragging and clamping
- WITHOUT adjusting timestep for any of them!
- same problem for sqrt $ x1^2
- does it work for centerAndRepelAdd? with weight=0, it only centers, and when resampled, can freeze. with weight=1000 (for example), it freezes
  - w/o linesearch: NaN problems and with weight=0 just centers them (how to weight?)
- or cubicCenterAndRadius? does work w/o linesearch
  - w/o linesearch: works some of the time, but some of the time freezes
- with interval discarding and ls: cubicCenterOrRadius seems to work
  - actually, all functions seem to work "most" of the time--objs are not getting stuck--but they are disappearing?? replaced NaN with 0 and things seem okay visually

Questions
- Why are (a,b,t) converging to the same number? (Interval shrinking to 0--can't satisfy conditions?)
- Why that number? 
- Does it do that over all parameters?
- Why does it start with armijo and not wolfe, boomerang between the two, and get stuck on not wolfe?
- Can I use a library to do this?
- Can I compare to someone else's implementation? 
  This looks almost exactly the same as https://hackage.haskell.org/package/optimization-0.1.7/docs/src/Optimization-LineSearch.html#wolfeSearch
  But the update function is different (they only iterate (* gamma) on t)

Things that could be going wrong:
- weak Wolfe vs. strong Wolfe
- choice of parameters c1, c2
- choice of function (center two objects)
- choice of initial state x0
- arithmetic mistyped or logic error
- error in choice of directional derivative or search direction
- no implementation error; math is just wrong

-- Things to try: 
- print the state, duf, descent direction, and state at each iteration
- and hand-evaluate it
- compare to output from normal gradient descent?
- reread the algorithm and try to understand weak Wolfe
- get f, df, armijo, wolfe in the REPL
- try finding a point on the function where A&W are already satisfied and seeing how the code responds
- also try the simpler functions (centerObj, or even simpler ones like minimizing x) X
- failure due to sqrt? no, fails on other fns too sometimes
- hardcode stopping if failed to find point
-}

-- Parameters for Armijo-Wolfe line search
-- NOTE: must maintain 0 < c1 < c2 < 1
c1 :: Floating a => a
c1 = 0.3 -- for Armijo, corresponds to alpha in backtracking line search (see below for explanation)
-- smaller c1 = shallower slope = less of a decrease in fn value needed = easier to satisfy

c2 :: Floating a => a
c2 = 0.5 -- for Wolfe, is the factor decrease needed in derivative value
-- new directional derivative value / old DD value <= c2
-- smaller c2 = smaller new derivative value = harder to satisfy

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

isInfinity x = (x == infinity)

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods", page TODO
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2
-- TODO summarize algorithm
awLineSearch :: ObjFn a -> (forall a . Floating a => V4 a -> a) -> V4 Double -> V4 Double -> Double
awLineSearch f duf descentDir x0 =
             -- results after a&w are satisfied are junk and can be discarded
             -- drop while a&w are not satisfied OR the interval is large enough
     let (af, bf, tf) = head $ dropWhile intervalOK_or_notArmijoAndWolfe
                              $ iterate update (a0, b0, t0) in tf
          where (a0, b0, t0) = (0, infinity, 1)
                update (a, b, t) =
                       let (a', b', sat) = if not $ armijo t then tr "** not armijo" (a, t, False)
                                           else if not $ weakWolfe t then tr "** not wolfe" (t, b, False)
                                           -- remember to change both wolfes
                                           else (a, b, True) in
                       if sat then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
                       else if b' < infinity then tr "b' < infinity" (a', b', (a' + b') / 2)
                       else tr "b' = infinity" (a', b', 2 * a')
                intervalOK_or_notArmijoAndWolfe (a, b, t) = not $
                      if armijo t && weakWolfe t then tr "stop: both sat" True -- takes precedence
                      else tr "stop: interval too small" (abs (b - a) < minInterval) -- stop
                -- TODO factor out armijo from btls too? they don't need a and b, i think
                armijo t = (f $ fromV ((tr "** x0" x0) ^+^ t *^ descentDir)) <= (fAtx0 + c1 * t * dufAtx0)
                strongWolfe t = abs (duf (x0 ^+^ t *^ descentDir)) <= c2 * abs dufAtx0
                weakWolfe t = duf ((tr "**x0" x0) ^+^ t *^ (tr "descentDir" descentDir)) >= c2 * dufAtx0          
                dufAtx0 = duf x0 -- cache some results, can cache more if needed
                fAtx0 = f (fromV x0)
                minInterval = 10 ** (-5) -- stop if the interval gets too small; might not terminate

-- TODO
  -- debug current non-termination (a,b,t converge to same number?)
  -- weak or strong wolfe? ask on slack
  -- port Vec4a to V4 or to List
  -- add new objective functions (repel, centerAndRepel, cubicCenter) and test them
  -- standardize btls to have the same type signature
------

-- Parameters for backtracking line search
alpha :: Floating a => a
alpha = 0.4 -- \in (0, 0.5): fraction of decrease in f predicted by lin. extrapolation that we will accept
  -- or, fraction by which we multiply the original slope (gradient)
beta :: Floating a => a
beta = 0.3 -- \in (0, 1): btls reduces step size by beta for each failed iteration
           -- higher beta = more granular and slower search
-- TODO why is the step size always the same except for the end, where it's 1 (1 makes sense--at minimum)
t0 :: Floating a => a
t0 = 1  -- inital t specified by algo, decreases by beta each iteration

-- Implements algorithm specified in Boyd. Only works for convex functions (e.g. centerObjs is convex)
-- This is line search with only the Armijo condition.
-- TODO fix the conversions between Vec4 a and V4 a
-- vector docs: https://hackage.haskell.org/package/linear-1.20.5/docs/Linear-Vector.html
backtrackingLineSearch :: ObjFn a -> V4 Double -> V4 Double -> V4 Double -> Double
backtrackingLineSearch f gradEval descentDir x0 = tr "timestep: " $
                       head $ dropWhile timestepTooLarge $ iterate ((*) beta) t0 -- infinite list
                       where -- f(x + tu) still lies above the shallow line specified by alpha
                             timestepTooLarge t = traceShowId $
                                  ((debugF (f $ fromV ((x0) ^+^ (t *^ descentDir)))) > (debugF $ minFnVal t))
                             minFnVal t = f (fromV x0) + alpha * t * (gradEval `dot` descentDir)

------------- Objective functions.

-- needed to give this a type signature, otherwise inferred that `a = Double`
objFn1 :: Floating a => Vec4 a -> a
objFn1 = centerAndRepelAdd

linesearch = True -- TODO move these parameters back

-- simple test function
minx1 :: ObjFn a -- timestep t
minx1 (x1, x2, y1, y2) = x1^2

-- only center the first object (for debugging)
centerObjNoSqrt :: ObjFn a
centerObjNoSqrt (x1, x2, y1, y2) = x1^2 + y1^2

-- center both objects without sqrt
centerObjsNoSqrt :: ObjFn a
centerObjsNoSqrt (x1, x2, y1, y2) = x1^2 + y1^2 + x2^2 + y2^2

centerx1Sqrt :: ObjFn a -- discontinuous, timestep = 100 * t. autodiff behaves differently for this vs abs
centerx1Sqrt (x1, x2, y1, y2) = sqrt $ x1^2

centerObj :: ObjFn a -- with sqrt
centerObj (x1, x2, y1, y2) = sqrt $ x1^2 + y1^2

-- TODO: gradients are now obsolete
gradCenterObj :: GradFn a
gradCenterObj (x1, x2, y1, y2) = (dfdx1, dfdx2, dfdy1, dfdy2)
              where dfdx1 = 2 * x1
                    dfdx2 = 0
                    dfdy1 = 2 * y2
                    dfdy2 = 0

-- center both objects (2 objects hardcoded; TODO handle arbitrary numbers of them)
centerObjs :: ObjFn a -- timestep 100 * t
centerObjs (x1, x2, y1, y2) = sqrt (x1^2 + y1^2) + sqrt (x2^2 + y2^2)

-- gradient of 'f(x1, x2, y1, y2) =  sqrt(x1^2 + y1^2) + sqrt(x2^2+y2^2)'
-- evaluated at x1 x2 y1 y2
gradCenterObjs :: GradFn a
gradCenterObjs (x1, x2, y1, y2) = (dfdx1, dfdx2, dfdy1, dfdy2)
              where dfdx1 = x1 / sqrt(x1^2 + y1^2)
                    dfdx2 = {-debugXY x1 x2 y1 y2 $-} x2 / sqrt(x2^2 + y2^2)
                    dfdy1 = y1 / sqrt(x1^2 + y1^2)
                    dfdy2 = y2 / sqrt(x2^2 + y2^2)
              -- TODO just step one object for now

-- repel two objects
repelInverse :: ObjFn a -- line search seems okay with asymptotes
repelInverse (x1, x2, y1, y2) = 1 / (((x1 - x2)^2 + (y1 - y2)^2))

-- TODO how to weight this properly?
centerAndRepelAdd :: ObjFn a -- timestep t
centerAndRepelAdd s = centerObjsNoSqrt s + 10000000 * repelInverse s

-- gradient of 'f(x1, x2, y1, y2) = 1/((x1 - x2)^2 + (y1 - y2)^2)'
-- mimics electrostatic force (1/dx^2)
-- TODO only the centerObjs fn is ported to have the right types + pair f with f'. port the rest too
gradRepelInverse :: (Show a, Floating a, Fractional a) => GradFn' a
gradRepelInverse t x1 x2 y1 y2 = (t', dfdx1, dfdx2, dfdy1, dfdy2)
              where -- TODO NaNs galore
                    t' = t * 10^10
                    dfdx1 = debugF $ -((2*(x1 - x2))/((x1 - x2)^2 + (y1 - y2)^2)^2)
                    dfdx2 = debugXY x1 x2 y1 y2 $ (2 * (x1-x2))/((x1-x2)^2+(y1-y2)^2)^2
                    dfdy1 = -((2 * (y1-y2))/((x1-x2)^2+(y1-y2)^2)^2)
                    dfdy2 = (2 * (y1-y2))/((x1-x2)^2+(y1-y2)^2)^2

-- The "center and repel" function from earlier summed the centerObjs function and the repelInverse
-- function above using stepObjFn as. I'm still debugging BTLS for one objective function, so
-- I rolled the obj-fn-adding functionality back. TODO reimplement it

doNothing :: ObjFn a -- for debugging
doNothing (x1, x2, y1, y2) = 0

------ Objective function to place a label either inside of or right outside of a set

c1' :: Floating a => a
c1' = rad -- both need to be non-neg

c2' :: Floating a => a
c2' = rad + eps'

eps' :: Floating a => a
eps' = 60 -- why is this 100??

cubicCenterOrRadius :: ObjFn a
cubicCenterOrRadius (x1, x2, y1, y2) = (sqrt((x1-x2)^2 + (y1-y2)^2))^3 - (c1' + c2') * (sqrt((x1-x2)^2 + (y1-y2)^2))^2 + c1' * c2' * (sqrt((x1-x2)^2 + (y1-y2)^2))
