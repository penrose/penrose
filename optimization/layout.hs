{-# LANGUAGE Rank2Types, UnicodeSyntax, MultiParamTypeClasses #-} 
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
-- import Numeric.AD

{-
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
-}
main = print "hello"

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
class Located b where
      getX :: b a -> a
      getY :: b a -> a
      setX :: a -> b a -> b a
      setY :: a -> b a -> b a

class Selectable b where
      select :: b a -> b a
      deselect :: b a -> b a
      selected :: b a -> Bool

-- The coords of a circle actually are members of the the typeclass Floating, but the norm is not
-- to have typeclass declarations in a data declaration.
-- We can't use Float because the `ad` library requires functions to be polymorphic,
-- and adding a `Floating a` to a `Float` returns a `Float`.
data Circ a = Circ { xc :: a
                 , yc :: a
                 , r :: a
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

data Label a = Label { xl :: a -- same deal as with Circ a
                   , yl :: a
                   , textl :: String
                   , scalel :: a  -- calculate h,w from it
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

data Obj a = C (Circ a) | L (Label a) -- | Label a | Point | Line // is there a better way to do this?

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
data State a = State { objs :: [Obj a]
                   , down :: Bool -- left mouse button is down (dragging)
                   , rng :: StdGen } -- random number benerator

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 11 -- deterministic RNG with seed

rad :: Floating a => a
rad = 100 -- TODO don't hardcode into constant
clampflag = True -- TODO remove
clamp1D y = if clampflag then 0 else y

-- Initial state of the world.
-- TODO randomly sample s0
initState :: (Floating a) => State a
initState = State { objs = objsInit, down = False, rng = initRng }
          where objsInit = [c1, c2] -- only handles two objects, with a non-working case for three
                -- TODO handle one obj...
                c1 = C $ Circ { xc = -100, yc = clamp1D 100, r = rad, selc = False }
                c2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad-50, selc = False }
                c3 = C $ Circ { xc = 300, yc = clamp1D 200, r = rad+50, selc = False }                    
                l1 = L $ Label { xl = -100, yl = clamp1D 200, textl = "B1", scalel = 0.2, sell = False }

-- divide two integers to obtain a float
-- divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 = picWidth `divf` 2
ph2 = picHeight `divf` 2
widthRange = (-pw2, pw2)
heightRange = (-ph2, ph2)

------------- The "Style" layer: render the state of the world.
renderCirc :: (Floating a) => Circ a -> Picture
renderCirc c = color scolor $ translate (xc c) (yc c) $ circle (r c)
           where scolor = if selected c then green else light violet

{-
renderLabel :: (Floating a) => Label a -> Picture
renderLabel l = color scolor $ translate (xl l) (yl l) $ scale 0.2 0.2 $ text (textl l)
            where scolor = if selected l then green else light violet

renderObj :: (Floating a) => Obj a -> Picture
renderObj (C circ) = renderCirc circ
renderObj (L label) = renderLabel label

picOfState :: (Floating a) => State a -> Picture
picOfState s = Pictures $ map renderObj (objs s)

picOf :: (Floating a) => State a -> Picture
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
sampleCoord :: (Floating a, Located b) => RandomGen g => g -> b a -> (a, g)
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
genState :: (Floating a, RandomGen g) => [Obj a] -> g -> ([Obj a], g)
genState shapes gen = stateMap gen sampleCoord shapes

-- sample entire state at once until constraint is satisfied
-- TODO doesn't take into account pairwise constraints or results from objects sampled first, sequentially
sampleConstrainedState :: (Floating a, RandomGen g) => g -> [Obj a] -> ([Obj a], g)
sampleConstrainedState gen shapes = (state', gen')
       where (state', gen') = crop constraint states
             states = genMany gen (genState shapes)
             -- init state params are ignored; we just need to know what kinds of objects are in it
--------------- Handle user input. "handler" is the main function here.
-- Whenever the library receives an input event, it calls "handler" with that event
-- and the current state of the world to handle it.

bbox :: Floating a => a
bbox = 60 -- TODO put all flags and consts together
-- hacky bounding box of label

-- dist :: Point -> Point -> Float -- distance
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- hardcode bbox of label at the center
-- TODO properly get bbox; rn text is centered at bottom left
-- inObj :: (Floating a) => (Float, Float) -> Obj a -> Bool
inObj (xm, ym) (L o) = abs (xm - getX o) <= bbox && abs (ym - getY o) <= bbox -- is label
inObj (xm, ym) (C o) = dist (xm, ym) (xc o, yc o) <= r o -- is circle

-- TODO "in object" tests
-- TODO press key to gradient descent step
-- for more on these constructors, see docs: https://hackage.haskell.org/package/gloss-1.10.2.3/docs/Graphics-Gloss-Interface-Pure-Game.html
handler :: (Floating a) => Event -> State a -> State a
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
-- clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

-- clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

-- TODO hack so I don't have to deal with pairwise derivatives of an arbitrary-length list. 
firstTwo :: [a] -> (a, a)
firstTwo (x1 : x2 : _) = (x1, x2) -- Unsafe pattern-match.

-- Some debugging functions.
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

msgTrace :: Show a => String -> a -> a
msgTrace s x = trace "---" $ trace s $ traceShowId x -- prints in left to right order

-- These functions are now obsolete, since we aren't doing constrained optimization.
-- Still, they might be useful later.
constraint = if constraintFlag then noOverlap else \x -> True
noOverlapPair :: Circ a -> Circ a -> Bool
noOverlapPair c1 c2 = dist (xc c1, yc c1) (xc c2, yc c2) > r c1 + r c2 

-- return true iff satisfied
-- TODO deal with labels and more than two objects
noOverlap :: (Floating a) => [Obj a] -> Bool
noOverlap ((C c1) : (C c2) : (C c3) : _) = noOverlapPair c1 c2 && noOverlapPair c2 c3 && noOverlapPair c1 c3
-- noOverlap _ _ = True

-- Type aliases for shorter type signatures.
-- type GradFn' a = Time -> a -> a -> a -> a -> (Time, a, a, a, a) -- old type
type Time a = a
type Vec4 a = (a, a, a, a) -- TODO use V4
type V4' a = V4 a
type ObjFn a = Vec4 a -> a
type GradFn a = Vec4 a -> Vec4 a -- TODO clean up input types

toV :: Vec4 a -> V4' a
toV (x1, x2, y1, y2) = V4 x1 x2 y1 y2

fromV :: V4' a -> Vec4 a
fromV (V4 x1 x2 y1 y2) = (x1, x2, y1, y2)

-- Flags for debugging the surrounding functions.
stepFlag = True
clampflag = False
debug = True
constraintFlag = False
btls = True

{-
-- TODO put these back together with the above
objFn1 = centerObjs
objFn2 = doNothing -- TODO repelInverse
gradFn1 = gradCenterObjs

-------- Step the world by one timestep (provided by the library).
step :: Floating a => Time a -> State a -> State a
step t s = -- if down s then s -- don't step when dragging 
            if stepFlag then s { objs = stepObjs t (objs s), down = down s} else s

-- Given the time, position, and evaluated directional derivative at the point, return the new position.
stepT :: Floating a => Time a -> a -> a -> a
stepT dt x dfdx = x - dt * dfdx

-- This function is here to remind me to generalize the objects (currently hard-coded to 2 objects)
-- to a list of them. Otherwise, this function doesn't do anything.
-- TODO assuming all objs are the same and have same obj function, apply obj pairwise step function
-- TODO generalize
-- TODO step one object. problem is that pairwise stepping is hardcoded everywhere
stepObjs :: Floating a => Time a -> [Obj a] -> [Obj a]
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
stepObjsPairwise :: Floating a => Time a -> (Obj a, Obj a) -> (Obj a, Obj a)
stepObjsPairwise t (o1, o2) = objs'
        where (x1, y1, x2, y2) = (getX o1, getY o1, getX o2, getY o2)
              -- extract the locations of the two objs, ignoring size
              (x1', x2', y1', y2') = stepWithObjective t x1 x2 y1 y2
              -- get new positions. objective function is a global param
              (x1'c, x2'c, y1'c, y2'c) = (clampX x1', clampX x2', clampY y1', clampY y2')
              -- keep objects on canvas
              objs' = (setX x1'c $ setY y1'c o1, setX x2'c $ setY y2'c o2)
              -- update object positions

stopEps :: Floating a => a
stopEps = 0.4
--10 ** (-10) -- TODO magnitude of gradient is ~0.13 when x,y ~ 0.08... still large

-- Calculates the new state by calculating the directional derivatives (via autodiff) 
-- and timestep (via line search), then using them to step the current state.
-- TODO why isn't the magnitude of the gradient changing with btls?
stepWithObjective :: (Ord a, Floating a, Show a) => Time a -> a -> a -> a -> a -> (a, a, a, a)
stepWithObjective t x1 x2 y1 y2 = if stoppingCriterion (V4 dfdx1 dfdx2 dfdy1 dfdy2) then
                                     trace "STOP" (x1, x2, y1, y2) 
                                  else (stepT t' x1 dfdx1, stepT t' x2 dfdx2,
                                        stepT t' y1 dfdy1, stepT t' y2 dfdy2)
                  where (t', (dfdx1, dfdx2, dfdy1, dfdy2)) = (0, (0, 0, 0, 0))
                                     --TODO* timeAndGrad objFn1 gradFn1 t (x1, x2, y1, y2)
                        -- choose the timestep via backtracking (for now) line search
                        stoppingCriterion gradEval = (msgTrace "gradient: " $ norm $ msgTrace "grad comp:" $ gradEval) <= stopEps

-- Generalized version of above that adds the obj fns. (Currently unused.) Need to generalize to arbirary #s of obj fns.
-- TODO generalize to add line search to this later
-- could two objective functions be working on different timesteps?
-- should each take into account the dx, dy from the previous?
{-
stepWithGradFns :: Floating a => GradFn' a -> GradFn' a -> Time a -> a -> a -> a -> a -> Vec4 a
stepWithGradFns f1 f2 t x1 x2 y1 y2 = (x1'', x2'', y1'', y2'')
             where (t1, dfdx1_1, dfdx2_1, dfdy1_1, dfdy2_1) = f1 t x1 x2 y1 y2 
                   (t2, dfdx1_2, dfdx2_2, dfdy1_2, dfdy2_2) = f2 t x1 x2 y1 y2 
                   (x1', x2', y1', y2') = (stepT t1 x1 dfdx1_1, stepT t1 x2 dfdx2_1,
                                           stepT t1 y1 dfdy1_1, stepT t1 y2 dfdy2_1)
                   (x1'', x2'', y1'', y2'') = (stepT t2 x1' dfdx1_2, stepT t2 x2' dfdx2_2,
                                               stepT t2 y1' dfdy1_2, stepT t2 y2' dfdy2_2) -}

-- TODO try it with list <-> vector conversion
-- timeAndGrad' :: Floating a => (forall a . Floating a => [a] -> a) -> [a] -> [a]
-- timeAndGrad' f l = grad f l

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- TODO change stepWithGradFn(s) to use this fn and its type
timeAndGrad :: (Show a, Ord a, Floating a) => ObjFn a -> GradFn a -> Time a -> Vec4 a -> (Time a, Vec4 a)
timeAndGrad f gradF t (x1, x2, y1, y2) = (timestep, gradEval)
            where gradEval = gradF (x1, x2, y1, y2)
                  gradEvalV = toV gradEval
                  -- Use line search to find a good timestep.
                  timestep = if not btls then t/10 else -- use a fixed timestep for debugging
                             backtrackingLineSearch f gradEvalV (negated gradEvalV) (V4 x1 x2 y1 y2)
                  -- hardcodes descent direction to be the negated evaluated gradient vector
                  -- but we could use a different descent direction if desired

-- Parameters for line search
alpha :: Floating a => a
alpha = 0.4 -- \in (0, 0.5): fraction of decrease in f predicted by lin. extrapolation that we will accept
  -- or, increase in shallowness of slope
beta :: Floating a => a
beta = 0.3 -- \in (0, 1): btls reduces step size by beta for each failed iteration
-- TODO why is the step size always the same except for the end, where it's 1 (1 makes sense--at minimum)

t0 :: Floating a => a
t0 = 1 -- inital t specified by algo, decreases by beta each iteration

-- Implements algorithm specified in Boyd. Only works for convex functions (e.g. centerObjs is convex)
-- TODO fix the conversions between Vec4 a and V4' a 
backtrackingLineSearch :: (Show a, Floating a, Ord a) => ObjFn a -> V4' a -> V4' a -> V4' a -> a
backtrackingLineSearch f gradEval descentDir x0 = msgTrace "timestep: " $
                       head $ dropWhile timestepTooLarge $ iterate ((*) beta) t0 -- infinite list
                       where timestepTooLarge t = traceShowId $ ((debugF (f $ fromV ((x0) ^+^ (t *^ descentDir))))> (debugF $ minFnVal t))
                             -- f(x + tu) still lies above the shallow line specified by alpha
                             minFnVal t = f (fromV x0) + alpha * t * (gradEval `dot` descentDir)

------------- Some sample objective functions.

-- only center the first object (for debugging)
-- f paired with its gradient (below)
centerObj :: Floating a => Vec4 a -> a
centerObj (x1, x2, y1, y2) = x1^2 + y1^2

gradCenterObj :: Floating a => Vec4 a -> Vec4 a
gradCenterObj (x1, x2, y1, y2) = (dfdx1, dfdx2, dfdy1, dfdy2)
              where dfdx1 = 2 * x1
                    dfdx2 = 0
                    dfdy1 = 2 * y2
                    dfdy2 = 0

-- center both objects (2 objects hardcoded; TODO handle arbitrary numbers of them)
centerObjs :: Floating a => Vec4 a -> a
centerObjs (x1, x2, y1, y2) = sqrt (x1^2 + y1^2) + sqrt (x2^2 + y2^2)

-- gradient of 'f(x1, x2, y1, y2) =  sqrt(x1^2 + y1^2) + sqrt(x2^2+y2^2)'
-- evaluated at x1 x2 y1 y2
gradCenterObjs :: Floating a => Vec4 a -> Vec4 a
gradCenterObjs (x1, x2, y1, y2) = (dfdx1, dfdx2, dfdy1, dfdy2)
              where dfdx1 = x1 / sqrt(x1^2 + y1^2)
                    dfdx2 = {-debugXY x1 x2 y1 y2 $-} x2 / sqrt(x2^2 + y2^2)
                    dfdy1 = y1 / sqrt(x1^2 + y1^2)
                    dfdy2 = y2 / sqrt(x2^2 + y2^2)
              -- TODO just step one object for now

-- repel two objects
repelInverse :: Floating a => Vec4 a -> a
repelInverse (x1, x2, y1, y2) = 1 / (((x1 - x2)^2 + (y1 - y2)^2))

-- gradient of 'f(x1, x2, y1, y2) = 1/((x1 - x2)^2 + (y1 - y2)^2)'
-- mimics electrostatic force (1/dx^2)
-- TODO only the centerObjs fn is ported to have the right types + pair f with f'. port the rest too
{-
gradRepelInverse :: (Show a, Floating a, Fractional a) => GradFn' a
gradRepelInverse t x1 x2 y1 y2 = (t', dfdx1, dfdx2, dfdy1, dfdy2)
              where -- TODO NaNs galore
                    t' = t * 10^10
                    dfdx1 = debugF $ -((2*(x1 - x2))/((x1 - x2)^2 + (y1 - y2)^2)^2)
                    dfdx2 = debugXY x1 x2 y1 y2 $ (2 * (x1-x2))/((x1-x2)^2+(y1-y2)^2)^2
                    dfdy1 = -((2 * (y1-y2))/((x1-x2)^2+(y1-y2)^2)^2)
                    dfdy2 = (2 * (y1-y2))/((x1-x2)^2+(y1-y2)^2)^2 -}

-- The "center and repel" function from earlier summed the centerObjs function and the repelInverse
-- function above using stepObj Fn as. I'm still debugging BTLS for one objective function, so
-- I rolled the obj-fn-adding functionality back. TODO reimplement it

doNothing :: Floating a => Vec4 a -> a
doNothing (x1, x2, y1, y2) = 0

------ Old code relating to the objective function to either center a label 
{-
-- or position it right outside a set. (The types are obsolete.)
--- Parameters specific to cubicCenterOrRadius
eps :: Floating a => a
eps = 60 -- why is this 100??

c1 :: Floating a => a
c1 = rad -- both need to be non-neg

c2 :: Floating a => a
c2 = rad + eps

tFactor :: Floating a => a
tFactor = 5 * 10^3

maxGrad :: Floating a => a
maxGrad = 10^7

minGrad :: Floating a => a
minGrad = 10^6

zeroClamp :: Floating a => a
zeroClamp = 5 * 10^3

-- clamp abs val of grad within [minGrad, maxGrad]
-- TODO clamp so small gradient = 0 movement, so no jitter
gradClamp :: (Ord a, Floating a) => a -> a
gradClamp g = if abs g < zeroClamp then 0 -- clamp down
              else if abs g < minGrad then sign * minGrad -- clamp up
              else if abs g > maxGrad then sign * maxGrad -- clamp down
              else g
              where sign = if g < 0 then -1 else 1

-- objective functions, differentiated and discretized
-- attract label to center of circle or to outside of circle
-- wolframalpha: derivative with respect to x1 of f(x1, y1, x2, y2) = (sqrt((x1-x2)^2 + (y1-y2)^2))^3 - (c1 + c2) (sqrt((x1-x2)^2 + (y1-y2)^2))^2 + c1 * c2 * (sqrt((x1-x2)^2 + (y1-y2)^2))
-- to debug, use traceShowId :: Show a => a -> a
cubicCenterOrRadius :: (Ord a, Floating a) => Time a -> a -> a -> a -> a -> (Time a, a, a, a, a)
cubicCenterOrRadius t x1 x2 y1 y2 = (t', dfdx1, dfdx2, dfdy1, dfdy2)
              where t' = t/tFactor -- otherwise it jitters b/t -inf and inf, doesn't reach zeroes
                    -- doesn't settle in the outside correctly
                    -- instantly jumps to inside bc grad is probably very large, but slow inside
                    -- and if too far away, jitters between -bigval and bigval until clicked/dragged
                    -- also, these are in fact dx1/dt (etc.)
                    -- need to hand-calibrate timestepping and clamping
                    -- TODO if x1 = x2 and y1 = y2, then NaN
                    -- TODO step each one WRT the already-stepped ones to reduce jitter?
                    dfdx1 = gradClamp $ {-traceShowId $-} (-2)*(c1 + c2)*(x1 - x2) + (c1*c2*(x1 - x2))/sqrt((x1 - x2)^2 + (y1 - y2)^2) + 3*(x1 - x2)*sqrt((x1 - x2)^2 + (y1 - y2)^2)
                    dfdy1 = gradClamp $ (-2)*(c1 + c2)*(y1 - y2) + (c1*c2*(y1 - y2))/sqrt((x1 - x2)^2 + (y1 - y2)^2) + 3*sqrt((x1 - x2)^2 + (y1 - y2)^2)*(y1 - y2)
                    -- same as dx1 and dy1 except moving toward each other
                    dfdx2 = -1 * (gradClamp $ (-2)*(c1 + c2)*(x1 - x2) + (c1*c2*(x1 - x2))/sqrt((x1 - x2)^2 + (y1 - y2)^2) + 3*(x1 - x2)*sqrt((x1 - x2)^2 + (y1 - y2)^2))
                    dfdy2 = -1 * (gradClamp $ (-2)*(c1 + c2)*(y1 - y2) + (c1*c2*(y1 - y2))/sqrt((x1 - x2)^2 + (y1 - y2)^2) + 3*sqrt((x1 - x2)^2 + (y1 - y2)^2)*(y1 - y2))

-- these functions' types are old and don't match the current step fn
-- attract: f(x1, x2) = (x1-x2)^2
-- df/dx1 = 2(x1-x2), df/dx2 = -2(x1-x2)
distance1d :: Floating a => Time a -> a -> a -> (a, a)
distance1d t x1 x2 = (x1 - t * 2 * (x1 - x2), x2 + t * 2 * (x1 - x2))
-- x2 does not use the updated x1

-- repel
negdistance1d :: Floating a => Time a -> a -> a -> (a, a)
negdistance1d t x1 x2 = (x1 + t * 2 * (x1 - x2), x2 - t * 2 * (x1 - x2))   

-- f(x) = x^2
parabola' :: Floating a => Time a -> a -> a
parabola' t x = x - t * 2 * x

neg_parabola' :: Floating a => Time a -> a -> a
neg_parabola' t x = x + t * 2 * x
-}
-}
-}
