import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Linear.V2 -- vectors
import Linear.V4
import Linear.Metric
import Linear.Vector
import Numeric.AD -- autodiff
import Numeric.AD.Internal.Reverse 
-- deal with conversion to/from float and the weird ad internal type Reverse s a

main = play                    -- TODO change to play
       (InWindow "optimization-based layout" -- display mode, window name
                  (picWidth, picHeight)   -- size
                  (10, 10))    -- position
       white                   -- background color
       50                     -- number of simulation steps to take for each second of real time
       initState                   -- the initial world
       picOf                   -- fn to convert world to a pic
       handler                 -- fn to handle input events
       step                    -- step the world one iteration; passed period of time (in secs) to be advanced

picWidth :: Int 
picWidth = 800

picHeight :: Int
picHeight = 700

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
                 , selc :: Bool } 

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
-- instance Located Obj

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

data State = State { objs :: [Obj]
                   , down :: Bool -- left mouse button is down (dragging)
                   , rng :: StdGen } 

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 11 -- deterministic RNG with seed

rad = 100 -- TODO don't hardcode into constant

clamp1D y = if clampflag then 0 else y

-- TODO randomly sample s0
initState :: State
initState = State { objs = objsInit, down = False, rng = initRng }
          where objsInit = [c1, c2] -- only handles two objects, with a non-working case for three
                -- TODO handle one obj...
                c1 = C $ Circ { xc = -100, yc = clamp1D 100, r = rad, selc = False }
                c2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad-50, selc = False }
                c3 = C $ Circ { xc = 300, yc = clamp1D 200, r = rad+50, selc = False }                    
                l1 = L $ Label { xl = -100, yl = clamp1D 200, textl = "B1", scalel = 0.2, sell = False }

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 = picWidth `divf` 2
ph2 = picHeight `divf` 2
widthRange = (-pw2, pw2)
heightRange = (-ph2, ph2)

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

---- sampling
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
----

bbox = 60 -- TODO put all flags and consts together

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

-- hardcode bbox of at the center
-- TODO properly get bbox; rn text is centered at bottom left
inObj :: (Float, Float) -> Obj -> Bool
inObj (xm, ym) (L o) = abs (xm - getX o) <= bbox && abs (ym - getY o) <= bbox -- is label
inObj (xm, ym) (C o) = dist (xm, ym) (xc o, yc o) <= r o -- is circle

-- TODO "in object" tests
-- TODO press key to GD step
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

handler (EventKey (MouseButton LeftButton) Up _ _) s =
        s { objs = map deselect $ objs s, down = False }

-- if you press a key while down, then the handler resets the entire state (then Up will just reset again)
handler (EventKey (Char 'r') Up _ _) s =
        State { objs = objs', down = False, rng = rng' }
        where (objs', rng') = sampleConstrainedState (rng s) (objs s)
handler _ s = s

-- TODO clamp needs to take into account bbox of object
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

-- TODO hack so i don't have to deal with pairwise derivatives of an arbitrary-length list
firstTwo :: [a] -> (a, a)
firstTwo (x1 : x2 : _) = (x1, x2)

debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id
constraint = if constraintFlag then noOverlap else \x -> True
type GradFn' a = Time a -> Float -> Float -> Float -> Float -> (Time a, Float, Float, Float, Float) -- TODO remove; old
type Time a = a

-- this are now obsolete, since we aren't doing constrained optimization
noOverlapPair :: Circ -> Circ -> Bool
noOverlapPair c1 c2 = dist (xc c1, yc c1) (xc c2, yc c2) > r c1 + r c2 

-- return true iff satisfied
-- TODO deal with labels and more than two objects
noOverlap :: [Obj] -> Bool
noOverlap ((C c1) : (C c2) : (C c3) : _) = noOverlapPair c1 c2 && noOverlapPair c2 c3 && noOverlapPair c1 c3
-- noOverlap _ _ = True

-- implement gradient descent
-- TODO: is there a haskell autodifferentiator?
-- step :: Time a -> State -> State
step t s = -- if down s then s -- don't step when dragging 
            if stepFlag then s { objs = stepObjs t (objs s), down = down s} else s

-- stepT :: Time a -> Float -> Float -> Float
stepT dt x dfdx = x - dt * dfdx

-- TODO assuming all objs are the same and have same obj function, apply obj pairwise step function
-- TODO generalize
-- stepObjs :: Time a -> [Obj] -> [Obj]
stepObjs t objs@(o1 : o2 : _) = if constraint objs' then objs' else objs
         where (o1', o2') = stepObjsPairwise t (o1, o2)
               objs' = [o1', o2']
-- stepObjs t objs@(o1 : o2 : o3 : _) = if constraint objs' then objs' else objs
--          where (o1', _) = stepObjsPairwise t (o1, o2) -- uses already stepped objs, not original state
--                (o2', _) = stepObjsPairwise t (o2, o3) -- TODO ^ hack bc the gradients aren't fns of the others
--                (o3', _) = stepObjsPairwise t (o3, o1) -- TODO order matters!
--                objs' = [o1', o2', o3']

-- TODO step one object. problem is that pairwise stepping is hardcoded everywhere
-- stepObj :: Time a -> Obj -> Obj
         
-- Layer of stepping relative to actual objects (their sizes, properties, bbox) and top-level bbox
-- step only if the constraint on the state is satisfied
-- the state will be stuck if the constraint starts out unsatisfied. TODO let GD attempt to satisfy constraint
-- TODO: also enforce for mouse dragging and initial sampling
-- stepObjsPairwise :: Time a -> (Obj, Obj) -> (Obj, Obj)
stepObjsPairwise t (o1, o2) = objs'
        where (x1, y1, x2, y2) = (getX o1, getY o1, getX o2, getY o2)
              (x1', x2', y1', y2') = stepWithObjective t x1 x2 y1 y2 -- obj and gradient are global params
              (x1'c, x2'c, y1'c, y2'c) = (clampX x1', clampX x2', clampY y1', clampY y2')
              objs' = (setX x1'c $ setY y1'c o1, setX x2'c $ setY y2'c o2)

stepFlag = True
clampflag = False
debug = True
constraintFlag = False
objFn1 = centerObjs
-- objFn2 = doNothing -- TODO repelInverse
-- gradFn1 = gradCenterObjs
btls = False

stopEps :: Float
stopEps = 10**(-3)
--10 ** (-10) -- TODO magnitude of gradient is ~0.13 when x,y ~ 0.08... still large

-- calculates the new state
-- TODO why isn't the magnitude of the gradient changing?
-- stepWithObjective :: Time a -> Float -> Float -> Float -> Float -> (Float, Float, Float, Float)
stepWithObjective t x1 x2 y1 y2 = if stoppingCriterion (V4 dfdx1 dfdx2 dfdy1 dfdy2) then
                                     trace "STOP" (x1, x2, y1, y2) 
                                  else (stepT t' x1 dfdx1, stepT t' x2 dfdx2,
                                        stepT t' y1 dfdy1, stepT t' y2 dfdy2)
                  where (t', [dfdx1, dfdx2, dfdy1, dfdy2]) =
                                     timeAndGrad centerObjs t (x1, x2, y1, y2)
                        -- choose the timestep via backtracking (for now) line search
                        stoppingCriterion gradEval = (msgTrace "gradient: " $ norm $ msgTrace "grad comp:" $ gradEval) <= stopEps

-- TODO generalize to add line search to this later
-- generalized version of above that adds the obj fns. need to generalize to arbirary #s of obj fns
-- could two objective functions be working on different timesteps??
-- should each take into account the dx, dy from the previous?
-- stepWithGradFns :: GradFn' a -> GradFn' a -> Time a -> Float -> Float -> Float -> Float -> Vec4 a
stepWithGradFns f1 f2 t x1 x2 y1 y2 = (x1'', x2'', y1'', y2'')
             where (t1, dfdx1_1, dfdx2_1, dfdy1_1, dfdy2_1) = f1 t x1 x2 y1 y2 
                   (t2, dfdx1_2, dfdx2_2, dfdy1_2, dfdy2_2) = f2 t x1 x2 y1 y2 
                   (x1', x2', y1', y2') = (stepT t1 x1 dfdx1_1, stepT t1 x2 dfdx2_1,
                                           stepT t1 y1 dfdy1_1, stepT t1 y2 dfdy2_1)
                   (x1'', x2'', y1'', y2'') = (stepT t2 x1' dfdx1_2, stepT t2 x2' dfdx2_2,
                                               stepT t2 y1' dfdy1_2, stepT t2 y2' dfdy2_2)

toV :: Vec4 a -> V4 a
toV (x1, x2, y1, y2) = V4 x1 x2 y1 y2

fromV :: V4 a -> Vec4 a
fromV (V4 x1 x2 y1 y2) = (x1, x2, y1, y2)

-- TODO generalize to a list of inputs instead of a vector
type Vec4 a = (a, a, a, a) -- TODO use V4
-- type V4 a = V4 Float
type ObjFn a = Vec4 a -> a
type GradFn a = Vec4 a -> Vec4 a -- TODO clean up input types
-- type ObjFn2 a = (a, a, a, a) -> a 
-- type GradFn2 a = (a, a, a, a) -> (a, a, a, a)
type GradFn2 a = [a] -> [a]

-- note: unsafe pattern match
-- list4toTup :: [t] -> (t, t, t, t)
list4toTup [a, b, c, d] = (a, b, c, d)

-- extract floats etc from ad library's weird wrapper type
extractAll :: [Reverse t a] -> [a]
extractAll xs = map extract xs
         where extract (Lift x) = x -- non-exhaustive pattern match

-- uses grad (a function from autodiff library) which takes f (as a function that takes a list of inputs)
-- a list of inputs, and produces the evaluated gradient (list of partial derivatives)
-- gradF :: (Num t, Fractional t, Floating t) => ObjFn2 t -> GradFn2 t
-- gradF f (a, b, c, d) = grad f [a, b, c, d]

      -- where fListOfWrapped = f . list4toTup 
      -- convert f from :: Vec4 a -> Float to 
      -- :: [Reverse s Float] -> Reverse s Float (weird internal type in ad lib.)

type ObjFn2 a = [a] -> a -- TODO unsafe pattern matches for now, and Vec4 obsolete

-- TODO change stepWithGradFn(s) to use this fn and its type
timeAndGrad :: (Ord a, Show a, Num a, Fractional a, Floating a) => ObjFn2 a -> Time a -> Vec4 a -> (Time a, [a])
timeAndGrad f t (x1, x2, y1, y2) = (timestep, gradEval)
            where
                  gradEval = grad f [x1, x2, y1, y2] -- can partially eval, too
                  timestep = 100
                  -- gradEval = gradCenterObjs (x1, x2, y1, y2) -- TODO remove

-- f paired with its gradient (below)
centerObjs :: (Num a, Fractional a, Floating a) => ObjFn2 a
centerObjs [x1, x2, y1, y2] = sqrt (x1^2 + y1^2) + sqrt (x2^2 + y2^2)

msgTrace :: Show a => String -> a -> a
msgTrace s x = trace "---" $ trace s $ traceShowId x -- prints in left to right order

--everything else commented out for debugging purposes
{-                  gradEvalV = toV gradEval
                  timestep = if not btls then t * 100 else -- default for debugging
                             backtrackingLineSearch f gradEvalV (negated gradEvalV) (V4 x1 x2 y1 y2)
            -- hardcodes descent direction to be the negated evaluated gradient vector

-- TODO why is the step size always the same except for the end, where it's 1 (1 makes sense--at minimum)
-- alpha :: Float
alpha :: (Num a, Fractional a, Floating a) => a
alpha = 0.4 -- \in (0, 0.5): fraction of decrease in f predicted by lin. extrapolation that we will accept
  -- or, increase in shallowness of slope
-- beta :: Float
beta :: (Num a, Fractional a, Floating a) => a
beta = 0.3 -- \in (0, 1): btls reduces step size by beta for each failed iteration

-- TODO doNothing btls
-- current issues: NaN after both reach center; only tested with one objective function
-- sometimes after one object reaches the center, the other will continue moving there really slowly
-- and sometimes dragging an object after one has reached the center will cause it to pause
  -- dragging the other causes them to start moving again?
  -- and sometimes they will halt not in the center??

-- TODO reimplement the vector operations here on lists
backtrackingLineSearch :: (Ord a, Show a, Num a, Fractional a, Floating a) => ObjFn2 a -> V4 a -> V4 a -> V4 a -> a
backtrackingLineSearch f gradEval descentDir x0 = msgTrace "timestep: " $
                       head $ dropWhile timestepTooLarge $ iterate ((*) beta) t0 -- infinite list
                       where t0 = 1 -- inital t specified by algo, decreases by beta each iteration
                             -- f(x + tu) still lies above the shallow line specified by alpha
                             timestepTooLarge t = traceShowId $ ((debugF (f $ fromV ((x0) ^+^ (t *^ descentDir))))> (debugF $ minFnVal t))
                             minFnVal t = f (fromV x0) + alpha * t * (gradEval `dot` descentDir)

-- f paired with its gradient (below)
centerObj :: (Num a, Fractional a, Floating a) => ObjFn2 a
centerObj [x1, x2, y1, y2] = x1^2 + y1^2

gradCenterObj :: (Num a, Fractional a, Floating a) => [a] -> [a]
gradCenterObj [x1, x2, y1, y2] = [dfdx1, dfdx2, dfdy1, dfdy2]
              where dfdx1 = 2 * x1
                    dfdx2 = 0
                    dfdy1 = 2 * y2
                    dfdy2 = 0
              -- TODO just step one object for now

-- derivative with respect to x1 of 'f(x1, x2, y1, y2) =  sqrt(x1^2 + y1^2) + sqrt(x2^2+y2^2)'
-- evaluated at x1 x2 y1 y2
-- TODO use autodiff
gradCenterObjs :: (Num a, Fractional a, Floating a) => [a] -> [a]
gradCenterObjs [x1, x2, y1, y2] = [dfdx1, dfdx2, dfdy1, dfdy2]
              where dfdx1 = x1 / sqrt(x1^2 + y1^2)
                    dfdx2 = {-debugXY x1 x2 y1 y2 $-} x2 / sqrt(x2^2 + y2^2)
                    dfdy1 = y1 / sqrt(x1^2 + y1^2)
                    dfdy2 = y2 / sqrt(x2^2 + y2^2)
              -- TODO just step one object for now
-}
