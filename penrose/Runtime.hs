{-# LANGUAGE Rank2Types, UnicodeSyntax #-}
-- for autodiff, requires passing in a polymorphic fn

-- module Runtime where

import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Debug.Trace
import Numeric.AD
import GHC.Float -- float <-> double conversions
import System.IO
import System.Environment
import Data.List
import qualified Compiler as C
       -- (subPrettyPrint, styPrettyPrint, subParse, styParse)
       -- TODO limit export/import

divLine = putStr "\n--------\n\n"

main = do
     -- Reading in from file
     -- Objective function is currently hard-coded
       -- args <- getArgs
       -- let (subFile, styFile) = (head args, args !! 1) -- TODO usage
       -- subIn <- readFile subFile
       -- styIn <- readFile styFile
       -- putStrLn "\nSubstance program:\n"
       -- putStrLn subIn
       -- divLine
       -- putStrLn "Style program:\n"
       -- putStrLn styIn
       -- divLine

       -- let subParsed = C.subParse subIn
       -- putStrLn "Parsed Substance program:\n"
       -- putStrLn $ C.subPrettyPrint' subParsed

       -- let styParsed = C.styParse styIn
       -- divLine
       -- putStrLn "Parsed Style program:\n"
       -- putStrLn $ C.styPrettyPrint styParsed

       -- divLine
       -- putStrLn "Intermediate layout representation: TODO\n"

       -- let initState = compilerToRuntimeTypes $ C.subToLayoutRep subParsed
       -- divLine
       -- putStrLn "Optimization representation:\n"
       -- putStrLn $ show initState

       -- divLine
       -- putStrLn "Visualizing notation:\n"

       -- Running with hardcoded parameters
       (play
        (InWindow "optimization-based layout" -- display mode, window name
                  (picWidth, picHeight)   -- size
                  (10, 10))    -- position
        white                   -- background color
        50                     -- number of simulation steps to take for each second of real time
        initState                   -- the initial world, defined as a type below
        picOf                   -- fn to convert world to a pic
        handler                 -- fn to handle input events
        step)                    -- step the world one iteration; passed period of time (in secs) to be advanced

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
     deriving (Eq, Show)

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
     deriving (Eq, Show)

instance Located Label where
         getX l = xl l
         getY l = yl l
         setX x l = l { xl = x }
         setY y l = l { yl = y }

instance Selectable Label where
         select x = x { sell = True }
         deselect x = x { sell = False }
         selected x = sell x

data Obj = C Circ | L Label deriving (Eq, Show)

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
     deriving (Show)

------

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 11 -- deterministic RNG with seed

objOf :: C.Obj -> Obj
objOf (C.C circ) = C $ Circ { xc = C.xc circ, yc = C.yc circ, r = C.r circ, selc = False }
objOf (C.L label) = error "Layout -> Opt doesn't support labels yet"

-- Convert Compiler's abstract layout representation to the types that the optimization code needs.
compilerToRuntimeTypes :: [C.Obj] -> State
compilerToRuntimeTypes compileState = State { objs = runtimeState', down = False, rng = initRng'}
                       where (runtimeState', initRng') = genState runtimeState initRng
                             runtimeState = map objOf compileState

rad :: Floating a => a
rad = 200 -- TODO don't hardcode into constant
clamp1D y = if clampflag then 0 else y

rad1 :: Floating a => a
rad1 = rad-100

rad2 :: Floating a => a
rad2 = rad+50

-- Initial state of the world, reading from Substance/Style input
-- TODO randomly sample s0
initState :: State
initState = State { objs = objsInit, down = False, rng = initRng }
          -- where objsInit = state4
                
-- Initial state of the world. Hardcoded for testing.
-- TODO randomly sample s0
initStateHardcode :: State
initStateHardcode = State { objs = objsInit, down = False, rng = initRng }
          where objsInit = [c1, c2] -- only handles two objects, with a non-working case for three
                -- TODO handle one obj...
                c1 = C $ Circ { xc = -100, yc = clamp1D 200, r = rad, selc = False }
                c2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad-100, selc = False }
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
radiusRange = (0, picWidth `divf` 4)

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
                         $ text objText

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

-- randomly sample location (for circles and labels) and radius (for circles)
sampleCoord :: RandomGen g => g -> Obj -> (Obj, g)
sampleCoord gen o = let o_loc = setX x' $ setY (clamp1D y') o in
                    case o of
                    C circ -> let (r', gen3) = randomR radiusRange gen2 in
                              (C $ circ { r = r'}, gen3)
                    L lab -> (o_loc, gen2) -- only sample location
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

dist :: Floating a => (a, a) -> (a, a) -> a -- distance
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
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

-- Some debugging functions.
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

-- To send output to a file, do ./EXECUTABLE 2> FILE.txt
tr :: Show a => String -> a -> a
tr s x = if debug then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

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
type TimeInit = Float
type Time = Double
type ObjFn a = forall a . Floating a => [a] -> a -- the new type we're trying to convert to
     -- TODO: convert lists to lists of type-level length, and define an interface for object state (pos, size)
     -- also need to check the input length matches obj fn lengths, e.g. in awlinesearch

-------- Step the world by one timestep (provided by the library).
-- gloss operates on floats, but the optimization code should be done with doubles, so we
-- convert float to double for the input and convert double to float for the output.
step :: Floating a => TimeInit -> State -> State
step t s = -- if down s then s -- don't step when dragging
            if stepFlag then s { objs = stepObjs (float2Double t) (objs s), down = down s} else s

-- may differ between different kinds of objs: circles may have a size param but points may not.
-- how should the gradient function distinguish? all objs need to have a return list of the same size
-- with Infinity as the value if the attribute is missing?
-- or "hooks" to info outside of the function?
objInfo :: Obj -> [Float]
objInfo o = [getX o, getY o] -- TODO add size

stateSize :: Int
stateSize = 2

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

updateObj :: (Obj, [Float]) -> Obj
updateObj (obj, l) = if not $ length l == stateSize then error "input obj list wrong size"
                     else let [x', y'] = l in -- update obj's position while keeping it on canvas 
                     setY (clampY y') $ setX (clampX x') obj

-- unpacks all objects into a big state vector, steps that state, and repacks the new state into the objects
-- NOTE: all downstream functions (objective functions, line search, etc.) expect a state in the form of 
-- a big list of floats with the object parameters grouped together: [x1, y1, size1, ... xn, yn, sizen]
stepObjs :: Time -> [Obj] -> [Obj]
stepObjs t objs = if constraint objs' then objs' else objs
        where state = map float2Double $ concatMap objInfo objs -- extract locations of objs, ignoring size
              -- get new positions. objective function is a global param
              state' = map double2Float $ stepWithObjective t state
              -- break state list into chunks corresponding to new state for each obj
              stateLists = chunksOf stateSize state' 
              -- re-pack each object's state list into object, assuming the zipped lists are the same size
              objs' = map updateObj ((\ (a, b) -> zip a b) $ checkSizesMatch objs stateLists)
              checkSizesMatch a b = if not $ length a == length b then error "length mismatch" else (a, b)
                    
-- Flags for debugging the surrounding functions.
stepFlag = True
clampflag = False
debug = True
constraintFlag = False
objFn2 = doNothing -- TODO repelInverse

stopEps :: Floating a => a
stopEps = 10 ** (-10)

-- Given the time, position, and evaluated gradient (or other search direction) at the point, 
-- return the new position.
stepT :: Time -> Double -> Double -> Double
stepT dt x dfdx = x - dt * dfdx

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
stepWithObjective :: Time -> [Double] -> [Double]
stepWithObjective t state =
                  if stoppingCriterion gradEval then tr "STOP. position:" state
                  -- [stepT t' x1 dfdx1, stepT t' y1 dfdy1, ...]
                  else map (\(v, dfdv) -> stepT t' v dfdv) (zip state gradEval)
                  where (t', gradEval) = timeAndGrad objFn1 t state
                  -- gradEval = [dfdx1, dfdy1, dfdsize1, ...]
                  -- choose the timestep via backtracking (for now) line search
                        stoppingCriterion gradEval =
                                    (tr "gradient norm: " $ norm $ tr "evaluated gradient:" $ gradEval) <= stopEps

-- a version of grad with a clearer type signature
appGrad :: Floating a => (forall a . Floating a => [a] -> a) -> [a] -> [a]
appGrad f l = grad f l

nanSub :: (RealFloat a, Floating a) => a
nanSub = 9999

removeNaN' :: (RealFloat a, Floating a) => a -> a
removeNaN' x = if isNaN x then nanSub else x

removeNaN :: (RealFloat a, Floating a) => [a] -> [a]
removeNaN = map removeNaN'

----- Lists-as-vectors utility functions, TODO split out of file

-- define operator precedence: higher precedence = evaluated earlier
infixl 6 +. --, -.
infixl 7 *. -- .*, /.

-- assumes lists are of the same length
dotL :: Floating a => [a] -> [a] -> a
dotL u v = if not $ length u == length v then error "cannot take dot product of different-length lists"
           else sum $ zipWith (*) u v

(+.) :: Floating a => [a] -> [a] -> [a] -- add two vectors
(+.) u v = if not $ length u == length v then error "cannot add different-length lists"
           else zipWith (+) u v

negL :: Floating a => [a] -> [a]
negL = map negate

(*.) :: Floating a => a -> [a] -> [a] -- multiply by a constant
(*.) c v = map ((*) c) v

norm :: Floating a => [a] -> a
norm = sqrt . sum . map (^ 2)

normsq :: Floating a => [a] -> a
normsq = sum . map (^ 2)

-----

-- Given the objective function, gradient function, timestep, and current state,
-- return the timestep (found via line search) and evaluated gradient at the current state.
-- TODO change stepWithGradFn(s) to use this fn and its type
-- note: continue to use floats throughout the code, since gloss uses floats
-- the autodiff library requires that objective functions be polymorphic with Floating a
-- M-^ = delete indentation
timeAndGrad :: Floating a => ObjFn a -> Time -> [Double] -> (Time, [Double])
timeAndGrad f t state = (timestep, gradEval)
            where gradF :: Floating a => [a] -> [a]
                  gradF = appGrad f
                  gradEval = removeNaN $ gradF state
                  -- Use line search to find a good timestep.
                  -- Redo if it's NaN, defaulting to 0 if all NaNs. TODO
                  descentDir = negL gradEval
                  timestep :: Time
                  timestep = if not linesearch then t / 100 else -- use a fixed timestep for debugging
                             let resT = awLineSearch f duf descentDir state in
                             if isNaN resT then tr "returned timestep is NaN" nanSub else resT
                  -- directional derivative at u, where u is the negated gradient in awLineSearch
                  -- descent direction need not have unit norm
                  -- we could also use a different descent direction if desired
                  duf :: Floating a => [a] -> [a] -> a
                  duf u x = gradF x `dotL` u

-- Parameters for Armijo-Wolfe line search
-- NOTE: must maintain 0 < c1 < c2 < 1
c1 :: Floating a => a
c1 = 0.4 -- for Armijo, corresponds to alpha in backtracking line search (see below for explanation)
-- smaller c1 = shallower slope = less of a decrease in fn value needed = easier to satisfy
-- turn Armijo off: c1 = 0

c2 :: Floating a => a
c2 = 0.6 -- for Wolfe, is the factor decrease needed in derivative value
-- new directional derivative value / old DD value <= c2
-- smaller c2 = smaller new derivative value = harder to satisfy
-- turn Wolfe off: c1 = 1 (basically backatracking line search onlyl

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

isInfinity x = (x == infinity)

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods", page TODO
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2
-- TODO summarize algorithm
-- TODO what happens if there are NaNs in awLineSearch?
awLineSearch :: ObjFn a -> (forall a . Floating a => [a] -> [a] -> a) -> [Double] -> [Double] -> Double
awLineSearch f duf_noU descentDir x0 =
             -- results after a&w are satisfied are junk and can be discarded
             -- drop while a&w are not satisfied OR the interval is large enough
     let (af, bf, tf) = head $ dropWhile intervalOK_or_notArmijoAndWolfe
                              $ iterate update (a0, b0, t0) in tf
          where (a0, b0, t0) = (0, infinity, 1)
                duf = duf_noU descentDir
                update (a, b, t) =
                       let (a', b', sat) = if not $ armijo t then tr "not armijo" (a, t, False)
                                           else if not $ weakWolfe t then tr "not wolfe" (t, b, False)
                                           -- remember to change both wolfes
                                           else (a, b, True) in
                       if sat then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
                       else if b' < infinity then tr "b' < infinity" (a', b', (a' + b') / 2)
                       else tr "b' = infinity" (a', b', 2 * a')
                intervalOK_or_notArmijoAndWolfe (a, b, t) = not $
                      if armijo t && weakWolfe t then tr "stop: both sat" True -- takes precedence
                      else if abs (b - a) < minInterval then tr "stop: interval too small" True
                      else False -- could be shorter; long for debugging purposes
                armijo t = (f ((tr "** x0" x0) +. t *. descentDir)) <= (fAtx0 + c1 * t * dufAtx0)
                strongWolfe t = abs (duf (x0 +. t *. descentDir)) <= c2 * abs dufAtx0
                weakWolfe t = duf_x_tu >= (c2 * dufAtx0) -- split up for debugging purposes
                          where duf_x_tu = tr "Duf(x + tu)" (duf (x0 +. t' *. descentDir'))
                                t' = tr "t" t
                                descentDir' = tr "descentDir" descentDir
                dufAtx0 = duf x0 -- cache some results, can cache more if needed
                fAtx0 = f x0
                minInterval = if intervalMin then 10 ** (-5) else 0 
                -- stop if the interval gets too small; might not terminate

------------- Objective functions.

-- initial states
s1 = C $ Circ { xc = -100, yc = clamp1D 200, r = rad, selc = False }
s2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad1, selc = False }
s3 = C $ Circ { xc = 300, yc = clamp1D 200, r = rad2, selc = False }
s4 = C $ Circ { xc = -50, yc = clamp1D (-100), r = rad1 + 50, selc = False }
l1 = L $ Label { xl = -100, yl = clamp1D 200, textl = "B1", scalel = 0.2, sell = False }

state0 = []
state1 = [s1]
state2 = [s1, s2]
state3 = [s1, s2, s3]
state4 = [s1, s2, s3, s4]
staten n = take n $ repeat s3 -- they're all the same size on the first sampling, so it looks like one circle

objsInit = staten 7
------

objText = "objective: center all objects + pairwise repel each other"

-- needed to give this a type signature, otherwise inferred that `a = Double`
objFn1 :: Floating a => [a] -> a
objFn1 = centerAndRepelAdd

linesearch = True -- TODO move these parameters back
intervalMin = True -- true = force halt if interval gets too small; false = no forced halt

-- simple test function
minx1 :: ObjFn a -- timestep t
minx1 xs = if length xs == 0 then error "minx1 empty list" else (head xs)^2

-- only center the first object (for debugging). NOTE: need to pass in parameters in the right order
centerObjNoSqrt :: ObjFn a
centerObjNoSqrt (x1 : y1 : x2 : y2 : _) = x1^2 + y1^2 -- sum $

-- center both objects without sqrt
centerObjsNoSqrt :: ObjFn a
centerObjsNoSqrt = sum . map (^2)

centerx1Sqrt :: ObjFn a -- discontinuous, timestep = 100 * t. autodiff behaves differently for this vs abs
centerx1Sqrt (x1 : _) = sqrt $ x1^2

-- lot of "interval too small"s happening with the objfns on lists now
centerObjs :: ObjFn a -- with sqrt
centerObjs = sqrt . centerObjsNoSqrt

-- repel two objects
repelInverse2 :: ObjFn a
repelInverse2 [x1, y1, x2, y2] = 1 / ((x1 - x2)^2 + (y1 - y2)^2)

-- pairwise repel on a list of objects
-- TODO abstract out this general pattern (and make the function more readable)
repelInverse :: ObjFn a
repelInverse l = sum $ map (1 /) denoms
                 where denoms = map diffSq allPairs
                       diffSq [[x1, y1], [x2, y2]] = (x1 - x2)^2 + (y1 - y2)^2
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       -- TODO implement more efficient version. also, subseq only returns *unique* subseqs
                       objs = chunksOf stateSize l 

-- this works really nicely with A-W line search!
-- TODO debug: with line search, optimizer finds NaN state (all at exactly same pos in top right)
-- and even when re-sampling, they end up centered there??
-- but when dragged a bit, they find the local minima
centerAndRepelAdd :: ObjFn a -- timestep t
centerAndRepelAdd s = centerObjsNoSqrt s + weight * repelInverse s
                   where weight = 10 ** 10

doNothing :: ObjFn a -- for debugging
doNothing _ = 0

-- TODO this needs the set size info. hardcode radii for now
-- TODO to use "min rad rad1" we need to add "Ord a" to the type signatures everywhere
-- we want the distance between the sets to be <overlap> less than having them just touch
-- this isn't working? and isn't resampling?
-- also i'm getting interval shrinking problems just with this function (using 'distance' only)
setsIntersect2 :: ObjFn a
setsIntersect2 [x1, y1, x2, y2] = (dist (x1, y1) (x2, y2) - overlap)^2
              where overlap = rad + rad1 - 0.5 * rad1 -- should be "min rad rad1"

------ Objective function to place a label either inside of or right outside of a set

c1' :: Floating a => a
c1' = rad -- both need to be non-neg

c2' :: Floating a => a
c2' = rad + eps'

eps' :: Floating a => a
eps' = 60 -- why is this 100??

-- TODO still some nontermination in A-W ls when circles to corner/side
-- TODO I still can't get it to settle in the center
-- TODO this expression can be simplified--see slides
cubicCenterOrRadius2 :: ObjFn a 
cubicCenterOrRadius2 [x1, y1, x2, y2] = (sqrt((x1-x2)^2 + (y1-y2)^2))^3 - (c1' + c2') * (sqrt((x1-x2)^2 + (y1-y2)^2))^2 + c1' * c2' * (sqrt((x1-x2)^2 + (y1-y2)^2))
