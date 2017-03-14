{-# LANGUAGE RankNTypes, UnicodeSyntax, NoMonomorphismRestriction #-}
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
import qualified Data.Map as Map
import qualified Compiler as C
       -- (subPrettyPrint, styPrettyPrint, subParse, styParse)
       -- TODO limit export/import

divLine = putStr "\n--------\n\n"

main = do
     -- Reading in from file
     -- Objective function is currently hard-coded
     -- Comment in (or out) this block of code to read from a file (need to fix parameter tuning!)
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
       -- putStrLn "Intermediate layout representation:\n"
       -- let intermediateRep = C.subToLayoutRep subParsed
       -- putStrLn $ show intermediateRep

       -- let initState = compilerToRuntimeTypes intermediateRep
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
        stepsPerSecond         -- number of simulation steps to take for each second of real time
        initState               -- the initial world, defined as a type below
        picOf                   -- fn to convert world to a pic
        handler                 -- fn to handle input events
        step)                    -- step the world one iteration; passed period of time (in secs) to be advanced

picWidth :: Int
picWidth = 800

picHeight :: Int
picHeight = 700

stepsPerSecond :: Int
stepsPerSecond = 50

calcTimestep :: Float -- for use in forcing stepping in handler
calcTimestep = 1 / (int2Float stepsPerSecond)

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

class Sized a where
      getSize :: a -> Float
      setSize :: Float -> a -> a

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

instance Sized Circ where
         getSize x = r x
         setSize size x = x { r = size }

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

instance Sized Label where
         getSize x = xl x -- TODO generalize label size, distance to corner? ignores scale
         setSize size x = x { xl = size, yl = size } -- TODO currently sets both of them, ignores scale
                 -- changing a label's size doesn't actually do anything right now, but should use the scale
                 -- and the base font size

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

instance Sized Obj where
         getSize o = case o of
                 C c -> getSize c
                 L l -> getSize l
         setSize x o = case o of
                C c -> C $ setSize x c
                L l -> L $ setSize x l

data Params = Params { weight :: Float
                     } deriving (Show)

-- State of the world
data State = State { objs :: [Obj]
                   , down :: Bool -- left mouse button is down (dragging)
                   , rng :: StdGen -- random number generator
                   , autostep :: Bool -- automatically step optimization or not
                   , params :: Params
                   } deriving (Show)

------

initRng :: StdGen
initRng = mkStdGen seed
    where seed = 11 -- deterministic RNG with seed

-- data Label = Label { xl :: Float
--                    , yl :: Float
--                    , textl :: String
--                    , scalel :: Float  -- calculate h,w from it
--                    , sell :: Bool } -- selected label

objOf :: C.Obj -> Obj
objOf (C.C circ) = C $ Circ { xc = C.xc circ, yc = C.yc circ, r = C.r circ, selc = False }
objOf (C.L label) = L $ Label { xl = C.xl label, yl = C.yl label, textl = C.textl label,
                                scalel = C.scalel label, sell = False }

-- Convert Compiler's abstract layout representation to the types that the optimization code needs.
compilerToRuntimeTypes :: [C.Obj] -> State
compilerToRuntimeTypes compileState = State { objs = runtimeState', down = False, rng = initRng',
                       autostep = True, params = Params { weight = constraintWeight } }
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
initState :: State
initState = State { objs = objsInit, down = False, rng = initRng, autostep = False,
                    params = Params { weight = constraintWeight } }
          -- where objsInit = state4

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 :: Float
pw2 = picWidth `divf` 2

ph2 :: Float
ph2 = picHeight `divf` 2

widthRange = (-pw2, pw2)
heightRange = (-ph2, ph2)
radiusRange = (0, picWidth `divf` 6)

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
picOf s = Pictures [picOfState s, objectiveText, constraintText, stateText, paramText]
                    -- lineXbot, lineXtop, lineYbot, lineYtop,
                    -- lineXbot', lineXtop', lineYbot', lineYtop']
    where -- TODO display constraint instead of hardcoding
          -- (picture for bounding box for bound constraints)
          -- first_two_objs_box = [(0, (-300, -100)), (1, (0, 200)), (4, (100, 300)), (5, (-100, -400))] 
          -- lineXbot = color red $ Line [(-300, 0), (-100, 0)] 
          -- lineXtop = color red $ Line [(-300, 200), (-100, 200)] 
          -- lineYbot = color red $ Line [(-300, 0), (-300, 200)]
          -- lineYtop = color red $ Line [(-100, 0), (-100, 200)]

          -- lineXbot' = color red $ Line [(100, -100), (300, -100)] 
          -- lineXtop' = color red $ Line [(100, -400), (300, -400)] 
          -- lineYbot' = color red $ Line [(100, -100), (100, -400)]
          -- lineYtop' = color red $ Line [(300, -100), (300, -400)]

          -- TODO generate this text more programmatically
          objectiveText = translate xInit yInit $ scale sc sc
                         $ text objText
          constraintText = translate xInit (yInit - yConst) $ scale sc sc
                         $ text constrText
          stateText = let res = if autostep s then "on" else "off" in
                      translate xInit (yInit - 2 * yConst) $ scale sc sc
                      $ text ("autostep: " ++ res)
          paramText = translate xInit (yInit - 3 * yConst) $ scale sc sc
                      $ text ("penalty function weight: " ++ show (weight $ params s))
          xInit = -pw2+50
          yInit = ph2-50
          yConst = 30
          sc = 0.1

------- Sampling the state subject to a constraint. Currently not used since we are doing unconstrained optimization.

-- generate an infinite list of sampled elements
-- keep the last generator for the "good" element
genMany :: RandomGen g => g -> (g -> (a, g)) -> [(a, g)]
genMany gen genOne = iterate (\(c, g) -> genOne g) (genOne gen)

-- take the first element that satisfies the condition
-- not the most efficient impl. also assumes infinite list s.t. head always exists
crop :: RandomGen g => (a -> Bool) -> [(a, g)] -> (a, g)
crop cond xs = --(takeWhile (not . cond) (map fst xs), -- drops gens
                head $ dropWhile (\(x, _) -> not $ cond x) xs -- drops while top-level condition true. keeps good's gen

-- randomly sample location (for circles and labels) and radius (for circles)
sampleCoord :: RandomGen g => g -> Obj -> (Obj, g)
sampleCoord gen o = let o_loc = setX x' $ setY (clamp1D y') o in
                    case o_loc of
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

distsq :: Floating a => (a, a) -> (a, a) -> a -- distance
distsq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

-- Hardcode bbox of label at the center
-- TODO properly get bbox; rn text is centered at bottom left
inObj :: (Float, Float) -> Obj -> Bool
inObj (xm, ym) (L o) = abs (xm - getX o) <= bbox && abs (ym - getY o) <= bbox -- is label
inObj (xm, ym) (C o) = dist (xm, ym) (xc o, yc o) <= r o -- is circle

-- UI so far: pressing and releasing 'r' will re-sample all objects' sizes and positions within some preset range
-- if autostep is set, then dragging will move an object while optimization continues
-- if autostep is not set, then optimization will only step when 's' is pressed. dragging will move an object while optimization is not happening

-- for more on these constructors, see docs: https://hackage.haskell.org/package/gloss-1.10.2.3/docs/Graphics-Gloss-Interface-Pure-Game.html
-- pattern matches not fully fuzzed--assume that user only performs one action at once
-- (e.g. not left-clicking while stepping the optimization)
-- TODO "in object" tests
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
        if down s then s { objs = map (ifSelectedMoveTo (xm, ym)) (objs s), down = down s } else s
        where ifSelectedMoveTo (xm, ym) o = if selected o then setX xm $ setY (clamp1D ym) o else o

-- button released, so deselect all objects
handler (EventKey (MouseButton LeftButton) Up _ _) s =
        s { objs = map deselect $ objs s, down = False }

-- if you press a key while down, then the handler resets the entire state (then Up will just reset again)
handler (EventKey (Char 'r') Up _ _) s =
        s { objs = objs', down = False, rng = rng' }
        where (objs', rng') = sampleConstrainedState (rng s) (objs s)

-- turn autostep on or off (press same button to turn on or off)
handler (EventKey (Char 'a') Up _ _) s = if autostep s then s { autostep = False }
                                         else s { autostep = True }

-- pressing 's' (down) while autostep is off will step the optimization once, overriding the step function 
-- (which doesn't step if autostep is off). this is the same code as the step function but with reverse condition
-- if autostep is on, this does nothing
handler (EventKey (Char 's') Down _ _) s =
        if not $ autostep s then s { objs = stepObjs (float2Double calcTimestep) (params s) (objs s) } else s

-- change the weights in the barrier/penalty method (scale by 10). don't step objects
handler (EventKey (SpecialKey KeyUp) Down _ _) s =
        trace ("weight: " ++ show currWeight) $ -- TODO print on screen
              s { params = Params { weight = currWeight * 10 } } -- should modify params
        where currWeight = weight $ params s

handler (EventKey (SpecialKey KeyDown) Down _ _) s =
        trace ("weight: " ++ show currWeight) $ -- TODO print on screen
              s { params = Params { weight = currWeight / 10 } } -- should modify params
        where currWeight = weight $ params s

handler _ s = s

----------- Stepping the state the world via gradient descent.
 -- First, miscellaneous helper functions.

-- Clamp objects' positions so they don't go offscreen.
-- TODO clamp needs to take into account bbox of object
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

minSize :: Float 
minSize = 5

clampSize :: Float -> Float -- TODO assumes the size is a radius
clampSize s = if s < minSize then minSize
              else if s > ph2 || s > pw2 then min pw2 ph2 else s

-- Some debugging functions. @@@
debugF :: (Show a) => a -> a
debugF x = if debug then traceShowId x else x
debugXY x1 x2 y1 y2 = if debug then trace (show x1 ++ " " ++ show x2 ++ " " ++ show y1 ++ " " ++ show y2 ++ "\n") else id

-- To send output to a file, do ./EXECUTABLE 2> FILE.txt
tr :: Show a => String -> a -> a
tr s x = if debug then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

trStr :: String -> a -> a
trStr s x = if debug then trace "---" $ trace s x else x -- prints in left to right order

tr' :: Show a => String -> a -> a
tr' s x = if debugLineSearch then trace "---" $ trace s $ traceShowId x else x -- prints in left to right order

noOverlapPair :: Obj -> Obj -> Bool
noOverlapPair (C c1) (C c2) = dist (xc c1, yc c1) (xc c2, yc c2) > r c1 + r c2
noOverlapPair _ _ = True -- TODO, ignores labels

-- return true iff satisfied
-- TODO deal with labels and more than two objects
noneOverlap :: [Obj] -> Bool
noneOverlap objs = let allPairs = filter (\x -> length x == 2) $ subsequences objs in -- TODO factor out
                 all id $ map (\[o1, o2] -> noOverlapPair o1 o2) allPairs
-- noOverlap (c1 : c2 : []) = noOverlapPair c1 c2
-- noOverlap (c1 : c2 : c3 : _) = noOverlapPair c1 c2 && noOverlapPair c2 c3 && noOverlapPair c1 c3 -- TODO
-- noOverlap _ _ = True

-- allOverlap vs. not noOverlap--they're different!
allOverlap :: [Obj] -> Bool
allOverlap objs = let allPairs = filter (\x -> length x == 2) $ subsequences objs in -- TODO factor out
                 all id $ map (\[o1, o2] -> not $ noOverlapPair o1 o2) allPairs

-- Type aliases for shorter type signatures.
type TimeInit = Float
type Time = Double
type ObjFn1 a = forall a . (Floating a, Ord a) => [a] -> a
type GradFn a = forall a . (Ord a, Floating a) => [a] -> [a]
type Constraints = [(Int, (Double, Double))]
     -- TODO: convert lists to lists of type-level length, and define an interface for object state (pos, size)
     -- also need to check the input length matches obj fn lengths, e.g. in awlinesearch

-------- Step the world by one timestep (provided by the library). 
-- this function actually ignores the input timestep, because line search calculates the appropriate timestep to use,
-- but it's left in, in case we want to debug the line search.
-- gloss operates on floats, but the optimization code should be done with doubles, so we
-- convert float to double for the input and convert double to float for the output.
step :: Floating a => TimeInit -> State -> State
step t s = -- if down s then s -- don't step when dragging
            if autostep s then s { objs = stepObjs (float2Double t) (params s) (objs s) } else s 

-- Utility functions for getting object info (currently unused)
objInfo :: Obj -> [Float]
objInfo o = [getX o, getY o, getSize o] -- TODO deal with labels, also do stuff at type level

stateSize :: Int
stateSize = 3

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

objsInfo :: [a] -> [[a]]
objsInfo = chunksOf stateSize

-- from [x,y,s] over all objs, return [x,y] over all
objsCoords :: [a] -> [a]
objsCoords = concatMap (\[x, y, s] -> [x, y]) . objsInfo

-- from [x,y,s] over all objs, return [s] over all
objsSizes :: [a] -> [a]
objsSizes = map (\[x, y, s] -> s) . objsInfo
----

-- unpacks all objects into a big state vector, steps that state, and repacks the new state into the objects
-- NOTE: all downstream functions (objective functions, line search, etc.) expect a state in the form of 
-- a big list of floats with the object parameters grouped together: [x1, y1, size1, ... xn, yn, sizen]
stepObjs :: Time -> Params -> [Obj] -> [Obj]
stepObjs t stateParams objs = objs' --if constraint objs' then objs' else objs
        where (fixed, stateVarying) = tupMap (map float2Double) $ unpackFn objs
              -- get new positions. objective function is a global param
              stateVarying' = map double2Float $ stepWithObjective fixed stateParams t stateVarying
              -- re-pack each object's state list into object
              objs' = packFn objs stateVarying'
              checkSizesMatch a b = if not $ length a == length b then error "length mismatch" else (a, b)
       
tupMap :: (a -> b) -> (a, a) -> (b, b)
tupMap f (a, b) = (f a, f b)
             
-- Given the time, position, and evaluated gradient (or other search direction) at the point, 
-- return the new position.
stepT :: Time -> Double -> Double -> Double
stepT dt x dfdx = x - dt * dfdx

-- does not project onto an arbitrary set, only intervals
projCoordInterval :: (Double, Double) -> Double -> Double
projCoordInterval (lower, upper) x = (sort [lower, upper, x]) !! 1 -- median of the list

-- for each element, if there's a constraint on it (by index), project it onto the interval
lookInAndProj :: Constraints -> (Int, Double) -> [Double] -> [Double]
lookInAndProj constraints (index, x) acc = 
              case (Map.lookup index constraintsMap) of
              Just bounds -> projCoordInterval bounds x : acc
              Nothing     -> x : acc
              where constraintsMap = Map.fromList constraints

-- don't change the order of elements in the state!! use foldr, not foldl
projectOnto :: Constraints -> [Double] -> [Double]
projectOnto constraints state =
            let indexedState = zip [0..] state in
            foldr (lookInAndProj constraints) [] indexedState

-- Calculates the new state by calculating the directional derivatives (via autodiff)
-- and timestep (via line search), then using them to step the current state.
stepWithObjective :: Real a => [a] -> Params -> Time -> [Double] -> [Double]
stepWithObjective fixed stateParams t state =
                  if stoppingCriterion (tr "evaluated gradient:" gradEval) then trStr "STOP" state
                  -- project onto constraints (each coordinate may lie within a region)
                  else steppedState
                  -- TODO generalize bound constraint projection into EP method
                  where (t', gradEval) = trStr "objFn" $ timeAndGrad objFnApplied t state
                        -- get timestep via line search, and evaluated gradient at the state
                        -- step each parameter of the state with the time and gradient
                        -- gradEval :: [Double]; gradEval = [dfdx1, dfdy1, dfdsize1, ...]
                        steppedState = let state' = map (\(v, dfdv) -> stepT t' v dfdv) (zip state gradEval) in
                                       trStr ("||x' - x||: " ++ (show $ norm (state -. state'))
                                              ++ "\n|f(x') - f(x)|: " ++
                                             (show $ abs (objFnApplied state - objFnApplied state')))
                                       state'
                        stoppingCriterion gradEval = (norm gradEval <= stopEps)
                        objFnApplied = objFn (realToFrac cWeight) (map realToFrac fixed)
                        cWeight = weight stateParams

-- a version of grad with a clearer type signature
appGrad :: (Ord a, Floating a) => (forall a . (Ord a, Floating a) => [a] -> a) -> [a] -> [a]
appGrad f l = grad f l

nanSub :: (RealFloat a, Floating a) => a
nanSub = 0

removeNaN' :: (RealFloat a, Floating a) => a -> a
removeNaN' x = if isNaN x then nanSub else x

removeNaN :: (RealFloat a, Floating a) => [a] -> [a]
removeNaN = map removeNaN'

removeInf' :: (RealFloat a, Floating a) => a -> a
removeInf' x = if isInfinity x then bignum else if isNegInfinity x then (-bignum) else x
           where bignum = 10**10

removeInf :: (RealFloat a, Floating a) => [a] -> [a]
removeInf = map removeInf'

----- Lists-as-vectors utility functions, TODO split out of file

-- define operator precedence: higher precedence = evaluated earlier
infixl 6 +., -.
infixl 7 *. -- .*, /.

-- assumes lists are of the same length
dotL :: Floating a => [a] -> [a] -> a
dotL u v = if not $ length u == length v then error "cannot take dot product of different-length lists"
           else sum $ zipWith (*) u v

(+.) :: Floating a => [a] -> [a] -> [a] -- add two vectors
(+.) u v = if not $ length u == length v then error "cannot add different-length lists"
           else zipWith (+) u v

(-.) :: Floating a => [a] -> [a] -> [a] -- subtract two vectors
(-.) u v = if not $ length u == length v then error "cannot subtract different-length lists"
           else zipWith (-) u v

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
timeAndGrad :: ObjFn1 a -> Time -> [Double] -> (Time, [Double])
-- timeAndGrad :: Floating a => ([a] -> a) -> Time -> [Double] -> (Time, [Double])
timeAndGrad f t state = (timestep, gradEval)
            where gradF :: GradFn a
                  gradF = appGrad f
                  gradEval = gradF state
                  -- Use line search to find a good timestep.
                  -- Redo if it's NaN, defaulting to 0 if all NaNs. TODO
                  descentDir = negL gradEval
                  timestep :: Time
                  timestep = if not linesearch then t else -- use a fixed timestep for debugging
                             let resT = awLineSearch f duf descentDir state in
                             if isNaN resT then tr "returned timestep is NaN" nanSub else resT
                  -- directional derivative at u, where u is the negated gradient in awLineSearch
                  -- descent direction need not have unit norm
                  -- we could also use a different descent direction if desired
                  duf :: (Ord a, Floating a) => [a] -> [a] -> a
                  duf u x = gradF x `dotL` u

-- Parameters for Armijo-Wolfe line search
-- NOTE: must maintain 0 < c1 < c2 < 1
c1 :: Floating a => a
c1 = 0.4 -- for Armijo, corresponds to alpha in backtracking line search (see below for explanation)
-- smaller c1 = shallower slope = less of a decrease in fn value needed = easier to satisfy
-- turn Armijo off: c1 = 0

c2 :: Floating a => a
c2 = 0.2 -- for Wolfe, is the factor decrease needed in derivative value
-- new directional derivative value / old DD value <= c2
-- smaller c2 = smaller new derivative value = harder to satisfy
-- turn Wolfe off: c1 = 1 (basically backatracking line search onlyl

infinity :: Floating a => a
infinity = 1/0 -- x/0 == Infinity for any x > 0 (x = 0 -> Nan, x < 0 -> -Infinity)
-- all numbers are smaller than infinity except infinity, to which it's equal

negInfinity :: Floating a => a
negInfinity = -infinity

isInfinity x = (x == infinity)
isNegInfinity x = (x == negInfinity)

-- Implements Armijo-Wolfe line search as specified in Keenan's notes, converges on nonconvex fns as well
-- based off Lewis & Overton, "Nonsmooth optimization via quasi-Newton methods", page TODO
-- duf = D_u(f), the directional derivative of f at descent direction u
-- D_u(x) = <gradF(x), u>. If u = -gradF(x) (as it is here), then D_u(x) = -||gradF(x)||^2
-- TODO summarize algorithm
-- TODO what happens if there are NaNs in awLineSearch? or infinities
awLineSearch :: ObjFn1 a -> ObjFn2 a -> [Double] -> [Double] -> Double
awLineSearch f duf_noU descentDir x0 =
             -- results after a&w are satisfied are junk and can be discarded
             -- drop while a&w are not satisfied OR the interval is large enough
     let (af, bf, tf) = head $ dropWhile intervalOK_or_notArmijoAndWolfe
                              $ iterate update (a0, b0, t0) in tf
          where (a0, b0, t0) = (0, infinity, 1)
                duf = duf_noU descentDir
                update (a, b, t) =
                       let (a', b', sat) = if not $ armijo t then tr' "not armijo" (a, t, False)
                                           else if not $ weakWolfe t then tr' "not wolfe" (t, b, False)
                                           -- remember to change both wolfes
                                           else (a, b, True) in
                       if sat then (a, b, t) -- if armijo and wolfe, then we use (a, b, t) as-is
                       else if b' < infinity then tr' "b' < infinity" (a', b', (a' + b') / 2)
                       else tr' "b' = infinity" (a', b', 2 * a')
                intervalOK_or_notArmijoAndWolfe (a, b, t) = not $
                      if armijo t && weakWolfe t then -- takes precedence
                           tr ("stop: both sat. |-gradf(x0)| = " ++ show (norm descentDir)) True 
                      else if abs (b - a) < minInterval then
                           tr ("stop: interval too small. |-gradf(x0)| = " ++ show (norm descentDir)) True
                      else False -- could be shorter; long for debugging purposes
                armijo t = (f ((tr' "** x0" x0) +. t *. (tr' "descentDir" descentDir))) <= ((tr' "fAtX0"fAtx0) + c1 * t * (tr' "dufAtX0" dufAtx0))
                strongWolfe t = abs (duf (x0 +. t *. descentDir)) <= c2 * abs dufAtx0
                weakWolfe t = duf_x_tu >= (c2 * dufAtx0) -- split up for debugging purposes
                          where duf_x_tu = tr' "Duf(x + tu)" (duf (x0 +. t' *. descentDir'))
                                t' = tr' "t" t
                                descentDir' = descentDir --tr' "descentDir" descentDir
                dufAtx0 = duf x0 -- cache some results, can cache more if needed
                fAtx0 =f x0 -- TODO debug why NaN. even using removeNaN' didn't help
                minInterval = if intervalMin then 10 ** (-10) else 0 
                -- stop if the interval gets too small; might not terminate

------------- Initial states

-- initial states
s1 = C $ Circ { xc = -100, yc = clamp1D 200, r = rad, selc = False }
s2 = C $ Circ { xc = 300, yc = clamp1D (-200), r = rad1, selc = False }
s3 = C $ Circ { xc = 300, yc = clamp1D 200, r = rad2, selc = False }
s4 = C $ Circ { xc = -50, yc = clamp1D (-100), r = rad1 + 50, selc = False }

-- if objects start at exactly the same position, there may be problems
l1 = L $ Label { xl = 50, yl = clamp1D (-300), textl = "B1", scalel = 0.2, sell = False }
l2 = L $ Label { xl = -300, yl = clamp1D (-200), textl = "B2", scalel = 0.2, sell = False }

initStateRng :: StdGen
initStateRng = mkStdGen seed
    where seed = 4 -- deterministic RNG with seed

state0 = []
state1 = [s1]
state2 = [s1, s2]
state3 = [s1, s2, s3]
state4 = [s1, s2, s3, s4]
-- they're all the same size and in the same place for this state generator, so it looks like one circle
staten n = take n $ repeat s3 
statenRand n = let (objs', _) = sampleConstrainedState initStateRng (staten n) in objs'

-- TODO gen state with more labels
-- TODO port existing objective functions to deal with labels (or not)
state1lab = [s1, l1]
state2lab = [s1, l1, s2, l2]

-- TODO change label text
-- state may not satisify constraint
staten_label n = concat $ map labelN $ take n $ zip [1..] (repeat state1lab)
             where labelN (x, [obj, L lab]) = [obj, L $ lab { textl = ("B" ++ show x) }]
-- samples a state that satisfies the constraint
staten_label_rand n = let (objs', _) = sampleConstrainedState initStateRng (staten_label n) in objs'

------------------------ ### frequently-changed params for debugging
-- objsInit = staten_label_rand 5
objsInit = statenRand 8

-- now all other obj fns need to have this type
-- objFn :: Floating a => a -> ObjFn2 a
objFn :: (Floating a, Ord a) => a -> [a] -> [a] -> a
objFn weight = centerAndRepel_dist weight
-- objFn = centerRepelLabel

-- if the list of constraints is empty, it behaves as unconstrained optimization
boundConstraints :: Constraints
boundConstraints = [] -- first_two_objs_box

constraint = if constraintFlag then allOverlap else \x -> True

-- for use in barrier/penalty method (interior/exterior point method)
-- seems if the point starts in interior + weight starts v small and increases, then it converges
-- not quite... if the weight is too small then the constraint will be violated
constraintWeight :: Floating a => a
constraintWeight = 10 ** (-5)

-- Flags for debugging the surrounding functions.
clampflag = False
debug = True
debugLineSearch = False
constraintFlag = False
objFnOn = True -- in centerRepel_dist, turns obj function on or off (for debugging constraints only)
constraintFnOn = False

stopEps :: Floating a => a
stopEps = 10 ** (-10)

------------ Various constants and helper functions related to objective functions

epsd :: Floating a => a -- to prevent 1/0 (infinity). put it in the denominator
epsd = 10 ** (-10)

objText = "objective: none"
constrText = "constraint: no set is a subset of another (exterior point method, max/exp penalty, exponent = 2)"

-- separates fixed parameters (here, size) from varying parameters (here, location)
-- ObjFn2 has two parameters, ObjFn1 has one (partially applied)
type ObjFn2 a = forall a . (Ord a, Floating a) => [a] -> [a] -> a
type Fixed = [Float]
type Varying = [Float]

-- all objective functions so far use these two pack/unpack functions
unpackFn :: [Obj] -> (Fixed, Varying)
unpackFn = sizeLoc_unpack

packFn :: [Obj] -> Varying -> [Obj]
packFn = sizeLoc_pack

linesearch = True -- TODO move these parameters back
intervalMin = True -- true = force halt if interval gets too small; false = no forced halt

sumMap :: Floating b => (a -> b) -> [a] -> b -- common pattern in objective functions
sumMap f l = sum $ map f l

-- fixed parameters = sizes (in a list); varying parameters = locations (in a list of [x,y])
-- if you want the sizes to vary, you'll have to write different objective functions and pack/unpack functions
-- including size clamping
sizeLoc_unpack :: [Obj] -> (Fixed, Varying)
sizeLoc_unpack objs = (map getSize objs, concatMap (\o -> [getX o, getY o]) objs)

sizeLoc_pack :: [Obj] -> Varying -> [Obj]
sizeLoc_pack objs varying = let positions = chunksOf 2 varying in
                                     map (\(o, [x, y]) -> setX (clampX x) $ setY (clampY y) o)
                                         (zip objs positions)

-------------- Sample constraints

-- x-coord of first object's center in [-300,-200], y-coord of first object's center in [0, 200]
first_two_objs_box :: Constraints
first_two_objs_box = [(0, (-300, -100)), (1, (0, 200)), (4, (100, 300)), (5, (-100, -400))] 

-- TODO add more constraints

-------------- Objective functions

-- simple test function
minx1 :: ObjFn2 a -- timestep t
minx1 _ xs = if length xs == 0 then error "minx1 empty list" else (head xs)^2

-- only center the first object (for debugging). NOTE: need to pass in parameters in the right order
centerObjNoSqrt :: ObjFn2 a
centerObjNoSqrt _ (x1 : y1 : _) = x1^2 + y1^2 -- sum $

-- center both objects without sqrt
centerObjsNoSqrt :: ObjFn2 a
centerObjsNoSqrt _ = sumMap (^2) 

centerx1Sqrt :: ObjFn2 a -- discontinuous, timestep = 100 * t. autodiff behaves differently for this vs abs
centerx1Sqrt _ (x1 : _) = sqrt $ x1^2

-- lot of "interval too small"s happening with the objfns on lists now
centerObjs :: ObjFn2 a -- with sqrt
centerObjs fixed = sqrt . (centerObjsNoSqrt fixed)

-- Repel two objects
repel2 :: ObjFn2 a
repel2 _ [x1, y1, x2, y2] = 1 / ((x1 - x2)^2 + (y1 - y2)^2 + epsd)

-- pairwise repel on a list of objects (by distance b/t their centers)
repelCenter :: ObjFn2 a
repelCenter _ locs = sumMap (\x -> 1 / (x + epsd)) denoms
                 where denoms = map diffSq allPairs
                       diffSq [[x1, y1], [x2, y2]] = (x1 - x2)^2 + (y1 - y2)^2
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       -- TODO implement more efficient version. also, subseq only returns *unique* subseqs
                       objs = chunksOf 2 locs

-- does not deal with labels
centerAndRepel :: ObjFn2 a -- timestep t
centerAndRepel fixed varying = centerObjsNoSqrt fixed varying + weight * repelCenter fixed varying
                   where weight = 10 ** (9.8) -- TODO calculate this weight as a function of radii and bbox

-- pairwise repel on a list of objects (by distance b/t their centers)
-- TODO: version of above function that separates fixed parameters (size) from varying parameters (location)
-- assuming 1 size for each two locs, and s1 corresponds to x1, y1 (and so on)

-- TODO clean this up, there's some interior/exterior point method stuff not cleanly split out in here
repelDist :: ObjFn2 a
repelDist sizes locs = sumMap pairToPenalties allPairs
          -- the overall penalty function is the sum (unweighted)
                 where -- for each pair, for each constraint on that pair, compose w/ penalty function and sum
                       -- TODO generalize to arbitrary number of constraints and different pairwise, n-wise
                       -- TODO need to add Show to the typeclasses everywhere so I can debug w/ traceShow...
                       pairToPenalties :: (Ord a, Floating a) => [[a]] -> a
                       pairToPenalties pair = sum $ [ (penaltyFn . pairwiseConstraint1) pair,
                                                      (penaltyFn . pairwiseConstraint2) pair]
                       pairwiseConstraint1 = noConstraint
                       pairwiseConstraint2 = noSubset
                       -- generates all unique pairs
                       allPairs = filter (\x -> length x == 2) $ subsequences objs
                       objs = zipWith (++) locPairs sizes'
                       (sizes', locPairs) = (map (\x -> [x]) sizes, chunksOf 2 locs)

                       noConstraint x = 0 :: Floating a => a -- dummy constraint

                       -- exterior point method constraint: all sets must pairwise-strict-intersect
                       -- is this possible with n sets? try it with 2, 3 sets first. yes but only 1 config
                       -- multiple constraints = sum the penalty functions
                       -- looseIntersect just turns into subset... TODO write one 
                       looseIntersect [[x1, y1, s1], [x2, y2, s2]] = ((x1 - x2)^2 + (y1 - y2)^2) - (s1 + s2)^2
                       -- is it a problem because it is pairwise? c1 -> c2 and c2 -> c1
                       -- oh wait this only generates all unique pairs and it's not clear which is max
                       -- so there's only one unique constraint on the pair
                       -- hm, the energy actually increases so it always settles around the offset
                       -- oh that's bc i am centering all of them--test w/objective off
                       -- TODO flatten energy afterward, or get it to be *far* from the other set
                       noSubset [[x1, y1, s1], [x2, y2, s2]] = let offset = 10 in
                                -((x1 - x2)^2 + (y1 - y2)^2) + ((max s2 s1) - (min s2 s1) + offset)^2
                       strictSubset [[x1, y1, s1], [x2, y2, s2]] = ((x1 - x2)^2 + (y1 - y2)^2)
                                                                   - ((max s2 s1) - (min s2 s1))^2

                       -- exterior point method constraint: no intersection
                       noIntersectExt [[x1, y1, s1], [x2, y2, s2]] = -((x1 - x2)^2 + (y1 - y2)^2) + (s1 + s2)^2
                       -- interior point method constraint: no intersection
                       -- avoid NaN (log of a negative number) and inf (log 0)--breaks interior method
                       noIntersectInt [[x1, y1, s1], [x2, y2, s2]] = (x1 - x2)^2 + (y1 - y2)^2 - (s1 + s2)^2

                       -- interior point method: barrier functions
                       barrierFnInv = (\x -> 1 / (x + epsd))
                       barrierFnLog = (\x -> - (log (x + epsd))) -- the negative sign is why it works??
                       -- doesn't work if i flip the negatives between log and noIntersect...
                       -- i wonder if autodiff is having trouble with the dlog? 1/x -> -inf -> always the min

                       -- exterior point method: penalty function
                       penaltyFn = (\x -> (max x 0)^q) -- weights should get progressively larger in cr_dist
                       q = 2 -- also, may need to sample OUTSIDE feasible set

nonDifferentiable :: ObjFn2 a
nonDifferentiable sizes locs = let q = head locs in
                  -- max q 0
                  abs q

-- attempts to account for the radii of the objects
-- modified to try to be an interior point method with repel
-- currently, they repel each other "too much"--want them to be as centered as possible
-- not sure whether to use sqrt or not
-- try multiple objects?
-- TODO clean up this structure, which I've appropriated as a penalty function compiler...
centerAndRepel_dist :: (Floating a, Ord a) => a -> [a] -> [a] -> a
centerAndRepel_dist weight fixed varying =
                    (if objFnOn then
                        -- centerObjsNoSqrt fixed varying
                        -- repelCenter fixed varying
                        -- centerAndRepel fixed varying
                        nonDifferentiable fixed varying
                    else 0) 
                    + (if constraintFnOn then weight * (repelDist fixed varying) else 0)
       -- works, but doesn't take the sizes into account correctly
       -- the sum of squares should have a sqrt, but if i do that, the function will become negative
       -- should really be doing min _ 0 (need to add ord)
       -- where weight = 100 -- TODO factor out weight into config
-- but with the NaNs removed, now the log barrier function doesn't work??

doNothing :: ObjFn2 a -- for debugging
doNothing _ _ = 0

-- TODO these need separate pack/unpack functions because they change the sizes. these don't currently work
grow2 :: ObjFn2 a
grow2 _ [_, _, s1, _, _, s2] = 1 / (s1 + epsd) + 1 / (s2 + epsd)

grow :: ObjFn2 a
grow _ varying = sumMap (\x -> 1 / (x + epsd)) $ varying

-- TODO this needs to use the set size info. hardcode radii for now
-- TODO to use "min rad rad1" we need to add "Ord a" to the type signatures everywhere
-- we want the distance between the sets to be <overlap> less than having them just touch
-- this isn't working? and isn't resampling?
-- also i'm getting interval shrinking problems just with this function (using 'distance' only)
setsIntersect2 :: ObjFn2 a
setsIntersect2 sizes [x1, y1, x2, y2] = (dist (x1, y1) (x2, y2) - overlap)^2
              where overlap = rad + rad1 - 0.5 * rad1 -- should be "min rad rad1"

------ Objective function to place a label either inside of or right outside of a set

eps' :: Floating a => a
eps' = 60 -- why is this 100??

-- two parabolas, one at f(d) = d^2 and one at f(d) = (d-c)^2, intersecting at c/2
-- (i could try making the first one bigger and solving for the new intersection pt if i want the threshold
-- to be greater than (r + margin)/2

-- note: whenever an objective function has a partial derivative that might be fractional, 
-- and vary in the denominator, need to add epsilon to denominator to avoid 1/0, or avoid it altogether
-- e.g. f(x) = sqrt(x) -> f'(x) = 1/(2sqrt(x))
-- in the first branch, we square the distance, because the objective there is to minimize the distance (resulting in 1/0). 
-- in the second branch, the objective is to keep the distance at (r_set + margin), not at 0--so there’s no NaN in the denominator
centerOrRadParabola2 :: Bool -> ObjFn2 a 
centerOrRadParabola2 inSet [r_set, _] [x1, y1, x2, y2] =
                     if dsq <= r_set^2 then dsq
                     else (if inSet then dsq else coeff * (d - const)^2) -- false -> can lay it outside as well
                     where d = dist (x1, y1) (x2, y2) -- + epsd
                           dsq = distsq (x1, y1) (x2, y2) -- + epsd
                           coeff = r_set^2 / (r_set - const)^2 -- chosen s.t. parabolas intersect at r
                           const = r_set + margin -- second parabola's zero
                           margin = if r_set <= 30 then 30 else 60  -- distance from edge of set (as a fn of r)
                           -- we want r to be close to r_set+margin, otherwise if r is small it converges slowly?

-- NOTE: assumes that object and label are exactly contiguous in list: sizes of [o1, l1, o2, l2...]
-- and locs: [x_o1, y_o1, x_l1, y_l1, x_o2, y_o2, x_l2, y_l2...]
-- TODO abstract out repelDist / labelSum pattern 
labelSum :: Bool -> ObjFn2 a
labelSum inSet objLabelSizes objLabelLocs = 
               let objLabelSizes' = chunksOf 2 objLabelSizes in
               let objLabelLocs' = chunksOf 4 objLabelLocs in
               sumMap (\(sizes, locs) -> centerOrRadParabola2 inSet sizes locs) (zip objLabelSizes' objLabelLocs')

------

-- Start composing set-wise functions (centerAndRepel) with set-label functions (labelSum)
-- Sets repel each other, labels repel each other, and sets are labeled
-- TODO non-label sets should repel those labels
-- TODO resample initial state s.t. labels start inside the set (the centerOrRad is mostly useful if there are other objects inside the set that might repel the label)
-- TODO abstract out the unpacking functions here and factor out the weights
centerRepelLabel :: ObjFn2 a
centerRepelLabel olSizes olLocs =
                 centerAndRepel oSizes oLocs + weight * repelCenter lSizes lLocs + labelSum inSet olSizes olLocs
                 where (oSizes, lSizes) = (map fst zippedSizes, map snd zippedSizes)
                       zippedSizes = map (\[obj, lab] -> (obj, lab)) $ chunksOf 2 olSizes
                       (oLocs, lLocs) = (concatMap fst zippedLocs, concatMap snd zippedLocs)
                       zippedLocs = map (\[xo, yo, xl, yl] -> ([xo, yo], [xl, yl])) $ chunksOf 4 olLocs
                       weight = 10 ** 6
                       inSet = True -- label only in set vs. in or at radius
