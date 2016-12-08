import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game
import System.Random

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

-- TODO randomly sample s0
initState :: State
initState = State { objs = objsInit, down = False, rng = initRng }
          where objsInit = [c1, l1] -- only handles two objects!
                c1 = C $ Circ { xc = -300, yc = 200, r = rad, selc = False }
                c2 = C $ Circ { xc = 300, yc = -200, r = 50, selc = False }          
                l1 = L $ Label { xl = 10, yl = 0, textl = "B1", scalel = 0.2, sell = False }

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
                         $ text "objective function: f(x1, x2) = (x1-x2)^2, f(y1, y2) = (y1-y2)^2"

-- TODO 1D clamp flag
sample :: RandomGen g => g -> Obj -> (Obj, g)
sample gen o = (setX x' $ setY y' o, gen'')
       where (x', gen') = randomR widthRange gen
             (y', gen'') = randomR heightRange gen'

stateMap :: RandomGen g => g -> (g -> a -> (b, g)) -> [a] -> ([b], g)
stateMap gen f [] = ([], gen)
stateMap gen f (x:xs) = let (x', gen') = f gen x in
                        let (xs', gen'') = stateMap gen' f xs in
                        (x' : xs', gen'')

bbox = 60

-- hardcode bbox of 100x100 px at the center
-- text is centered at bottom left
inObj :: Located a => (Float, Float) -> a -> Bool
inObj (xm, ym) o = abs (xm - getX o) <= bbox && abs (ym - getY o) <= bbox

-- TODO "in object" tests
-- TODO what if you press a key while down? then reset the entire state (then Up will just reset again)
handler :: Event -> State -> State
handler (EventKey (MouseButton LeftButton) Down _ (xm, ym)) s =
        s { objs = objsFirstSelected, down = True }
        -- so that clicking doesn't select all overlapping objects in bbox
        where (objsFirstSelected, _) = foldl (selectFirstIfContains (xm, ym)) ([], False) (objs s)
              selectFirstIfContains (x, y) (xs, alreadySelected) o =
                                    if alreadySelected || (not $ inObj (x, y) o) then (o : xs, alreadySelected)
                                    else (select (setX xm $ setY ym o) : xs, True)
-- dragging mouse when down
handler (EventMotion (xm, ym)) s =
        if down s then s { objs = map (ifSelectedMoveTo (xm, ym)) (objs s), down = down s }
        else s
        where ifSelectedMoveTo (xm, ym) o = if selected o then setX xm $ setY ym o else o
handler (EventKey (MouseButton LeftButton) Up _ _) s =
        s { objs = map deselect $ objs s, down = False }
handler (EventKey (Char 'r') Up _ _) s =
        State { objs = objs', down = False, rng = rng' }
        where (objs', rng') = stateMap (rng s) sample (objs s)
handler _ s = s

-- TODO clamp needs to take into account bbox
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

-- TODO hack so i don't have to deal with pairwise derivatives of an arbitrary-length list
firstTwo :: [a] -> (a, a)
firstTwo (x1 : x2 : _) = (x1, x2)

-- implement gradient descent
-- TODO: is there a haskell autodifferentiator?
-- TODO: step state
type Time = Float
   
step :: Time -> State -> State
step t s = -- if down s then s -- don't step when dragging 
            s { objs = stepObjs t $ firstTwo (objs s), down = down s}

stepObjs :: Located a => Time -> (a, a) -> [a]
stepObjs t (o1, o2) = [setX x1'c $ setY y1'c o1, setX x2'c $ setY y2'c o2]
        where (x1, y1, x2, y2) = (getX o1, getY o1, getX o2, getY o2)
              (x1', x2', y1', y2') = cubicCenterOrRadius t x1 x2 y1 y2
              (x1'c, x2'c, y1'c, y2'c) = (clampX x1', clampX x2', clampY y1', clampY y2')

eps :: Float
eps = 100

c1 :: Float
c1 = rad - eps -- both need to be non-neg

c2 :: Float
c2 = rad + eps

stepT :: Time -> Float -> Float -> Float
stepT t x dx = x + t * dx

-- objective functions, differentiated and discretized
-- attract label to center of circle or to outside of circle
-- MAKE SURE x1 is the circle?? TODO
cubicCenterOrRadius :: Time -> Float -> Float -> Float -> Float -> (Float, Float, Float, Float)
cubicCenterOrRadius t x1 x2 y1 y2 = (stepT t x1 dx1, stepT t x2 dx2,
                                     stepT t y1 dy1, stepT t y2 dy2)
                    -- TODO factor out common parts and t
                    -- should these all be the same?
              where dx1 = c1 * c2 * ((x1 - x2)^2 + (y1 - y2)^2) - (c1 + c2) * ((x1 - x2)^2 + (y1 - y2)^2)^2 + ((x1 - x2)^2 + (y1 - y2)^2)^3
                    dx2 = c1 * c2 * ((x1 - x2)^2 + (y1 - y2)^2) - (c1 + c2) * ((x1 - x2)^2 + (y1 - y2)^2)^2 + ((x1 - x2)^2 + (y1 - y2)^2)^3
                    dy1 = c1 * c2 * ((x1 - x2)^2 + (y1 - y2)^2) - (c1 + c2) * ((x1 - x2)^2 + (y1 - y2)^2)^2 + ((x1 - x2)^2 + (y1 - y2)^2)^3
                    dy2 = c1 * c2 * ((x1 - x2)^2 + (y1 - y2)^2) - (c1 + c2) * ((x1 - x2)^2 + (y1 - y2)^2)^2 + ((x1 - x2)^2 + (y1 - y2)^2)^3

-- attract: f(x1, x2) = (x1-x2)^2
-- df/dx1 = 2(x1-x2), df/dx2 = -2(x1-x2)
distance1d :: Time -> Float -> Float -> (Float, Float)
distance1d t x1 x2 = (x1 - t * 2 * (x1 - x2), x2 + t * 2 * (x1 - x2))
-- x2 does not use the updated x1

-- repel
negdistance1d :: Time -> Float -> Float -> (Float, Float)
negdistance1d t x1 x2 = (x1 + t * 2 * (x1 - x2), x2 - t * 2 * (x1 - x2))   

-- f(x) = x^2
parabola' :: Time -> Float -> Float
parabola' t x = x - t * 2 * x

neg_parabola' :: Time -> Float -> Float
neg_parabola' t x = x + t * 2 * x
