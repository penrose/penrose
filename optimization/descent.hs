import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game

main = play                    -- TODO change to play
       (InWindow "optimization-based layout" -- display mode, window name
                  (picWidth, picHeight)   -- size
                  (10, 10))    -- position
       white                   -- background color
       100                     -- number of simulation steps to take for each second of real time
       initState                   -- the initial world
       picOf                   -- fn to convert world to a pic
       handler                 -- fn to handle input events
       step                    -- step the world one iteration; is passed period of time (in secs) to be advanced

picWidth :: Int 
picWidth = 800

picHeight :: Int
picHeight = 700

-- gonna need typeclasses...
data Circ = Circ { xc :: Float
                     , yc :: Float
                     , r :: Float }

data Label = Label { xl :: Float
                   , yl :: Float
                   , textl :: String
                   , scalel :: Float } -- calculate h,w from it

data Obj = C Circ | L Label -- | Label | Point | Line

data State = State { objs :: [Obj]
                   , down :: Bool } -- left mouse button is down (dragging)

-- TODO randomly sample s0
initState :: State
initState = State { objs = objsInit, down = False }
          where objsInit = [C $ Circ { xc = -300, yc = 300, r = 30 },
                            L $ Label { xl = 500, yl = -500, textl = "CircLabel", scalel = 0.2 } ]

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 = picWidth `divf` 2
ph2 = picHeight `divf` 2

renderCirc :: Circ -> Picture
renderCirc c = color (light violet) $ translate (xc c) (yc c) $ circle (r c)

renderLabel :: Label -> Picture
renderLabel l = color azure $ scale 0.2 0.2 $ translate (xl l) (yl l) $ text (textl l)

renderObj :: Obj -> Picture
renderObj (C circ) = renderCirc circ
renderObj (L label) = renderLabel label

picOfState :: State -> Picture
picOfState s = Pictures $ map renderObj (objs s)

picOf :: State -> Picture
picOf s = Pictures [lineX, lineY, picOfState s, objectiveTxt]
    where lineX = Line [(-pw2, 0), (pw2, 0)]
          lineY = Line [(0, -ph2), (0, ph2)]
          objectiveTxt = translate (-pw2+50) (ph2-100) $ scale 0.2 0.2 
                         $ text "objective function: f(x, y) = x^2 + y^2"

-- TODO "in object" tests
-- TODO what if you press a key while down? then reset the entire state (then Up will just reset again)
handler :: Event -> State -> State
handler (EventKey (MouseButton LeftButton) Down _ (xm, ym)) s = 
        State { objs = objs s, down = True }
-- dragging mouse when down
handler (EventMotion (xm, ym)) s = 
        if down s then State { objs = objs s, down = down s }
        else s
handler (EventKey (MouseButton LeftButton) Up _ (xm, ym)) s = 
        State { objs = objs s, down = False }
handler _ s = s

-- TODO clamp needs to take into account bbox
clampX :: Float -> Float
clampX x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

clampY :: Float -> Float
clampY y = if y < -ph2 then -ph2 else if y > ph2 then ph2 else y

-- implement gradient descent
-- TODO: is there a haskell autodifferentiator?
-- TODO: step state
step :: Float -> State -> State
step t s = if down s then s -- don't step when dragging
           else State { objs = map (stepObj t) (objs s), down = down s}

-- currently ignores rest of state 
-- TODO typeclasses???
-- TODO differentiate type-level b/t timestep and coord
stepObj :: Float -> Obj -> Obj
stepObj t (C c) = C $ Circ { xc = x', yc = y', r = r c }
        where (x', y') = gradDescent t (xc c) (yc c)
stepObj t (L l) = L $ Label { xl = x', yl = y', textl = textl l, scalel = scalel l }
        where (x', y') = gradDescent t (xl l) (yl l)

gradDescent :: Float -> Float -> Float -> (Float, Float)
gradDescent t x y = (clampX x', clampY y')
            where x' = parabola' t x
                  y' = parabola' t y

-- objective function, differentiated and discretized
-- f(x) = x^2
parabola' :: Float -> Float -> Float
parabola' t x = x - t * 2 * x

neg_parabola' :: Float -> Float -> Float
neg_parabola' t x = x + t * 2 * x
