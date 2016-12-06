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

data State = State { x :: Float
                   , y :: Float
                   , down :: Bool } -- left mouse button is down (dragging)

initState :: State
initState = State { x = 0, y = 0, down = False }

-- divide two integers to obtain a float
divf :: Int -> Int -> Float
divf a b = (fromIntegral a) / (fromIntegral b)

pw2 = picWidth `divf` 2
ph2 = picHeight `divf` 2

picOf :: State -> Picture
picOf s = Pictures [line', pt', objectiveTxt]
    where pt' = color (light violet) $ translate (x s) (y s) $ circleSolid 10
          line' = Line [(-pw2, 0), (pw2, 0)]
          objectiveTxt = translate (-pw2+50) (ph2-100) $ scale 0.2 0.2 $ text "objective function: f(x) = -x^2"

handler :: Event -> State -> State
handler (EventKey (MouseButton LeftButton) Down _ (xm, ym)) s = State { x = xm, y = y s, down = True }
-- dragging mouse when down
handler (EventMotion (xm, ym)) s =
        if down s then State {x = xm, y = y s, down = down s } else s
handler (EventKey (MouseButton LeftButton) Up _ (xm, ym)) s = State {x = xm, y = y s, down = False }
handler _ s = s

clamp :: Float -> Float
clamp x = if x < -pw2 then -pw2 else if x > pw2 then pw2 else x

-- implement gradient descent
-- TODO: is there a haskell autodifferentiator?
step :: Float -> State -> State
step t s = if down s then s -- don't step when dragging
           else State { x = clamp x', y = y s, down = down s}
                where x' = neg_parabola' (x s) t

-- objective function, differentiated and discretized
-- f(x) = x^2
parabola' :: Float -> Float -> Float
parabola' x t = x - t * 2 * x

neg_parabola' :: Float -> Float -> Float
neg_parabola' x t = x + t * 2 * x
