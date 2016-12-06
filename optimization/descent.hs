import Graphics.Gloss
import Data.Function
import Graphics.Gloss.Interface.Pure.Game

main = play                    -- TODO change to play
       (InWindow "Nice Window" -- display mode, window name
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

picOf :: State -> Picture
picOf s = Pictures [line', pt']
    where pt' = color (light violet) $ translate (x s) (y s) $ circleSolid 10
          line' = Line [(-picWidth `divf` 2, 0), (picWidth `divf` 2, 0)]

handler :: Event -> State -> State
handler (EventKey (MouseButton LeftButton) Down _ (xm, ym)) s = State { x = xm, y = y s, down = True }
-- dragging mouse when down
handler (EventMotion (xm, ym)) s =
        if down s then State {x = xm, y = y s, down = down s } else s
handler (EventKey (MouseButton LeftButton) Up _ (xm, ym)) s = State {x = xm, y = y s, down = False }
handler _ s = s

-- TODO gradient descent here
step :: Float -> State -> State
step t s = if down s then s -- don't step when dragging
           else State { x = x s + 1, y = y s, down = down s}
