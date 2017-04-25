{-# LANGUAGE RankNTypes #-}
import GHC.Float
import Numeric.AD

data Obj1 = Obj1 { p1 :: Float }
-- data Obj2 a = Obj2 { p2 :: Floating a => a }
data Obj2 a = Obj2 { p2 :: Floating a => a }

obj1Of c = Obj1 { p1 = c }

obj2Of :: a -> Obj2 a
obj2Of c = Obj2 { p2 = c }

-- lessons learned: Obj2 needs `a` param
-- should i move the typeclass constraint out of the type declaration or not?

-- how come i can do `let x = 10 :: Floating a => a; let y = x :: Float`
-- in ghci?? because taking an input that can be anything that satisfies floating
-- is different from a single param
f :: (Floating a) => a -> a
f x = p2 $ obj2Of x

g :: (Floating a) => a -> a
g x = realToFrac 2
-- but (2 :: Float) doesn't work...

-- let xf = x :: Float in
      -- (x :: Float) + (2 :: Float)
      -- x + auto 2
  
