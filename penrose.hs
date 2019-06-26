-- | Main module of the Penrose system (logic is moved to ShadowMain for testing)
module Main
  ( main
  ) where

import           Penrose.ShadowMain (shadowMain)

-- | `main` runs the Penrose system
main :: IO ()
main = shadowMain
