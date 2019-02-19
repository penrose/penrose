module Plugins where

import qualified Data.Map.Strict as M

-- First entry: name of instantiator
-- Second entry: path to binary from src directory (which the plugins directory is in as well)
plugins = M.fromList [
        ("haskell-test", "plugins/haskell-instantiator/Main"),
        ("raytracing",   "PATH TODO"),
        ("ddgjs",        "PATH TODO")
       ]

main :: IO ()
main = do
     putStrLn "hello"
