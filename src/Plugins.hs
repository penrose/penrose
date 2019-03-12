module Plugins where

import qualified Data.Map.Strict as M

-- First entry: plugin name
-- Second entry: plugin directory (relative path OK), no trailing "/"
-- Third entry: command to run, relative to plugin directory

plugins :: M.Map String (String, String)
plugins = M.fromList [
        ("haskell-test", ("plugins/haskell-instantiator", "./Main")),
        ("ddgjs",        ("plugins/mesh-plugin", "node mesh-plugin-simple.js")),
        ("alloy",        ("plugins/alloy", "java -cp \"*:.\" AlloyPlugin")),
        ("raytracing",   ("TODO: path", "TODO: command"))
       ]
