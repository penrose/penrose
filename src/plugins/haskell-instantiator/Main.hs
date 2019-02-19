module Main where

import System.IO
import System.Directory

instantiate :: String -> String
instantiate x = "Point a"

main :: IO ()
main = do
     path <- getCurrentDirectory
     inFile <- makeAbsolute "Sub_enduser.sub"
     outFile <- makeAbsolute "Sub_instantiated.sub"

     putStrLn "-------"
     putStrLn $ "plugin dir path: " ++ path
     putStrLn $ "plugin reading from file: " ++ inFile
     subProg <- readFile inFile
     putStrLn "plugin received Sub prog: "
     putStrLn subProg

     let result = instantiate subProg

     putStrLn $ "plugin writing to file: " ++ outFile
     writeFile outFile result
     putStrLn "done"
     putStrLn "-------"
